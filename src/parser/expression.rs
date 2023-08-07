use crate::exceptions::{LocatedException, CompilationResult};
use crate::lexer::tokens::{Priority, Symbol};
use crate::location::{Located, LocatedResult, Sequence};
use crate::reference::Reference;
use crate::parser::token_stream::{Construct, TokenStream};
use crate::utils::extensions::VecExtensions;

/// An expression is a peace of code that can be evaluated.
#[derive(Debug)]
pub enum Expression {
    /// A character literal.
    CharacterLiteral(u8),
    /// An integer literal.
    IntegerLiteral(i32),
    /// A tuple literal.
    TupleLiteral(Sequence<Expression>),
    /// An array literal.
    ArrayLiteral(Sequence<Expression>),
    /// A reference to a variable.
    Variable(Located<Reference>),
    /// A unary arithmetic operation that operates on an other expression.
    UnaryArithmetic(Symbol, Located<Box<Expression>>),
    /// A binary arithmetic operation that operates on two other expressions.
    BinaryArithmetic(Symbol, Located<Box<Expression>>, Located<Box<Expression>>),
    /// An array indexing.
    Subscript { array: Located<Box<Expression>>, subscript: Located<Box<Expression>> },
    /// An array slicing.
    Slice { array: Located<Box<Expression>>, start: Located<Box<Expression>>, stop: Located<Box<Expression>> },
    /// A call to a function.
    FunctionCall(Reference, Sequence<Expression>),
}

impl Expression {
    fn tuple_from_characters(characters: &Sequence<u8>) -> Self {
        let elements = characters.iter()
            .map(|Located { location, value: character }| {
                let literal = Self::CharacterLiteral(*character);
                Located::new(location.clone(), literal)
            })
            .collect();
        Self::TupleLiteral(elements)
    }

    /// Parses an operand for a prefix or infix operation.
    fn locate_operand(tokens: &mut TokenStream) -> LocatedResult<Self> {
        let operand = tokens.parse_choice()
            // Parenthesized expression
            .branch(|tokens| {
                tokens.expect(Symbol::OpenParenthesis)?;
                let value = Self::locate_tuple(tokens)?.value;
                tokens.consume(Symbol::CloseParenthesis)?;
                Ok(value)
            })
            // Array literal
            .branch(|tokens| {
                tokens.expect(Symbol::OpenBracket)?;
                let elements = Expression::parse_delimited_separated_sequence(tokens, Symbol::Comma, Symbol::CloseBracket)?;
                Ok(Self::ArrayLiteral(elements))
            })
            // Function call
            .branch(|tokens| {
                let reference = Reference::parse(tokens)?;
                tokens.consume(Symbol::OpenParenthesis)?;
                let arguments = Expression::parse_delimited_separated_sequence(tokens, Symbol::Comma, Symbol::CloseParenthesis)?;
                Ok(Self::FunctionCall(reference, arguments))
            })
            // Variable
            .branch(|tokens| {
                let identifier = Reference::locate(tokens)?;
                Ok(Self::Variable(identifier))
            })
            // String literal
            .branch(|tokens| {
                let characters = tokens.read_string()?;
                Ok(Self::tuple_from_characters(&characters))
            })
            // Character literal
            .branch(|tokens| {
                let value = tokens.read_character()?;
                Ok(Self::CharacterLiteral(value))
            })
            // Integer literal
            .branch(|tokens| {
                let location = tokens.location();
                let value = tokens.read_integer()?;
                match value.try_into() {
                    Ok(value) => Ok(Self::IntegerLiteral(value)),
                    Err(_) => Err(LocatedException::invalid_char_literal(location))
                }
            })
            .locate("value")?;
        Self::locate_postfix_operations(tokens, operand)
    }

    /// Parses postfix operations for the passed operand.
    fn locate_postfix_operations(tokens: &mut TokenStream, operand: Located<Self>) -> LocatedResult<Self> {
        if tokens.eat(Symbol::OpenBracket) {
            let index = Self::locate_tuple(tokens)?;
            if tokens.eat(Symbol::CloseBracket) {
                // Subscript
                let location = operand.location.extended_to(&tokens.location());
                let subscript = Self::Subscript { array: operand.boxed(), subscript: index.boxed() };
                Self::locate_postfix_operations(tokens, Located::new(location, subscript))
            } else if tokens.eat(Symbol::Colon) {
                // Slice
                let location = operand.location.extended_to(&tokens.location());
                let index_stop = Self::locate_tuple(tokens)?;
                tokens.consume(Symbol::CloseBracket)?;
                let slice = Self::Slice { array: operand.boxed(), start: index.boxed(), stop: index_stop.boxed() };
                Self::locate_postfix_operations(tokens, Located::new(location, slice))
            } else {
                Err(LocatedException::expected(tokens.location(), format!("{} or {}", Symbol::CloseBracket, Symbol::Colon)))
            }
        } else {
            Ok(operand)
        }
    }

    fn locate_prefix_operation(tokens: &mut TokenStream) -> LocatedResult<Self> {
        let start_location = tokens.location();
        match tokens.read_unary_operator() {
            Some(operator) => {
                let operand = Self::locate_prefix_operation(tokens)?;
                let location = tokens.location_from(&start_location);
                Ok(Located::new(location, Expression::UnaryArithmetic(operator, operand.boxed())))
            }
            None => Self::locate_operand(tokens)
        }
    }

    fn locate_infix_operation(tokens: &mut TokenStream) -> LocatedResult<Self> {
        /// `current_left_operand` must have been determined to be higher priority than
        /// `current_priority`, which must be the priority of `current_operator`.
        fn aux(tokens: &mut TokenStream, current_left_operand: Located<Expression>, current_operator: Symbol, current_priority: Priority) -> LocatedResult<Expression> {
            let current_right_operand = Expression::locate_prefix_operation(tokens)?;
            match tokens.read_binary_operator() {
                Some((new_operator, new_priority)) => {
                    if current_priority <= new_priority {
                        let new_left_operand_location = current_left_operand.location.extended_to(&current_right_operand.location);
                        let new_left_operand = Expression::BinaryArithmetic(
                            current_operator,
                            current_left_operand.boxed(),
                            current_right_operand.boxed(),
                        );
                        aux(tokens, Located::new(new_left_operand_location, new_left_operand), new_operator, new_priority)
                    } else {
                        let new_right_operand = aux(tokens, current_right_operand, new_operator, new_priority)?;
                        let location = current_left_operand.location.extended_to(&new_right_operand.location);
                        let expression = Expression::BinaryArithmetic(
                            current_operator,
                            current_left_operand.boxed(),
                            new_right_operand.boxed(),
                        );
                        Ok(Located::new(location, expression))
                    }
                }
                None => {
                    let location = current_left_operand.location.extended_to(&current_right_operand.location);
                    let expression = Expression::BinaryArithmetic(
                        current_operator,
                        current_left_operand.boxed(),
                        current_right_operand.boxed(),
                    );
                    Ok(Located::new(location, expression))
                }
            }
        }
        let operand = Self::locate_prefix_operation(tokens)?;
        match tokens.read_binary_operator() {
            Some((operator, priority)) => aux(tokens, operand, operator, priority),
            None => Ok(operand)
        }
    }

    /// Parses and locates a tuple of one or more elements.
    ///
    /// This function parses an unparenthesized tuple. To force tuples to be parenthesized, use
    /// [`Expression::locate`].
    pub(super) fn locate_tuple(tokens: &mut TokenStream) -> LocatedResult<Self> {
        let start_location = tokens.location();
        let elements = Expression::parse_separated_sequence(tokens, Symbol::Comma)?;
        let location = tokens.location_from(&start_location);
        match elements.get_single_element_or_self() {
            Ok(value) => Ok(value),
            Err(elements) => Ok(Located::new(location, Self::TupleLiteral(elements)))
        }
    }
}

impl Construct for Expression {
    /// Parses an expression.
    ///
    /// This function does not allow unparenthesized tuples. To allow unparenthesized tuples, use
    /// [`Expression::locate_tuple`].
    fn parse(tokens: &mut TokenStream) -> CompilationResult<Self> {
        Self::locate_infix_operation(tokens).map(|expression| expression.value)
    }

    /// Parses and locates an expression.
    ///
    /// This function does not allow unparenthesized tuples. To allow unparenthesized tuples, use
    /// [`Expression::locate_tuple`].
    fn locate(tokens: &mut TokenStream) -> LocatedResult<Self> {
        Self::locate_infix_operation(tokens)
    }
}
