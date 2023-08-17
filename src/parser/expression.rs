use crate::exceptions::{CompilationResult, LocatedException};
use crate::lexer::tokens::{Priority, Symbol};
use crate::location::{Located, LocatedResult, Sequence};
use crate::parser::token_stream::{Construct, TokenStream};
use crate::reference::Reference;
use crate::utils::product::{MaybeProduct2, Product};

/// An expression is a peace of code that can be evaluated.
#[derive(Debug)]
pub enum Expression {
    /// The unit expression: `()`.
    Unit,
    /// A character literal.
    CharacterLiteral(u8),
    /// An integer literal.
    IntegerLiteral(i32),
    /// A tuple literal.
    TupleLiteral(Product<Located<Expression>, 2>),
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
    fn tuple_from_characters(characters: Sequence<u8>) -> Self {
        let elements = characters.into_iter()
            .map(|Located { location, value: character }| {
                let literal = Self::CharacterLiteral(character);
                Located::new(location, literal)
            })
            .collect();
        match elements {
            MaybeProduct2::None => Self::Unit,
            MaybeProduct2::Single(element) => element.value,
            MaybeProduct2::Product(elements) => Self::TupleLiteral(elements),
        }
    }

    /// Parses an operand for a prefix or infix operation.
    fn locate_operand(tokens: &mut TokenStream) -> LocatedResult<Self> {
        let start_location = tokens.location();
        let operand = if tokens.eat(Symbol::OpenParenthesis) {
            // Unit, parenthesized expression, or tuple
            let elements = Expression::parse_delimited_separated_sequence(tokens, Symbol::Comma, Symbol::CloseParenthesis)?;
            match elements.into() {
                MaybeProduct2::None => Self::Unit,
                MaybeProduct2::Single(element) => element.value,
                MaybeProduct2::Product(elements) => Self::TupleLiteral(elements),
            }
        } else if tokens.eat(Symbol::OpenBracket) {
            // Array literal
            let elements = Expression::parse_delimited_separated_sequence(tokens, Symbol::Comma, Symbol::CloseBracket)?;
            Self::ArrayLiteral(elements)
        } else if let Some(reference) = tokens.eat_located_reference_with(Symbol::OpenParenthesis) {
            // Function call
            let arguments = Expression::parse_delimited_separated_sequence(tokens, Symbol::Comma, Symbol::CloseParenthesis)?;
            Self::FunctionCall(reference.value, arguments)
        } else if let Some(reference) = tokens.eat_located_reference() {
            // Variable
            Self::Variable(reference)
        } else if let Some(value) = tokens.eat_integer() {
            // Integer literal
            match value.try_into() {
                Ok(value) => Self::IntegerLiteral(value),
                Err(_) => {
                    return Err(LocatedException::integer_literal_too_large(tokens.previous_location()));
                }
            }
        } else if let Some(value) = tokens.eat_character() {
            // Character literal
            Self::CharacterLiteral(value)
        } else if let Some(characters) = tokens.eat_string() {
            // String literal
            Self::tuple_from_characters(characters)
        } else {
            return Err(LocatedException::expected_expression(start_location));
        };
        let location = tokens.location_from(&start_location);
        Self::locate_postfix_operations(tokens, Located::new(location, operand))
    }

    /// Parses postfix operations for the passed operand.
    fn locate_postfix_operations(tokens: &mut TokenStream, operand: Located<Self>) -> LocatedResult<Self> {
        if tokens.eat(Symbol::OpenBracket) {
            let index = Self::locate(tokens)?;
            if tokens.eat(Symbol::CloseBracket) {
                // Subscript
                let location = operand.location.extended_to(&tokens.location());
                let subscript = Self::Subscript { array: operand.boxed(), subscript: index.boxed() };
                Self::locate_postfix_operations(tokens, Located::new(location, subscript))
            } else if tokens.eat(Symbol::Colon) {
                // Slice
                let location = operand.location.extended_to(&tokens.location());
                let index_stop = Self::locate(tokens)?;
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
        match tokens.eat_unary_operator() {
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
            match tokens.eat_binary_operator() {
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
        match tokens.eat_binary_operator() {
            Some((operator, priority)) => aux(tokens, operand, operator, priority),
            None => Ok(operand)
        }
    }
}

impl Construct for Expression {
    fn parse(tokens: &mut TokenStream) -> CompilationResult<Self> {
        Self::locate_infix_operation(tokens).map(|expression| expression.value)
    }

    fn locate(tokens: &mut TokenStream) -> LocatedResult<Self> {
        Self::locate_infix_operation(tokens)
    }
}
