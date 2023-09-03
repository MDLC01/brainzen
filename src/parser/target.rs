use crate::exceptions::CompilationResult;
use crate::lexer::lexemes::Symbol;
use crate::location::Located;
use crate::parser::expression::Expression;
use crate::parser::token_stream::{Construct, TokenStream};
use crate::tokenizer::tokens::BracketKind;
use crate::utils::product::{MaybeProduct2, Product};

/// A target is a pattern that can be used to unpack a value.
///
/// The simplest possible target is a single identifier.
#[derive(Debug)]
pub enum Target<D> {
    Unit,
    Destination(D),
    Tuple(Product<Located<Target<D>>, 2>),
}

impl<D: Construct> Construct for Target<D> {
    fn read(tokens: &mut TokenStream) -> CompilationResult<Self> {
        if let Some(body) = tokens.next_parenthesized(BracketKind::Round) {
            let elements = Self::parse_separated_sequence(body, Symbol::Comma)?;
            match elements.into() {
                MaybeProduct2::None => Ok(Self::Unit),
                MaybeProduct2::Single(element) => Ok(element.value),
                MaybeProduct2::Product(elements) => Ok(Self::Tuple(elements)),
            }
        } else {
            let destination = D::read(tokens)?;
            Ok(Self::Destination(destination))
        }
    }
}


#[derive(Debug)]
pub enum DefinitionTargetDestination {
    Variable(String),
}

impl Construct for DefinitionTargetDestination {
    fn read(tokens: &mut TokenStream) -> CompilationResult<Self> {
        let identifier = tokens.read_identifier()?;
        Ok(Self::Variable(identifier))
    }
}

/// A definition target is what is on the left of the equal sign in a variable definition.
pub type DefinitionTarget = Target<DefinitionTargetDestination>;


#[derive(Debug)]
pub enum AssignmentTargetDestination {
    Variable(String),
    Subscript(Located<Box<AssignmentTargetDestination>>, Located<Expression>),
}

impl Construct for AssignmentTargetDestination {
    fn read(tokens: &mut TokenStream) -> CompilationResult<Self> {
        let start_location = tokens.location();
        let identifier = tokens.read_identifier()?;
        let mut location = tokens.location_from(&start_location);
        let mut target = Self::Variable(identifier);
        while tokens.eat(Symbol::OpenBracket) {
            let subscript = Expression::locate(tokens)?;
            tokens.consume(Symbol::CloseBracket)?;
            target = Self::Subscript(location.attach(target), subscript);
            location = tokens.location_from(&start_location)
        }
        Ok(target)
    }
}

/// An assignment target is what is on the left of the equal sign in an assignment.
pub type AssignmentTarget = Target<AssignmentTargetDestination>;
