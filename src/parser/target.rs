use crate::exceptions::CompilationResult;
use crate::lexer::tokens::Symbol;
use crate::location::{Located, Sequence};
use crate::parser::expression::Expression;
use crate::parser::token_stream::{Construct, TokenStream};
use crate::reference::Reference;

/// A target is a pattern that can be used to unpack a value.
///
/// The simplest possible target is a single identifier.
#[derive(Debug)]
pub enum Target<D> {
    Destination(D),
    Tuple(Sequence<Target<D>>),
}

fn parse_tuple_target_element<D: Construct>(tokens: &mut TokenStream) -> CompilationResult<Target<D>> {
    if tokens.eat(Symbol::OpenParenthesis) {
        let target = Target::parse(tokens)?;
        tokens.expect(Symbol::CloseParenthesis)?;
        Ok(target)
    } else {
        let destination = D::parse(tokens)?;
        Ok(Target::Destination(destination))
    }
}

impl<D: Construct> Construct for Target<D> {
    fn parse(tokens: &mut TokenStream) -> CompilationResult<Self> {
        let mut elements = tokens.read_separated_items(Symbol::Comma, parse_tuple_target_element)?;
        if elements.len() == 1 {
            Ok(elements.remove(0).value())
        } else {
            Ok(Target::Tuple(elements))
        }
    }
}


#[derive(Debug)]
pub enum DefinitionTargetDestination {
    Variable(String),
}

impl Construct for DefinitionTargetDestination {
    fn parse(tokens: &mut TokenStream) -> CompilationResult<Self> {
        let identifier = tokens.read_word()?;
        Ok(Self::Variable(identifier))
    }
}

/// A definition target is what is on the left of the equal sign in a variable definition.
pub type DefinitionTarget = Target<DefinitionTargetDestination>;


#[derive(Debug)]
pub enum AssignmentTargetDestination {
    Variable(Reference),
    Subscript(Located<Box<AssignmentTargetDestination>>, Located<Expression>),
}

impl Construct for AssignmentTargetDestination {
    fn parse(tokens: &mut TokenStream) -> CompilationResult<Self> {
        let start_location = tokens.location();
        let reference = Reference::parse(tokens)?;
        let mut location = tokens.location_from(&start_location);
        let mut target = Self::Variable(reference);
        while tokens.eat(Symbol::OpenBracket) {
            let subscript = Expression::locate_tuple(tokens)?;
            tokens.consume(Symbol::CloseBracket)?;
            target = Self::Subscript(location.attach(target), subscript);
            location = tokens.location_from(&start_location)
        }
        Ok(target)
    }
}

/// An assignment target is what is on the left of the equal sign in an assignment.
pub type AssignmentTarget = Target<AssignmentTargetDestination>;
