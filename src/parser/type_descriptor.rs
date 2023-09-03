use crate::exceptions::CompilationResult;
use crate::lexer::lexemes::Symbol;
use crate::location::{Located, LocatedResult, Sequence};
use crate::parser::token_stream::{Construct, TokenStream};
use crate::reference::Reference;

/// A type descriptor is a sort of expression that corresponds to a type instead of a runtime value.
#[derive(Debug)]
pub enum TypeDescriptor {
    Unit,
    Reference(Located<Reference>),
    Product(Sequence<TypeDescriptor>),
    Array(Located<Box<TypeDescriptor>>),
}

impl TypeDescriptor {
    fn parse_operand(tokens: &mut TokenStream) -> CompilationResult<Self> {
        if tokens.eat(Symbol::OpenParenthesis) {
            if tokens.eat(Symbol::CloseParenthesis) {
                Ok(Self::Unit)
            } else {
                let descriptor = TypeDescriptor::read(tokens)?;
                tokens.expect(Symbol::CloseParenthesis)?;
                Ok(descriptor)
            }
        } else {
            let reference = tokens.read_located_reference()?;
            Ok(Self::Reference(reference))
        }
    }

    fn parse_factor(tokens: &mut TokenStream) -> LocatedResult<Self> {
        let start_location = tokens.location();
        let mut operand = Self::parse_operand(tokens)?;
        let mut location = tokens.location_from(&start_location);
        while tokens.eat(Symbol::OpenBracket) {
            tokens.consume(Symbol::CloseBracket)?;
            operand = Self::Array(location.attach(operand));
            location = tokens.location_from(&start_location);
        }
        Ok(Located::new(location, operand))
    }
}

impl Construct for TypeDescriptor {
    fn read(tokens: &mut TokenStream) -> CompilationResult<Self> {
        let mut factors = tokens.read_separated_items(Symbol::Star, Self::parse_factor)?;
        if factors.len() == 1 {
            Ok(factors.remove(0).value)
        } else {
            Ok(Self::Product(factors))
        }
    }
}
