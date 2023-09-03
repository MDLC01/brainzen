use crate::exceptions::{CompilationResult, LocatedException};
use crate::lexer::lexemes::Symbol;
use crate::location::{Located, Sequence};
use crate::parser::expression::Expression;
use crate::parser::statement::StatementBlock;
use crate::parser::token_stream::{Construct, TokenStream};
use crate::parser::type_descriptor::TypeDescriptor;
use crate::tokenizer::tokens::{BracketKind, Keyword};
use crate::utils::Visibility;

/// An argument in a subroutine signature.
#[derive(Debug)]
pub struct SubroutineArgument {
    pub r#type: TypeDescriptor,
    pub name: String,
}

impl Construct for SubroutineArgument {
    fn read(tokens: &mut TokenStream) -> CompilationResult<SubroutineArgument> {
        let identifier = tokens.read_identifier()?;
        tokens.expect(Symbol::Colon)?;
        let r#type = TypeDescriptor::read(tokens)?;
        Ok(SubroutineArgument { name: identifier, r#type })
    }
}


/// The body  of a subroutine: either some statements, or native code.
#[derive(Debug)]
pub enum SubroutineBody {
    StatementBlock(Located<StatementBlock>),
    Native { offset: Located<Expression>, code: String },
}


/// The content of a namespace element. Metadata is stored in [`NamespaceElementHolder`].
#[derive(Debug)]
pub enum NamespaceElement {
    Constant(Located<Expression>),
    TypeAlias(TypeDescriptor),
    Subroutine(Sequence<SubroutineArgument>, Option<Located<TypeDescriptor>>, SubroutineBody),
    Namespace(Sequence<NamespaceElementHolder>),
}


/// A [`NamespaceElement`] alongside some metadata like its visibility and identifier.
#[derive(Debug)]
pub struct NamespaceElementHolder {
    pub visibility: Visibility,
    pub identifier: String,
    pub element: NamespaceElement,
}

/// Reads a subroutine header (identifier, arguments and return type).
fn read_subroutine_header(tokens: &mut TokenStream) -> CompilationResult<(String, Sequence<SubroutineArgument>, Option<Located<TypeDescriptor>>)> {
    let identifier = tokens.read_identifier()?;
    let Some(body) = tokens.next_parenthesized(BracketKind::Round) else {
        return Err(LocatedException::expected_subroutine_argument_declaration(tokens.location()));
    };
    let arguments = SubroutineArgument::parse_separated_sequence(body, Symbol::Comma)?;
    let return_type = if tokens.eat(Symbol::Arrow) {
        Some(TypeDescriptor::locate(tokens)?)
    } else {
        None
    };
    Ok((identifier, arguments, return_type))
}

impl Construct for NamespaceElementHolder {
    /// Reads a full namespace element definition.
    fn read(tokens: &mut TokenStream) -> CompilationResult<Self> {
        let visibility = if tokens.eat(Keyword::Public) {
            Visibility::Public
        } else {
            Visibility::Private
        };
        let (identifier, element) = if tokens.eat(Symbol::Hash) {
            // Constant
            let identifier = tokens.read_identifier()?;
            tokens.expect(Symbol::Equal)?;
            let value = Expression::locate(tokens)?;
            (identifier, NamespaceElement::Constant(value))
        } else if tokens.eat(Keyword::Type) {
            // Type alias
            let identifier = tokens.read_identifier()?;
            tokens.expect(Symbol::Equal)?;
            let value = TypeDescriptor::read(tokens)?;
            tokens.expect(Symbol::Semicolon)?;
            (identifier, NamespaceElement::TypeAlias(value))
        } else if tokens.eat(Keyword::Func) {
            // Subroutine
            let (identifier, arguments, return_type) = read_subroutine_header(tokens)?;
            let body = StatementBlock::locate(tokens)?;
            let subroutine = SubroutineBody::StatementBlock(body);
            (identifier, NamespaceElement::Subroutine(arguments, return_type, subroutine))
        } else if tokens.eat(Keyword::Native) {
            // Native subroutine
            tokens.consume(Keyword::Func)?;
            let (identifier, arguments, return_type) = read_subroutine_header(tokens)?;
            tokens.consume(Symbol::Tilde)?;
            let offset = Expression::locate(tokens)?;
            let code = tokens.read_native_code_block()?;
            let body = SubroutineBody::Native { offset, code };
            (identifier, NamespaceElement::Subroutine(arguments, return_type, body))
        } else if tokens.eat(Keyword::Namespace) {
            // Namespace
            let identifier = tokens.read_identifier()?;
            let Some(body) = tokens.next_parenthesized(BracketKind::Curly) else {
                return Err(LocatedException::expected_namespace_body(tokens.location()));
            };
            let elements = NamespaceElementHolder::parse_sequence(body)?;
            (identifier, NamespaceElement::Namespace(elements))
        } else {
            return Err(LocatedException::expected_namespace_element(tokens.location()));
        };
        Ok(Self { visibility, identifier, element })
    }
}
