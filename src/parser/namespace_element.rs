use crate::exceptions::CompilationResult;
use crate::lexer::tokens::{FUNC_KEYWORD, NAMESPACE_KEYWORD, NATIVE_KEYWORD, PUBLIC_KEYWORD, TYPE_KEYWORD};
use crate::lexer::tokens::Symbol;
use crate::location::{Located, Sequence};
use crate::parser::expression::Expression;
use crate::parser::statement::StatementBlock;
use crate::parser::token_stream::{Construct, TokenStream};
use crate::parser::type_descriptor::TypeDescriptor;
use crate::utils::Visibility;

/// An argument in a subroutine signature.
#[derive(Debug)]
pub struct SubroutineArgument {
    pub r#type: TypeDescriptor,
    pub name: String,
}

impl Construct for SubroutineArgument {
    fn parse(tokens: &mut TokenStream) -> CompilationResult<SubroutineArgument> {
        let identifier = tokens.read_word()?;
        tokens.expect(Symbol::Colon)?;
        let r#type = TypeDescriptor::parse(tokens)?;
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

/// Parses a subroutine header (identifier, arguments and return type).
fn parse_subroutine_header(tokens: &mut TokenStream) -> CompilationResult<(String, Sequence<SubroutineArgument>, Option<Located<TypeDescriptor>>)> {
    let identifier = tokens.read_word()?;
    tokens.consume(Symbol::OpenParenthesis)?;
    let arguments = SubroutineArgument::parse_delimited_separated_sequence(tokens, Symbol::Comma, Symbol::CloseParenthesis)?;
    let return_type = if tokens.eat(Symbol::Arrow) {
        Some(TypeDescriptor::locate(tokens)?)
    } else {
        None
    };
    Ok((identifier, arguments, return_type))
}

impl Construct for NamespaceElementHolder {
    /// Parses a full namespace element definition.
    fn parse(tokens: &mut TokenStream) -> CompilationResult<Self> {
        let visibility = if tokens.eat(PUBLIC_KEYWORD) {
            Visibility::Public
        } else {
            Visibility::Private
        };
        let (identifier, element) = tokens.parse_choice()
            // Constant
            .branch(|tokens| {
                tokens.expect(Symbol::Hash)?;
                let identifier = tokens.read_word()?;
                tokens.expect(Symbol::Equal)?;
                let value = Expression::locate(tokens)?;
                Ok((identifier, NamespaceElement::Constant(value)))
            })
            // Type alias
            .branch(|tokens| {
                tokens.expect(TYPE_KEYWORD)?;
                let identifier = tokens.read_word()?;
                tokens.expect(Symbol::Equal)?;
                let value = TypeDescriptor::parse(tokens)?;
                tokens.expect(Symbol::Semicolon)?;
                Ok((identifier, NamespaceElement::TypeAlias(value)))
            })
            // Subroutine
            .branch(|tokens| {
                tokens.expect(FUNC_KEYWORD)?;
                let (identifier, arguments, return_type) = parse_subroutine_header(tokens)?;
                let body = StatementBlock::locate(tokens)?;
                let subroutine = SubroutineBody::StatementBlock(body);
                Ok((identifier, NamespaceElement::Subroutine(arguments, return_type, subroutine)))
            })
            // Native subroutine
            .branch(|tokens| {
                tokens.expect(NATIVE_KEYWORD)?;
                tokens.consume(FUNC_KEYWORD)?;
                let (identifier, arguments, return_type) = parse_subroutine_header(tokens)?;
                tokens.consume(Symbol::Tilde)?;
                let offset = Expression::locate(tokens)?;
                let code = tokens.read_native_code_block()?;
                let body = SubroutineBody::Native { offset, code };
                Ok((identifier, NamespaceElement::Subroutine(arguments, return_type, body)))
            })
            // Namespace
            .branch(|tokens| {
                tokens.expect(NAMESPACE_KEYWORD)?;
                let identifier = tokens.read_word()?;
                tokens.consume(Symbol::OpenBrace)?;
                let elements = NamespaceElementHolder::parse_delimited_sequence(tokens, Symbol::CloseBrace)?;
                Ok((identifier, NamespaceElement::Namespace(elements)))
            })
            .parse("namespace element")?;
        Ok(Self { visibility, identifier, element })
    }
}
