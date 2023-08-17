use std::path::Path;

use crate::exceptions::CompilationResult;
use crate::lexer::tokens::Token;
use crate::location::Sequence;
use crate::parser::namespace_element::NamespaceElementHolder;
use crate::parser::token_stream::{Construct, TokenStream};

mod token_stream;
pub mod type_descriptor;
pub mod namespace_element;
pub mod statement;
pub mod expression;
pub mod target;


pub struct BrainzenFile(Sequence<NamespaceElementHolder>);

impl BrainzenFile {
    pub fn elements(self) -> Sequence<NamespaceElementHolder> {
        self.0
    }
}


pub fn parse_file(file: impl AsRef<Path>, tokens: Sequence<Token>) -> CompilationResult<BrainzenFile> {
    let mut token_stream = TokenStream::new(file, tokens);
    let elements = Sequence::parse(&mut token_stream)?;
    Ok(BrainzenFile(elements))
}


/// The `public` keyword.
pub const PUBLIC_KEYWORD: &str = "public";
/// The `type` keyword.
pub const TYPE_KEYWORD: &str = "type";
/// The `native` keyword.
pub const NATIVE_KEYWORD: &str = "native";
/// The `func` keyword.
pub const FUNC_KEYWORD: &str = "func";
/// The `namespace` keyword.
pub const NAMESPACE_KEYWORD: &str = "namespace";
/// The `loop` keyword.
pub const LOOP_KEYWORD: &str = "loop";
/// The `while` keyword.
pub const WHILE_KEYWORD: &str = "while";
/// The `do` keyword.
pub const DO_KEYWORD: &str = "do";
/// The `if` keyword.
pub const IF_KEYWORD: &str = "if";
/// The `else` keyword.
pub const ELSE_KEYWORD: &str = "else";
/// The `let` keyword.
pub const LET_KEYWORD: &str = "let";
/// The `return` keyword.
pub const RETURN_KEYWORD: &str = "return";
