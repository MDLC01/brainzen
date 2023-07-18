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
    let elements = NamespaceElementHolder::parse_sequence(&mut token_stream)?;
    Ok(BrainzenFile(elements))
}
