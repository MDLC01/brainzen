//! # The parser
//!
//! The *parser* converts a sequence of [tokens](crate::tokenizer) into an abstract syntax tree.
//! This is done using the [`parse_file`] function.

use std::path::Path;

use crate::exceptions::CompilationResult;
use crate::location::Sequence;
use crate::parser::namespace_element::NamespaceElementHolder;
use crate::parser::token_stream::{Construct, TokenStream};
use crate::tokenizer::tokens::Token;

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

impl Construct for BrainzenFile {
    fn read(tokens: &mut TokenStream) -> CompilationResult<Self> {
        let elements = Sequence::read(tokens)?;
        Ok(BrainzenFile(elements))
    }
}


/// Generates the abstract syntax tree from the tokens of a Brainzen program.
pub fn parse_file(file: impl AsRef<Path>, tokens: Sequence<Token>) -> CompilationResult<BrainzenFile> {
    let mut token_stream = TokenStream::new(file, tokens);
    BrainzenFile::read(&mut token_stream)
}
