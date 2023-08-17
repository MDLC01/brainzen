//! # The lexer
//!
//! The *lexer* is responsible for converting a string of characters into Brainzen tokens,
//! represented by the [`Token`] type. This conversion is achieved using the [`tokenize`] function.

use std::path::Path;

use crate::exceptions::{CompilationResult, LocatedException};
use crate::lexer::reader::Reader;
use crate::lexer::tokens::Token;
use crate::location::{Located, Sequence};
use crate::reference::Reference;

pub mod tokens;
mod reader;


/// Reads a single word.
///
/// The word might be empty.
fn locate_word(reader: &mut Reader) -> String {
    let mut word = String::new();
    while let Some(c) = reader.next_word_character() {
        word.push(c)
    }
    word
}


/// Reads a token from a [`Reader`]. If no token can be read due to the end of the input having been
/// reached, [`Ok(None)`] is returned.
fn read_token(reader: &mut Reader) -> CompilationResult<Option<Located<Token>>> {
    reader.advance();
    let location = reader.location();
    let token = if reader.has_digit() {
        // Numeric literal
        let radix = if reader.eat_string("0x") { 16 } else { 10 };
        let mut value = 0;
        while let Some(digit) = reader.next_digit(radix) {
            value = value * radix + digit
        }
        Token::Numeric(value)
    } else if reader.eat('\'') {
        // Character literal
        if reader.has('\'') {
            return Err(LocatedException::empty_character_literal(reader.location()));
        }
        let character = reader.expect_literal_character('\'')?.value;
        if !reader.eat('\'') {
            return Err(LocatedException::unterminated_character_literal(reader.location()));
        }
        Token::Character(character)
    } else if reader.eat('"') {
        // String literal
        let mut characters = Vec::new();
        while !reader.eat('"') {
            characters.push(reader.expect_literal_character('"')?)
        };
        Token::String(characters)
    } else if reader.has_word_character() {
        // Reference
        // References are parsed at the lexer level because it makes the parser LL2.
        let start_location = reader.location();
        let word = locate_word(reader);
        let mut reference = Reference::new_identifier(word);
        while reader.eat_string("::") {
            let location = reader.location_from(&start_location);
            let identifier = locate_word(reader);
            reference = Reference::new(identifier, Located::new(location, reference));
        }
        Token::Reference(reference)
    } else if reader.eat('`') {
        // Native code
        let mut delimiter_size = 1;
        while reader.eat('`') {
            delimiter_size += 1;
        }
        let delimiter = "`".repeat(delimiter_size);
        let mut code = String::new();
        while !reader.eat_string(&delimiter) {
            match reader.next() {
                Some(character) => code.push(character),
                None => return Err(LocatedException::unterminated_native_code_block(reader.location_from(&location), &delimiter)),
            }
        }
        Token::NativeCodeBlock(code)
    } else if let Some(symbol) = reader.next_symbol() {
        // Symbol
        Token::Symbol(symbol)
    } else if !reader.can_read() {
        // End of file
        return Ok(None);
    } else {
        // Invalid character
        return Err(LocatedException::unexpected_character(location));
    };
    Ok(Some(Located::new(reader.location_from(&location), token)))
}

/// Converts a string to a sequence of tokens.
pub fn tokenize(source: impl AsRef<Path>, content: &str) -> CompilationResult<Sequence<Token>> {
    let mut reader = Reader::new(content, source);
    let mut tokens = Vec::new();
    while let Some(token) = read_token(&mut reader)? {
        tokens.push(token)
    }
    Ok(tokens)
}
