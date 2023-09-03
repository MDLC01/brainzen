//! # The lexer
//!
//! The *lexer* is responsible for converting a string of characters into Brainzen lexemes, each
//! represented by the [`Lexeme`] type. This conversion is achieved using the [`lex`] function.

use std::path::Path;

use crate::exceptions::{CompilationResult, LocatedException};
use crate::lexer::lexemes::Lexeme;
use crate::lexer::reader::Reader;
use crate::location::{Located, Sequence};

mod reader;
pub mod lexemes;


/// Reads a lexeme from a [`Reader`]. If no lexeme can be read due to the end of the input having
/// been reached, [`Ok(None)`] is returned.
fn read_lexeme(reader: &mut Reader) -> CompilationResult<Option<Located<Lexeme>>> {
    reader.advance();
    let location = reader.location();
    let lexeme = if reader.has_digit() {
        // Numeric literal
        let radix = if reader.eat_string("0x") { 16 } else { 10 };
        let mut value = 0;
        while let Some(digit) = reader.next_digit(radix) {
            value = value * radix + digit
        }
        Lexeme::Numeric(value)
    } else if reader.eat('\'') {
        // Character literal
        if reader.has('\'') {
            return Err(LocatedException::empty_character_literal(reader.location()));
        }
        let character = reader.expect_literal_character('\'')?.value;
        if !reader.eat('\'') {
            return Err(LocatedException::unterminated_character_literal(reader.location()));
        }
        Lexeme::Character(character)
    } else if reader.eat('"') {
        // String literal
        let mut characters = Vec::new();
        while !reader.eat('"') {
            characters.push(reader.expect_literal_character('"')?)
        };
        Lexeme::String(characters)
    } else if reader.has_word_character() {
        // Word
        let mut word = String::new();
        while let Some(c) = reader.next_word_character() {
            word.push(c)
        }
        Lexeme::Word(word)
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
        Lexeme::NativeCodeBlock(code)
    } else if let Some(symbol) = reader.next_symbol() {
        // Symbol
        Lexeme::Symbol(symbol)
    } else if !reader.can_read() {
        // End of file
        return Ok(None);
    } else {
        // Invalid character
        return Err(LocatedException::unexpected_character(location));
    };
    Ok(Some(Located::new(reader.location_from(&location), lexeme)))
}

/// Converts a string to a sequence of lexemes.
pub fn lex(source: impl AsRef<Path>, content: &str) -> CompilationResult<Sequence<Lexeme>> {
    let mut reader = Reader::new(content, source);
    let mut lexemes = Vec::new();
    while let Some(lexeme) = read_lexeme(&mut reader)? {
        lexemes.push(lexeme)
    }
    Ok(lexemes)
}
