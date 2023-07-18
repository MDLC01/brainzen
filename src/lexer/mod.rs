use std::path::Path;

use crate::exceptions::{CompilationException, CompilationResult};
use crate::lexer::reader::Reader;
use crate::lexer::tokens::Token;
use crate::location::{Located, Sequence};

pub mod tokens;
mod reader;


/// Reads a token from a [`Reader`]. If no token can be read due to the end of the input having been
/// reached, [`Ok(None)`] is returned.
fn read_token(reader: &mut Reader) -> CompilationResult<Option<Located<Token>>> {
    reader.advance();
    let location = reader.location();
    let token = if reader.has_digit() {
        // Numeric literal
        let radix = if reader.eat_string("0x") { 16 } else { 10 };
        let mut value = 0;
        while let Some(digit) = reader.eat_digit(radix) {
            value = value * radix + digit
        }
        Token::Numeric(value)
    } else if reader.eat('\'') {
        // Character literal
        if reader.has('\'') {
            return Err(CompilationException::empty_character_literal(reader.location()));
        }
        let Located(_, character) = reader.expect_literal_character('\'')?;
        if !reader.eat('\'') {
            return Err(CompilationException::unterminated_character_literal(reader.location()));
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
        // Word
        let mut word = String::new();
        while let Some(c) = reader.eat_word_character() {
            word.push(c)
        }
        Token::Word(word)
    } else if reader.eat('`') {
        // Native code
        let delimiter = if reader.eat_string("``") { "```" } else { "`" };
        let mut code = String::new();
        while !reader.eat_string(delimiter) {
            match reader.next() {
                Some(character) => code.push(character),
                None => return Err(CompilationException::unterminated_native_code_block(reader.location_from(location), delimiter)),
            }
        }
        Token::NativeCodeBlock(code)
    } else if let Some(symbol) = reader.eat_symbol() {
        // Symbol
        Token::Symbol(symbol)
    } else if !reader.can_read() {
        // End of file
        return Ok(None);
    } else {
        // Invalid character
        return Err(CompilationException::unexpected_character(location));
    };
    Ok(Some(Located(reader.location_from(location), token)))
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
