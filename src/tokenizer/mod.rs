//! # The tokenizer
//!
//! The *tokenizer* converts a sequence of [lexemes](crate::lexer) into a sequence of tokens, each
//! represented by the [`Token`] type.
//! This can be done using the [`tokenize`] function.

use crate::exceptions::{CompilationResult, LocatedException};
use crate::lexer::lexemes::{Lexeme, Symbol};
use crate::location::{Located, Sequence};
use crate::reference::Reference;
use crate::tokenizer::tokens::{BracketKind, Keyword, Token};

pub mod tokens;


#[derive(Debug)]
struct LexemeReader {
    lexemes: Sequence<Lexeme>,
    index: usize,
}

impl LexemeReader {
    pub fn new(lexemes: Sequence<Lexeme>) -> Self {
        Self {
            lexemes,
            index: 0,
        }
    }

    fn peek(&self, offset: usize) -> Option<Located<Lexeme>> {
        self.lexemes.get(self.index + offset).cloned()
    }

    pub fn advance(&mut self) {
        self.index += 1;
    }

    pub fn backtrack(&mut self) {
        self.index -= 1;
    }

    pub fn next(&mut self) -> Option<Located<Lexeme>> {
        if let Some(lexeme) = self.peek(0) {
            self.advance();
            Some(lexeme)
        } else {
            None
        }
    }

    /// If the next lexeme is a [`Symbol::DoubleColon`], followed by a word, advances the reader and
    /// returns the word with its location. Otherwise, returns [`None`] without advancing the
    /// reader.
    pub fn next_reference_part(&mut self, symbol: Symbol) -> Option<Located<String>> {
        if self.peek(0).is_some_and(|lexeme| lexeme.value == Lexeme::Symbol(symbol)) {
            if let Some(Located { location, value: Lexeme::Word(word) }) = self.peek(1) {
                self.advance();
                self.advance();
                Some(Located::new(location, word))
            } else {
                None
            }
        } else {
            None
        }
    }
}


/// Reads the next token from the specified reader. If no more token can be read, returns [`None`].
fn read_next_token(reader: &mut LexemeReader) -> CompilationResult<Option<Located<Token>>> {
    let Some(lexeme) = reader.next() else {
        return Ok(None);
    };
    match lexeme.value {
        Lexeme::Symbol(symbol) => {
            if let Some(brackets) = BracketKind::from_opening(symbol) {
                let tokens = read_tokens(reader)?;
                let Some(closing) = reader.next() else {
                    return Err(LocatedException::unmatched_bracket(lexeme.location, brackets));
                };
                if closing.value != Lexeme::Symbol(brackets.get_closing()) {
                    return Err(LocatedException::unmatched_bracket(lexeme.location, brackets));
                }
                let location = lexeme.location.extended_to(&closing.location);
                Ok(Some(Located::new(location, Token::Parenthesized(brackets, tokens))))
            } else if BracketKind::is_closing(symbol) {
                reader.backtrack();
                Ok(None)
            } else {
                Ok(Some(Located::new(lexeme.location, Token::Symbol(symbol))))
            }
        }
        Lexeme::Word(word) => {
            if let Some(keyword) = Keyword::from(&word) {
                Ok(Some(Located::new(lexeme.location, Token::Keyword(keyword))))
            } else {
                let mut location = lexeme.location;
                let mut reference = Reference::new_identifier(word);
                while let Some(identifier) = reader.next_reference_part(Symbol::DoubleColon) {
                    let new_location = location.extended_to(&identifier.location);
                    reference = Reference::new(identifier.value, Located::new(location, reference));
                    location = new_location;
                }
                Ok(Some(Located::new(location, Token::Reference(reference))))
            }
        }
        Lexeme::Numeric(value) => {
            match value.try_into() {
                Ok(value) => {
                    Ok(Some(Located::new(lexeme.location, Token::Numeric(value))))
                }
                Err(_) => {
                    Err(LocatedException::integer_literal_too_large(lexeme.location))
                }
            }
        }
        Lexeme::Character(character) => {
            Ok(Some(Located::new(lexeme.location, Token::Character(character as u8))))
        }
        Lexeme::String(string) => {
            let characters = string.into_iter()
                .map(|character| {
                    character.map(|c| c as u8)
                })
                .collect();
            Ok(Some(Located::new(lexeme.location, Token::String(characters))))
        }
        Lexeme::NativeCodeBlock(block) => {
            Ok(Some(Located::new(lexeme.location, Token::NativeCodeBlock(block))))
        }
    }
}

fn read_tokens(reader: &mut LexemeReader) -> CompilationResult<Sequence<Token>> {
    let mut tokens = Vec::new();
    while let Some(token) = read_next_token(reader)? {
        tokens.push(token)
    }
    Ok(tokens)
}

/// Converts a sequence of [`Lexeme`] to a sequence of [`Token`].
pub fn tokenize(lexemes: Sequence<Lexeme>) -> CompilationResult<Sequence<Token>> {
    read_tokens(&mut LexemeReader::new(lexemes))
}
