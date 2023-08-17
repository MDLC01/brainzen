use std::mem;
use std::path::Path;

use crate::exceptions::{LocatedException, CompilationResult};
use crate::lexer::tokens::Symbol;
use crate::location::{Located, Location};

/// A reader can be used to read Brainzen source code.
#[derive(Debug)]
pub struct Reader {
    chars: Vec<char>,
    cursor: usize,
    previous_location: Location,
    location: Location,
}

impl Reader {
    /// Creates a new reader from the content of a file.
    pub fn new(content: &str, source: impl AsRef<Path>) -> Self {
        let location = Location::start_of_file(source);
        Self {
            chars: content.chars().collect(),
            cursor: 0,
            previous_location: location.clone(),
            location,
        }
    }

    /// Updates the current location to the passed location.
    fn update_location(&mut self, location: Location) {
        self.previous_location = mem::replace(&mut self.location, location);
    }

    /// Returns the current location of the reader.
    pub fn location(&self) -> Location {
        self.location.clone()
    }

    /// Returns a location that starts at the start of the specified `start_location` and ends at
    /// the end of the previous location.
    pub fn location_from(&self, start_location: &Location) -> Location {
        start_location.extended_to(&self.previous_location)
    }

    /// Returns the character at a specific offset from the current position of the reader.
    fn peek_at(&self, offset: usize) -> Option<char> {
        self.chars.get(self.cursor + offset).copied()
    }

    /// Returns a copy of the next character.
    pub fn peek(&self) -> Option<char> {
        self.peek_at(0)
    }

    /// Tests if there are remaining characters to be read.
    pub fn can_read(&self) -> bool {
        self.peek().is_some()
    }

    /// Reads the next character. This operation has the side effect of advancing the reader and
    /// updating its location.
    pub fn next(&mut self) -> Option<char> {
        let next = self.peek();
        self.cursor += 1;
        match next {
            Some('\n') => {
                self.update_location(self.location.next_line());
            }
            Some(_) => {
                self.update_location(self.location.next_column());
            }
            _ => ()
        };
        next
    }

    /// Skips the next character.
    pub fn skip(&mut self) {
        self.next();
    }

    /// Skips a specific amount of characters.
    pub fn skip_amount(&mut self, amount: usize) {
        for _ in 0..amount {
            self.skip()
        }
    }

    /// Tests if the character at a specific offset from the current position of the reader is equal
    /// to the passed character.
    fn has_at(&self, offset: usize, character: char) -> bool {
        match self.peek_at(offset) {
            Some(c) => c == character,
            None => false,
        }
    }

    /// Tests if the next character is equal to the passed character.
    pub fn has(&self, character: char) -> bool {
        self.has_at(0, character)
    }

    /// Tests if the next characters are equal to the characters of the passed string.
    pub fn has_string(&self, string: &str) -> bool {
        string.chars()
            .enumerate()
            .all(|(offset, character)| self.has_at(offset, character))
    }

    /// Skips the next character and returns `true` if, and only if, the next character is equal to
    /// the passed character.
    pub fn eat(&mut self, character: char) -> bool {
        if self.has(character) {
            self.skip();
            true
        } else {
            false
        }
    }

    /// Skips the next characters and returns `true` if, and only if, they are equal to the
    /// characters of the passed string.
    pub fn eat_string(&mut self, string: &str) -> bool {
        if self.has_string(string) {
            self.skip_amount(string.chars().count());
            true
        } else {
            false
        }
    }

    /// Tests if the next character is a whitespace, according to [`char::is_whitespace`].
    pub fn has_whitespace(&self) -> bool {
        match self.peek() {
            Some(c) => c.is_whitespace(),
            None => false,
        }
    }

    /// Skips all characters until the next non-whitespace character, according to
    /// [`char::is_whitespace`].
    pub fn skip_whitespace(&mut self) {
        while self.has_whitespace() {
            self.skip()
        }
    }

    /// Skips all characters until, and including, the next newline (`'\n'`) character.
    pub fn skip_line(&mut self) {
        while !self.eat('\n') {
            self.skip()
        }
    }

    /// Skips whitespace and comments.
    pub fn advance(&mut self) {
        let mut inline_comment_depth = 0;
        loop {
            self.skip_whitespace();
            if self.eat_string("/*") {
                inline_comment_depth += 1
            } else if inline_comment_depth > 0 {
                if self.eat_string("*/") {
                    inline_comment_depth -= 1
                } else {
                    self.skip()
                }
            } else if self.eat_string("//") {
                self.skip_line()
            } else {
                break;
            }
        }
    }

    /// Tests if the next character is a digit (`'0'..='9'`).
    pub fn has_digit(&self) -> bool {
        matches!(self.peek(), Some('0'..='9'))
    }

    /// Tests if a character is allowed in a word.
    ///
    /// A character is allowed in a word if it is [alphanumeric](char::is_alphanumeric) or an
    /// underscore (`'_'`).
    pub fn is_word_character(c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    /// Tests if the next character is a word character, according to [`Self::is_word_character`].
    pub fn has_word_character(&self) -> bool {
        match self.peek() {
            Some(c) => Self::is_word_character(c),
            None => false,
        }
    }

    /// Skips the next character and returns it if, and only if, it is a word character, according
    /// to [`Self::is_word_character`].
    pub fn next_word_character(&mut self) -> Option<char> {
        match self.peek() {
            Some(c) if Self::is_word_character(c) => {
                self.next()
            }
            _ => None
        }
    }

    /// If there exists a [`Symbol`] such that its value can be eaten using [`Self::eat_string`],
    /// eats it and returns the corresponding symbol. Otherwise, returns [`None`].
    ///
    /// If multiple symbols correspond to this description, the one with the longest value is
    /// chosen. If multiple symbols have the same value, an arbitrary one is chosen.
    pub fn next_symbol(&mut self) -> Option<Symbol> {
        for (string, symbol) in Symbol::symbols() {
            if self.eat_string(string) {
                return Some(symbol);
            }
        }
        None
    }

    /// If the next character is a digit for the specified radix, skips it and returns its value,
    /// using [`char::to_digit`]. Otherwise, returns [`None`].
    pub fn next_digit(&mut self, radix: u32) -> Option<u32> {
        let digit = self.peek()
            .and_then(|c| c.to_digit(radix));
        if digit.is_some() {
            self.skip();
        }
        digit
    }

    /// If the next character is a digit for the specified radix, skips it and returns its value,
    /// using [`char::to_digit`]. Otherwise, returns an error.
    pub fn expect_digit(&mut self, radix: u32) -> CompilationResult<u32> {
        let location = self.location();
        self.next_digit(radix)
            .ok_or(LocatedException::expected_digit(location, radix))
    }

    /// Reads a character that is part of a character or string literal.
    ///
    /// If the next character is a backslash (`'\'`), an escape sequence for a single character is
    /// read.
    pub fn expect_literal_character(&mut self, delimiter: char) -> CompilationResult<Located<u8>> {
        let location = self.location();
        match self.next() {
            // Escape sequence
            Some('\\') => {
                let escape_location = self.location();
                let escaped_character = match self.next() {
                    Some('n') => b'\n',
                    Some('r') => b'\r',
                    Some('t') => b'\t',
                    Some('b') => b'\x08',
                    Some('f') => b'\x0C',
                    Some('\\') => b'\\',
                    Some('"') => b'"',
                    Some('\'') => b'\'',
                    Some('x') => (self.expect_digit(16)? * 16 + self.expect_digit(16)?) as u8,
                    Some(c) if c != '\n' => {
                        return Err(LocatedException::invalid_escape_sequence(location.extended_to(&escape_location)));
                    }
                    _ => {
                        return Err(LocatedException::unterminated_string_literal(escape_location, delimiter));
                    }
                };
                Ok(Located::new(self.location_from(&location), escaped_character))
            }
            // Invalid character
            Some(c) if !c.is_ascii() || c.is_control() => {
                Err(LocatedException::invalid_character_in_literal(location))
            }
            // Valid (ASCII, non-control) character
            Some(c) if c != '\n' => {
                Ok(Located::new(location, c as u8))
            }
            // EOL / EOF
            _ => {
                Err(LocatedException::unterminated_string_literal(location, delimiter))
            }
        }
    }
}
