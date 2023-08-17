use std::fmt::Display;
use std::path::Path;

use crate::exceptions::{CompilationResult, LocatedException};
use crate::lexer::tokens::{Priority, Symbol, Token};
use crate::location::{Located, LocatedResult, Location, Sequence};
use crate::reference::Reference;

/// A token matcher corresponds to a set of matching tokens.
pub(super) trait TokenMatcher: Display {
    /// Tests if this token matcher matches a token.
    fn matches(&self, token: &Token) -> bool;
}

impl<M: TokenMatcher> TokenMatcher for &M {
    fn matches(&self, token: &Token) -> bool {
        (*self).matches(token)
    }
}

impl TokenMatcher for Symbol {
    /// Matches a specific symbol.
    fn matches(&self, token: &Token) -> bool {
        match token {
            Token::Symbol(symbol) => symbol == self,
            _ => false,
        }
    }
}

impl TokenMatcher for &str {
    /// Matches a specific word.
    fn matches(&self, token: &Token) -> bool {
        match token {
            Token::Reference(Reference { namespace: None, identifier }) => identifier == self,
            _ => false,
        }
    }
}


/// A stream of tokens that can be used to parse [constructs](Construct).
#[derive(Debug)]
pub(super) struct TokenStream {
    /// The located tokens to yield.
    tokens: Sequence<Token>,
    /// The current position of the stream.
    index: usize,
    /// The location of the start of the stream.
    ///
    /// This is not necessarily the location of the first element of the stream.
    start_location: Location,
    /// The location after the stream.
    ///
    /// This is not necessarily the location of the last element of the stream.
    final_location: Location,
}

impl TokenStream {
    pub fn new(file: impl AsRef<Path>, tokens: Sequence<Token>) -> Self {
        let start_location = match tokens.first() {
            Some(token) => token.location.collapse(),
            None => Location::start_of_file(&file)
        };
        let final_location = match tokens.last() {
            Some(token) => token.location.after(),
            None => Location::start_of_file(&file)
        };
        Self {
            tokens,
            index: 0,
            start_location,
            final_location,
        }
    }

    /// Returns the current location of the stream.
    ///
    /// This corresponds to the location of the current token, or the location after the last token
    /// if the stream has no more tokens.
    pub fn location(&self) -> Location {
        match self.tokens.get(self.index) {
            Some(token) => token.location.clone(),
            None => self.final_location.clone(),
        }
    }

    /// Returns the previous location of the stream.
    ///
    /// This corresponds to the location of the previous token, or the start of the location of the
    /// first token if the stream is still at the first token.
    pub fn previous_location(&self) -> Location {
        if let Some(index) = self.index.checked_sub(1) {
            if let Some(token) = self.tokens.get(index) {
                return token.location.clone();
            }
        }
        self.start_location.clone()
    }

    /// Returns the range starting at the passed location and ending at the start of the current
    /// token.
    pub fn location_from(&self, location: &Location) -> Location {
        location.extended_to(&self.previous_location())
    }

    /// Tests if there are remaining tokens in the stream. Returns `true` if and only if a call to
    /// [`Self::peek`] at this point would return a [`Some`] variant.
    pub fn can_read(&self) -> bool {
        self.index < self.tokens.len()
    }

    /// Returns a reference to the token at position `index` in the token sequence.
    fn token_at(&self, index: usize) -> Option<&Token> {
        match self.tokens.get(index) {
            Some(token) => Some(&token.value),
            None => None,
        }
    }

    /// Returns a clone of the next token without advancing the stream.
    ///
    /// If there is no next token, returns [`None`].
    pub fn peek(&self) -> Option<Token> {
        self.token_at(self.index).cloned()
    }

    /// Tests if a token matcher matches the next token.
    ///
    /// If there is no next token, returns `false`.
    pub fn is(&self, matcher: impl TokenMatcher) -> bool {
        match self.token_at(self.index) {
            Some(token) if matcher.matches(token) => true,
            _ => false,
        }
    }

    /// Advances the stream.
    fn advance(&mut self) {
        if self.index < self.tokens.len() {
            self.index += 1
        }
    }

    /// Advances the stream and returns `true` if, and only if, the next token is matched.
    ///
    /// If the matcher does not match the next token, or if there is no next token, the stream is
    /// not mutated and `false` is returned.
    pub fn eat(&mut self, matcher: impl TokenMatcher) -> bool {
        if self.is(matcher) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Advances the stream if the next token is matched. Otherwise, returns an error located after
    /// the previous location without advancing the stream.
    ///
    /// The difference with [`Self::expect`] is that the error is located after the previous
    /// token instead of at the current token.
    pub fn consume(&mut self, matcher: impl TokenMatcher) -> CompilationResult<()> {
        if self.eat(&matcher) {
            Ok(())
        } else {
            Err(LocatedException::expected(self.previous_location().after(), format!("`{}`", matcher)))
        }
    }

    /// Advances the stream if the next token is matched. Otherwise, returns an error located at the
    /// current location without advancing the stream.
    ///
    /// The difference with [`Self::consume`]  is that the error is located at the current
    /// token instead of after the previous token.
    pub fn expect(&mut self, matcher: impl TokenMatcher) -> CompilationResult<()> {
        if self.eat(&matcher) {
            Ok(())
        } else {
            Err(LocatedException::expected(self.location(), format!("`{}`", matcher)))
        }
    }

    /// If the next token is a unary operator, returns the corresponding symbol and advances the
    /// stream. Otherwise, returns [`None`] without advancing the stream.
    pub fn eat_unary_operator(&mut self) -> Option<Symbol> {
        match self.peek() {
            Some(Token::Symbol(operator)) if operator.is_unary_operator() => {
                self.advance();
                Some(operator)
            }
            _ => None
        }
    }

    /// If the next token is an operator, returns the corresponding symbol and its priority and
    /// advances the stream. Otherwise, returns [`None`] without advancing the stream.
    pub fn eat_binary_operator(&mut self) -> Option<(Symbol, Priority)> {
        match self.peek() {
            Some(Token::Symbol(operator)) => {
                if let Some(priority) = operator.binary_operator_priority() {
                    self.advance();
                    Some((operator, priority))
                } else {
                    None
                }
            }
            _ => None
        }
    }

    /// If the next token is a reference, returns it with its location, and advances the stream.
    /// Otherwose, returns an error without advancing the stream.
    pub fn read_located_reference(&mut self) -> LocatedResult<Reference> {
        match self.peek() {
            Some(Token::Reference(reference)) => {
                let location = self.location();
                self.advance();
                Ok(Located::new(location, reference))
            }
            _ => Err(LocatedException::expected_reference(self.location())),
        }
    }

    /// If the next token is a reference, returns it with its location, and advances the stream.
    /// Otherwise, returns [`None`] without advancing the stream.
    pub fn eat_located_reference(&mut self) -> Option<Located<Reference>> {
        match self.peek() {
            Some(Token::Reference(reference)) => {
                let location = self.location();
                self.advance();
                Some(Located::new(location, reference))
            }
            _ => None,
        }
    }

    /// If the next token is a reference, followed by a token that matches a specific matcher,
    /// returns the reference with its location, and advances the stream. Otherwise, returns
    /// [`None`] without advancing the stream.
    pub fn eat_located_reference_with(&mut self, matcher: impl TokenMatcher) -> Option<Located<Reference>> {
        match self.peek() {
            Some(Token::Reference(reference))
            if self.token_at(self.index + 1).is_some_and(|token| matcher.matches(token)) => {
                let location = self.location();
                self.advance();
                self.advance();
                Some(Located::new(location, reference))
            }
            _ => None,
        }
    }

    /// If the next token is a single identifier, with no namespace, returns its value and advances
    /// the stream. Otherwise, returns an error without advancing the stream.
    pub fn read_identifier(&mut self) -> CompilationResult<String> {
        match self.peek() {
            Some(Token::Reference(Reference { namespace: None, identifier })) => {
                self.advance();
                Ok(identifier.clone())
            }
            _ => Err(LocatedException::expected_identifier(self.location())),
        }
    }

    /// If the next token is a numeric literal, returns its value and advances the stream.
    /// Otherwise, returns [`None`] without advancing the stream.
    pub fn eat_integer(&mut self) -> Option<u32> {
        match self.peek() {
            Some(Token::Numeric(value)) => {
                self.advance();
                Some(value)
            }
            _ => None,
        }
    }

    /// If the next token is a character literal, returns its value and advances the stream.
    /// Otherwise, returns [`None`] without advancing the stream.
    pub fn eat_character(&mut self) -> Option<u8> {
        match self.peek() {
            Some(Token::Character(value)) => {
                self.advance();
                Some(value)
            }
            _ => None,
        }
    }

    /// If the next token is a string literal, returns its value and advances the stream. Otherwise,
    /// returns [`None`] without advancing the stream.
    pub fn eat_string(&mut self) -> Option<Sequence<u8>> {
        match self.peek() {
            Some(Token::String(characters)) => {
                self.advance();
                Some(characters.clone())
            }
            _ => None,
        }
    }

    /// If the next token is a native code block, returns its value and advances the stream.
    /// Otherwise, returns an error without advancing the stream.
    pub fn read_native_code_block(&mut self) -> CompilationResult<String> {
        match self.peek() {
            Some(Token::NativeCodeBlock(native_code)) => {
                self.advance();
                Ok(native_code)
            }
            _ => Err(LocatedException::expected_native_code(self.location()))
        }
    }

    /// Reads a sequence of `separator`-separated items until one is not immediately followed by
    /// `separator`.
    ///
    /// After this function is called, the iterator will be advanced to after the sequence.
    ///
    /// The sequence must contain at least one item. Trailing separators are not allowed.
    ///
    /// # Arguments
    ///
    /// * `tokens` - The iterator to read the items from.
    /// * `separator` - A matcher that matches a token that separates items.
    /// * `item_reader` - A function that reads an item.
    ///
    /// ## Item reader
    ///
    /// The item reader must advance the iterator to the token after the item it read. It is not
    /// responsible for handling separators. Any error returned by the item reader will be propagated
    /// immediately.
    pub fn read_separated_items<T, F>(&mut self, separator: impl TokenMatcher, mut item_reader: F) -> CompilationResult<Sequence<T>>
    where
        F: FnMut(&mut TokenStream) -> CompilationResult<T> {
        let mut accumulator = Vec::new();
        // Do-while
        while {
            let start_location = self.location();
            let item = item_reader(self)?;
            let location = self.location_from(&start_location);
            accumulator.push(Located::new(location, item));
            self.eat(&separator)
        } {}
        Ok(accumulator)
    }
}


/// A syntactic construct is a piece of syntax that typically consists of multiple tokens.
///
/// A construct can be parsed from a [`tokenStream`] using [`Construct::parse`]. Alternatively, you
/// can use [`Construct::locate`] to get a [`Located`] construct.
pub(super) trait Construct: Sized {
    /// Parses this construct from a [`TokenStream`]. Advances the stream to after the construct if
    /// parsed successfully.
    fn parse(tokens: &mut TokenStream) -> CompilationResult<Self>;

    /// Parses and locates this construct from a [`TokenStream`]. Advances the stream to after the
    /// construct if parsed successfully.
    fn locate(tokens: &mut TokenStream) -> LocatedResult<Self> {
        let start_location = tokens.location();
        let construct = Self::parse(tokens)?;
        let location = tokens.location_from(&start_location);
        Ok(Located::new(location, construct))
    }

    /// Parses a sequence of `separator`-separated constructs until one is not immediately followed
    /// by `separator`.
    ///
    /// After this method is called, the stream will be advanced to after the sequence.
    ///
    /// The sequence must contain at least one item. Trailing separators are not allowed.
    fn parse_separated_sequence(tokens: &mut TokenStream, separator: impl TokenMatcher) -> CompilationResult<Sequence<Self>> {
        let mut accumulator = Vec::new();
        // Do-while
        while {
            let item = Self::locate(tokens)?;
            accumulator.push(item);
            tokens.eat(&separator)
        } {}
        Ok(accumulator)
    }

    /// Parses a sequence of constructs until one is immediately followed by `delimiter`.
    ///
    /// After this method is called, the stream will be advanced to after the delimiter.
    ///
    /// The sequence might be empty.
    fn parse_delimited_sequence(tokens: &mut TokenStream, delimiter: impl TokenMatcher) -> CompilationResult<Sequence<Self>> {
        let mut accumulator = Vec::new();
        while !tokens.eat(&delimiter) {
            let item = Self::locate(tokens)?;
            accumulator.push(item);
        }
        Ok(accumulator)
    }

    /// Parses a sequence of `separator`-separated constructs until `delimiter` is found.
    ///
    /// After this method is called, the stream will be advanced to after the delimiter.
    ///
    /// The sequence might be empty. A trailing separator before the delimiter is allowed.
    fn parse_delimited_separated_sequence(tokens: &mut TokenStream, separator: impl TokenMatcher, delimiter: impl TokenMatcher) -> CompilationResult<Sequence<Self>> {
        let mut accumulator = Vec::new();
        while !tokens.eat(&delimiter) {
            let item = Self::locate(tokens)?;
            accumulator.push(item);
            if !tokens.eat(&separator) {
                if !tokens.eat(&delimiter) {
                    return Err(LocatedException::expected(tokens.location(), format!("{} or {}", separator, delimiter)));
                }
                break;
            }
        }
        Ok(accumulator)
    }
}


impl<C: Construct> Construct for Sequence<C> {
    /// Parses a sequence of `C`s until the end of the stream is reached.
    ///
    /// After this method is called, the stream will be advanced to after the sequence, i.e., the
    /// end of the stream.
    ///
    /// The sequence might be empty.
    fn parse(tokens: &mut TokenStream) -> CompilationResult<Self> {
        let mut accumulator = Sequence::new();
        while tokens.can_read() {
            let item = C::locate(tokens)?;
            accumulator.push(item)
        }
        Ok(accumulator)
    }
}
