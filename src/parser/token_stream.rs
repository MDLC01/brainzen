use std::fmt::Display;
use std::path::Path;

use crate::exceptions::{CompilationException, CompilationResult};
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
            Token::Word(word) => word == self,
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
    /// If there is no next token, [`None`] is returned.
    pub fn peek(&self) -> Option<Token> {
        self.token_at(self.index).cloned()
    }

    /// Tests if a token matcher matches the next token.
    ///
    /// If there is no next token, `false` is returned.
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
    /// the previous location.
    ///
    /// The difference with [`Self::expect`] is that the error is located after the previous
    /// token instead of at the current token.
    pub fn consume(&mut self, matcher: impl TokenMatcher) -> CompilationResult<()> {
        if self.eat(&matcher) {
            Ok(())
        } else {
            Err(CompilationException::expected(self.previous_location().after(), format!("`{}`", matcher)))
        }
    }

    /// Advances the stream if the next token is matched. Otherwise, returns an error located at the
    /// current location.
    ///
    /// The difference with [`Self::consume`]  is that the error is located at the current
    /// token instead of after the previous token.
    pub fn expect(&mut self, matcher: impl TokenMatcher) -> CompilationResult<()> {
        if self.eat(&matcher) {
            Ok(())
        } else {
            Err(CompilationException::expected(self.location(), format!("`{}`", matcher)))
        }
    }

    /// If the next token is a unary operator, returns the corresponding symbol and advances the
    /// stream. Otherwise, returns [`None`].
    pub fn read_unary_operator(&mut self) -> Option<Symbol> {
        match self.peek() {
            Some(Token::Symbol(operator)) if operator.is_unary_operator() => {
                self.advance();
                Some(operator)
            }
            _ => None
        }
    }

    /// If the next token is an operator, returns the corresponding symbol and its priority and
    /// advances the stream. Otherwise, returns [`None`].
    pub fn read_binary_operator(&mut self) -> Option<(Symbol, Priority)> {
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

    /// If the next token is a word, returns its value and advances the stream. Otherwise, returns
    /// an error.
    pub fn read_word(&mut self) -> CompilationResult<String> {
        match self.peek() {
            Some(Token::Word(identifier)) => {
                self.advance();
                Ok(identifier.clone())
            }
            _ => Err(CompilationException::expected_identifier(self.location()))
        }
    }

    /// If the next token is a string literal, returns its value and advances the stream. Otherwise,
    /// returns an error.
    pub fn read_string(&mut self) -> CompilationResult<Sequence<u8>> {
        match self.peek() {
            Some(Token::String(characters)) => {
                self.advance();
                Ok(characters.clone())
            }
            _ => Err(CompilationException::expected_string(self.location()))
        }
    }

    /// If the next token is a numeric literal, returns its value and advances the stream.
    /// Otherwise, returns an error.
    pub fn read_integer(&mut self) -> CompilationResult<u32> {
        match self.peek() {
            Some(Token::Numeric(value)) => {
                self.advance();
                Ok(value)
            }
            _ => Err(CompilationException::expected_number(self.location()))
        }
    }

    /// If the next token is a character literal, returns its value and advances the stream.
    /// Otherwise, returns an error.
    pub fn read_character(&mut self) -> CompilationResult<u8> {
        match self.peek() {
            Some(Token::Character(value)) => {
                self.advance();
                Ok(value)
            }
            _ => Err(CompilationException::expected_character(self.location()))
        }
    }

    /// If the next token is a native code block, returns its value and advances the stream.
    /// Otherwise, returns an error.
    pub fn read_native_code_block(&mut self) -> CompilationResult<String> {
        match self.peek() {
            Some(Token::NativeCodeBlock(native_code)) => {
                self.advance();
                Ok(native_code)
            }
            _ => Err(CompilationException::expected_native_code(self.location()))
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

    /// Tries to parse a `T` using the provided parser, advancing the stream if, and only if, the
    /// [`Ok`] variant is returned.
    ///
    /// If the parser returns the [`Ok`] variant, the value is returned and the stream is advanced.
    /// If the parser returns the [`Err`] variant, the stream is not advanced,
    /// `Err((offset, error))` is returned, where `offset` is the amount the stream was advanced by
    /// after executing the parser, and `error` is the error that was returned.
    pub fn try_parse<T, E>(&mut self, parser: impl FnOnce(&mut Self) -> Result<T, E>) -> Result<T, (usize, E)> {
        let initial_index = self.index;
        match parser(self) {
            Ok(value) => {
                Ok(value)
            }
            Err(error) => {
                let offset = self.index - initial_index;
                self.index = initial_index;
                Err((offset, error))
            }
        }
    }

    /// Returns a [`ParseChoice`] which can be used to indicate multiple parsers that will be tried
    /// in order until one succeeds.
    pub fn parse_choice<T>(&mut self) -> ParseChoice<T> {
        let start_location = self.location();
        ParseChoice {
            tokens: self,
            start_location,
            state: ChoiceState::default(),
        }
    }
}


#[derive(Default)]
enum ChoiceState<T, E> {
    #[default] Nothing,
    Error(usize, E),
    Value(T),
}

impl<T, E> ChoiceState<T, E> {
    fn update(self, updater: impl FnOnce() -> Result<T, (usize, E)>) -> Self {
        match self {
            Self::Nothing => {
                match updater() {
                    Ok(value) => Self::Value(value),
                    Err((0, _)) => Self::Nothing,
                    Err((offset, error)) => Self::Error(offset, error),
                }
            }
            Self::Error(offset, error) => {
                match updater() {
                    Ok(value) => Self::Value(value),
                    Err((new_offset, new_error)) => {
                        if new_offset > offset {
                            Self::Error(new_offset, new_error)
                        } else {
                            Self::Error(offset, error)
                        }
                    }
                }
            }
            Self::Value(value) => Self::Value(value)
        }
    }
}

/// Lets you specify multiple parsers to try until one succeeds.
pub(super) struct ParseChoice<'a, T> {
    tokens: &'a mut TokenStream,
    start_location: Location,
    state: ChoiceState<T, CompilationException>,
}

impl<T> ParseChoice<'_, T> {
    /// Tries another parser. If a parser already succeeded, this does not do anything.
    #[inline]
    pub fn branch(mut self, parser: impl FnOnce(&mut TokenStream) -> CompilationResult<T>) -> Self {
        self.state = self.state.update(|| self.tokens.try_parse(parser));
        self
    }

    /// Returns the result of the first parser that succeeded. If no parser succeeded, returns the
    /// result of the provided default parser.
    #[inline]
    pub fn parse_or(self, default: impl FnOnce(&mut TokenStream) -> CompilationResult<T>) -> CompilationResult<T> {
        match self.state {
            ChoiceState::Value(value) => Ok(value),
            _ => default(self.tokens),
        }
    }

    /// Returns the result of the first parser that succeeded.
    ///
    /// If no parser succeeded, returns the [`CompilationException`] corresponding to the first
    /// parser that read at least one token. If no parser read at least one token, a generic
    /// "expected" compilation exception is returned.
    #[inline]
    pub fn parse(self, description: impl Display) -> CompilationResult<T> {
        match self.state {
            ChoiceState::Nothing => Err(CompilationException::expected(self.tokens.location(), description)),
            ChoiceState::Error(_, error) => Err(error),
            ChoiceState::Value(value) => Ok(value),
        }
    }

    /// Returns the located result of the first parser that succeeded. If no parser succeeded,
    /// returns a [`CompilationException`].
    #[inline]
    pub fn locate(self, description: impl Display) -> LocatedResult<T> {
        let location = self.tokens.location_from(&self.start_location);
        self.parse(description).map(|value| Located::new(location, value))
    }
}


/// A syntactic construct is a piece of syntax that typically consists of multiple tokens. It can be
/// parsed from a [`TokenStream`].
///
/// A construct can be parsed using [`Construct::parse`]. Alternatively, you can use
/// [`Construct::locate`] to get a [`Located`] construct.
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

    /// Parses a sequence of constructs until the end of the stream is reached.
    ///
    /// After this method is called, the stream will be advanced to after the sequence, i.e., the
    /// end of the stream.
    ///
    /// The sequence might be empty.
    fn parse_sequence(tokens: &mut TokenStream) -> CompilationResult<Sequence<Self>> {
        let mut accumulator = Vec::new();
        while tokens.can_read() {
            let item = Self::locate(tokens)?;
            accumulator.push(item)
        }
        Ok(accumulator)
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
                    return Err(CompilationException::expected(tokens.location(), format!("{} or {}", separator, delimiter)));
                }
                break;
            }
        }
        Ok(accumulator)
    }
}


impl Construct for Reference {
    fn parse(tokens: &mut TokenStream) -> CompilationResult<Self> {
        let start_location = tokens.location();
        let identifier = tokens.read_word()?;
        let mut reference = Self::new_identifier(identifier);
        let mut location = tokens.location_from(&start_location);
        while tokens.eat(Symbol::DoubleColon) {
            let identifier = tokens.read_word()?;
            reference = Reference::new(identifier, Located::new(location, reference));
            location = tokens.location_from(&start_location)
        }
        Ok(reference)
    }
}
