use std::fmt::Display;
use std::path::Path;
use std::rc::Rc;

use crate::exceptions::{CompilationResult, LocatedException};
use crate::lexer::lexemes::{Priority, Symbol};
use crate::location::{Located, LocatedResult, Location, Sequence};
use crate::reference::Reference;
use crate::tokenizer::tokens::{BracketKind, Token, TokenMatcher};

/// A stream of tokens that can be used to parse [constructs](Construct).
#[derive(Debug)]
pub struct TokenStream {
    /// The path of the file this token stream comes from.
    file: Rc<Path>,
    /// The located tokens to yield, from last to first.
    ///
    /// When a token is yielded, it is popped from this vector.
    tokens: Sequence<Token>,
    /// The location of the previous token.
    previous_location: Location,
    /// The location after the stream.
    ///
    /// This is not necessarily the location of the last element of the stream.
    final_location: Location,
    /// The kind of brackets surrounding this stream, if any.
    brackets: Option<BracketKind>,
}

impl TokenStream {
    fn new_parenthesized(file: Rc<Path>, initial_location: Location, brackets: Option<BracketKind>, mut tokens: Sequence<Token>) -> Self {
        let final_location = match tokens.last() {
            Some(token) => token.location.after(),
            None => initial_location.clone(),
        };
        tokens.reverse();
        Self {
            file,
            tokens,
            previous_location: initial_location,
            final_location,
            brackets,
        }
    }

    pub fn new(file: impl AsRef<Path>, tokens: Sequence<Token>) -> Self {
        let initial_location = match tokens.first() {
            Some(token) => token.location.collapse(),
            None => Location::start_of_file(&file)
        };
        Self::new_parenthesized(Rc::from(file.as_ref()), initial_location, None, tokens)
    }

    /// Returns the current location of the stream.
    ///
    /// This corresponds to the location of the current token, or the location after the last token
    /// if the stream has no more tokens.
    pub fn location(&self) -> Location {
        match self.tokens.last() {
            Some(token) => token.location.clone(),
            None => self.final_location.clone(),
        }
    }

    /// Returns the range starting at the passed location and ending at the start of the current
    /// token.
    pub fn location_from(&self, location: &Location) -> Location {
        location.extended_to(&self.previous_location)
    }

    /// Returns the closing bracket that is expected after this stream, if any.
    pub fn closing_bracket(&self) -> Option<Symbol> {
        self.brackets.map(BracketKind::get_closing)
    }

    /// Tests if there are remaining tokens in the stream.
    pub fn can_read(&self) -> bool {
        !self.tokens.is_empty()
    }

    /// Returns a reference to the token `offset` tokens forward in the sequence. If no such token
    /// exists, returns [`None`].
    ///
    /// An offset of zero corresponds to the next token.
    fn peek(&self, offset: usize) -> Option<&Token> {
        let index = self.tokens.len().checked_sub(offset + 1)?;
        match self.tokens.get(index) {
            Some(token) => Some(&token.value),
            None => None,
        }
    }

    /// Tests if a [`TokenMatcher`] matches the next token.
    ///
    /// If there is no next token, returns `false`.
    pub fn is(&self, matcher: impl TokenMatcher) -> bool {
        matches!(self.peek(0), Some(token) if matcher.matches(token))
    }

    /// Advances the stream.
    fn advance(&mut self) {
        if let Some(token) = self.tokens.pop() {
            self.previous_location = token.location;
        }
    }

    /// Returns the next token and advances the stream.
    fn next(&mut self) -> Option<Located<Token>> {
        if let Some(token) = self.tokens.pop() {
            self.previous_location = token.location.clone();
            Some(token)
        } else {
            None
        }
    }

    /// Advances the stream and returns `true` if, and only if, the next tokens is matched.
    ///
    /// If the next token is not matched, or if there is no next token, the stream is not mutated
    /// and `false` is returned.
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
    pub fn consume(&mut self, matcher: impl TokenMatcher + Display) -> CompilationResult<()> {
        let matcher_string = format!("`{}`", &matcher);
        if self.eat(matcher) {
            Ok(())
        } else {
            Err(LocatedException::expected(self.previous_location.after(), matcher_string))
        }
    }

    /// Advances the stream if the next token is matched. Otherwise, returns an error located at the
    /// current location without advancing the stream.
    ///
    /// The difference with [`Self::consume`]  is that the error is located at the current
    /// token instead of after the previous token.
    pub fn expect(&mut self, matcher: impl TokenMatcher + Display) -> CompilationResult<()> {
        let matcher_string = format!("`{}`", &matcher);
        if self.eat(matcher) {
            Ok(())
        } else {
            Err(LocatedException::expected(self.location(), matcher_string))
        }
    }

    /// If the next token is a parenthesized token sequence with the specified [`BracketKind`],
    /// returns a [`TokenStream`] for the token sequence and advances this stream. Otherwise,
    /// returns [`None`] without advancing this stream.
    pub fn next_parenthesized(&mut self, bracket_kind: BracketKind) -> Option<Self> {
        match self.tokens.pop() {
            Some(Located { location, value: Token::Parenthesized(brackets, tokens) })
            if brackets == bracket_kind => {
                self.previous_location = location.clone();
                let file = self.file.clone();
                Some(Self::new_parenthesized(file, location, Some(brackets), tokens))
            }
            Some(token) => {
                self.tokens.push(token);
                None
            }
            None => {
                None
            }
        }
    }

    /// If the next token is a unary operator, returns the corresponding symbol and advances the
    /// stream. Otherwise, returns [`None`] without advancing the stream.
    pub fn next_unary_operator(&mut self) -> Option<Symbol> {
        match self.peek(0) {
            Some(&Token::Symbol(operator)) if operator.is_unary_operator() => {
                self.advance();
                Some(operator)
            }
            _ => None
        }
    }

    /// If the next token is an operator, returns the corresponding symbol and its priority and
    /// advances the stream. Otherwise, returns [`None`] without advancing the stream.
    pub fn next_binary_operator(&mut self) -> Option<(Symbol, Priority)> {
        match self.peek(0) {
            Some(&Token::Symbol(operator)) => {
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
    /// Otherwise, returns [`None`] without advancing the stream.
    pub fn next_located_reference(&mut self) -> Option<Located<Reference>> {
        match self.peek(0) {
            Some(Token::Reference(_)) => {
                let reference = self.next().unwrap()
                    .map(|token| {
                        let Token::Reference(reference) = token else {
                            unreachable!()
                        };
                        reference
                    });
                Some(reference)
            }
            _ => None,
        }
    }

    /// If the next token is a reference, returns it with its location, and advances the stream.
    /// Otherwise, returns an error without advancing the stream.
    pub fn read_located_reference(&mut self) -> LocatedResult<Reference> {
        self.next_located_reference()
            .ok_or_else(|| LocatedException::expected_reference(self.location()))
    }

    /// If the next token is a reference, and it is followed by a token that is matched by a
    /// specific [`TokenMatcher`], returns the reference with its location, and advances the stream.
    /// Otherwise, returns [`None`] without advancing the stream.
    pub fn next_located_reference_with(&mut self, matcher: impl TokenMatcher) -> Option<Located<Reference>> {
        match self.peek(0) {
            Some(Token::Reference(_))
            if self.peek(1).is_some_and(|token| matcher.matches(token)) => {
                let located_reference = self.next_located_reference().unwrap();
                self.advance();
                Some(located_reference)
            }
            _ => None,
        }
    }

    /// If the next token is a reference, and it is followed by parenthesized tokens, returns the
    /// located reference and the parenthesized content, and advances the stream to after the
    /// parenthesized content. Otherwise, returns [`None`] without advancing the stream.
    pub fn next_located_reference_with_parenthesized(&mut self, brackets: BracketKind) -> Option<(Located<Reference>, Self)> {
        match self.peek(0) {
            Some(Token::Reference(_))
            if self.peek(1).is_some_and(|token| {
                matches!(token, &Token::Parenthesized(actual_brackets, _) if actual_brackets == brackets)
            }) => {
                let located_reference = self.next_located_reference().unwrap();
                let parenthesized_content = self.next_parenthesized(brackets).unwrap();
                Some((located_reference, parenthesized_content))
            }
            _ => None,
        }
    }

    /// If the next token is a single identifier, with no namespace, returns its value and advances
    /// the stream. Otherwise, returns an error without advancing the stream.
    pub fn read_identifier(&mut self) -> CompilationResult<String> {
        match self.peek(0) {
            Some(Token::Reference(Reference { namespace: None, .. })) => {
                Ok(self.next_located_reference().unwrap().value.identifier)
            }
            _ => Err(LocatedException::expected_identifier(self.location())),
        }
    }

    /// If the next token is a numeric literal, returns its value and advances the stream.
    /// Otherwise, returns [`None`] without advancing the stream.
    pub fn next_integer(&mut self) -> Option<i32> {
        match self.peek(0) {
            Some(&Token::Numeric(value)) => {
                self.advance();
                Some(value)
            }
            _ => None,
        }
    }

    /// If the next token is a character literal, returns its value and advances the stream.
    /// Otherwise, returns [`None`] without advancing the stream.
    pub fn next_character(&mut self) -> Option<u8> {
        match self.peek(0) {
            Some(&Token::Character(value)) => {
                self.advance();
                Some(value)
            }
            _ => None,
        }
    }

    /// If the next token is a string literal, returns its value and advances the stream. Otherwise,
    /// returns [`None`] without advancing the stream.
    pub fn next_string(&mut self) -> Option<Sequence<u8>> {
        match self.peek(0) {
            Some(Token::String(_)) => {
                let Token::String(characters) = self.next().unwrap().value else {
                    unreachable!()
                };
                Some(characters)
            }
            _ => None,
        }
    }

    /// If the next token is a native code block, returns its value and advances the stream.
    /// Otherwise, returns an error without advancing the stream.
    pub fn read_native_code_block(&mut self) -> CompilationResult<String> {
        match self.peek(0) {
            Some(Token::NativeCodeBlock(_)) => {
                let Token::NativeCodeBlock(native_code) = self.next().unwrap().value else {
                    unreachable!()
                };
                Ok(native_code)
            }
            _ => Err(LocatedException::expected_native_code(self.location()))
        }
    }

    /// Reads a sequence of items from this stream until the end is reached.
    ///
    /// If the [`Ok`] variant is returned, this token stream will be empty after the call.
    ///
    /// The sequence may be empty.
    ///
    /// ## Item reader
    ///
    /// The item reader must advance the stream to the token after the item it read. Any error
    /// returned by the item reader will be propagated immediately.
    pub fn read_items<T, F>(&mut self, mut item_reader: F) -> CompilationResult<Sequence<T>>
    where
        F: FnMut(&mut Self) -> LocatedResult<T>
    {
        let mut items = Sequence::new();
        while self.can_read() {
            let item = item_reader(self)?;
            items.push(item)
        }
        Ok(items)
    }

    /// Reads a sequence of `separator`-separated items until one is not immediately followed by
    /// `separator`.
    ///
    /// After this function is called, the iterator will be advanced to after the sequence.
    ///
    /// The sequence must contain at least one item. A trailing separator is not allowed.
    ///
    /// ## Item reader
    ///
    /// The item reader must advance the stream to the token after the item it read. It is not
    /// responsible for handling separators. Any error returned by the item reader will be
    /// propagated immediately.
    pub fn read_separated_items<T, F>(&mut self, separator: impl TokenMatcher + Copy, mut item_reader: F) -> CompilationResult<Sequence<T>>
    where
        F: FnMut(&mut Self) -> LocatedResult<T>
    {
        let mut items = Vec::new();
        // Do-while
        while {
            let item = item_reader(self)?;
            items.push(item);
            self.eat(separator)
        } {}
        Ok(items)
    }

    /// Parses a sequence of `separator`-separated items from this stream.
    ///
    /// In case the sequence ends before the end of this stream, an error is returned.
    ///
    /// The sequence may be empty. A trailing separator is allowed.
    ///
    /// ## Item reader
    ///
    /// The item reader must advance the stream to the token after the item it read. It is not
    /// responsible for handling separators. Any error returned by the item reader will be
    /// propagated immediately.
    pub fn parse_separated_items<T, F>(mut self, separator: impl TokenMatcher + Copy + Display, mut item_reader: F) -> CompilationResult<Sequence<T>>
    where
        F: FnMut(&mut Self) -> LocatedResult<T>
    {
        let mut items = Vec::new();
        while self.can_read() {
            let item = item_reader(&mut self)?;
            items.push(item);
            if !self.eat(separator) {
                if self.can_read() {
                    let expected = if let Some(bracket) = self.closing_bracket() {
                        format!("`{}` or `{}`", separator, bracket)
                    } else {
                        format!("`{}`", separator)
                    };
                    return Err(LocatedException::expected(self.previous_location, expected));
                } else {
                    break;
                }
            }
        }
        Ok(items)
    }
}


/// A syntactic construct is a piece of syntax that typically consists of multiple tokens.
///
/// A construct can be parsed from a [`tokenStream`] using [`Construct::read`]. Alternatively, you
/// can use [`Construct::locate`] to get a [`Located`] construct.
pub trait Construct: Sized {
    /// Reads this construct from a [`TokenStream`]. Advances the stream to after the construct if
    /// read successfully.
    fn read(tokens: &mut TokenStream) -> CompilationResult<Self>;

    /// Reads and locates this construct from a [`TokenStream`]. Advances the stream to after the
    /// construct if read successfully.
    fn locate(tokens: &mut TokenStream) -> LocatedResult<Self> {
        let start_location = tokens.location();
        let construct = Self::read(tokens)?;
        let location = tokens.location_from(&start_location);
        Ok(Located::new(location, construct))
    }

    /// Parses a sequence of constructs until the end of the stream is reached.
    ///
    /// See [`Sequence::read`].
    fn parse_sequence(mut tokens: TokenStream) -> CompilationResult<Sequence<Self>> {
        Sequence::read(&mut tokens)
    }

    /// Parses a sequence of `separator`-separated constructs until one is not immediately followed
    /// by `separator`.
    ///
    /// The sequence must contain at least one item. Trailing separators are not allowed.
    ///
    /// See [`TokenStream::parse_separated_items`].
    fn parse_separated_sequence(tokens: TokenStream, separator: impl TokenMatcher + Copy + Display) -> CompilationResult<Sequence<Self>> {
        tokens.parse_separated_items(separator, Self::locate)
    }
}


impl<C: Construct> Construct for Sequence<C> {
    /// Reads a sequence of `C`s until the end of the stream is reached.
    ///
    /// After this method is called, the stream will be advanced to after the sequence, i.e., the
    /// end of the stream, if the [`Ok`] variant was returned.
    ///
    /// The sequence might be empty.
    ///
    /// See [`TokenStream::read_items`].
    fn read(tokens: &mut TokenStream) -> CompilationResult<Self> {
        tokens.read_items(C::locate)
    }
}
