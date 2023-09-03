use std::fmt::{Display, Formatter};

use crate::lexer::lexemes::Symbol;
use crate::location::Sequence;
use crate::reference::Reference;

/// A bracket kind (`()`, `[]`, or `{}`).
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum BracketKind {
    /// Round parentheses: `()`.
    Round,
    /// Square brackets: `[]`.
    Square,
    /// Curly braces: `{}`.
    Curly,
}

impl BracketKind {
    /// Returns the [`BracketKind`] corresponding to an opening bracket, if any.
    pub const fn from_opening(symbol: Symbol) -> Option<Self> {
        match symbol {
            Symbol::OpenParenthesis => Some(Self::Round),
            Symbol::OpenBracket => Some(Self::Square),
            Symbol::OpenBrace => Some(Self::Curly),
            _ => None,
        }
    }

    /// Returns the opening bracket for this [`BracketKind`].
    pub const fn get_opening(self) -> Symbol {
        match self {
            Self::Round => Symbol::OpenParenthesis,
            Self::Square => Symbol::OpenBracket,
            Self::Curly => Symbol::OpenBrace,
        }
    }

    /// Returns the closing bracket for this [`BracketKind`].
    pub const fn get_closing(self) -> Symbol {
        match self {
            Self::Round => Symbol::CloseParenthesis,
            Self::Square => Symbol::CloseBracket,
            Self::Curly => Symbol::CloseBrace,
        }
    }

    /// Tests if a symbol is a closing bracket.
    pub fn is_closing(symbol: Symbol) -> bool {
        symbol == Symbol::CloseParenthesis
            || symbol == Symbol::CloseBracket
            || symbol == Symbol::CloseBrace
    }
}


/// A keyword (e.g., `public`, `func`, `else`).
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Keyword {
    /// `public`
    Public,
    /// `type`
    Type,
    /// `native`
    Native,
    /// `func`
    Func,
    /// `namespace`
    Namespace,
    /// `loop`
    Loop,
    /// `while`
    While,
    /// `do`
    Do,
    /// `if`
    If,
    /// `else`
    Else,
    /// `let`
    Let,
    /// `return`
    Return,
}

impl Keyword {
    /// Returns the [`Keyword`] corresponding to a word, if any.
    pub fn from(word: &str) -> Option<Self> {
        match word {
            "public" => Some(Self::Public),
            "type" => Some(Self::Type),
            "native" => Some(Self::Native),
            "func" => Some(Self::Func),
            "namespace" => Some(Self::Namespace),
            "loop" => Some(Self::Loop),
            "while" => Some(Self::While),
            "do" => Some(Self::Do),
            "if" => Some(Self::If),
            "else" => Some(Self::Else),
            "let" => Some(Self::Let),
            "return" => Some(Self::Return),
            _ => None,
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Keyword::Public => write!(f, "public"),
            Keyword::Type => write!(f, "type"),
            Keyword::Native => write!(f, "native"),
            Keyword::Func => write!(f, "func"),
            Keyword::Namespace => write!(f, "namespace"),
            Keyword::Loop => write!(f, "loop"),
            Keyword::While => write!(f, "while"),
            Keyword::Do => write!(f, "do"),
            Keyword::If => write!(f, "if"),
            Keyword::Else => write!(f, "else"),
            Keyword::Let => write!(f, "let"),
            Keyword::Return => write!(f, "return"),
        }
    }
}


/// A token is a part of the original source code, with additional meaning. It is a higher level of
/// abstraction than [lexemes](crate::lexer::lexemes::Lexeme).
#[derive(Clone, Debug)]
pub enum Token {
    /// A parenthesized sequence of tokens.
    Parenthesized(BracketKind, Sequence<Self>),
    /// A [`Symbol`].
    Symbol(Symbol),
    /// A [`Keyword`].
    Keyword(Keyword),
    /// A [`Reference`].
    Reference(Reference),
    /// A numeric literal.
    Numeric(i32),
    /// A character literal.
    Character(u8),
    /// A string literal.
    String(Sequence<u8>),
    /// A native code block.
    NativeCodeBlock(String),
}


/// A token matcher corresponds to a set of matching tokens.
pub trait TokenMatcher {
    /// Tests if this matcher matches a specific token.
    fn matches(&self, token: &Token) -> bool;
}

impl TokenMatcher for Symbol {
    fn matches(&self, token: &Token) -> bool {
        matches!(token, Token::Symbol(symbol) if symbol == self)
    }
}

impl TokenMatcher for Keyword {
    fn matches(&self, token: &Token) -> bool {
        matches!(token, Token::Keyword(keyword) if keyword == self)
    }
}
