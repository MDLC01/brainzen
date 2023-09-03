use std::fmt;
use std::fmt::{Display, Formatter};

use crate::location::Sequence;
use crate::utils::write_iterator;

/// Lower priorities are applied first. For example, [`Priority::Multiplication`] < [`Priority::Addition`].
#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum Priority {
    /// The priority of multiplicative operators.
    Multiplication,
    /// The priority of additive operators.
    Addition,
    /// A priority that is between [`Priority::Addition`] and [`Priority::Comparison`].
    Intermediate,
    /// The priority of relation symbols.
    Comparison,
    /// The priority of logical disjunction.
    Disjunction,
    /// The priority of logical conjunction.
    Conjunction,
}


/// Lets you define symbols and their behavior as unary and binary operators.
macro_rules! define_symbols {
    {
        $( #[$attribute:meta] )*
        $visibility:vis $enum_name:ident {
            $(
                $( #[$symbol_attribute:meta] )*
                $symbol_value:literal =>
                    $symbol_name:ident
                    $( [$symbol_unary:expr] )?
                    $( ($symbol_priority:expr) )?
            )*
        }
    } => {
        $( #[$attribute] )*
        #[derive(
            ::core::marker::Copy,
            ::core::clone::Clone,
            ::core::hash::Hash,
            ::std::cmp::Eq,
            ::std::cmp::PartialEq,
            ::core::fmt::Debug,
        )]
        $visibility enum $enum_name {
            $(
                $( #[$symbol_attribute] )*
                $symbol_name,
            )*
        }

        impl $enum_name {
            /// Returns a vector of `(value, symbol)` pairs, ordered by reverse length of `value`
            /// where `value` is the string representation of the corresponding `symbol`.
            pub fn symbols() -> Vec<(&'static str, Self)> {
                let mut symbols = vec![ $( ($symbol_value, Self::$symbol_name) ), * ];
                symbols.sort_by(|(string, _), (other_string, _)| other_string.len().cmp(&string.len()));
                symbols
            }

            /// Returns `true` if, and only if, this symbol is a unary operator.
            pub const fn is_unary_operator(self) -> bool {
                match self {
                    $(
                        Self::$symbol_name => {
                            false
                            $( ;$symbol_unary )?
                        }
                    )*
                }
            }

            /// If this symbol is a binary operator, returns its priority. Otherwise, returns
            /// [`None`].
            pub const fn binary_operator_priority(self) -> Option<Priority> {
                #[allow(path_statements)]
                match self {
                    $(
                        Self::$symbol_name => {
                            Option::<Priority>::None
                            $( ; Some($symbol_priority) )?
                        }
                    )*
                }
            }
        }

        impl ::std::fmt::Display for $enum_name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                match self {
                    $( Self::$symbol_name => write!(f, "{}", $symbol_value) ), *
                }
            }
        }
    };
}

define_symbols! {
    /// A symbol consists of a string of non-word characters called its *value*.
    ///
    /// A list of available symbols can be obtained with [`Symbol::symbols`].
    pub Symbol {
        "{"  => OpenBrace
        "}"  => CloseBrace
        "["  => OpenBracket
        "]"  => CloseBracket
        "("  => OpenParenthesis
        ")"  => CloseParenthesis
        ";"  => Semicolon
        ","  => Comma
        "!"  => Bang                [true]
        "!!" => DoubleBang          [true]
        "="  => Equal
        "==" => DoubleEqual                 (Priority::Comparison)
        "!=" => BangEqual                   (Priority::Comparison)
        "<"  => LessThan                    (Priority::Comparison)
        "<=" => LessThanEqual               (Priority::Comparison)
        ">"  => GreaterThan                 (Priority::Comparison)
        ">=" => GreaterThanEqual            (Priority::Comparison)
        "&"  => Ampersand
        "&&" => DoubleAmpersand             (Priority::Conjunction)
        "|"  => Pipe
        "||" => DoublePipe                  (Priority::Disjunction)
        "+"  => Plus                        (Priority::Addition)
        "++" => DoublePlus
        "-"  => Minus              [true]   (Priority::Addition)
        "--" => DoubleMinus
        "*"  => Star                        (Priority::Multiplication)
        "/"  => Slash                       (Priority::Multiplication)
        "%"  => Percent                     (Priority::Multiplication)
        "%%" => DoublePercent               (Priority::Multiplication)
        ":"  => Colon
        "::" => DoubleColon
        "->" => Arrow
        "~"  => Tilde
        "#"  => Hash
        "?"  => QuestionMark
        "??" => DoubleQuestionMark          (Priority::Intermediate)
        "@"  => At
    }
}


/// A Brainzen lexeme.
///
/// A lexeme corresponds to a substring of the input, and carries some additional meaning.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Lexeme {
    /// # Symbols
    ///
    /// A *symbol* is a specific string of non-word characters that corresponds to a known pattern
    /// (e.g., `==`, `->` or `}`).
    ///
    /// See [`Symbol`].
    Symbol(Symbol),
    /// # Words
    ///
    /// A *word* is a succession of so-called *word characters* (`A`–`Z`, `a`–`z`, `0`–`9`, `_`)
    /// that does not start with a digit, surrounded by non-word characters.
    Word(String),
    /// # Numeric literals
    ///
    /// A *numeric literal* is a succession of digits (`0`–`9`) surrounded by non-word characters,
    /// interpreted as a base-ten integer. Optionally, it can start with `0x` to indicate a base-
    /// sixteen integer.
    ///
    /// Note that the optional `-` before a numeric literal is interpreted as a separate
    /// [symbol](Lexeme::Symbol).
    Numeric(u32),
    /// # Character literals
    ///
    /// A *character literal* consists of a single character or escape sequence, surrounded by
    /// single quotes.
    Character(char),
    /// # String literals
    ///
    /// A *string literal* consists of a succession of characters or escape sequences, surrounded by
    /// double quotes.
    String(Sequence<char>),
    /// # Native code block
    ///
    /// A *native code block* is a succession of characters surrounded by backticks.
    NativeCodeBlock(String),
}

impl Display for Lexeme {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Symbol(symbol) => write!(f, "{}", symbol),
            Self::Word(word) => write!(f, "{}", word),
            Self::Numeric(value) => write!(f, "{}", value),
            Self::Character(character) => write!(f, "'{}'", character),
            Self::String(characters) => write_iterator!(f, characters, "", "\"", "\""),
            Self::NativeCodeBlock(code) => write!(f, "```{}```", code),
        }
    }
}
