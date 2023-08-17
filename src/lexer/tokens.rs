use std::fmt;
use std::fmt::{Display, Formatter};

use crate::location::Sequence;
use crate::reference::Reference;
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


#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Token {
    /// Represents a symbol (like `==`, `->` or `}`).
    Symbol(Symbol),
    /// Represents a [`Reference`].
    ///
    /// Might be a single word, i.e., a sequence of word characters (`A`–`Z`, `a`–`z`, `0`–`9`)
    /// surrounded by non-word characters.
    ///
    /// Note that keywords are actually parsed as a reference with no namespace. There is no such
    /// thing as keywords from the lexer's point of view.
    Reference(Reference),
    /// Represents a numeric literal.
    Numeric(u32),
    /// Represents a character literal.
    ///
    /// A character literal consists of a single ASCII character, surrounded by single quotes.
    Character(u8),
    /// Represents a string literal.
    String(Sequence<u8>),
    /// Represents a block of native (Brainfuck) code.
    NativeCodeBlock(String),
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Token::Symbol(symbol) => write!(f, "{}", symbol),
            Token::Reference(reference) => write!(f, "{}", reference),
            Token::Numeric(value) => write!(f, "{}", value),
            Token::Character(character) => write!(f, "'{}'", *character as char),
            Token::String(characters) => write_iterator!(f, characters, "", "\"", "\""),
            Token::NativeCodeBlock(code) => write!(f, "```{}```", code),
        }
    }
}
