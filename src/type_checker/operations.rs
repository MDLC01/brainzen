use crate::exceptions::{LocatedException, CompilationResult};
use crate::lexer::tokens::Symbol;
use crate::location::Location;
use crate::type_checker::types::{PredictedChar, Type};

macro_rules! define_operations {
    (
        $( #[$enum_attribute:meta] )*
        $visibility:vis $enum_name:ident($location:ident, $( $operand_name:ident ), + $(,)?) {
            $(
                $( #[$operation_attribute:meta] )*
                $symbol:pat => $operation_name:ident {
                    $(
                        $( ($( $operand_type:pat ), +) ) | + $( if $condition:expr )? => $return_type:expr
                    ), + $(,)?
                }
            )+
        }
    ) => {
        $( #[$enum_attribute] )*
        $visibility enum $enum_name {
            $(
                $( #[$operation_attribute] )*
                $operation_name(Type)
            ), +
        }

        impl $enum_name {
            pub(super) fn from_untyped($location: Location, operator: Symbol, $( $operand_name: Type ), +) -> CompilationResult<Self> {
                match (operator, [$( $operand_name ), +]) {
                    $(
                        $(
                            ($symbol, $( [$( $operand_type ), +] ) | +) $( if $condition )? => {
                                Ok(Self::$operation_name($return_type))
                            }
                        )+
                    )+
                    (operator, [$( $operand_name ), +]) => {
                        Err(LocatedException::invalid_operator($location, operator, &[$( $operand_name ), +]))
                    }
                }
            }

            /// Returns the type of the result of this operation.
            pub fn get_result_type(&self) -> Type {
                match self {
                    $( Self::$operation_name(return_type) => return_type.to_owned(), ) +
                }
            }

            /// If this operation returns a `char`, returns the optional predicted value.
            pub fn get_predicted_char_value(&self) -> Option<u8> {
                match self {
                    $( Self::$operation_name(Type::Char(PredictedChar::Exact(value))) => Some(*value), ) +
                    _ => None,
                }
            }
        }
    };
}

define_operations! {
    #[derive(Clone, Debug)]
    pub UnaryOperation(location, operand) {
        /// Negates a `char`.
        Symbol::Bang => Negation {
            (Type::Char(PredictedChar::Exact(value))) => Type::bool(value == 0),
            (Type::Char(_)) => Type::BOOL,
        }
        /// Converts the value of a `char` to a boolean value.
        Symbol::DoubleBang => BooleanNormalization {
            (Type::Char(PredictedChar::Exact(value))) => Type::bool(value != 0),
            (Type::Char(_)) => Type::BOOL,
        }
        /// Opposes a `char`. That is, computes `0 - x`.
        Symbol::Minus => Opposition {
            (Type::Char(PredictedChar::Exact(value))) => Type::char(value.wrapping_neg()),
            (Type::Char(_)) => Type::CHAR,
        }
    }
}


fn division(lhs: u8, rhs: u8) -> u8 {
    if rhs == 0 {
        0
    } else {
        lhs.wrapping_div(rhs)
    }
}

fn remainder(lhs: u8, rhs: u8) -> u8 {
    if rhs == 0 {
        0
    } else {
        lhs.wrapping_rem(rhs)
    }
}

define_operations! {
    #[derive(Clone, Debug)]
    pub BinaryOperation(location, left_operand, right_operand) {
        /// Tests if two values of the same type are equal.
        Symbol::DoubleEqual => EqualityTest {
            (Type::Char(PredictedChar::Exact(lhs)), Type::Char(PredictedChar::Exact(rhs))) => Type::bool(lhs == rhs),
            (Type::Integer(lhs), Type::Integer(rhs)) => Type::bool(lhs == rhs),
            (Type::Char(_), Type::Char(_)) | (Type::Char(_), Type::Integer(_)) | (Type::Integer(_), Type::Char(_)) => Type::BOOL,
            (lhs, rhs) if rhs.is_same_concrete_type(&lhs) => Type::BOOL,
        }
        /// Tests if two values of the same type are different.
        Symbol::BangEqual => DifferenceTest {
            (Type::Char(PredictedChar::Exact(lhs)), Type::Char(PredictedChar::Exact(rhs))) => Type::bool(lhs != rhs),
            (Type::Char(_), Type::Char(_)) => Type::BOOL,
            (Type::Integer(lhs), Type::Integer(rhs)) => Type::bool(lhs != rhs),
            (lhs, rhs) if rhs.is_same_concrete_type(&lhs) => Type::BOOL,
        }
        /// Tests if the left operand is strictly less than the right operand.
        Symbol::LessThan => StrictInequalityTest {
            (Type::Char(PredictedChar::Exact(lhs)), Type::Char(PredictedChar::Exact(rhs))) => Type::bool(lhs < rhs),
            (Type::Char(_), Type::Char(_)) => Type::BOOL,
            (Type::Integer(lhs), Type::Integer(rhs)) => Type::bool(lhs < rhs),
        }
        /// Tests if the left operand is less than or equal to the right operand.
        Symbol::LessThanEqual => LargeInequalityTest {
            (Type::Char(PredictedChar::Exact(lhs)), Type::Char(PredictedChar::Exact(rhs))) => Type::bool(lhs <= rhs),
            (Type::Char(_), Type::Char(_)) => Type::BOOL,
            (Type::Integer(lhs), Type::Integer(rhs)) => Type::bool(lhs <= rhs),
        }
        /// Tests if the left operand is strictly greater than the right operand.
        Symbol::GreaterThan => InverseStrictInequalityTest {
            (Type::Char(PredictedChar::Exact(lhs)), Type::Char(PredictedChar::Exact(rhs))) => Type::bool(lhs > rhs),
            (Type::Char(_), Type::Char(_)) => Type::BOOL,
            (Type::Integer(lhs), Type::Integer(rhs)) => Type::bool(lhs > rhs),
        }
        /// Tests if the left operand is greater than or equal to the right operand.
        Symbol::GreaterThanEqual => InverseLargeInequalityTest {
            (Type::Char(PredictedChar::Exact(lhs)), Type::Char(PredictedChar::Exact(rhs))) => Type::bool(lhs >= rhs),
            (Type::Char(_), Type::Char(_)) => Type::BOOL,
            (Type::Integer(lhs), Type::Integer(rhs)) => Type::bool(lhs >= rhs),
        }
        /// Computes the logical conjunction of two `char`s.
        Symbol::DoubleAmpersand => Conjunction {
            (Type::Char(PredictedChar::Exact(lhs)), Type::Char(PredictedChar::Exact(rhs))) => Type::bool((lhs != 0) && (rhs != 0)),
            (Type::Char(_), Type::Char(_)) => Type::BOOL,
            (Type::Integer(lhs), Type::Integer(rhs)) => Type::bool((lhs != 0) && (rhs != 0)),
        }
        /// Computes the logical disjunction of two `char`s.
        ///
        /// The result of this operation is a boolean value (either 0 or 1). To get the value of the
        /// first non-zero operand, use [`Self::Disjunction`].
        Symbol::DoublePipe => BooleanDisjunction {
            (Type::Char(PredictedChar::Exact(lhs)), Type::Char(PredictedChar::Exact(rhs))) => Type::bool((lhs != 0) || (rhs != 0)),
            (Type::Char(_), Type::Char(_)) => Type::BOOL,
            (Type::Integer(lhs), Type::Integer(rhs)) => Type::bool((lhs != 0) || (rhs != 0)),
        }
        /// Returns the left-hand side if non-zero, the right-hand side otherwise.
        ///
        /// To get a normalized boolean value (either 0 or 1), use [`Self::BooleanDisjunction`].
        Symbol::DoubleQuestionMark => Disjunction {
            (Type::Char(PredictedChar::Exact(lhs)), Type::Char(PredictedChar::Exact(rhs))) => Type::char(if lhs != 0 {lhs} else {rhs}),
            (Type::Char(_), Type::Char(_)) => Type::CHAR,
            (Type::Integer(lhs), Type::Integer(rhs)) => Type::Integer(if lhs != 0 {lhs} else {rhs}),
        }
        /// Computes the sum of two `char`s.
        ///
        /// Overflows result in a wrap around.
        Symbol::Plus => Addition {
            (Type::Char(PredictedChar::Exact(lhs)), Type::Char(PredictedChar::Exact(rhs))) => Type::char(lhs.wrapping_add(rhs)),
            (Type::Integer(lhs), Type::Integer(rhs)) => Type::Integer(lhs + rhs),
            (Type::Char(_) | Type::Integer(_), Type::Char(_) | Type::Integer(_)) => Type::CHAR,
        }
        /// Computes the subtraction of two `char`s.
        ///
        /// Overflows result in a wrap around.
        Symbol::Minus => Subtraction {
            (Type::Char(PredictedChar::Exact(lhs)), Type::Char(PredictedChar::Exact(rhs))) => Type::char(lhs.wrapping_sub(rhs)),
            (Type::Integer(lhs), Type::Integer(rhs)) => Type::Integer(lhs - rhs),
            (Type::Char(_) | Type::Integer(_), Type::Char(_) | Type::Integer(_)) => Type::CHAR,
        }
        /// Computes the product of two `char`s.
        ///
        /// Overflows result in a wrap around.
        Symbol::Star => Multiplication {
            (Type::Char(PredictedChar::Exact(lhs)), Type::Char(PredictedChar::Exact(rhs))) => Type::char(lhs.wrapping_mul(rhs)),
            (Type::Integer(lhs), Type::Integer(rhs)) => Type::Integer(lhs * rhs),
            (Type::Char(_) | Type::Integer(_), Type::Char(_) | Type::Integer(_)) => Type::CHAR,
        }
        /// Computes the division of two `char`s.
        ///
        /// Division by zero has an undefined result.
        Symbol::Slash => Division {
            (Type::Char(PredictedChar::Exact(lhs)), Type::Char(PredictedChar::Exact(rhs))) => Type::char(division(lhs, rhs)),
            (Type::Integer(lhs), Type::Integer(rhs)) => Type::Integer(lhs / rhs),
            (Type::Char(_) | Type::Integer(_), Type::Char(_) | Type::Integer(_)) => Type::CHAR,
        }
        /// Computes the remainder of the euclidean division of the left `char` by the right `char`.
        ///
        /// Modulo zero has an undefined result.
        Symbol::Percent => Modulo {
            (Type::Char(_), Type::Char(PredictedChar::Exact(0))) => Type::CHAR,
            (Type::Char(PredictedChar::Exact(lhs)), Type::Char(PredictedChar::Exact(rhs))) => Type::char(remainder(lhs, rhs)),
            (Type::Integer(lhs), Type::Integer(rhs)) => Type::Integer(lhs % rhs),
            (Type::Char(_) | Type::Integer(_), Type::Char(_) | Type::Integer(_)) => Type::CHAR,
        }
        /// Computes the euclidean division of the left `char` by the right `char`.
        ///
        /// Division by zero has an undefined result.
        Symbol::DoublePercent => EuclideanDivision {
            (Type::Char(PredictedChar::Exact(lhs)), Type::Char(PredictedChar::Exact(rhs))) => {
                let div = division(lhs, rhs);
                let rem = remainder(lhs, rhs);
                Type::Product(vec![Type::char(div), Type::char(rem)])
            },
            (Type::Integer(lhs), Type::Integer(rhs)) => Type::Product(vec![Type::Integer(lhs / rhs), Type::Integer(lhs % rhs)]),
            (Type::Char(_) | Type::Integer(_), Type::Char(_) | Type::Integer(_)) => Type::Product(vec![Type::CHAR, Type::CHAR]),
        }

    }
}
