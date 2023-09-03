use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::exceptions::{CompilationException, CompilationResult};
use crate::lexer::lexemes::Symbol;
use crate::location::{Located, Location};
use crate::type_checker::types::{Type, Value};
use crate::utils::product::Product;

/// An `Operation` has a [result type](Self::result_type) and can be [applied to values](Self::apply).
pub trait Operation<const N: usize>: Copy {
    /// Returns the operation corresponding to an operator and the types of its operands.
    fn from_untyped(location: Location, operator: Symbol, operands: [&Type; N]) -> CompilationResult<Self>;

    /// Returns the type of the result of this operation.
    fn result_type(self) -> Type;

    /// Applies this operation to a constant value.
    fn apply(self, operands: [Located<Value>; N]) -> CompilationResult<Value>;
}


#[derive(Copy, Clone, Debug)]
pub enum UnaryOperation {
    Negation,
    BooleanNormalization,
    Opposition,
}

impl Operation<1> for UnaryOperation {
    fn from_untyped(location: Location, operator: Symbol, operands: [&Type; 1]) -> CompilationResult<Self> {
        match (operator, operands) {
            (Symbol::Bang, [Type::Char]) => Ok(Self::Negation),
            (Symbol::DoubleBang, [Type::Char]) => Ok(Self::BooleanNormalization),
            (Symbol::Minus, [Type::Char]) => Ok(Self::Opposition),
            _ => Err(CompilationException::invalid_operator(location, operator, &operands))
        }
    }

    fn result_type(self) -> Type {
        match self {
            Self::Negation => Type::Char,
            Self::BooleanNormalization => Type::Char,
            Self::Opposition => Type::Char,
        }
    }

    fn apply(self, [operand]: [Located<Value>; 1]) -> CompilationResult<Value> {
        match self {
            Self::Negation => Ok(Value::bool(!operand.into_bool()?)),
            Self::BooleanNormalization => Ok(Value::bool(operand.into_bool()?)),
            Self::Opposition => Ok(Value::Char(operand.into_char()?.wrapping_neg())),
        }
    }
}

impl Display for UnaryOperation {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Negation => write!(f, "negation"),
            Self::BooleanNormalization => write!(f, "boolean normalization"),
            Self::Opposition => write!(f, "opposition"),
        }
    }
}


#[derive(Copy, Clone, Debug)]
pub enum BinaryOperation {
    /// Tests if two values of the same type are equal.
    EqualityTest,
    /// Tests if two values of the same type are different.
    DifferenceTest,
    /// Tests if the left operand is strictly less than the right operand.
    StrictInequalityTest,
    /// Tests if the left operand is less than or equal to the right operand.
    LargeInequalityTest,
    /// Tests if the left operand is strictly greater than the right operand.
    InverseStrictInequalityTest,
    /// Tests if the left operand is greater than or equal to the right operand.
    InverseLargeInequalityTest,
    /// Computes the logical conjunction of two [`char`s](Type::Char).
    Conjunction,
    /// Computes the logical disjunction of two [`char`s](Type::Char).
    ///
    /// The result of this operation is a boolean value (either 0 or 1). To get the value of the
    /// first non-zero operand, use [`Self::Disjunction`].
    BooleanDisjunction,
    /// Returns the left-hand side if non-zero, the right-hand side otherwise.
    ///
    /// To get a normalized boolean value (either 0 or 1), use [`Self::BooleanDisjunction`].
    Disjunction,
    /// Computes the sum of two [`char`s](Type::Char).
    ///
    /// Overflows result in a wrap around.
    Addition,
    /// Computes the subtraction of two [`char`s](Type::Char).
    ///
    /// Overflows result in a wrap around.
    Subtraction,
    /// Computes the product of two [`char`s](Type::Char).
    ///
    /// Overflows result in a wrap around.
    Multiplication,
    /// Computes the floor division of two [`char`s](Type::Char).
    ///
    /// Division by zero has an undefined result.
    Division,
    /// Computes the remainder of the euclidean division of two [`char`s](Type::Char).
    ///
    /// Modulo zero has an undefined result.
    Modulo,
    /// Computes the euclidean division of two [`char`s](Type::Char).
    ///
    /// Division by zero has an undefined result.
    EuclideanDivision,
}

fn division(lhs: u8, rhs: u8) -> u8 {
    if rhs == 0 {
        0
    } else {
        lhs.wrapping_div(rhs)
    }
}

fn modulo(lhs: u8, rhs: u8) -> u8 {
    if rhs == 0 {
        0
    } else {
        lhs.wrapping_rem(rhs)
    }
}

impl Operation<2> for BinaryOperation {
    fn from_untyped(location: Location, operator: Symbol, operands: [&Type; 2]) -> CompilationResult<Self> {
        match (operator, operands) {
            (Symbol::DoubleEqual, _) => Ok(Self::EqualityTest),
            (Symbol::BangEqual, _) => Ok(Self::DifferenceTest),
            (Symbol::LessThan, [Type::Char, Type::Char]) => Ok(Self::StrictInequalityTest),
            (Symbol::LessThanEqual, [Type::Char, Type::Char]) => Ok(Self::LargeInequalityTest),
            (Symbol::GreaterThan, [Type::Char, Type::Char]) => Ok(Self::InverseStrictInequalityTest),
            (Symbol::GreaterThanEqual, [Type::Char, Type::Char]) => Ok(Self::InverseLargeInequalityTest),
            (Symbol::DoubleAmpersand, [Type::Char, Type::Char]) => Ok(Self::Conjunction),
            (Symbol::DoublePipe, [Type::Char, Type::Char]) => Ok(Self::BooleanDisjunction),
            (Symbol::DoubleQuestionMark, [Type::Char, Type::Char]) => Ok(Self::Disjunction),
            (Symbol::Plus, [Type::Char, Type::Char]) => Ok(Self::Addition),
            (Symbol::Minus, [Type::Char, Type::Char]) => Ok(Self::Subtraction),
            (Symbol::Star, [Type::Char, Type::Char]) => Ok(Self::Multiplication),
            (Symbol::Slash, [Type::Char, Type::Char]) => Ok(Self::Division),
            (Symbol::Percent, [Type::Char, Type::Char]) => Ok(Self::Modulo),
            (Symbol::DoublePercent, [Type::Char, Type::Char]) => Ok(Self::EuclideanDivision),
            _ => Err(CompilationException::invalid_operator(location, operator, &operands))
        }
    }

    fn result_type(self) -> Type {
        match self {
            Self::EqualityTest => Type::Char,
            Self::DifferenceTest => Type::Char,
            Self::StrictInequalityTest => Type::Char,
            Self::LargeInequalityTest => Type::Char,
            Self::InverseStrictInequalityTest => Type::Char,
            Self::InverseLargeInequalityTest => Type::Char,
            Self::Conjunction => Type::Char,
            Self::BooleanDisjunction => Type::Char,
            Self::Disjunction => Type::Char,
            Self::Addition => Type::Char,
            Self::Subtraction => Type::Char,
            Self::Multiplication => Type::Char,
            Self::Division => Type::Char,
            Self::Modulo => Type::Char,
            Self::EuclideanDivision => Type::from_operands(&[Type::Char, Type::Char]),
        }
    }

    fn apply(self, [lhs, rhs]: [Located<Value>; 2]) -> CompilationResult<Value> {
        match self {
            BinaryOperation::EqualityTest => Ok(Value::bool(lhs == rhs)),
            BinaryOperation::DifferenceTest => Ok(Value::bool(lhs != rhs)),
            BinaryOperation::StrictInequalityTest => Ok(Value::bool(lhs.into_char()? < rhs.into_char()?)),
            BinaryOperation::LargeInequalityTest => Ok(Value::bool(lhs.into_char()? <= rhs.into_char()?)),
            BinaryOperation::InverseStrictInequalityTest => Ok(Value::bool(lhs.into_char()? > rhs.into_char()?)),
            BinaryOperation::InverseLargeInequalityTest => Ok(Value::bool(lhs.into_char()? >= rhs.into_char()?)),
            BinaryOperation::Conjunction => Ok(Value::bool(lhs.into_bool()? && rhs.into_bool()?)),
            BinaryOperation::BooleanDisjunction => Ok(Value::bool(lhs.into_bool()? || rhs.into_bool()?)),
            BinaryOperation::Disjunction => Ok(Value::Char({
                let l = lhs.into_char()?;
                let r = rhs.into_char()?;
                if l == 0 { r } else { l }
            })),
            BinaryOperation::Addition => Ok(Value::Char(lhs.into_char()?.wrapping_add(rhs.into_char()?))),
            BinaryOperation::Subtraction => Ok(Value::Char(lhs.into_char()?.wrapping_sub(rhs.into_char()?))),
            BinaryOperation::Multiplication => Ok(Value::Char(lhs.into_char()?.wrapping_mul(rhs.into_char()?))),
            BinaryOperation::Division => Ok(Value::Char(division(lhs.into_char()?, rhs.into_char()?))),
            BinaryOperation::Modulo => Ok(Value::Char(modulo(lhs.into_char()?, rhs.into_char()?))),
            BinaryOperation::EuclideanDivision => Ok({
                let l = lhs.into_char()?;
                let r = rhs.into_char()?;
                let div = Value::Char(division(l, r));
                let rem = Value::Char(modulo(l, r));
                Value::Tuple(Rc::new(Product::new_minimal([div, rem])))
            }),
        }
    }
}

impl Display for BinaryOperation {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::EqualityTest => write!(f, "equality test"),
            Self::DifferenceTest => write!(f, "difference test"),
            Self::StrictInequalityTest => write!(f, "strict inequality test"),
            Self::LargeInequalityTest => write!(f, "large inequality test"),
            Self::InverseStrictInequalityTest => write!(f, "inverse strict equality test"),
            Self::InverseLargeInequalityTest => write!(f, "inverse large inequality test"),
            Self::Conjunction => write!(f, "conjunction"),
            Self::BooleanDisjunction => write!(f, "boolean disjunction"),
            Self::Disjunction => write!(f, "disjunction"),
            Self::Addition => write!(f, "addition"),
            Self::Subtraction => write!(f, "subtraction"),
            Self::Multiplication => write!(f, "multiplication"),
            Self::Division => write!(f, "division"),
            Self::Modulo => write!(f, "modulo"),
            Self::EuclideanDivision => write!(f, "euclidean division"),
        }
    }
}
