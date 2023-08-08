use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::exceptions::{CompilationResult, LocatedException};
use crate::location::{Located, Location};
use crate::parser::expression::Expression;
use crate::parser::type_descriptor::TypeDescriptor;
use crate::type_checker::operations::{BinaryOperation, Operation, UnaryOperation};
use crate::type_checker::scope::NamespaceContext;
use crate::type_checker::typed_expressions::{TypeCheckedExpression, TypedExpression};
use crate::utils::extensions::TryCollectResult;
use crate::utils::product::{MaybeProduct2, Product};
use crate::utils::write_iterator;

/// The data type of any value in Brainzen.
///
/// A `Type` is cheap to clone.
#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum Type {
    /// # Primitive `()` ("unit") type
    ///
    /// A zero-sized type.
    ///
    /// This is the implicit return type of all functions that return nothing.
    Unit,
    /// # Primitive `char` type
    ///
    /// A `char` represents a single extended ASCII character, or an unsigned 8-bit integer.
    Char,
    /// # Product type
    ///
    /// A cartesian product: the type of a tuple.
    Product(Rc<Product<Type, 2>>),
}

impl Type {
    /// Resolves a type descriptor within the specified context.
    pub(super) fn resolve_descriptor(context: &mut NamespaceContext, location: Location, type_descriptor: TypeDescriptor) -> CompilationResult<Self> {
        match type_descriptor {
            TypeDescriptor::Unit => {
                Ok(Self::Unit)
            }
            TypeDescriptor::Reference(reference) if reference.value == "char" => {
                Ok(Self::Char)
            }
            TypeDescriptor::Reference(Located { location, value: reference }) => {
                context.find_type(location, &reference)
            }
            TypeDescriptor::Product(factors) => {
                let mut evaluated_factors = Vec::new();
                for Located { location, value: operand } in factors {
                    evaluated_factors.push(Self::resolve_descriptor(context, location, operand)?)
                }
                // We reverse so we can `pop`
                evaluated_factors.reverse();
                let Some(first) = evaluated_factors.pop() else {
                    return Ok(Self::Unit);
                };
                let Some(second) = evaluated_factors.pop() else {
                    return Ok(first);
                };
                evaluated_factors.reverse();
                let operands = Product::new([first, second], evaluated_factors);
                Ok(Self::Product(Rc::new(operands)))
            }
            TypeDescriptor::Array(_) => {
                Err(LocatedException::unimplemented_arrays(location))
            }
        }
    }

    /// Creates a [`Type`] from the operands of a product:
    /// - If no operand is passed, the type is a [unit](Type::Unit),
    /// - If a single operand is passed, the type is the operand,
    /// - Otherwise, the type is a [product](Type::Product) of the operands.
    pub fn from_operands(operands: &[Type]) -> Self {
        match operands {
            [] => Self::Unit,
            [r#type] => r#type.to_owned(),
            [first, second, remainder @ ..] => {
                Self::Product(Rc::new(Product::new([first.to_owned(), second.to_owned()], remainder)))
            }
        }
    }

    /// Tests if this type can be assigned to another type.
    ///
    /// Note that is assignability relation is *not* guaranteed to be an equivalence relation.
    pub fn is_assignable_to(&self, other: &Type) -> bool {
        self == other
    }

    /// Tests if this type is a string type.
    ///
    /// A string type is defined inductively as either:
    /// * A type that is assignable to a `char`, or
    /// * A (possibly empty, i.e., unit) product of string types.
    pub fn is_string(&self) -> bool {
        match self {
            Self::Unit => true,
            Self::Product(factors) => {
                factors.iter()
                    .all(Self::is_string)
            }
            r#type => {
                r#type.is_assignable_to(&Self::Char)
            }
        }
    }

    /// Returns the size of a variable of this type, in cells.
    pub fn size(&self) -> usize {
        match self {
            Self::Unit => 0,
            Self::Char => 1,
            Self::Product(factors) => {
                factors.iter()
                    .map(Self::size)
                    .sum()
            }
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Char => write!(f, "char"),
            Self::Product(factors) => write_iterator!(f, factors.iter(), " * ", "(", ")")
        }
    }
}


/// The representation in the Rust type system of a Brainzen value of.
///
/// A `Value` is cheap to clone.
#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum Value {
    /// A Brainzen [unit](Type::Unit).
    Unit,
    /// A Brainzen [`char`](Type::Char).
    Char(u8),
    /// A Brainzen [tuple](Type::Product).
    Tuple(Rc<Product<Value, 2>>),
}

impl Value {
    /// Creates a new `Value::Char` corresponding to a boolean (1 for `true`, 0 for `false`).
    pub fn bool(value: bool) -> Self {
        if value {
            Self::Char(1)
        } else {
            Self::Char(0)
        }
    }

    /// Returns the [`Type`] of a `Value`.
    pub fn get_type(&self) -> Type {
        match self {
            Self::Unit => Type::Unit,
            Self::Char(_) => Type::Char,
            Self::Tuple(elements) => Type::Product(Rc::new(
                elements.map_ref(|value| value.get_type())
            )),
        }
    }

    /// Evaluates a constant expression.
    pub(super) fn evaluate(context: &mut NamespaceContext, Located { location, value: expression }: Located<Expression>) -> CompilationResult<Self> {
        match expression {
            Expression::CharacterLiteral(value) => {
                Ok(Self::Char(value))
            }
            Expression::IntegerLiteral(value) => {
                if let Ok(c) = value.try_into() {
                    Ok(Self::Char(c))
                } else {
                    Err(LocatedException::does_not_fit_in_char(location, value))
                }
            }
            Expression::TupleLiteral(elements) => {
                elements.into_iter()
                    .map(|element| Self::evaluate(context, element))
                    .try_collect()
            }
            Expression::Variable(reference) => {
                context.find_constant_value(reference.location, &reference.value)
            }
            Expression::UnaryArithmetic(operator, operand) => {
                let operand_location = operand.location();
                let evaluated_operand = Value::evaluate(context, operand.unboxed())?;
                let operation = UnaryOperation::from_untyped(location, operator, [&evaluated_operand.get_type()])?;
                operation.apply([Located::new(operand_location, evaluated_operand)])
            }
            Expression::BinaryArithmetic(operator, left_operand, right_operand) => {
                let left_operand_location = left_operand.location();
                let evaluated_left_operand = Value::evaluate(context, left_operand.unboxed())?;
                let right_operand_location = right_operand.location();
                let evaluated_right_operand = Value::evaluate(context, right_operand.unboxed())?;
                let operation = BinaryOperation::from_untyped(location, operator, [&evaluated_left_operand.get_type(), &evaluated_right_operand.get_type()])?;
                operation.apply([Located::new(left_operand_location, evaluated_left_operand), Located::new(right_operand_location, evaluated_right_operand)])
            }
            _ => {
                Err(LocatedException::expected_constant_value(location))
            }
        }
    }
}

impl FromIterator<Value> for Value {
    fn from_iter<T: IntoIterator<Item=Value>>(iter: T) -> Self {
        match MaybeProduct2::from_iter(iter) {
            MaybeProduct2::None => Self::Unit,
            MaybeProduct2::Single(value) => value,
            MaybeProduct2::Product(elements) => Self::Tuple(Rc::new(elements)),
        }
    }
}

impl Located<Value> {
    /// If the associated value is a [`Value::Char`], returns its value as a Rust `bool` (`true` for
    /// any non-zero value, `false` otherwise).
    pub fn into_bool(self) -> CompilationResult<bool> {
        match self.value {
            Value::Char(value) => Ok(value != 0),
            _ => Err(LocatedException::wrong_type(self.location, &Type::Char, &self.value.get_type()))
        }
    }

    /// If the associated value is a [`Value::Char`], returns its value as a Rust `u8`. Otherwise,
    /// returns an error saying a `char` was expected.
    pub fn into_char(self) -> CompilationResult<u8> {
        match self.value {
            Value::Char(value) => Ok(value),
            _ => Err(LocatedException::wrong_type(self.location, &Type::Char, &self.value.get_type())),
        }
    }
}


impl From<Value> for TypedExpression {
    fn from(value: Value) -> Self {
        let r#type = value.get_type();
        match value {
            Value::Unit => {
                Self {
                    expression: TypeCheckedExpression::Unit,
                    r#type,
                }
            }
            Value::Char(c) => {
                Self {
                    expression: TypeCheckedExpression::Char(c),
                    r#type,
                }
            }
            Value::Tuple(elements) => {
                let typed_elements = (*elements).clone().map(Self::from);
                Self {
                    expression: TypeCheckedExpression::Tuple(typed_elements),
                    r#type,
                }
            }
        }
    }
}
