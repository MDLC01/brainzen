use std::fmt;
use std::fmt::{Display, Formatter};

use crate::exceptions::{CompilationResult, LocatedException};
use crate::location::{Located, Location};
use crate::parser::type_descriptor::TypeDescriptor;
use crate::type_checker::scope::ScopeStack;
use crate::type_checker::typed_expressions::{TypeCheckedExpression, TypedExpression};
use crate::utils::extensions::TryCollectResult;
use crate::utils::write_iterator;

/// The predicted value for a [`char`](Type::Char).
#[derive(Copy, Clone, Debug)]
pub enum PredictedChar {
    Exact(u8),
    Bool,
    Unknown,
}

/// A `Type` holds the data type of a value, at well at its value if the value is declared constant.
#[derive(Clone, Debug)]
pub enum Type {
    /// # Primitive `char` type
    ///
    /// Represents a single extended ASCII character, or an unsigned 8-bits integer.
    Char(PredictedChar),
    /// # Constant integer type
    ///
    /// An integer only exists at compile time. It has to be coerced to a [`char`](Self::Char)
    /// before being used as a runtime value.
    ///
    /// This type is mainly used for array lengths and subscripts.
    Integer(i32),
    /// # Product type
    ///
    /// A cartesian product the type of all tuples of elements of the types of the factors.
    Product(Vec<Type>),
}

impl Type {
    /// The type of a [`char`](Self::Char) that is known to have a boolean value.
    pub const BOOL: Self = Self::Char(PredictedChar::Bool);
    /// The type of a [`char`](Self::Char).
    pub const CHAR: Self = Self::Char(PredictedChar::Unknown);

    /// The type of a [`char`](Self::Char) that is known to have a specific boolean value.
    pub fn bool(value: bool) -> Self {
        Self::Char(PredictedChar::Exact(value as u8))
    }

    /// The type of a [`char`](Self::Char) that is known to have a specific value.
    pub fn char(value: u8) -> Self {
        Self::Char(PredictedChar::Exact(value))
    }

    /// Resolves a type descriptor within the specified context.
    pub(super) fn resolve_descriptor(context: &mut ScopeStack, location: Location, type_descriptor: TypeDescriptor) -> CompilationResult<Self> {
        match type_descriptor {
            TypeDescriptor::Reference(reference) if reference.value == "bool" => {
                Ok(Self::BOOL)
            }
            TypeDescriptor::Reference(reference) if reference.value == "char" => {
                Ok(Self::CHAR)
            }
            TypeDescriptor::Reference(Located { location, value: reference }) => {
                context.find_type(location, &reference)
            }
            TypeDescriptor::Product(factors) => {
                let mut evaluated_factors = Vec::new();
                for Located { location, value: operand } in factors {
                    evaluated_factors.push(Self::resolve_descriptor(context, location, operand)?)
                }
                Ok(Self::Product(evaluated_factors))
            }
            TypeDescriptor::Array(_) => {
                Err(LocatedException::unimplemented_arrays(location))
            }
        }
    }

    pub fn is_constant(&self) -> bool {
        match self {
            Self::Char(PredictedChar::Exact(_)) | Self::Integer(..) => true,
            Self::Product(factors) => {
                factors.iter()
                    .all(Type::is_constant)
            }
            _ => false,
        }
    }

    /// Tests if this type is a [`char`](Type::Char).
    pub fn is_char(&self) -> bool {
        matches!(self, Self::Char(_))
    }

    /// Tests if this type is a string type.
    ///
    /// A string type is defined inductively as either:
    /// * `char` (or assignable to a `char`), or
    /// * `T[n]` where `T` is a string type.
    pub fn is_string(&self) -> bool {
        match self {
            Self::Product(factors) => {
                factors.iter()
                    .all(Type::is_string)
            }
            r#type => {
                r#type.is_assignable_to(&Type::CHAR)
            }
        }
    }

    /// Returns the size of a variable of this type, in cells.
    pub fn size(&self) -> usize {
        match self {
            Self::Char(_) => 1,
            Self::Integer(_) => 1,
            Self::Product(factors) => {
                factors.iter()
                    .map(Type::size)
                    .sum()
            }
        }
    }

    /// Tests if this type is assignable to another type, meaning it can be coerced to this type.
    ///
    /// For example, a [constant integer](Type::Integer) is assignable to a [`char`](Type::Char).
    pub fn is_assignable_to(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Char(_) | Self::Integer(_), Self::Char(_)) => true,
            (Self::Integer(_), Self::Integer(_)) => true,
            (Self::Product(factors), Self::Product(other_factors)) => {
                factors.len() == other_factors.len()
                    && factors.iter()
                    .zip(other_factors)
                    .all(|(factor, other_factor)| factor.is_assignable_to(other_factor))
            }
            _ => false,
        }
    }

    pub fn is_same_concrete_type(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Char(_) | Self::Integer(_), Self::Char(_) | Self::Integer(_)) => true,
            (Self::Product(factors), Self::Product(other_factors)) => {
                factors.len() == other_factors.len()
                    && factors.iter()
                    .zip(other_factors)
                    .all(|(factor, other_factor)| factor.is_same_concrete_type(other_factor))
            }
            _ => false,
        }
    }
}

impl PartialEq for Type {
    /// Tests if two types are the same concrete type.
    fn eq(&self, other: &Self) -> bool {
        self.is_same_concrete_type(other)
    }
}


impl TryFrom<Type> for TypedExpression {
    type Error = LocatedException;

    fn try_from(r#type: Type) -> Result<Self, Self::Error> {
        match &r#type {
            Type::Char(PredictedChar::Exact(value)) => {
                Ok(Self {
                    expression: TypeCheckedExpression::Char(*value),
                    r#type,
                })
            }
            Type::Product(factors) => {
                let tuple_elements = factors.clone().into_iter()
                    .map(Self::try_from)
                    .try_collect()?;
                Ok(Self {
                    r#type,
                    expression: TypeCheckedExpression::Tuple(tuple_elements),
                })
            }
            Type::Integer(value) => {
                let char_value = (value % u8::MAX as i32) as u8;
                Ok(Self {
                    expression: TypeCheckedExpression::Char(char_value),
                    r#type: Type::Char(PredictedChar::Exact(char_value)),
                })
            }
            _ => {
                todo!("Write a proper error message with a location")
            }
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Char(_) => write!(f, "char"),
            Self::Integer(_) => write!(f, "<integer>"),
            Self::Product(factors) => write_iterator!(f, factors, " * ", "(", ")"),
        }
    }
}
