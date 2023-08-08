use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

use crate::exceptions::{LocatedException, CompilationResult};
use crate::location::{Located, Location};
use crate::parser::target::{AssignmentTarget, AssignmentTargetDestination, DefinitionTarget, DefinitionTargetDestination, Target};
use crate::type_checker::scope::SubroutineContext;
use crate::type_checker::types::Type;
use crate::utils::extensions::TryCollectResult;
use crate::utils::product::{MaybeProduct, MaybeProduct2, Product};

fn unpack_tuple<D, T, F>(mut target_checker: F, location: Location, tuple_targets: Product<Located<Target<D>>, 2>, expected_type: &Type) -> CompilationResult<Product<T, 2>>
where
    F: FnMut(Located<Target<D>>, Type) -> CompilationResult<T>
{
    match expected_type {
        Type::Product(factors) => {
            if tuple_targets.len() != factors.len() {
                Err(LocatedException::invalid_unpack_size(location, factors.len(), tuple_targets.len()))
            } else {
                Ok(
                    tuple_targets.into_iter()
                        .zip(factors.iter())
                        .map(|(located_target, target_type)| {
                            target_checker(located_target, target_type.to_owned())
                        })
                        .try_collect::<MaybeProduct2<T>>()?
                        .product()
                        // SAFETY: The iterator is of length at least 2 because it comes from
                        // zipping two iterators of length at least 2.
                        .unwrap()
                )
            }
        }
        _ => {
            Err(LocatedException::unpack_non_tuple(location, expected_type))
        }
    }
}


/// The target of a definition: a pattern containing variables to define.
#[derive(Debug)]
pub enum TypeCheckedDefinitionTarget {
    Unit,
    Variable(Type, String),
    Tuple(Type, Product<TypeCheckedDefinitionTarget, 2>),
}

impl TypeCheckedDefinitionTarget {
    fn type_check(Located { location, value: target }: Located<DefinitionTarget>, expected_type: Type, created_variables: &mut HashMap<String, Located<Type>>) -> CompilationResult<Self> {
        match target {
            Target::Unit => {
                Ok(Self::Unit)
            }
            Target::Destination(DefinitionTargetDestination::Variable(identifier)) => {
                match created_variables.insert(identifier.to_owned(), Located::new(location.clone(), expected_type.to_owned())) {
                    None => {
                        Ok(Self::Variable(expected_type, identifier))
                    }
                    Some(Located { location: initial_location, value: _ }) => {
                        Err(LocatedException::identifier_appears_multiple_times_in_target(location, identifier, initial_location))
                    }
                }
            }
            Target::Tuple(tuple_targets) => {
                let type_checked_tuple_targets = unpack_tuple(
                    |located_element, target_type| Self::type_check(located_element, target_type, created_variables),
                    location,
                    tuple_targets,
                    &expected_type,
                )?;
                Ok(Self::Tuple(expected_type, type_checked_tuple_targets))
            }
        }
    }

    /// Type checks a target and registers the defined variables to the current context.
    pub(super) fn type_check_and_register_variables(context: &mut SubroutineContext, target: Located<DefinitionTarget>, expected_type: Type) -> CompilationResult<Self> {
        let mut created_variables = HashMap::new();
        let type_checked_target = Self::type_check(target, expected_type, &mut created_variables)?;
        for (identifier, Located { location, value: r#type }) in created_variables {
            context.register_variable(location, identifier, r#type)?
        }
        Ok(type_checked_target)
    }

    fn push_identifiers(&self, identifiers: &mut Vec<(String, usize)>) {
        match self {
            Self::Unit => {}
            Self::Variable(r#type, identifier) => {
                identifiers.push((identifier.to_owned(), r#type.size()))
            }
            Self::Tuple(_, targets) => {
                for target in targets {
                    target.push_identifiers(identifiers)
                }
            }
        }
    }

    /// Returns a vector of `(identifier, size)` pairs corresponding to the variables that are
    /// defined by this definition target, in the order in which they are laid out in memory.
    pub fn get_identifiers(&self) -> Vec<(String, usize)> {
        let mut identifiers = Vec::new();
        self.push_identifiers(&mut identifiers);
        identifiers
    }
}


/// The destination of a [`TypeCheckedAssignmentTarget`]: at some offset of a variable.
#[derive(Clone, Debug)]
pub struct TypeCheckedAssignmentTargetDestination {
    pub variable: Rc<String>,
    /// Always zero for now.
    pub offset: usize,
}

impl TypeCheckedAssignmentTargetDestination {
    fn type_check(context: &mut SubroutineContext, Located { location, value: destination }: Located<AssignmentTargetDestination>, expected_type: &Type) -> CompilationResult<Self> {
        match destination {
            AssignmentTargetDestination::Variable(identifier) => {
                let r#type = context.get_variable_type(location.clone(), &identifier)?;
                if expected_type.is_assignable_to(&r#type) {
                    Ok(Self { variable: Rc::new(identifier), offset: 0 })
                } else {
                    Err(LocatedException::wrong_type(location, expected_type, &r#type))
                }
            }
            AssignmentTargetDestination::Subscript(..) => {
                Err(LocatedException::unimplemented_arrays(location))
            }
        }
    }
}


/// The target of an assignment: an irrefutable pattern.
#[derive(Debug)]
pub enum TypeCheckedAssignmentTarget {
    Unit,
    Destination(Type, TypeCheckedAssignmentTargetDestination),
    Tuple(Type, Product<TypeCheckedAssignmentTarget, 2>),
}

impl TypeCheckedAssignmentTarget {
    pub(super) fn from_untyped(context: &mut SubroutineContext, Located { location, value: target }: Located<AssignmentTarget>, expected_type: Type) -> CompilationResult<Self> {
        match target {
            Target::Unit => {
                Ok(Self::Unit)
            }
            Target::Destination(destination) => {
                let type_checked_destination = TypeCheckedAssignmentTargetDestination::type_check(context, Located::new(location, destination), &expected_type)?;
                Ok(Self::Destination(expected_type, type_checked_destination))
            }
            Target::Tuple(tuple_targets) => {
                let type_checked_tuple_targets = unpack_tuple(
                    |located_element, target_type| Self::from_untyped(context, located_element, target_type),
                    location,
                    tuple_targets,
                    &expected_type,
                )?;
                Ok(Self::Tuple(expected_type, type_checked_tuple_targets))
            }
        }
    }

    fn push_destinations(&self, destinations: &mut Vec<(TypeCheckedAssignmentTargetDestination, usize)>) {
        match self {
            Self::Unit => {
                // No destination
            }
            Self::Destination(r#type, destination) => {
                destinations.push((destination.to_owned(), r#type.size()))
            }
            Self::Tuple(_, targets) => {
                for target in targets {
                    target.push_destinations(destinations)
                }
            }
        }
    }

    /// Returns a list of destinations that this target contains as well as, for each destination,
    /// its size, in cells.
    pub fn get_destinations(&self) -> Vec<(TypeCheckedAssignmentTargetDestination, usize)> {
        let mut destinations = Vec::new();
        self.push_destinations(&mut destinations);
        destinations
    }
}
