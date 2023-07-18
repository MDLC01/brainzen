use std::collections::HashMap;
use std::fmt::Debug;

use crate::exceptions::{CompilationException, CompilationResult};
use crate::location::{Located, Location, Sequence};
use crate::parser::target::{AssignmentTarget, AssignmentTargetDestination, DefinitionTarget, DefinitionTargetDestination, Target};
use crate::reference::Reference;
use crate::type_checker::scope::ScopeStack;
use crate::type_checker::types::Type;

/// A target is a pattern to which a value is assigned.
#[derive(Clone, Debug)]
pub enum TypeCheckedTarget<D> {
    Destination(Type, D),
    Tuple(Type, Vec<TypeCheckedTarget<D>>),
}

fn unpack_tuple<D, T, F>(mut target_checker: F, location: Location, tuple_targets: Sequence<Target<D>>, expected_type: &Type) -> CompilationResult<Vec<T>>
where
    F: FnMut(Located<Target<D>>, Type) -> CompilationResult<T> {
    match expected_type {
        Type::Product(factors) => {
            debug_assert_ne!(factors.len(), 1);
            if tuple_targets.len() != factors.len() {
                Err(CompilationException::invalid_unpack_size(location, factors.len(), tuple_targets.len()))
            } else {
                tuple_targets.into_iter()
                    .zip(factors)
                    .map(|(located_target, target_type)| {
                        target_checker(located_target, target_type.to_owned())
                    })
                    .collect()
            }
        }
        _ => {
            Err(CompilationException::unpack_non_tuple(location, expected_type))
        }
    }
}


pub type TypeCheckedDefinitionTarget = TypeCheckedTarget<String>;

impl TypeCheckedDefinitionTarget {
    fn type_check(Located(location, target): Located<DefinitionTarget>, expected_type: Type, created_variables: &mut HashMap<String, Located<Type>>) -> CompilationResult<Self> {
        match target {
            Target::Destination(DefinitionTargetDestination::Variable(identifier)) => {
                match created_variables.insert(identifier.to_owned(), Located(location.clone(), expected_type.to_owned())) {
                    None => {
                        Ok(Self::Destination(expected_type, identifier))
                    }
                    Some(Located(initial_location, _)) => {
                        Err(CompilationException::identifier_appears_multiple_times_in_target(location, identifier, initial_location))
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
    pub(super) fn type_check_and_register_variables(context: &mut ScopeStack, target: Located<DefinitionTarget>, expected_type: Type) -> CompilationResult<Self> {
        let mut created_variables = HashMap::new();
        let type_checked_target = Self::type_check(target, expected_type, &mut created_variables)?;
        for (identifier, Located(location, r#type)) in created_variables {
            context.register_variable(location, identifier, r#type)?
        }
        Ok(type_checked_target)
    }

    fn push_identifiers(&self, identifiers: &mut Vec<(String, usize)>) {
        match self {
            Self::Destination(r#type, identifier) => {
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


#[derive(Clone, Debug)]
pub struct TypeCheckedAssignmentTargetDestination {
    pub variable: Reference,
    /// Always zero for now.
    pub offset: usize,
}

impl TypeCheckedAssignmentTargetDestination {
    fn type_check(context: &mut ScopeStack, Located(location, destination): Located<AssignmentTargetDestination>, expected_type: &Type) -> CompilationResult<Self> {
        match destination {
            AssignmentTargetDestination::Variable(reference) => {
                let r#type = context.find_value_type(location.clone(), &reference)?;
                if r#type.is_assignable_to(expected_type) {
                    Ok(Self { variable: reference, offset: 0 })
                } else {
                    Err(CompilationException::wrong_type(location, expected_type, r#type))
                }
            }
            AssignmentTargetDestination::Subscript(..) => {
                Err(CompilationException::unimplemented_arrays(location))
            }
        }
    }
}


pub type TypeCheckedAssignmentTarget = TypeCheckedTarget<TypeCheckedAssignmentTargetDestination>;

impl TypeCheckedAssignmentTarget {
    pub(super) fn from_untyped(context: &mut ScopeStack, Located(location, target): Located<AssignmentTarget>, expected_type: Type) -> CompilationResult<Self> {
        match target {
            Target::Destination(destination) => {
                let type_checked_destination = TypeCheckedAssignmentTargetDestination::type_check(context, Located(location, destination), &expected_type)?;
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
            TypeCheckedAssignmentTarget::Destination(r#type, destination) => {
                destinations.push((destination.to_owned(), r#type.size()))
            }
            TypeCheckedAssignmentTarget::Tuple(_, targets) => {
                for target in targets {
                    target.push_destinations(destinations)
                }
            }
        }
    }

    pub fn get_destinations(&self) -> Vec<(TypeCheckedAssignmentTargetDestination, usize)> {
        let mut destinations = Vec::new();
        self.push_destinations(&mut destinations);
        destinations
    }
}
