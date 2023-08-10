use crate::exceptions::CompilationResult;
use crate::location::Location;
use crate::OptimizationSettings;
use crate::parser::BrainzenFile;
use crate::reference::Reference;
use crate::type_checker::scope::{Namespace, NamespaceContext};
use crate::type_checker::subroutines::TypeCheckedSubroutine;

pub mod types;
pub mod statements;
pub mod expressions;
pub mod subroutines;
pub mod targets;
pub mod operations;
mod scope;


/// Type checks a file and returns the type checked subroutines, as well as the index of the main
/// procedure in the vector.
pub fn type_check_and_get_subroutines(main_procedure_reference: &Reference, file: BrainzenFile, optimizations: &OptimizationSettings) -> CompilationResult<(Vec<TypeCheckedSubroutine>, usize)> {
    let mut context = NamespaceContext::default();
    Namespace::type_check_and_register_elements(&mut context, file.elements(), optimizations)?;
    let main_procedure_id = context.find_subroutine_index(Location::ARGS, main_procedure_reference)?;
    Ok((context.collect_subroutines(), main_procedure_id))
}

/// Type checks a file and returns the type checked subroutines, as well as the indices of the
/// subroutines under the specified test namespace.
#[cfg(test)]
pub fn get_tests(test_namespace: &Reference, file: BrainzenFile, optimizations: &OptimizationSettings) -> CompilationResult<(Vec<TypeCheckedSubroutine>, std::collections::HashMap<String, usize>)> {
    let mut context = NamespaceContext::default();
    Namespace::type_check_and_register_elements(&mut context, file.elements(), optimizations)?;
    let tests = context.find_subroutine_indices(Location::UNKNOWN, test_namespace)?;
    Ok((context.collect_subroutines(), tests))
}
