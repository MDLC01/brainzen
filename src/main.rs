#![warn(missing_debug_implementations)]

use std::fmt::Display;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::Path;

use utils::extensions::VecExtensions;

use crate::exceptions::CompilationResult;
use crate::generator::brainfuck_code::BrainfuckCode;
use crate::generator::compile_subroutines;
use crate::lexer::tokenize;
use crate::parser::parse_file;
use crate::reference::Reference;
use crate::type_checker::type_check_and_get_subroutines;

mod location;
mod exceptions;
mod parser;
mod lexer;
mod type_checker;
mod generator;
mod test;
mod utils;
mod reference;


/// A set of optimizations to apply or not.
#[derive(Debug)]
pub struct OptimizationSettings {
    /// Whether to precompute expressions whose value is known at compile time.
    ///
    /// For example, the expression `3 * 7` will be replaced by the literal `21` instead of being
    /// compiled as a multiplication, that will then be calculated at runtime.
    pub precompute_expressions: bool,
}

impl Default for OptimizationSettings {
    fn default() -> Self {
        Self {
            precompute_expressions: true,
        }
    }
}

impl OptimizationSettings {
    /// No optimization. Useful for debugging.
    pub fn none() -> Self {
        Self {
            precompute_expressions: false,
        }
    }
}


/// Writes content to a file.
fn write_file(path: impl AsRef<Path>, content: impl Display) {
    let mut file = File::create(path).unwrap();
    write!(file, "{}", content).unwrap();
}

const MAIN_SUBROUTINE: &str = "main";

/// Converts Brainzen source code to Brainfuck code.
fn compile(source: impl AsRef<Path>, content: &str, optimizations: &OptimizationSettings) -> CompilationResult<BrainfuckCode> {
    let tokens = tokenize(&source, content)?;
    let parsed_file = parse_file(&source, tokens)?;
    let main_subroutine = Reference::with_identifier(MAIN_SUBROUTINE);
    let (subroutines, main_procedure_id) = type_check_and_get_subroutines(&main_subroutine, parsed_file, optimizations)?;
    let compiled_subroutines = compile_subroutines(subroutines);
    Ok(compiled_subroutines.into_nth(main_procedure_id).unwrap())
}

const IN_PATH: &str = "tests/tests.bz";
const OUT_PATH: &str = "tests/out.bf";

fn main() -> Result<(), ()> {
    let input = fs::read_to_string(IN_PATH)
        .expect("Input file should be readable");
    match compile(IN_PATH, &input, &OptimizationSettings::default()) {
        Ok(code) => {
            write_file(OUT_PATH, code);
            Ok(())
        }
        Err(error) => {
            error.print_with_input(&input);
            Err(())
        }
    }
}
