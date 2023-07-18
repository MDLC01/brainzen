//! This module is responsible for running all tests.
//!
//! See `/tests/README.md` for more information.

#![cfg(test)]

use std::fs;
use std::path::PathBuf;

use crate::exceptions::CompilationResult;
use crate::generator::compile_subroutines;
use crate::lexer::tokenize;
use crate::parser::parse_file;
use crate::reference::Reference;
use crate::test::brainfuck::interpret;
use crate::type_checker::get_tests;

mod brainfuck {
    use std::cmp::min;

    #[derive(Debug, Default)]
    struct Tape {
        pointer: isize,
        values: Vec<u8>,
        /// Index of the value that corresponds to the initial cell.
        origin: isize,
    }

    impl Tape {
        /// Creates a new tape containing zeroed cells.
        pub fn new() -> Self {
            Self::default()
        }

        /// Moves the pointer to the left.
        pub fn left(&mut self) {
            self.pointer -= 1;
        }

        /// Moves the pointer to the right.
        pub fn right(&mut self) {
            self.pointer += 1;
        }

        fn first_index(&self) -> isize {
            -self.origin
        }

        fn last_index(&self) -> isize {
            (min(self.values.len(), isize::MAX as usize) as isize) - self.origin - 1
        }

        /// Extends the tape to make the specified index valid.
        fn extend_to_index(&mut self, index: isize) {
            if index > self.last_index() {
                self.values.resize((self.origin + index + 1) as usize, 0)
            } else if index < self.first_index() {
                let mut new_values: Vec<u8> = Vec::new();
                new_values.resize((-index - self.origin) as usize, 0);
                new_values.append(&mut self.values);
                self.values = new_values;
                self.origin += -index - self.origin
            }
        }

        /// Returns a mutable reference a cell.
        fn get_current_cell_mut(&mut self) -> &mut u8 {
            self.extend_to_index(self.pointer);
            &mut self.values[(self.origin + self.pointer) as usize]
        }

        /// Increments the value of the current cell.
        pub fn increment(&mut self) {
            let cell = self.get_current_cell_mut();
            *cell = cell.wrapping_add(1);
        }

        /// Decrements the value of the current cell.
        pub fn decrement(&mut self) {
            let cell = self.get_current_cell_mut();
            *cell = cell.wrapping_sub(1);
        }

        /// Returns the value of the current cell.
        pub fn get(&self) -> u8 {
            if (self.first_index()..=self.last_index()).contains(&self.pointer) {
                self.values[(self.origin + self.pointer) as usize]
            } else {
                0
            }
        }
    }

    /// Interprets some Brainfuck code and returns `(stdout, captures)`.
    pub fn interpret(input: &str) -> (Vec<u8>, Vec<u8>) {
        let chars: Vec<_> = input.chars().collect();
        let mut tape = Tape::new();
        let mut out = Vec::new();
        let mut captures = Vec::new();
        let mut loop_stack = Vec::new();
        let mut i = 0;
        loop {
            match chars.get(i) {
                Some('>') => tape.right(),
                Some('<') => tape.left(),
                Some('+') => tape.increment(),
                Some('-') => tape.decrement(),
                Some('.') => out.push(tape.get()),
                Some(',') => panic!("Tests are not allowed to read from stdin"),
                Some('@') => captures.push(tape.get()),
                Some('[') => {
                    if tape.get() == 0 {
                        let mut depth = 1;
                        let mut j = i + 1;
                        while depth != 0 {
                            match chars.get(j) {
                                Some('[') => depth += 1,
                                Some(']') => depth -= 1,
                                None => panic!("Unmatched `[` at position {}", i),
                                _ => {}
                            }
                            j += 1;
                        }
                        i = j - 1;
                    } else {
                        loop_stack.push(i)
                    }
                }
                Some(']') => {
                    let Some(j) = loop_stack.pop() else {
                        panic!("Unmatched `]` at position {}", i)
                    };
                    i = j - 1;
                }
                Some(_) => {}
                None => break,
            }
            i += 1;
        }
        (out, captures)
    }
}

const TEST_NAMESPACE: &str = "Tests";
const REFERENCE_DIRECTORY: &str = "tests/reference";
const TEST_FILE: &str = "tests/tests.bz";

#[test]
fn test_samples() -> CompilationResult<()> {
    let content = fs::read_to_string(TEST_FILE)
        .expect("Unable to locate test samples");
    let tokens = tokenize(TEST_FILE, &content)?;
    let parsed_file = parse_file(TEST_FILE, tokens)?;
    let test_namespace = Reference::with_identifier(TEST_NAMESPACE);
    let (subroutines, tests) = get_tests(&test_namespace, parsed_file)?;
    let compiled_subroutines = compile_subroutines(subroutines);

    for (name, index) in tests {
        let (output, captures) = interpret(&compiled_subroutines[index].serialize());
        let capture_count = captures.len();
        for (i, capture) in captures.iter().enumerate() {
            if *capture != 1 {
                panic!("Test '{}' failed: capture {}/{} was 0x{:02x}", name, i + 1, capture_count, capture)
            }
        }
        let reference_path = PathBuf::from(REFERENCE_DIRECTORY).join(&name);
        match fs::read(&reference_path) {
            Ok(reference) => {
                assert_eq!(output, reference, "Test '{}' failed: stdout (left) did not match reference (right)", &name);
            }
            Err(_) => {
                // Create a reference if none exist for this test
                fs::write(&reference_path, output)
                    .expect(&format!("Unable to register reference for test '{}'", &name));
            }
        }
    }

    Ok(())
}
