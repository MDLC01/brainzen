use std::{iter, mem};
use std::collections::HashMap;

use crate::generate;
use crate::generator::brainfuck_code::{BalancedCode, BrainfuckCode};
use crate::generator::memory::{Cell, Page};
use crate::generator::segment::Generator;
use crate::reference::Reference;
use crate::type_checker::operations::{BinaryOperation, UnaryOperation};
use crate::type_checker::subroutines::{SubroutineSignature, TypeCheckedSubroutine, TypeCheckedSubroutineBody};
use crate::type_checker::type_checked_statements::{TypeCheckedInstruction, TypeCheckedStatement};
use crate::type_checker::typed_expressions::{TypeCheckedExpression, TypedExpression};
use crate::type_checker::types::Type;

pub mod brainfuck_code;
mod segment;
mod memory;

/// A variable is the information of an origin cell and a size.
#[derive(Copy, Clone, Debug)]
struct Variable {
    origin: Cell,
    size: usize,
}

impl Variable {
    /// Returns the cell at a specific offset within this variable.
    ///
    /// The offset must not be greater than the variable size.
    pub fn at(&self, offset: usize) -> Cell {
        debug_assert!(offset < self.size);
        self.origin.offset(offset)
    }

    /// Returns the first cell of this variable.
    pub fn origin(&self) -> Cell {
        self.at(0)
    }
}


#[derive(Debug)]
struct CallableSegment {
    code: BalancedCode,
    input_offset: usize,
    output_offset: usize,
    input_size: usize,
    output_size: usize,
}

impl From<CallableSegment> for BalancedCode {
    fn from(value: CallableSegment) -> Self {
        value.code
    }
}

impl From<CallableSegment> for BrainfuckCode {
    fn from(value: CallableSegment) -> Self {
        value.code.into()
    }
}


/// Represents the current context: a stack of variable scopes, as well as accessible subroutines.
#[derive(Debug)]
struct Context {
    /// The accessible subroutines, with input and output offsets.
    subroutines: Vec<CallableSegment>,
    parents: Vec<HashMap<String, Variable>>,
    names: HashMap<String, Variable>,
}

impl Context {
    pub fn new(subroutines: Vec<CallableSegment>) -> Self {
        Self {
            subroutines,
            parents: Vec::new(),
            names: HashMap::new(),
        }
    }

    /// Registers a new identifier on the active scope.
    pub fn register(&mut self, identifier: String, origin: Cell, size: usize) {
        self.names.insert(identifier, Variable { origin, size });
    }

    /// Returns the variable associated with an identifier.
    pub fn get_variable(&self, identifier: &str) -> Variable {
        self.parents.iter()
            .chain(iter::once(&self.names))
            .rev()
            .filter_map(|scope| scope.get(identifier).cloned())
            .next()
            .expect("variable should exist at this point")
    }

    /// Returns the variable associated with a reference.
    ///
    /// The reference should be a single identifier.
    pub fn get_variable_by_reference(&self, reference: &Reference) -> Variable {
        assert!(reference.namespace.is_none());
        self.get_variable(&reference.identifier)
    }

    /// Pushes a scope onto the stack.
    pub fn push(&mut self) {
        let names = mem::take(&mut self.names);
        self.parents.push(names)
    }

    /// Pops a scope from the stack.
    pub fn pop(&mut self) {
        self.names = self.parents.pop().expect("calls to `push` and `pop` should define a well-bracketing");
    }

    /// Returns an iterator over all accessible names.
    pub fn get_all_names(&self) -> impl Iterator<Item=&str> {
        self.parents.iter()
            .chain(iter::once(&self.names))
            .flat_map(|scope| scope.keys())
            .map(|s| s as &str)
    }
}


/// Compiles the evaluation of a unary operation. The result is computed at runtime in the
/// `destination` cell.
fn compile_unary_operation(generator: &mut Generator, context: &Context, operation: &UnaryOperation, operand: &TypedExpression, destination: Cell) {
    let op = allocate_and_evaluate(generator, context, operand);
    if let Some(value) = operation.get_predicted_char_value() {
        generator.generate_set(destination, value);
        return;
    }
    match operation {
        UnaryOperation::Negation(_) => generate! {
            (generator)
            SET destination 1;
            IF op [
                DECR destination;
            ];
        },
        UnaryOperation::BooleanNormalization(_) => generate! {
            (generator)
            RESET destination;
            IF op [
                INCR destination;
            ];
        },
        UnaryOperation::Opposition(_) => generate! {
            (generator)
            RESET destination;
            LOOP op [
                DECR destination;
            ];
        },
    }
}


/// Compiles the computation of the quotient and the remainder of the division of `lhs` by `rhs` at
/// once. The results are computed in the corresponding cells (`quo` for the quotient; `rem` for the
/// remainder), and `lhs` and `rhs` are reset.
///
/// This method is used by [`compile_binary_operation`].
fn compile_divmod(generator: &mut Generator, lhs: Cell, rhs: Cell, quo: Cell, rem: Cell) {
    generate! {
            (generator)
            RESET {quo, rem};
            // Subtract the divisor from the dividend until zero is reached
            WHILE lhs [
                // Decrement the dividend and the divisor and increment the remainder...
                DECR lhs;
                DECR rhs;
                INCR rem;
                // ... then, test if we have subtracted a full divisor ...
                ALLOC tmp @rhs;
                ALLOC is_divisor_null 1;
                IF tmp [
                    DECR is_divisor_null;
                ];
                // ... and if so, increment the quotient and reset the divisor to its initial value
                // and the remainder to zero.
                IF is_divisor_null [
                    INCR quo;
                    // The divisor is now stored in the remainder, we can get it from there.
                    MOVE rem rhs;
                ];
            ];
        }
}

/// Compiles the evaluation of a binary operation. The result is computed at runtime in the
/// `destination` cell.
fn compile_binary_operation(generator: &mut Generator, context: &Context, operation: &BinaryOperation, left_operand: &TypedExpression, right_operand: &TypedExpression, destination: Cell) {
    let left_operand_size = left_operand.size();
    let right_operand_size = right_operand.size();
    let lhs = allocate_and_evaluate(generator, context, left_operand);
    let rhs = allocate_and_evaluate(generator, context, right_operand);
    if let Some(value) = operation.get_predicted_char_value() {
        generator.generate_set(destination, value);
        return;
    }
    match operation {
        BinaryOperation::EqualityTest(_) => {
            // If the values have different sizes, they cannot be equal
            if right_operand_size != left_operand_size {
                generator.generate_set(destination, 0);
            } else {
                generator.generate_set(destination, 1);
                // For each pair of cells, test if they are equal
                for i in 0..left_operand_size {
                    // Subtract `rhs[i]` from `lhs[i]`
                    generator.generate_loop(rhs.offset(i), |generator| {
                        generator.generate_decrement(rhs.offset(i));
                        generator.generate_decrement(lhs.offset(i))
                    });
                    generator.generate_branch(lhs.offset(i), |generator| {
                        generator.generate_set(destination, 0)
                    })
                }
            }
        }
        BinaryOperation::DifferenceTest(_) => {
            // If the values have different sizes, they cannot be equal
            if right_operand_size != left_operand_size {
                generator.generate_set(destination, 1);
            } else {
                generator.generate_set(destination, 0);
                // For each pair of cells, test if they are equal
                for i in 0..left_operand_size {
                    // Subtract `rhs[i]` from `lhs[i]`
                    generator.generate_loop(rhs.offset(i), |generator| {
                        generator.generate_decrement(rhs.offset(i));
                        generator.generate_decrement(lhs.offset(i))
                    });
                    // Executed once, iff the cells were different
                    generator.generate_loop(lhs.offset(i), |generator| {
                        generator.generate_reset(lhs.offset(i));
                        generator.generate_set(destination, 1)
                    })
                }
            }
        }
        BinaryOperation::StrictInequalityTest(_) => generate! {
            (generator)
            RESET destination;
            LOOP rhs [
                IFNULL @lhs [
                    // `rhs` will reach 0 before `destination` gets a chance to be decremented again
                    INCR destination;
                ];
                DECR lhs;
            ];
        },
        BinaryOperation::LargeInequalityTest(_) => generate! {
            (generator)
            SET destination 1;
            LOOP lhs [
                IFNULL @rhs [
                    // `lhs` will reach 0 before `destination` gets a chance to be decremented again
                    DECR destination;
                ];
                DECR rhs;
            ];
        },
        BinaryOperation::InverseStrictInequalityTest(_) => generate! {
            (generator)
            RESET destination;
            LOOP lhs [
                IFNULL @rhs [
                    // `lhs` will reach 0 before `destination` gets a chance to be decremented again
                    INCR destination;
                ];
                DECR rhs;
            ];
        },
        BinaryOperation::InverseLargeInequalityTest(_) => generate! {
            (generator)
            SET destination 1;
            LOOP rhs [
                IFNULL @lhs [
                    // `rhs` will reach 0 before `destination` gets a chance to be decremented again
                    DECR destination;
                ];
                DECR lhs;
            ];
        },
        BinaryOperation::Conjunction(_) => generate! {
            (generator)
            SET destination 1;
            IFNULL lhs [
                DECR destination;
            ];
            IFNULL rhs [
                RESET destination;
            ];
        },
        BinaryOperation::BooleanDisjunction(_) => generate! {
            (generator)
            RESET destination;
            IF lhs [
                INCR destination;
            ];
            IF rhs [
                SET destination 1;
            ];
        },
        BinaryOperation::Disjunction(_) => generate! {
            (generator)
            RESET destination;
            ALLOC is_first_false 1;
            // First, try to copy `lhs`
            IF lhs [
                DECR is_first_false;
                MOVE lhs destination;
            ];
            // If `lhs` was false, copy `rhs`
            IF is_first_false [
                MOVE rhs destination;
            ];
        },
        BinaryOperation::Addition(_) => generate! {
            (generator)
            RESET destination;
            MOVE lhs destination;
            MOVE rhs destination;
        },
        BinaryOperation::Subtraction(_) => generate! {
            (generator)
            RESET destination;
            MOVE lhs destination;
            LOOP rhs [
                DECR destination;
            ];
        },
        BinaryOperation::Multiplication(_) => generate! {
            (generator)
            RESET destination;
            LOOP lhs [
                COPY rhs destination;
            ];
        },
        BinaryOperation::Division(_) => {
            let remainder = generator.allocate();
            compile_divmod(generator, lhs, rhs, destination, remainder)
        }
        BinaryOperation::Modulo(_) => {
            let quotient = generator.allocate();
            compile_divmod(generator, lhs, rhs, quotient, destination)
        }
        BinaryOperation::EuclideanDivision(_) => {
            compile_divmod(generator, lhs, rhs, destination, destination.offset(1))
        }
    }
}

/// Compiles the call of a subroutine and returns a cell containing the output of the subroutine.
///
/// `index` is the index of the subroutine. `arguments` are the arguments to the subroutine.
fn compile_call(generator: &mut Generator, context: &Context, index: usize, arguments: &[TypedExpression]) -> Cell {
    let input = generator.allocate();
    compile_adjacent_expression_evaluation(generator, context, arguments, input);
    let output = generator.allocate();
    generator.generate_call(&context.subroutines[index], input, output);
    output
}

/// Compiles the evaluation of an expression in `destination`.
fn compile_expression_evaluation(generator: &mut Generator, context: &Context, TypedExpression { r#type, expression }: &TypedExpression, destination: Cell) {
    match expression {
        TypeCheckedExpression::Char(value) => {
            generator.generate_set(destination, *value);
        }
        TypeCheckedExpression::Tuple(elements) => {
            compile_adjacent_expression_evaluation(generator, context, elements, destination);
        }
        TypeCheckedExpression::Reference(reference, offset, size) => {
            let variable = context.get_variable_by_reference(reference);
            for i in 0..*size {
                generator.generate_copy(variable.at(offset + i), destination.offset(i))
            }
        }
        TypeCheckedExpression::UnaryOperation(operation, operand) => {
            compile_unary_operation(generator, context, operation, operand, destination);
        }
        TypeCheckedExpression::BinaryOperation(operation, left_operand, right_operand) => {
            compile_binary_operation(generator, context, operation, left_operand, right_operand, destination);
        }
        TypeCheckedExpression::FunctionCall { index, arguments } => {
            let output = compile_call(generator, context, *index, arguments);
            for i in 0..r#type.size() {
                generator.generate_move(output.offset(i), destination.offset(i))
            }
        }
        TypeCheckedExpression::InputCall => {
            generator.generate_read(destination)
        }
    }
}

/// Returns a cell containing the evaluated expression.
///
/// If the expression is a reference to something that is already stored somewhere (like a
/// variable), the cell already containing the value is returned. Otherwise, a new cell is allocated
/// and the expression is evaluated in that cell. As such, *the returned cell should not be modified
/// in by the caller.*
///
/// This is intended to be uses in places where we do not modify the expression, such as when
/// printing a value.
fn compile_immutable_expression_evaluation(generator: &mut Generator, context: &Context, expression: &TypedExpression) -> Cell {
    match &expression.expression {
        TypeCheckedExpression::Reference(reference, _, _) => {
            context.get_variable_by_reference(reference).origin()
        }
        _ => {
            allocate_and_evaluate(generator, context, expression)
        }
    }
}

/// Compiles the evaluation of multiple expressions in consecutive cells, starting at `destination`.
fn compile_adjacent_expression_evaluation(generator: &mut Generator, context: &Context, expressions: &[TypedExpression], destination: Cell) {
    let mut offset = 0;
    for expression in expressions {
        let size = expression.size();
        compile_expression_evaluation(generator, context, expression, destination.offset(offset));
        offset += size;
    }
}

/// Compiles the evaluation of an expression in a new memory block, and returns the block.
fn allocate_and_evaluate(generator: &mut Generator, context: &Context, expression: &TypedExpression) -> Cell {
    let block = generator.allocate();
    compile_expression_evaluation(generator, context, expression, block);
    block
}

/// Compiles code that will log the value contained in the passed cell, as the specified type.
fn compile_log(generator: &mut Generator, r#type: &Type, value: Cell) {
    match r#type {
        Type::Char(_) => {
            generator.generate_static_write("'");
            generator.generate_write(value);
            generator.generate_static_write("'\n");
        }
        Type::Integer(value) => {
            generator.generate_static_write(&value.to_string());
        }
        Type::Product(factors) => {
            generator.generate_static_write("(");
            let mut is_first = true;
            let mut offset = 0;
            for factor in factors {
                if is_first {
                    is_first = false
                } else {
                    generator.generate_static_write(", ");
                }
                compile_log(generator, factor, value.offset(offset));
                offset += factor.size();
            }
            generator.generate_static_write(")");
        }
    }
}

/// Adds a context snapshot comment after moving the cursor to the specified cell (if not [`None`]).
fn compile_context_snapshot(generator: &mut Generator, context: &Context, cell: Option<Cell>) {
    let variables = context.get_all_names()
        .map(|name| {
            (name.to_owned(), context.get_variable(name).origin)
        })
        .collect();
    generator.generate_context_snapshot(cell, variables);
}

/// Compiles an instruction.
fn compile_instruction(generator: &mut Generator, context: &mut Context, instruction: &TypeCheckedInstruction) {
    match instruction {
        TypeCheckedInstruction::Write { arguments, end } => {
            for argument in arguments {
                let size = argument.size();
                let value = compile_immutable_expression_evaluation(generator, context, argument);
                for i in 0..size {
                    generator.generate_write(value.offset(i));
                }
            }
            generator.generate_static_write(end);
        }
        TypeCheckedInstruction::Read => {
            let tmp = generator.allocate();
            generator.generate_read(tmp);
        }
        TypeCheckedInstruction::Log(expression) => {
            let r#type = expression.r#type.clone();
            let value = compile_immutable_expression_evaluation(generator, context, expression);
            compile_log(generator, &r#type, value);
        }
        TypeCheckedInstruction::ProcedureCall(index, arguments) => {
            compile_call(generator, context, *index, arguments);
        }
        TypeCheckedInstruction::Increment(reference) => {
            let variable = context.get_variable_by_reference(reference);
            generator.generate_increment(variable.origin());
        }
        TypeCheckedInstruction::Decrement(reference) => {
            let variable = context.get_variable_by_reference(reference);
            generator.generate_decrement(variable.origin());
        }
        TypeCheckedInstruction::Definition(target, optional_value) => {
            let origin = generator.allocate();
            let mut offset = 0;
            for (identifier, size) in target.get_identifiers() {
                context.register(identifier, origin.offset(offset), size);
                offset += size;
            }
            if let Some(value) = optional_value {
                compile_expression_evaluation(generator, context, value, origin);
            }
        }
        TypeCheckedInstruction::Assignment(target, value) => {
            let result = allocate_and_evaluate(generator, context, value);
            let mut offset = 0;
            for (destination, size) in target.get_destinations() {
                let variable = context.get_variable_by_reference(&destination.variable);
                for i in 0..size {
                    generator.generate_move(result.offset(offset + i), variable.at(destination.offset + i))
                }
                offset += size;
            }
        }
        TypeCheckedInstruction::Return(expression) => {
            compile_expression_evaluation(generator, context, expression, Page::Output.base());
        }
        TypeCheckedInstruction::ContextSnapshot(reference) => {
            let cell = reference
                .as_ref()
                .map(|reference| context.get_variable_by_reference(reference).origin());
            compile_context_snapshot(generator, context, cell);
        }
        TypeCheckedInstruction::Capture(test) => {
            let result = allocate_and_evaluate(generator, context, test);
            generator.generate_capture(result);
        }
    }
}

/// Compiles a statement.
fn compile_statement(generator: &mut Generator, context: &mut Context, statement: &TypeCheckedStatement) {
    match statement {
        TypeCheckedStatement::Block(body) => {
            context.push();
            for statement in body {
                compile_statement(generator, context, statement)
            }
            context.pop()
        }
        TypeCheckedStatement::Loop { count, body } => {
            let counter = allocate_and_evaluate(generator, context, count);
            generator.generate_loop(counter, |generator| {
                compile_statement(generator, context, body);
                generator.generate_decrement(counter)
            })
        }
        TypeCheckedStatement::WhileLoop { test, body } => {
            let condition = allocate_and_evaluate(generator, context, test);
            generator.generate_loop(condition, |generator| {
                compile_statement(generator, context, body);
                compile_expression_evaluation(generator, context, test, condition)
            })
        }
        TypeCheckedStatement::DoWhileLoop { body, test } => {
            let condition = generator.allocate();
            generator.generate_increment(condition);
            generator.generate_loop(condition, |generator| {
                compile_statement(generator, context, body);
                compile_expression_evaluation(generator, context, test, condition)
            })
        }
        TypeCheckedStatement::ConditionalBranching { test, if_body, else_body } => {
            let condition = allocate_and_evaluate(generator, context, test);
            let is_else = generator.allocate();
            generator.generate_increment(is_else);
            generator.generate_branch(condition, |generator| {
                compile_statement(generator, context, if_body);
                generator.generate_decrement(is_else)
            });
            if let Some(else_body) = else_body {
                generator.generate_branch(is_else, |generator| {
                    compile_statement(generator, context, else_body)
                })
            }
        }
        TypeCheckedStatement::Instruction(instruction) => {
            compile_instruction(generator, context, instruction)
        }
    }
}

/// Declares the arguments of a subroutine.
fn declare_arguments(context: &mut Context, signature: SubroutineSignature) {
    let mut offset = 0;
    for argument in signature.arguments {
        let (identifier, r#type) = argument.value;
        let size = r#type.size();
        context.register(identifier, Page::Input.at(offset), size);
        offset += size;
    }
}

pub fn compile_subroutines(subroutines: Vec<TypeCheckedSubroutine>) -> Vec<BrainfuckCode> {
    subroutines.into_iter()
        .fold(Vec::new(), |compiled_subroutines, subroutine| {
            let mut generator = Generator::new(
                subroutine.signature.arguments_size(),
                subroutine.signature.return_size(),
            );
            let mut context = Context::new(compiled_subroutines);
            declare_arguments(&mut context, subroutine.signature);
            match &subroutine.body {
                TypeCheckedSubroutineBody::StatementBlock(body) => {
                    compile_statement(&mut generator, &mut context, body);
                }
                TypeCheckedSubroutineBody::Native { .. } => {
                    todo!()
                }
            }
            let mut compiled_subroutines = context.subroutines;
            compiled_subroutines.push(generator.generate());
            compiled_subroutines
        })
        .into_iter()
        .map(|segment| segment.into())
        .collect()
}
