use crate::exceptions::{CompilationException, CompilationResult};
use crate::location::Located;
use crate::parser::statement::{Instruction, Statement, StatementBlock};
use crate::reference::Reference;
use crate::type_checker::scope::ScopeStack;
use crate::type_checker::targets::{TypeCheckedAssignmentTarget, TypeCheckedDefinitionTarget};
use crate::type_checker::typed_expressions::TypedExpression;
use crate::type_checker::types::Type;
use crate::utils::extensions::{TryCollectResult, VecExtensions};

#[derive(Debug)]
pub enum TypeCheckedInstruction {
    Write { arguments: Vec<TypedExpression>, end: String },
    /// Reads a single character from the input and discards it.
    Read,
    Log(TypedExpression),
    ProcedureCall(usize, Vec<TypedExpression>),
    Increment(Reference),
    Decrement(Reference),
    Definition(TypeCheckedDefinitionTarget, Option<TypedExpression>),
    Assignment(TypeCheckedAssignmentTarget, TypedExpression),
    Return(TypedExpression),
    ContextSnapshot(Option<Reference>),
    Capture(TypedExpression),
}

impl TypeCheckedInstruction {
    fn from_untyped(context: &mut ScopeStack, Located(location, untyped_instruction): Located<Instruction>, return_type: Option<&Type>) -> CompilationResult<Self> {
        match untyped_instruction {
            Instruction::ProcedureCall(reference, arguments)
            if reference == "print" || reference == "println" => {
                let mut typed_arguments = Vec::new();
                for argument in arguments {
                    let argument_location = argument.location();
                    let typed_argument = TypedExpression::infer_type(context, argument)?;
                    if !typed_argument.r#type.is_string() {
                        return Err(CompilationException::expected_string_like(argument_location, &typed_argument.r#type));
                    }
                    typed_arguments.push(typed_argument)
                }
                let end = if reference.identifier.ends_with("ln") { "\n" } else { "" };
                Ok(Self::Write { arguments: typed_arguments, end: String::from(end) })
            }
            Instruction::ProcedureCall(reference, arguments) if reference == "input" => {
                if arguments.is_empty() {
                    Ok(Self::Read)
                } else {
                    Err(CompilationException::wrong_arity(location, &reference, 0, arguments.len()))
                }
            }
            Instruction::ProcedureCall(reference, arguments) if reference == "log" => {
                let argument_count = arguments.len();
                if let Some(argument) = arguments.get_single_element() {
                    let typed_argument = TypedExpression::infer_type(context, argument)?;
                    Ok(Self::Log(typed_argument))
                } else {
                    Err(CompilationException::wrong_arity(location, &reference, 1, argument_count))
                }
            }
            Instruction::ProcedureCall(reference, arguments) => {
                let subroutine_index = context.find_subroutine_index(location.clone(), &reference)?;
                let signature = context.find_subroutine_signature(location.clone(), &reference)?;
                let expected_types = signature.argument_types();
                if expected_types.len() != arguments.len() {
                    Err(CompilationException::wrong_arity(location, &reference, expected_types.len(), arguments.len()))
                } else {
                    let mut typed_arguments = Vec::new();
                    for (argument, expected_type) in arguments.into_iter().zip(expected_types.into_iter()) {
                        let typed_argument = TypedExpression::expect_type(context, argument, &expected_type)?;
                        typed_arguments.push(typed_argument)
                    }
                    Ok(Self::ProcedureCall(subroutine_index, typed_arguments))
                }
            }
            Instruction::Increment(Located(location, reference)) => {
                if context.find_value_type(location.clone(), &reference)?.is_char() {
                    Ok(Self::Increment(reference))
                } else {
                    Err(CompilationException::increment_non_char(location))
                }
            }
            Instruction::Decrement(Located(location, reference)) => {
                if context.find_value_type(location.clone(), &reference)?.is_char() {
                    Ok(Self::Decrement(reference))
                } else {
                    Err(CompilationException::decrement_non_char(location))
                }
            }
            Instruction::Declaration(target, Located(descriptor_location, type_descriptor)) => {
                let r#type = Type::resolve_descriptor(context, descriptor_location, type_descriptor)?;
                let type_checked_target = TypeCheckedDefinitionTarget::type_check_and_register_variables(context, target, r#type)?;
                Ok(Self::Definition(type_checked_target, None))
            }
            Instruction::Initialization(target, value) => {
                let typed_expression = TypedExpression::infer_type(context, value)?;
                let type_checked_target = TypeCheckedDefinitionTarget::type_check_and_register_variables(context, target, typed_expression.r#type.clone())?;
                Ok(Self::Definition(type_checked_target, Some(typed_expression)))
            }
            Instruction::Assignment(target, value) => {
                let typed_expression = TypedExpression::infer_type(context, value)?;
                let type_checked_target = TypeCheckedAssignmentTarget::from_untyped(context, target, typed_expression.r#type.clone())?;
                Ok(Self::Assignment(type_checked_target, typed_expression))
            }
            Instruction::Return(value) => {
                match return_type {
                    Some(return_type) => {
                        let typed_expression = TypedExpression::expect_type(context, value, return_type)?;
                        Ok(Self::Return(typed_expression))
                    }
                    None => {
                        let typed_expression = TypedExpression::infer_type(context, value)?;
                        Err(CompilationException::unexpected_return(location, &typed_expression.r#type))
                    }
                }
            }
            Instruction::ContextSnapshot(Some(Located(location, reference))) => {
                // Makes sure the variable exists
                let _ = context.find_value_type(location, &reference)?;
                Ok(Self::ContextSnapshot(Some(reference)))
            }
            Instruction::ContextSnapshot(None) => {
                Ok(Self::ContextSnapshot(None))
            }
            Instruction::Capture(test) => {
                let typed_test = TypedExpression::expect_type(context, test, &Type::CHAR)?;
                Ok(Self::Capture(typed_test))
            }
        }
    }
}

#[derive(Debug)]
pub enum TypeCheckedStatement {
    Block(Vec<TypeCheckedStatement>),
    Loop { count: TypedExpression, body: Box<TypeCheckedStatement> },
    WhileLoop { test: TypedExpression, body: Box<TypeCheckedStatement> },
    DoWhileLoop { body: Box<TypeCheckedStatement>, test: TypedExpression },
    ConditionalBranching { test: TypedExpression, if_body: Box<TypeCheckedStatement>, else_body: Option<Box<TypeCheckedStatement>> },
    Instruction(TypeCheckedInstruction),
}

impl TypeCheckedStatement {
    pub(super) fn type_check_block(context: &mut ScopeStack, block: StatementBlock, return_type: Option<&Type>) -> CompilationResult<Self> {
        context.with_subscope(|context| {
            let type_checked_statements = block.into_iter()
                .map(|statement| Self::type_check(context, statement, return_type))
                .try_collect()?;
            Ok(Self::Block(type_checked_statements))
        })
    }

    pub(super) fn type_check(context: &mut ScopeStack, Located(_, statement): Located<Statement>, return_type: Option<&Type>) -> CompilationResult<Self> {
        match statement {
            Statement::Block(block) => {
                Self::type_check_block(context, block, return_type)
            }
            Statement::Loop { count, body } => {
                let count = TypedExpression::expect_type(context, count, &Type::CHAR)?;
                let type_checked_body = Self::type_check_block(context, body, return_type)?;
                Ok(Self::Loop { count, body: Box::new(type_checked_body) })
            }
            Statement::WhileLoop { test, body } => {
                let type_checked_test = TypedExpression::expect_type(context, test, &Type::CHAR)?;
                let type_checked_body = Self::type_check_block(context, body, return_type)?;
                Ok(Self::WhileLoop { test: type_checked_test, body: Box::new(type_checked_body) })
            }
            Statement::DoWhileLoop { body, test } => {
                let type_checked_body = Self::type_check_block(context, body, return_type)?;
                let type_checked_test = TypedExpression::expect_type(context, test, &Type::CHAR)?;
                Ok(Self::DoWhileLoop { body: Box::new(type_checked_body), test: type_checked_test })
            }
            Statement::ConditionalBranching { test, if_body, else_body: Some(else_body) } => {
                let type_checked_test = TypedExpression::expect_type(context, test, &Type::CHAR)?;
                let type_checked_if_body = Self::type_check_block(context, if_body, return_type)?;
                let type_checked_else_body = Self::type_check_block(context, else_body, return_type)?;
                Ok(Self::ConditionalBranching { test: type_checked_test, if_body: Box::new(type_checked_if_body), else_body: Some(Box::new(type_checked_else_body)) })
            }
            Statement::ConditionalBranching { test, if_body, else_body: None } => {
                let type_checked_test = TypedExpression::expect_type(context, test, &Type::CHAR)?;
                let type_checked_if_body = Self::type_check_block(context, if_body, return_type)?;
                Ok(Self::ConditionalBranching { test: type_checked_test, if_body: Box::new(type_checked_if_body), else_body: None })
            }
            Statement::Instruction(instruction) => {
                let type_checked_instruction = TypeCheckedInstruction::from_untyped(context, instruction, return_type)?;
                Ok(Self::Instruction(type_checked_instruction))
            }
        }
    }
}
