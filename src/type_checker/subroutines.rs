use crate::exceptions::{CompilationException, CompilationResult};
use crate::location::{Located, Sequence};
use crate::parser::namespace_element::{SubroutineArgument, SubroutineBody};
use crate::parser::type_descriptor::TypeDescriptor;
use crate::type_checker::scope::ScopeStack;
use crate::type_checker::type_checked_statements::TypeCheckedStatement;
use crate::type_checker::typed_expressions::TypedExpression;
use crate::type_checker::types::Type;
use crate::utils::extensions::TryCollectResult;

#[derive(Clone, Debug)]
pub struct SubroutineSignature {
    pub arguments: Sequence<(String, Type)>,
    pub return_type: Option<Type>,
}

impl SubroutineSignature {
    pub(super) fn from_untyped(context: &mut ScopeStack, untyped_arguments: Sequence<SubroutineArgument>, return_type: Option<Located<TypeDescriptor>>) -> CompilationResult<Self> {
        let arguments = untyped_arguments.into_iter()
            .map(|Located { location, value: argument }| {
                let r#type = Type::resolve_descriptor(context, location.clone(), argument.r#type)?;
                Ok(Located::new(location, (argument.name, r#type)))
            })
            .try_collect()?;
        let return_type = return_type
            .map(|descriptor| Type::resolve_descriptor(context, descriptor.location, descriptor.value))
            .transpose()?;
        Ok(Self { arguments, return_type })
    }

    pub(super) fn register_variables(&self, context: &mut ScopeStack) -> CompilationResult<()> {
        for Located { location, value: (identifier, r#type) } in self.arguments.clone() {
            context.register_variable(location, identifier, r#type)?;
        }
        Ok(())
    }

    /// Returns the sum of the size of the arguments of the subroutine.
    pub fn arguments_size(&self) -> usize {
        self.arguments.iter()
            .map(|Located { location: _, value: (_, r#type) }| r#type.size())
            .sum()
    }

    pub fn return_size(&self) -> usize {
        if let Some(return_type) = &self.return_type {
            return_type.size()
        } else {
            0
        }
    }

    pub fn argument_types(&self) -> Vec<Type> {
        self.arguments.iter()
            .map(|Located { location: _, value: (_, r#type) }| r#type.clone())
            .collect()
    }

    pub fn return_type(&self) -> Option<&Type> {
        self.return_type.as_ref()
    }
}

#[derive(Debug)]
pub enum TypeCheckedSubroutineBody {
    StatementBlock(TypeCheckedStatement),
    Native { code: String, offset: i32 },
}

impl TypeCheckedSubroutineBody {
    pub(super) fn type_check(context: &mut ScopeStack, body: SubroutineBody, return_type: Option<&Type>) -> CompilationResult<Self> {
        match body {
            SubroutineBody::StatementBlock(block) => {
                context.with_subscope(|context| {
                    let type_checked_statement = TypeCheckedStatement::type_check_block(context, block.value, return_type)?;
                    Ok(Self::StatementBlock(type_checked_statement))
                })
            }
            SubroutineBody::Native { offset, code } => {
                let offset_location = offset.location();
                let evaluated_offset = TypedExpression::expect_constant_integer(context, offset)?;
                if evaluated_offset < 0 {
                    Err(CompilationException::negative_offset(offset_location))
                } else {
                    Ok(Self::Native { code, offset: evaluated_offset })
                }
            }
        }
    }
}


#[derive(Debug)]
pub struct TypeCheckedSubroutine {
    pub signature: SubroutineSignature,
    pub body: TypeCheckedSubroutineBody,
}
