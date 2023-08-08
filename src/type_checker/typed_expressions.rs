use std::rc::Rc;

use crate::exceptions::{CompilationResult, LocatedException};
use crate::location::Located;
use crate::parser::expression::Expression;
use crate::reference::Reference;
use crate::type_checker::operations::{BinaryOperation, Operation, UnaryOperation};
use crate::type_checker::scope::SubroutineContext;
use crate::type_checker::types::{Type, Value};
use crate::utils::extensions::TryCollectResult;
use crate::utils::product::Product;

#[derive(Clone, Debug)]
pub enum TypeCheckedExpression {
    Unit,
    Char(u8),
    Tuple(Product<TypedExpression, 2>),
    /// The first associated integer corresponds to an offset relative to the first cell of the
    /// variable, in cells.
    ///
    /// The second integer corresponds to the size (in cells) of the evaluated expression.
    Reference(Reference, usize, usize),
    UnaryOperation(UnaryOperation, Box<TypedExpression>),
    BinaryOperation(BinaryOperation, Box<TypedExpression>, Box<TypedExpression>),
    FunctionCall { index: usize, arguments: Vec<TypedExpression> },
    InputCall,
}

#[derive(Clone, Debug)]
pub struct TypedExpression {
    pub r#type: Type,
    pub expression: TypeCheckedExpression,
}

impl TypedExpression {
    pub fn size(&self) -> usize {
        self.r#type.size()
    }

    /// Infers the type of an expression.
    pub(super) fn infer_type(context: &mut SubroutineContext, Located { location, value: untyped_expression }: Located<Expression>) -> CompilationResult<Self> {
        match untyped_expression {
            Expression::Unit => {
                Ok(Self {
                    r#type: Type::Unit,
                    expression: TypeCheckedExpression::Unit,
                })
            }
            Expression::CharacterLiteral(value) => {
                Ok(Self {
                    r#type: Type::Char,
                    expression: TypeCheckedExpression::Char(value),
                })
            }
            Expression::IntegerLiteral(value) => {
                if let Ok(c) = value.try_into() {
                    Ok(Self {
                        r#type: Type::Char,
                        expression: TypeCheckedExpression::Char(c),
                    })
                } else {
                    Err(LocatedException::does_not_fit_in_char(location, value))
                }
            }
            Expression::TupleLiteral(elements) => {
                let typed_elements = elements.try_map(|element| {
                    Self::infer_type(context, element)
                })?;
                let types = typed_elements.map_ref(|element| element.r#type.to_owned());
                Ok(Self {
                    r#type: Type::Product(Rc::new(types)),
                    expression: TypeCheckedExpression::Tuple(typed_elements),
                })
            }
            Expression::ArrayLiteral(_elements) => {
                Err(LocatedException::unimplemented_arrays(location))
            }
            Expression::Variable(Located { location, value: reference }) => {
                let r#type = context.get_value_type(location, &reference)?;
                Ok(Self {
                    r#type: r#type.to_owned(),
                    expression: TypeCheckedExpression::Reference(reference, 0, r#type.size()),
                })
            }
            Expression::UnaryArithmetic(operator, operand) => {
                let typed_operand = Self::infer_type(context, operand.unboxed())?;
                let operation = UnaryOperation::from_untyped(location, operator, [&typed_operand.r#type])?;
                Ok(Self {
                    r#type: operation.result_type(),
                    expression: TypeCheckedExpression::UnaryOperation(operation, Box::new(typed_operand)),
                })
            }
            Expression::BinaryArithmetic(operator, left_operand, right_operand) => {
                let typed_left_operand = Self::infer_type(context, left_operand.unboxed())?;
                let typed_right_operand = Self::infer_type(context, right_operand.unboxed())?;
                let operation = BinaryOperation::from_untyped(location, operator, [&typed_left_operand.r#type, &typed_right_operand.r#type])?;
                Ok(Self {
                    r#type: operation.result_type(),
                    expression: TypeCheckedExpression::BinaryOperation(operation, Box::new(typed_left_operand), Box::new(typed_right_operand)),
                })
            }
            Expression::Subscript { .. } => {
                Err(LocatedException::unimplemented_arrays(location))
            }
            Expression::Slice { .. } => {
                Err(LocatedException::unimplemented_arrays(location))
            }
            Expression::FunctionCall(reference, _)
            if reference == "print"
                || reference == "println"
                || reference == "log" => {
                Err(LocatedException::not_a_function(location, &reference))
            }
            Expression::FunctionCall(reference, arguments)
            if reference == "input" => {
                if arguments.is_empty() {
                    Ok(Self {
                        r#type: Type::Char,
                        expression: TypeCheckedExpression::InputCall,
                    })
                } else {
                    Err(LocatedException::wrong_arity(location, &reference, 0, arguments.len()))
                }
            }
            Expression::FunctionCall(reference, arguments) => {
                let namespace_context = context.namespace_context();
                let signature = namespace_context.find_subroutine_signature(location.clone(), &reference)?;
                match signature.return_type().cloned() {
                    Some(return_type) => {
                        let expected_types = signature.argument_types();
                        if arguments.len() != expected_types.len() {
                            Err(LocatedException::wrong_arity(location, &reference, expected_types.len(), arguments.len()))
                        } else {
                            let index = namespace_context.find_subroutine_index(location.clone(), &reference)?;
                            let typed_arguments = arguments.into_iter()
                                .zip(expected_types)
                                .map(|(argument, expected_type)| Self::expect_type(context, argument, &expected_type))
                                .try_collect()?;
                            Ok(Self {
                                r#type: return_type,
                                expression: TypeCheckedExpression::FunctionCall { index, arguments: typed_arguments },
                            })
                        }
                    }
                    None => {
                        Err(LocatedException::not_a_function(location, &reference))
                    }
                }
            }
        }
    }

    /// Type checks an expression. That is, infer the type of an expression using
    /// [`Self::infer_type`] and assert it is assignable to an expected type.
    pub(super) fn expect_type(context: &mut SubroutineContext, expression: Located<Expression>, expected_type: &Type) -> CompilationResult<Self> {
        let location = expression.location();
        let typed_expression = Self::infer_type(context, expression)?;
        if typed_expression.r#type.is_assignable_to(expected_type) {
            Ok(typed_expression)
        } else {
            Err(LocatedException::wrong_type(location, expected_type, &typed_expression.r#type))
        }
    }

    /// Expects an integer literal or an expression that evaluates at compile time to a character
    /// and returns its value.
    pub(super) fn expect_constant_integer(context: &mut SubroutineContext, expression: Located<Expression>) -> CompilationResult<i32> {
        let location = expression.location();
        match expression.value {
            Expression::IntegerLiteral(value) => Ok(value),
            _ => {
                match Value::evaluate(context.namespace_context_mut(), expression)? {
                    Value::Char(c) => Ok(c as i32),
                    value => Err(LocatedException::expected_constant_integer(location, &value.get_type()))
                }
            }
        }
    }
}
