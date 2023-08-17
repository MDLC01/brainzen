use crate::exceptions::CompilationResult;
use crate::lexer::tokens::Symbol;
use crate::location::{Located, Sequence};
use crate::parser::{DO_KEYWORD, ELSE_KEYWORD, IF_KEYWORD, LET_KEYWORD, LOOP_KEYWORD, RETURN_KEYWORD, WHILE_KEYWORD};
use crate::parser::expression::Expression;
use crate::parser::target::{AssignmentTarget, DefinitionTarget};
use crate::parser::token_stream::{Construct, TokenStream};
use crate::parser::type_descriptor::TypeDescriptor;
use crate::reference::Reference;

/// An instruction is a statement that does not contain child statements.
#[derive(Debug)]
pub enum Instruction {
    Declaration(Located<DefinitionTarget>, Located<TypeDescriptor>),
    Initialization(Located<DefinitionTarget>, Located<Expression>),
    Increment(Located<Reference>),
    Decrement(Located<Reference>),
    ProcedureCall(Reference, Sequence<Expression>),
    Assignment(Located<AssignmentTarget>, Located<Expression>),
    Return(Located<Expression>),
    Capture(Located<Expression>),
    ContextSnapshot(Option<Located<Reference>>),
}

impl Construct for Instruction {
    /// Parses a single instruction, without the trailing semicolon.
    fn parse(tokens: &mut TokenStream) -> CompilationResult<Self> {
        if tokens.eat(LET_KEYWORD) {
            // Definition
            let target = DefinitionTarget::locate(tokens)?;
            if tokens.eat(Symbol::Colon) {
                // Declaration
                let r#type = TypeDescriptor::locate(tokens)?;
                Ok(Self::Declaration(target, r#type))
            } else if tokens.eat(Symbol::Equal) {
                // Initialization
                let value = Expression::locate(tokens)?;
                Ok(Self::Initialization(target, value))
            } else {
                todo!()
            }
        } else if tokens.eat(RETURN_KEYWORD) {
            // Return
            let value = Expression::locate(tokens)?;
            Ok(Self::Return(value))
        } else if let Some(reference) = tokens.eat_located_reference_with(Symbol::DoublePlus) {
            // Increment
            Ok(Self::Increment(reference))
        } else if let Some(reference) = tokens.eat_located_reference_with(Symbol::DoubleMinus) {
            // Decrement
            Ok(Self::Decrement(reference))
        } else if let Some(reference) = tokens.eat_located_reference_with(Symbol::OpenParenthesis) {
            // Procedure call
            let arguments = Expression::parse_delimited_separated_sequence(tokens, Symbol::Comma, Symbol::CloseParenthesis)?;
            Ok(Self::ProcedureCall(reference.value, arguments))
        } else if let Some(reference) = tokens.eat_located_reference_with(Symbol::QuestionMark) {
            // Context snapshot on variable
            Ok(Self::ContextSnapshot(Some(reference)))
        } else if tokens.eat(Symbol::QuestionMark) {
            // Global context snapshot
            Ok(Self::ContextSnapshot(None))
        } else if tokens.eat(Symbol::At) {
            // Capture
            let test = Expression::locate(tokens)?;
            Ok(Self::Capture(test))
        } else {
            // Assignment
            let target = AssignmentTarget::locate(tokens)?;
            tokens.consume(Symbol::Equal)?;
            let value = Expression::locate(tokens)?;
            Ok(Self::Assignment(target, value))
        }
        // FIXME: In case the syntax is wrong here, the error message will mention assignment
        //  targets only, which is not ideal.
    }
}


/// A statement is a piece of code that can be executed.
#[derive(Debug)]
pub enum Statement {
    Block(StatementBlock),
    Loop { count: Located<Expression>, body: StatementBlock },
    WhileLoop { test: Located<Expression>, body: StatementBlock },
    DoWhileLoop { body: StatementBlock, test: Located<Expression> },
    ConditionalBranching { test: Located<Expression>, if_body: StatementBlock, else_body: Option<StatementBlock> },
    Instruction(Located<Instruction>),
}


impl Construct for Statement {
    /// Parses a statement.
    ///
    /// The statement might be a block. To only allow blocks, use [`StatementBlock::parse_block`].
    fn parse(tokens: &mut TokenStream) -> CompilationResult<Self> {
        if tokens.is(Symbol::OpenBrace) {
            // Block
            let block = StatementBlock::parse(tokens)?;
            Ok(Self::Block(block))
        } else if tokens.eat(LOOP_KEYWORD) {
            // Loop
            let count = Expression::locate(tokens)?;
            let body = StatementBlock::parse(tokens)?;
            Ok(Self::Loop { count, body })
        } else if tokens.eat(WHILE_KEYWORD) {
            // While loop
            let test = Expression::locate(tokens)?;
            let body = StatementBlock::parse(tokens)?;
            Ok(Self::WhileLoop { test, body })
        } else if tokens.eat(DO_KEYWORD) {
            // Do-while loop
            let body = StatementBlock::parse(tokens)?;
            tokens.consume(WHILE_KEYWORD)?;
            let test = Expression::locate(tokens)?;
            tokens.consume(Symbol::Semicolon)?;
            Ok(Self::DoWhileLoop { body, test })
        } else if tokens.eat(IF_KEYWORD) {
            // Conditional branching
            let test = Expression::locate(tokens)?;
            let if_body = StatementBlock::parse(tokens)?;
            if tokens.eat(ELSE_KEYWORD) {
                let else_body = StatementBlock::parse(tokens)?;
                Ok(Self::ConditionalBranching { test, if_body, else_body: Some(else_body) })
            } else {
                Ok(Self::ConditionalBranching { test, if_body, else_body: None })
            }
        } else {
            // Instruction
            let instruction = Instruction::locate(tokens)?;
            tokens.consume(Symbol::Semicolon)?;
            Ok(Self::Instruction(instruction))
        }
    }
}


#[derive(Debug)]
pub struct StatementBlock(Sequence<Statement>);

impl IntoIterator for StatementBlock {
    type Item = Located<Statement>;
    type IntoIter = <Sequence<Statement> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Construct for StatementBlock {
    /// Parses a statement block.
    ///
    /// To allow any statement, use [`Statement::parse`].
    fn parse(tokens: &mut TokenStream) -> CompilationResult<Self> {
        tokens.expect(Symbol::OpenBrace)?;
        let statements = Statement::parse_delimited_sequence(tokens, Symbol::CloseBrace)?;
        Ok(Self(statements))
    }
}

impl From<StatementBlock> for Statement {
    fn from(block: StatementBlock) -> Self {
        Self::Block(block)
    }
}
