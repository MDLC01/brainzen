use crate::exceptions::CompilationResult;
use crate::lexer::tokens::{DO_KEYWORD, ELSE_KEYWORD, IF_KEYWORD, LET_KEYWORD, LOOP_KEYWORD, RETURN_KEYWORD, Symbol, WHILE_KEYWORD};
use crate::location::{Located, Sequence};
use crate::parser::expression::Expression;
use crate::parser::target::{AssignmentTarget, DefinitionTarget};
use crate::parser::token_stream::{Construct, TokenStream};
use crate::parser::type_descriptor::TypeDescriptor;
use crate::reference::Reference;

/// An instruction is a statement that does not contain child statements.
#[derive(Debug)]
pub enum Instruction {
    Initialization(Located<DefinitionTarget>, Located<Expression>),
    Declaration(Located<DefinitionTarget>, Located<TypeDescriptor>),
    Increment(Located<Reference>),
    Decrement(Located<Reference>),
    ProcedureCall(Reference, Sequence<Expression>),
    Assignment(Located<AssignmentTarget>, Located<Expression>),
    Return(Located<Expression>),
    Capture(Located<Expression>),
    ContextSnapshot(Option<Located<Reference>>),
}

impl Construct for Instruction {
    /// Parses a single instruction, including the trailing semicolon.
    fn parse(tokens: &mut TokenStream) -> CompilationResult<Self> {
        tokens.parse_choice()
            // Initialization
            .branch(|tokens| {
                tokens.expect(LET_KEYWORD)?;
                let target = DefinitionTarget::locate(tokens)?;
                tokens.consume(Symbol::Equal)?;
                let value = Expression::locate_tuple(tokens)?;
                tokens.consume(Symbol::Semicolon)?;
                Ok(Self::Initialization(target, value))
            })
            // Declaration
            .branch(|tokens| {
                tokens.expect(LET_KEYWORD)?;
                let target = DefinitionTarget::locate(tokens)?;
                tokens.consume(Symbol::Colon)?;
                let r#type = TypeDescriptor::locate(tokens)?;
                tokens.consume(Symbol::Semicolon)?;
                Ok(Self::Declaration(target, r#type))
            })
            // Return
            .branch(|tokens| {
                tokens.expect(RETURN_KEYWORD)?;
                let value = Expression::locate(tokens)?;
                tokens.consume(Symbol::Semicolon)?;
                Ok(Self::Return(value))
            })
            // Increment
            .branch(|tokens| {
                let reference = Reference::locate(tokens)?;
                tokens.consume(Symbol::DoublePlus)?;
                tokens.consume(Symbol::Semicolon)?;
                Ok(Self::Increment(reference))
            })
            // Decrement
            .branch(|tokens| {
                let reference = Reference::locate(tokens)?;
                tokens.consume(Symbol::DoubleMinus)?;
                tokens.consume(Symbol::Semicolon)?;
                Ok(Self::Decrement(reference))
            })
            // Procedure call
            .branch(|tokens| {
                let reference = Reference::locate(tokens)?;
                tokens.consume(Symbol::OpenParenthesis)?;
                let arguments = Expression::parse_delimited_separated_sequence(tokens, Symbol::Comma, Symbol::CloseParenthesis)?;
                tokens.consume(Symbol::Semicolon)?;
                Ok(Self::ProcedureCall(reference.value, arguments))
            })
            // Assignment
            .branch(|tokens| {
                let target = AssignmentTarget::locate(tokens)?;
                tokens.consume(Symbol::Equal)?;
                let value = Expression::locate_tuple(tokens)?;
                tokens.consume(Symbol::Semicolon)?;
                Ok(Self::Assignment(target, value))
            })
            // Context snapshot on variable
            .branch(|tokens| {
                let reference = Reference::locate(tokens)?;
                tokens.consume(Symbol::QuestionMark)?;
                tokens.consume(Symbol::Semicolon)?;
                Ok(Self::ContextSnapshot(Some(reference)))
            })
            // Global context snapshot
            .branch(|tokens| {
                tokens.expect(Symbol::QuestionMark)?;
                tokens.consume(Symbol::Semicolon)?;
                Ok(Self::ContextSnapshot(None))
            })
            // Test
            .branch(|tokens| {
                tokens.expect(Symbol::At)?;
                let test = Expression::locate(tokens)?;
                tokens.consume(Symbol::Semicolon)?;
                Ok(Self::Capture(test))
            })
            .parse("instruction")
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
        tokens.parse_choice()
            // Block
            .branch(|tokens| {
                let block = StatementBlock::parse(tokens)?;
                Ok(Self::Block(block))
            })
            // Loop
            .branch(|tokens| {
                tokens.expect(LOOP_KEYWORD)?;
                let count = Expression::locate(tokens)?;
                let body = StatementBlock::parse(tokens)?;
                Ok(Self::Loop { count, body })
            })
            // While
            .branch(|tokens| {
                tokens.expect(WHILE_KEYWORD)?;
                let test = Expression::locate(tokens)?;
                let body = StatementBlock::parse(tokens)?;
                Ok(Self::WhileLoop { test, body })
            })
            // Do-while
            .branch(|tokens| {
                tokens.expect(DO_KEYWORD)?;
                let body = StatementBlock::parse(tokens)?;
                tokens.expect(WHILE_KEYWORD)?;
                let test = Expression::locate(tokens)?;
                tokens.consume(Symbol::Semicolon)?;
                Ok(Self::DoWhileLoop { body, test })
            })
            // If
            .branch(|tokens| {
                tokens.expect(IF_KEYWORD)?;
                let test = Expression::locate(tokens)?;
                let if_body = StatementBlock::parse(tokens)?;
                if tokens.eat(ELSE_KEYWORD) {
                    let else_body = StatementBlock::parse(tokens)?;
                    Ok(Self::ConditionalBranching { test, if_body, else_body: Some(else_body) })
                } else {
                    Ok(Self::ConditionalBranching { test, if_body, else_body: None })
                }
            })
            // Instruction
            .parse_or(|tokens| {
                let instruction = Instruction::locate(tokens)?;
                Ok(Self::Instruction(instruction))
            })
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
