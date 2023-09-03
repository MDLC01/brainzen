use crate::exceptions::{CompilationResult, LocatedException};
use crate::lexer::lexemes::Symbol;
use crate::location::{Located, Sequence};
use crate::parser::expression::Expression;
use crate::parser::target::{AssignmentTarget, DefinitionTarget};
use crate::parser::token_stream::{Construct, TokenStream};
use crate::parser::type_descriptor::TypeDescriptor;
use crate::reference::Reference;
use crate::tokenizer::tokens::{BracketKind, Keyword};

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
    /// Reads a single instruction, without the trailing semicolon.
    fn read(tokens: &mut TokenStream) -> CompilationResult<Self> {
        if tokens.eat(Keyword::Let) {
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
                Err(LocatedException::expected_definition_colon_or_equal(tokens.location()))
            }
        } else if tokens.eat(Keyword::Return) {
            // Return
            let value = Expression::locate(tokens)?;
            Ok(Self::Return(value))
        } else if let Some(reference) = tokens.next_located_reference_with(Symbol::DoublePlus) {
            // Increment
            Ok(Self::Increment(reference))
        } else if let Some(reference) = tokens.next_located_reference_with(Symbol::DoubleMinus) {
            // Decrement
            Ok(Self::Decrement(reference))
        } else if let Some((reference, body)) = tokens.next_located_reference_with_parenthesized(BracketKind::Round) {
            // Procedure call
            let arguments = Expression::parse_separated_sequence(body, Symbol::Comma)?;
            Ok(Self::ProcedureCall(reference.value, arguments))
        } else if let Some(reference) = tokens.next_located_reference_with(Symbol::QuestionMark) {
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
            let location = tokens.location();
            match AssignmentTarget::locate(tokens) {
                Ok(target) => {
                    // Assignment
                    tokens.consume(Symbol::Equal)?;
                    let value = Expression::locate(tokens)?;
                    Ok(Self::Assignment(target, value))
                }
                Err(_) => {
                    // Syntax error
                    Err(LocatedException::expected_instruction(location))
                }
            }
        }
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
    /// Reads a statement.
    ///
    /// The statement might be a block. To only allow blocks, use [`StatementBlock::parse_block`].
    fn read(tokens: &mut TokenStream) -> CompilationResult<Self> {
        if tokens.is(Symbol::OpenBrace) {
            // Block
            let block = StatementBlock::read(tokens)?;
            Ok(Self::Block(block))
        } else if tokens.eat(Keyword::Loop) {
            // Loop
            let count = Expression::locate(tokens)?;
            let body = StatementBlock::read(tokens)?;
            Ok(Self::Loop { count, body })
        } else if tokens.eat(Keyword::While) {
            // While loop
            let test = Expression::locate(tokens)?;
            let body = StatementBlock::read(tokens)?;
            Ok(Self::WhileLoop { test, body })
        } else if tokens.eat(Keyword::Do) {
            // Do-while loop
            let body = StatementBlock::read(tokens)?;
            tokens.consume(Keyword::While)?;
            let test = Expression::locate(tokens)?;
            tokens.consume(Symbol::Semicolon)?;
            Ok(Self::DoWhileLoop { body, test })
        } else if tokens.eat(Keyword::If) {
            // Conditional branching
            let test = Expression::locate(tokens)?;
            let if_body = StatementBlock::read(tokens)?;
            if tokens.eat(Keyword::Else) {
                let else_body = StatementBlock::read(tokens)?;
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
    /// Reads a statement block.
    ///
    /// To allow any statement, use [`Statement::read`].
    fn read(tokens: &mut TokenStream) -> CompilationResult<Self> {
        let Some(body) = tokens.next_parenthesized(BracketKind::Curly) else {
            return Err(LocatedException::expected_statement_block(tokens.location()));
        };
        let statements = Statement::parse_sequence(body)?;
        Ok(Self(statements))
    }
}

impl From<StatementBlock> for Statement {
    fn from(block: StatementBlock) -> Self {
        Self::Block(block)
    }
}
