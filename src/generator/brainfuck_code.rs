use std::fmt::{Display, Formatter};

#[derive(Clone, Eq, PartialEq, Debug)]
enum BrainfuckInstruction {
    /// A move to the right by the specified amount.
    ///
    /// If the amount is negative, the move is to the left. If the amount is zero, this is a no-op.
    Right(i32),
    /// An incrementation by the specified amount.
    Add(u8),
    /// A read.
    Read,
    /// A write.
    Write,
    /// A loop with the specified body.
    Loop(BrainfuckCode),
    /// A string that might contain instructions.
    Unchecked(String),
    /// A string that will be escaped.
    Comment(String),
}

impl BrainfuckInstruction {
    pub fn reset() -> Self {
        Self::Loop(Self::Add(u8::MAX).into())
    }

    pub fn compile(&self) -> String {
        match self {
            &Self::Right(amount) if amount >= 0 => {
                ">".repeat(amount as usize)
            }
            &Self::Right(amount) => {
                "<".repeat(-amount as usize)
            }
            &Self::Add(amount) => {
                if amount <= u8::MAX / 2 {
                    "+".repeat(amount as usize)
                } else {
                    "-".repeat((u8::MAX - amount + 1) as usize)
                }
            }
            Self::Read => {
                ",".to_owned()
            }
            Self::Write => {
                ".".to_owned()
            }
            Self::Loop(body) => {
                format!("[{}]", body.serialize())
            }
            Self::Unchecked(code) => {
                code.clone()
            }
            Self::Comment(comment) => {
                comment
                    .replace('<', "(lt)")
                    .replace('>', "(gt)")
                    .replace('+', "(plus)")
                    .replace('-', "(minus)")
                    .replace('.', "(period)")
                    .replace(',', ";")
                    .replace('[', "(")
                    .replace(']', ")")
            }
        }
    }

    pub fn compile_with_indent(&self, depth: usize) -> String {
        match self {
            &Self::Right(amount) if amount >= 0 => {
                ">".repeat(amount as usize)
            }
            &Self::Right(amount) => {
                "<".repeat(-amount as usize)
            }
            &Self::Add(amount) => {
                if amount <= u8::MAX / 2 {
                    "+".repeat(amount as usize)
                } else {
                    "-".repeat((u8::MAX - amount + 1) as usize)
                }
            }
            Self::Read => {
                ",".to_owned()
            }
            Self::Write => {
                ".".to_owned()
            }
            Self::Loop(body) => {
                if body.instructions.len() <= 1 {
                    format!("[{}]", body.serialize_with_indent(depth))
                } else {
                    let indent = "  ".repeat(depth);
                    format!("[\n{indent}  {}\n{indent}]", body.serialize_with_indent(depth + 1))
                }
            }
            Self::Unchecked(code) => {
                code.clone()
            }
            Self::Comment(comment) => {
                let serialized_comment = comment
                    .replace('<', "(lt)")
                    .replace('>', "(gt)")
                    .replace('+', "(plus)")
                    .replace('-', "(minus)")
                    .replace('.', "(period)")
                    .replace(',', ";")
                    .replace('[', "(")
                    .replace(']', ")");
                if comment.len() > 5 || comment.contains('\n') {
                    let indent = String::from("\n") + &"  ".repeat(depth);
                    let indented_comment = serialized_comment
                        .replace('\n', &indent);
                    format!("{}{}{}", indent, indented_comment, indent)
                } else {
                    serialized_comment
                }
            }
        }
    }
}


#[derive(Clone, Eq, PartialEq, Default, Debug)]
pub struct BrainfuckCode {
    instructions: Vec<BrainfuckInstruction>,
}

impl BrainfuckCode {
    /// Creates new, empty, Brainfuck code.
    pub fn new() -> Self {
        Self::default()
    }

    /// Appends a Brainfuck instruction, optionally simplifying the resulting code.
    fn push(&mut self, instruction: BrainfuckInstruction) {
        match (instruction, self.instructions.last_mut()) {
            (BrainfuckInstruction::Right(additional_amount), Some(BrainfuckInstruction::Right(amount))) => {
                *amount += additional_amount
            }
            (BrainfuckInstruction::Add(additional_amount), Some(BrainfuckInstruction::Add(amount))) => {
                *amount = amount.wrapping_add(additional_amount)
            }
            (command, _) => {
                self.instructions.push(command)
            }
        }
    }

    /// Pushes the instructions of another code in order.
    fn extend(&mut self, code: Self) {
        let mut instructions = code.instructions.into_iter();
        // Optimize first instruction
        if let Some(instruction) = instructions.next() {
            self.push(instruction)
        }
        // Remaining instructions are already optimized
        self.instructions.extend(instructions);
    }

    pub fn serialize(&self) -> String {
        self.instructions.iter()
            .map(BrainfuckInstruction::compile)
            .fold(String::new(), |a, b| a + &b)
    }

    pub fn serialize_with_indent(&self, depth: usize) -> String {
        let mut code = String::new();
        for instruction in &self.instructions {
            code.push_str(&instruction.compile_with_indent(depth))
        }
        code
    }
}

impl From<BrainfuckInstruction> for BrainfuckCode {
    fn from(instruction: BrainfuckInstruction) -> Self {
        Self {
            instructions: vec![instruction]
        }
    }
}

impl Display for BrainfuckCode {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.serialize_with_indent(0))
    }
}


#[derive(Clone, Debug)]
pub(super) struct BalancedCode {
    code: BrainfuckCode,
    origin: usize,
    cursor: usize,
}

impl BalancedCode {
    pub fn new(origin: usize) -> Self {
        Self {
            code: BrainfuckCode::new(),
            origin,
            cursor: origin,
        }
    }

    /// Pushes an instruction that moves the pointer to a cell.
    fn push_goto(&mut self, index: usize) {
        self.code.push(BrainfuckInstruction::Right(index as i32 - self.cursor as i32));
        self.cursor = index;
    }

    /// Pushes instructions that add an amount to the value of a cell.
    pub fn push_add(&mut self, index: usize, amount: u8) {
        self.push_goto(index);
        self.code.push(BrainfuckInstruction::Add(amount));
    }

    /// Pushes instructions that set the value of a cell.
    pub fn push_set(&mut self, index: usize, value: u8) {
        self.push_goto(index);
        self.code.push(BrainfuckInstruction::reset());
        if value != 0 {
            self.code.push(BrainfuckInstruction::Add(value));
        }
    }

    /// Pushes instructions that reads a value to a cell.
    pub fn push_read(&mut self, index: usize) {
        self.push_goto(index);
        self.code.push(BrainfuckInstruction::Read);
    }

    /// Pushes instructions that writes a value from a cell.
    pub fn push_write(&mut self, index: usize) {
        self.push_goto(index);
        self.code.push(BrainfuckInstruction::Write);
    }

    /// Pushes instructions that result in a loop with the specified origin and body.
    pub fn push_loop(&mut self, origin: usize, body: Self) {
        self.push_goto(origin);
        self.code.push(BrainfuckInstruction::Loop(body.into()));
    }

    /// Pushes instructions that result in the value of `source` being moved to `destination`.
    ///
    /// `source` and `destination` must be different.
    pub fn push_move(&mut self, source: usize, destination: usize) {
        assert_ne!(source, destination);
        let mut body = Self::new(source);
        body.push_add(source, u8::MAX);
        body.push_add(destination, 1);
        self.push_loop(source, body);
    }

    /// Pushes a comment after moving the cursor to a specific position.
    pub fn push_comment(&mut self, comment: String, position: usize) {
        self.push_goto(position);
        self.code.push(BrainfuckInstruction::Comment(comment));
    }

    /// Adds a comment.
    pub fn comment(&mut self, comment: &str) {
        self.code.push(BrainfuckInstruction::Comment(String::from(comment)))
    }

    /// Pushes the instructions of another balanced code in order, after moving the cursor to the
    /// specified origin.
    pub fn extend(&mut self, origin: usize, code: Self) {
        self.push_goto(origin);
        self.code.extend(code.into());
    }
}

impl From<BalancedCode> for BrainfuckCode {
    fn from(mut value: BalancedCode) -> Self {
        value.push_goto(value.origin);
        value.code
    }
}
