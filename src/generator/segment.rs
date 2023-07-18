use std::collections::{HashMap, HashSet};
use std::mem;

use crate::generator::brainfuck_code::BalancedCode;
use crate::generator::CallableSegment;
use crate::generator::memory::{Allocator, Cell, CellValue, ChunkIndex, ChunkSize, Page};
use crate::utils::extensions::VecExtensions;
use crate::utils::UniquenessProvider;

/// A `Command` is an action that should be performed by the final Brainfuck code.
///
/// Commands allow for easier optimization, as they do not exactly map one-to-one to regular
/// Brainfuck commands.
#[derive(Debug)]
enum Command {
    /// Adds a specific amount to a cell.
    Add(Cell, u8),
    /// Overwrites the value of a cell.
    ///
    /// This command is meant to be used when the previous value of the cell is unknown. If the
    /// previous value of the cell is known, [`Self::Add`] should be preferred as it saves a reset
    /// operation (`[-]`).
    Set(Cell, u8),
    /// Reads a value from standard input and writes it inside a cell.
    ReadIn(Cell),
    /// Writes the value of a cell to standard output.
    WriteFrom(Cell),
    /// Creates a loop.
    ///
    /// The cell is the origin: the cell that is used as the loop condition. The commands are the
    /// body of the loop.
    Loop(Cell, Segment),
    /// Invokes a segment.
    Invoke {
        /// The size of the input.
        input_size: ChunkSize,
        /// The size of the output.
        output_size: ChunkSize,
        /// The first cell to read the input from.
        input_source: Cell,
        /// The first cell to write the output to.
        output_destination: Cell,
        /// The offset to write the input to before invoking the segment.
        input_destination: ChunkIndex,
        /// The offset to read the output from after invoking the segment.
        output_source: ChunkIndex,
        /// The compiled segment to invoke.
        segment: BalancedCode,
    },
    /// Adds a context snapshot with the specified context.
    ///
    /// The cell is the cell to move the cursor to before writing the comment.
    ///
    /// The context is a collection of `(name, cell)` pairs.
    ContextSnapshot(Option<Cell>, Vec<(String, Cell)>),
    /// Moves the cursor to a cell and adds a `@` in the generated code.
    Capture(Cell),
}


/// Contains information about a page, such as the values of its cells.
#[derive(Clone, Default, Debug)]
pub struct PageInfo {
    /// The index of the first command that accesses this page.
    pub first_access: Option<usize>,
    /// The index of the last command that accesses this page.
    pub last_access: Option<usize>,
    /// The, possibly unknown, value of each cell of this page.
    ///
    /// The first element corresponds to the cell 0; not the cell `min_offset`.
    cells: Vec<CellValue>,
}

impl PageInfo {
    /// Returns the known value of a cell.
    pub fn get_known_value(&self, offset: usize) -> CellValue {
        self.cells.get(offset).cloned().unwrap_or_default()
    }

    /// Mutates this page info to indicate an access to a cell.
    fn access(&mut self, offset: usize, command_index: usize) {
        if self.first_access.is_none() {
            self.first_access = Some(command_index)
        }
        self.last_access = Some(command_index);
        if offset >= self.cells.len() {
            self.cells.resize(offset + 1, CellValue::default())
        }
    }

    /// Mutates this page info to indicate a read access to a cell and returns its known value.
    pub fn read(&mut self, offset: usize, command_index: usize) -> CellValue {
        self.access(offset, command_index);
        self.cells[offset]
    }


    /// Mutates this page info to indicate a write access to a cell, setting its new value.
    pub fn write(&mut self, offset: usize, value: CellValue, command_index: usize) {
        self.access(offset, command_index);
        self.cells[offset] = value
    }

    pub fn size(&self) -> usize {
        self.cells.len()
    }

    /// Returns the index of the first command that accesses this page.
    pub fn first_access(&self) -> Option<usize> {
        self.first_access
    }

    /// Returns the index of the last command that accesses this page.
    pub fn last_access(&self) -> Option<usize> {
        self.last_access
    }
}


/// Holds a sequence of commands and information about they access and modify memory.
#[derive(Default, Debug)]
struct Segment {
    /// The commands of this segment.
    commands: Vec<Command>,
    /// Information about the pages owned by this segment.
    pages: HashMap<Page, PageInfo>,
    /// A set of cells not owned by this segment that are read by its commands.
    read_cells: HashSet<Cell>,
    /// A set of cells not owned by this segment that are written to by its commands.
    written_cells: HashSet<Cell>,
}

impl Segment {
    /// Registers and returns a new memory page.
    fn register_page(&mut self, uniqueness_provider: &mut UniquenessProvider) -> Page {
        let page = Page::with_id(uniqueness_provider.next());
        let previous_value = self.pages.insert(page, PageInfo::default());
        debug_assert!(previous_value.is_none());
        page
    }

    /// Returns the known value of a cell.
    fn get_cell_value(&self, cell: Cell) -> CellValue {
        if let Some(info) = self.pages.get(&cell.page) {
            info.get_known_value(cell.offset)
        } else {
            // The cell belongs to another (outer) segment: we know nothing about it
            CellValue::Unknown
        }
    }

    /// Marks a cell as being read in the next command and returns its value.
    fn mark_cell_read(&mut self, cell: Cell) -> CellValue {
        if let Some(info) = self.pages.get_mut(&cell.page) {
            info.read(cell.offset, self.commands.len())
        } else {
            // The cell belongs to another (outer) segment: we know nothing about it
            self.read_cells.insert(cell);
            CellValue::Unknown
        }
    }

    /// Marks a cell as being written in the next command to and update related information.
    fn mark_cell_write(&mut self, cell: Cell, value: CellValue) {
        if let Some(info) = self.pages.get_mut(&cell.page) {
            info.write(cell.offset, value, self.commands.len())
        } else {
            // The page belongs to another (outer) segment
            self.written_cells.insert(cell);
        }
    }

    /// Marks a cell as being read and written to in the next command and updates its known value by
    /// calling `f` with the current known value.
    fn mark_cell_update(&mut self, cell: Cell, f: impl FnOnce(CellValue) -> CellValue) {
        let value = f(self.mark_cell_read(cell));
        self.mark_cell_write(cell, value)
    }

    /// Marks a cell as being read and written to in an unpredictable way, settings its value to
    /// [`CellValue::Unknown`].
    fn mark_cell_shuffle(&mut self, cell: Cell) {
        self.mark_cell_update(cell, |_| CellValue::Unknown)
    }

    /// Adds a command, keeping track of page usage and optionally performing optimizations.
    pub fn add_command(&mut self, mut command: Command) {
        match &command {
            &Command::Add(cell, amount) => {
                self.mark_cell_update(cell, |value| value + amount)
            }
            &Command::Set(cell, value) => {
                if let CellValue::Known(current_value) = self.mark_cell_read(cell) {
                    command = Command::Add(cell.to_owned(), value - current_value)
                };
                self.mark_cell_write(cell, CellValue::Known(value))
            }
            Command::Loop(origin, body) => {
                for &cell in &body.read_cells {
                    self.mark_cell_read(cell);
                }
                for &cell in &body.written_cells {
                    self.mark_cell_shuffle(cell)
                }
                // The cell has to be read and reset for the loop to end
                self.mark_cell_update(*origin, |_| CellValue::Known(0))
            }
            &Command::ReadIn(cell) => {
                self.mark_cell_write(cell, CellValue::Unknown)
            }
            &Command::WriteFrom(cell) => {
                self.mark_cell_read(cell);
            }
            &Command::Invoke { input_size, output_size, input_source, output_destination, .. } => {
                for i in 0..input_size {
                    self.mark_cell_read(input_source.offset(i));
                }
                for i in 0..output_size {
                    self.mark_cell_write(output_destination.offset(i), CellValue::Unknown);
                }
            }
            Command::ContextSnapshot(None, _) => {}
            &Command::ContextSnapshot(Some(cell), _) => {
                self.mark_cell_read(cell);
            }
            &Command::Capture(cell) => {
                self.mark_cell_read(cell);
            }
        }
        self.commands.push(command)
    }

    /// Converts this segment to Brainfuck code.
    pub fn into_code(self, cursor: usize, allocator: &mut Allocator) -> BalancedCode {
        // Get the list of memory pages to allocate and free at every command
        let mut creations = Vec::<Vec<_>>::with_len(self.commands.len());
        let mut deletions = Vec::<Vec<_>>::with_len(self.commands.len());
        for (id, info) in self.pages {
            let size = info.size();
            if let Some(i) = info.first_access() {
                creations[i].push((id, size));
            }
            if let Some(i) = info.last_access() {
                deletions[i].push((id, size));
            }
        }
        // Generate code
        let mut code = BalancedCode::new(cursor);
        for (command_index, command) in self.commands.into_iter().enumerate() {
            // Allocate new pages
            for &(page, size) in &creations[command_index] {
                if let Some(id) = page.get_internal_page_id() {
                    allocator.alloc(id, size);
                }
            }
            // Compile command
            match command {
                Command::Add(cell, amount) => {
                    code.push_add(allocator.index(cell), amount);
                }
                Command::Set(cell, value) => {
                    code.push_set(allocator.index(cell), value);
                }
                Command::ReadIn(cell) => {
                    code.push_read(allocator.index(cell));
                }
                Command::WriteFrom(cell) => {
                    code.push_write(allocator.index(cell));
                }
                Command::Loop(origin, segment) => {
                    let origin_index = allocator.index(origin);
                    let body = segment.into_code(origin_index, allocator);
                    code.push_loop(origin_index, body);
                }
                Command::Invoke {
                    input_size,
                    output_size,
                    input_source,
                    output_destination,
                    input_destination,
                    output_source,
                    segment,
                } => {
                    let segment_origin = allocator.frame_size();
                    // Move input
                    for i in 0..input_size {
                        let source = allocator.index(input_source.offset(i));
                        let destination = segment_origin + input_destination + i;
                        code.push_move(source, destination);
                    }
                    code.extend(segment_origin, segment);
                    // Move output
                    for i in 0..output_size {
                        let source = segment_origin + output_source + i;
                        let destination = allocator.index(output_destination.offset(i));
                        code.push_move(source, destination);
                    }
                }
                Command::ContextSnapshot(cell, names) => {
                    let mut context = String::from("? Context snapshot");
                    let cursor = cell.map_or(0, |cell| allocator.index(cell));
                    context.push_str(&format!("\n| Cursor at index {}", cursor));
                    if names.is_empty() {
                        context.push_str("\n| No names");
                    } else {
                        context.push_str("\n| Names:");
                        for (name, cell) in names {
                            let index = allocator.index(cell);
                            context.push_str(&format!("\n    {}: {}", name, index));
                        }
                    }
                    code.push_comment(context, cursor);
                }
                Command::Capture(test) => {
                    let index = allocator.index(test);
                    code.push_comment(String::from("@"), index)
                }
            }
            // Free unused pages
            for &(page, size) in &deletions[command_index] {
                if let Some(id) = page.get_internal_page_id() {
                    let origin = allocator.index(page.base());
                    for i in 0..size {
                        code.push_set(origin + i, 0)
                    }
                    allocator.free(id);
                }
            }
        }
        code
    }
}


/// A generator is used to create a [`Segment`].
///
/// # [`generate!`] macro
///
/// Most methods on a `Generator` have an assembly-like equivalent that can be used in the
/// [`generate!`] macro. When such an equivalent exists, it is mentioned in the method's
/// documentation, under "GASM" (for "`generate!`-assembly").
#[derive(Debug)]
pub(super) struct Generator {
    segment: Segment,
    input_size: usize,
    output_size: usize,
    uniqueness_provider: UniquenessProvider,
}

impl Generator {
    pub fn new(input_size: usize, output_size: usize) -> Self {
        Self::with_uniqueness_provider(UniquenessProvider::new(), input_size, output_size)
    }

    pub fn with_uniqueness_provider(uniqueness_provider: UniquenessProvider, input_size: usize, output_size: usize) -> Self {
        Self {
            segment: Segment::default(),
            input_size,
            output_size,
            uniqueness_provider,
        }
    }

    /// Creates a child generator (that shares the uniqueness provider with this generator), calls
    /// `f` on it, and returns the resulting commands.
    fn generate_child(&mut self, f: impl FnOnce(&mut Self)) -> Segment {
        let uniqueness_provider = mem::take(&mut self.uniqueness_provider);
        let mut child_generator = Self::with_uniqueness_provider(uniqueness_provider, self.input_size, self.output_size);
        f(&mut child_generator);
        mem::swap(&mut self.uniqueness_provider, &mut child_generator.uniqueness_provider);
        child_generator.segment
    }

    /// Allocates a memory page on the stack and returns its first cell.
    ///
    /// # GASM
    ///
    /// This methods has the corresponding [`generate!`]-assembly instruction `ALLOC`. It is
    /// followed by the identifier of a new variable to store the cell in.
    ///
    /// ## Example
    ///
    /// The following example allocates memory for a cell `tmp`, sets its value to 67, and writes
    /// it.
    ///
    /// ```gasm
    /// ALLOC tmp;
    /// SET tmp 67;
    /// WRITE tmp;
    /// ```
    pub fn allocate(&mut self) -> Cell {
        self.segment.register_page(&mut self.uniqueness_provider).base()
    }

    /// Adds commands that decrement a cell.
    ///
    /// # GASM
    ///
    /// This methods has the corresponding [`generate!`]-assembly instruction `DECR`. It is
    /// followed by the cell to decrement.
    ///
    /// ## Example
    ///
    /// The following example decrements the value of `x`.
    ///
    /// ```gasm
    /// DECR x;
    /// ```
    pub fn generate_decrement(&mut self, cell: Cell) {
        self.segment.add_command(Command::Add(cell, u8::MAX))
    }

    /// Adds commands that increment a cell.
    ///
    /// # GASM
    ///
    /// This methods has the corresponding [`generate!`]-assembly instruction `INCR`. It is followed
    /// by the cell to increment.
    ///
    /// ## Example
    ///
    /// The following example increments the value of `x`.
    ///
    /// ```gasm
    /// INCR x;
    /// ```
    pub fn generate_increment(&mut self, cell: Cell) {
        self.segment.add_command(Command::Add(cell, 1))
    }

    /// Adds commands that set the value of a cell.
    ///
    /// # GASM
    ///
    /// This methods has the corresponding [`generate!`]-assembly instruction `SET`. It is followed
    /// by the cell to set, and the new value.
    ///
    /// ## Example
    ///
    /// The following example sets the value of `x` to 42.
    ///
    /// ```gasm
    /// SET x 42;
    /// ```
    pub fn generate_set(&mut self, cell: Cell, value: u8) {
        self.segment.add_command(Command::Set(cell, value))
    }

    /// Adds commands that reset the value of a cell to 0.
    ///
    /// # GASM
    ///
    /// This methods has the corresponding [`generate!`]-assembly instruction `RESET`. It is
    /// followed by the cell to reset. Alternatively, you can specify multiple comma-separated cells
    /// to reset by surrounding them by braces.
    ///
    /// ## Examples
    ///
    /// The following example resets the value of `x`.
    ///
    /// ```gasm
    /// RESET x;
    /// ```
    ///
    /// The following example resets the values of `x`, `y` and `z`.
    ///
    /// ```gasm
    /// RESET {x, y, z};
    /// ```
    pub fn generate_reset(&mut self, cell: Cell) {
        self.generate_set(cell, 0)
    }

    /// Adds commands that create a balanced loop.
    ///
    /// A balanced loop is characterized by its *origin* and its *body*.
    ///
    /// # Origin
    ///
    /// The origin of a balanced loop is the cell that is pointed to at the beginning and the end of
    /// each iteration of the loop. This means the loop executes until the origin is null, which
    /// means the origin will always be null after a loop.
    ///
    /// # Body
    ///
    /// The body of the loop is a sequence of commands that are executed within the loop. The `body`
    /// argument is a function that accepts a reference to a mutable generator and constructs the
    /// body of the loop using it.
    ///
    /// # GASM
    ///
    /// This methods has the corresponding [`generate!`]-assembly instruction `WHILE`. It is
    /// followed by the origin, and the body of the loop, as GASM instructions wrapped in square
    /// brackets.
    ///
    /// In case the origin of the loop is decremented each iteration, you can use the `LOOP`
    /// instruction instead of adding a manual `DECR` within the body of the loop. The decrement
    /// will happen at the end of the loop.
    ///
    /// ## Examples
    ///
    /// The following example adds the value of `x` to `y` using a `WHILE` instruction.
    ///
    /// ```gasm
    /// WHILE x [
    ///     DECR x;
    ///     INCR y;
    /// ];
    /// ```
    ///
    /// The following example adds the value of `x` to `y` using a `LOOP` instruction.
    ///
    /// ```gasm
    /// LOOP x [
    ///     INCR y;
    ///     // `x` is decremented here
    /// ];
    /// ```
    pub fn generate_loop(&mut self, origin: Cell, body: impl FnOnce(&mut Self)) {
        let commands = self.generate_child(body);
        self.segment.add_command(Command::Loop(origin, commands))
    }

    /// Adds commands that execute a body once if, and only if, a condition holds.
    ///
    /// Note that the value of the condition is reset after the branch is executed.
    ///
    /// # GASM
    ///
    /// This methods has the corresponding [`generate!`]-assembly instruction `IF`. It is followed
    /// by the condition, and the body of the branch, as gasm instructions wrapped in square
    /// brackets.
    ///
    /// ## Example
    ///
    /// The following example writes the value of `x` if it is non-zero.
    ///
    /// ```gasm
    /// IF x [
    ///     WRITE x;
    /// ];
    /// ```
    pub fn generate_branch(&mut self, condition: Cell, body: impl FnOnce(&mut Self)) {
        self.generate_loop(condition, |generator| {
            body(generator);
            generator.generate_reset(condition)
        });
    }

    /// Adds commands that read a value and stores it into a cell.
    ///
    /// # GASM
    ///
    /// This methods has the corresponding [`generate!`]-assembly instruction `READ`. It is followed
    /// by the cell to read the value into.
    ///
    /// ## Examples
    ///
    /// The following example reads a value and writes it back.
    ///
    /// ```gasm
    /// ALLOC x;
    /// READ x;
    /// WRITE x;
    /// ```
    pub fn generate_read(&mut self, destination: Cell) {
        self.segment.add_command(Command::ReadIn(destination))
    }

    /// Add commands that write the value of a cell.
    ///
    /// # GASM
    ///
    /// This methods has the corresponding [`generate!`]-assembly instruction `WRITE`. It is
    /// followed by the cell to write the value of.
    ///
    /// ## Examples
    ///
    /// The following example writes the value of `x`.
    ///
    /// ```gasm
    /// WRITE x;
    /// ```
    pub fn generate_write(&mut self, source: Cell) {
        self.segment.add_command(Command::WriteFrom(source))
    }

    /// Adds commands that write a static value.
    pub fn generate_char_write(&mut self, character: u8) {
        let tmp = self.allocate();
        self.generate_set(tmp, character);
        self.generate_write(tmp);
    }

    /// Adds commands that write the ASCII characters of a static string.
    ///
    /// If the passed string contains non-ASCII characters, this function will panic.
    ///
    /// # GASM
    ///
    /// This methods has the corresponding [`generate!`]-assembly instruction `WRITE`. It is
    /// followed by a string literal.
    ///
    /// ## Examples
    ///
    /// The following example writes the the string `"Hello, World!"`.
    ///
    /// ```gasm
    /// WRITE "Hello, World!";
    /// ```
    pub fn generate_static_write(&mut self, characters: &str) {
        for c in characters.chars() {
            self.generate_char_write(c as u8);
        }
    }

    /// Moves the value of a source cell to a destination cell.
    ///
    /// Note that the value of the destination is not reset before copying, which means the value of
    /// the source is actually added to the value of the destination. Additionally, it should be
    /// noted that this operation ends up reseting the value of the source cell.
    ///
    /// # GASM
    ///
    /// This method has the corresponding [`generate!`]-assembly instruction `MOVE`. It is followed
    /// by the cell containing the value to move and the cell to move it to.
    ///
    /// ## Example
    ///
    /// The following example moves the value of `x` into `y`.
    ///
    /// ```gasm
    /// MOVE x y;
    /// ```
    pub fn generate_move(&mut self, source: Cell, destination: Cell) {
        assert_ne!(source, destination);
        self.generate_loop(source, |generator| {
            generator.generate_decrement(source);
            generator.generate_increment(destination)
        });
    }

    /// Copies the value of a source cell to a destination cell.
    ///
    /// Note that the value of the destination is not reset before copying, which means the value of
    /// the source is actually added to the value of the destinations.
    ///
    /// # GASM
    ///
    /// This methods has the corresponding [`generate!`]-assembly instruction `COPY`. It is followed
    /// by the cell to copy from and the cell to copy into.
    ///
    /// ## Example
    ///
    /// The following example copies the value of `x` into `y`.
    ///
    /// ```gasm
    /// COPY x y;
    /// ```
    pub fn generate_copy(&mut self, source: Cell, destination: Cell) {
        let tmp = self.allocate();
        // Move
        self.generate_loop(source, |generator| {
            generator.generate_decrement(source);
            generator.generate_increment(tmp);
            generator.generate_increment(destination)
        });
        // Restore
        self.generate_loop(tmp, |generator| {
            generator.generate_decrement(tmp);
            generator.generate_increment(source)
        })
    }

    /// Calls a subroutine with some input, and moves its output starting at the specified cell.
    pub fn generate_call(&mut self, subroutine: &CallableSegment, input: Cell, output: Cell) {
        self.segment.add_command(Command::Invoke {
            input_size: subroutine.input_size,
            output_size: subroutine.output_size,
            input_source: input,
            output_destination: output,
            input_destination: subroutine.input_offset,
            output_source: subroutine.output_offset,
            segment: subroutine.code.to_owned(),
        })
    }

    /// Generates code that moves the pointer to a cell and adds a context snapshot. A context
    /// snapshot is a comment that is added to the generated Brainfuck code, containing information
    /// about the internal state of the compiler.
    pub fn generate_context_snapshot(&mut self, cell: Option<Cell>, context: Vec<(String, Cell)>) {
        self.segment.add_command(Command::ContextSnapshot(cell, context));
    }

    /// Generates code that moves the pointer to a cell and adds a `@` to the final Brainfuck code.
    ///
    /// This is used by some [tests](crate::test).
    pub fn generate_capture(&mut self, cell: Cell) {
        self.segment.add_command(Command::Capture(cell));
    }

    /// Generates code and returns the indices of the input and output cells as well.
    pub fn generate(self) -> CallableSegment {
        let mut allocator = Allocator::new(self.input_size, self.output_size);
        let mut code = self.segment.into_code(0, &mut allocator);
        // Reset input
        let input_index = allocator.input_index();
        for i in 0..self.input_size {
            code.push_set(input_index + i, 0)
        }
        CallableSegment {
            code,
            input_offset: input_index,
            output_offset: allocator.output_index(),
            input_size: self.input_size,
            output_size: self.output_size,
        }
    }
}


/// An assembly-like syntax for generating commands.
///
/// # Commands
///
/// Each command consists of a command name, followed by its arguments, and a semicolon.
///
/// ## `ALLOC`
///
/// Creates a new variable and allocates memory for it. See [`Generator::allocate`].
///
/// The only argument is the identifier of a Rust variable to declare, that will hold the allocated
/// cell.
///
/// You can also an initial value as an additional argument: either a Rust expression that evaluates
/// to a `u8`, or a cell to copy the value from, prefixed with a `@`.
///
/// ### Examples
///
/// The following example allocates memory for a cell `tmp`, sets its value to 67, and writes it.
///
/// ```gasm
/// ALLOC tmp;
/// SET tmp 67;
/// WRITE tmp;
/// ```
///
/// This is equivalent to:
///
/// ```gasm
/// ALLOC tmp 67;
/// WRITE tmp;
/// ```
///
/// The following example allocates memory for a cell `tmp` and sets its value to the value of the
/// cell `x`.
///
/// ```gasm
/// ALLOC tmp;
/// COPY x tmp;
/// ```
///
/// This is equivalent to:
///
/// ```gasm
/// ALLOC tmp @x;
/// ```
///
/// ## `DECR`
///
/// Decrements a cell. See [`Generator::generate_decrement`].
///
/// The only argument is the cell to decrement.
///
/// ## `INCR`
///
/// Increments a cell. See [`Generator::generate_increment`].
///
/// The only argument is the cell to increment.
///
/// ## `SET`
///
/// Sets the value of a cell. See [`Generator::generate_set`].
///
/// The arguments are the cell to set, and the value. The value can be either a Rust expression that
/// evaluates to a `u8`, or a cell to copy the value of, prefixed with a `@`.
///
/// ## Example
///
/// The following example sets the value of `x` to 2, and then copies this value into `y`.
///
/// ```gasm
/// SET x 1 + 1;
/// SET y @x;
/// ```
///
/// ## `RESET`
///
/// Resets the value of a cell. See [`Generator::generate_reset`].
///
/// The only argument is the cell to reset. Alternatively, you can specify multiple comma-separated
/// cells to reset by surrounding them by braces.
///
/// ### Example
///
/// The following example resets the values of `x`, `y` and `z`.
///
/// ```gasm
/// RESET {x, y, z};
/// ```
///
/// ## `WHILE`
///
/// Loops while a cell (the origin) is non-zero. See [`Generator::generate_loop`].
///
/// The arguments are the origin of the loop, and the body of the loop, as a sequence of
/// instructions, wrapped in square brackets.
///
/// Note that this instruction has the side effect of resetting the value of the origin.
///
/// ### Example
///
/// The following example adds the value of `x` to `y`.
///
/// ```gasm
/// WHILE x [
///     DECR x;
///     INCR y;
/// ];
/// ```
///
/// ## `LOOP`
///
/// Loops as many times as the value of a cell (the origin). See [`Generator::generate_loop`].
///
/// The origin is decremented at the end of each iteration and the loop ends when it reaches zero.
///
/// The arguments are the origin of the loop, and the body of the loop.
///
/// Note that this instruction has the side effect of resetting the value of the origin.
///
/// ### Example
///
/// The following example adds the value of `x` to `y`.
///
/// ```gasm
/// LOOP x [
///     INCR y;
/// ];
/// ```
///
/// ## `IF`
///
/// Executes instructions iff a cell's value is non-zero. See [`Generator::generate_branch`].
///
/// The arguments are the condition cell, and the instructions to execute.
///
/// Note that this instruction has the side effect of resetting the value of the condition cell
/// after executing the body. To prevent this behavior, prefix the cell with a `@`.
///
/// ### Examples
///
/// The following example writes the value of `x` twice if it is non-zero.
///
/// ```gasm
/// IF @x [
///     WRITE x;
/// ];
/// // `x` still has the same value
/// IF x [
///     WRITE x;
/// ];
/// // `x` is now 0
/// ```
///
/// Of course, a better way to achieve the same effect would be:
///
/// ```gasm
/// IF x [
///     WRITE x;
///     WRITE x;
/// ];
/// ```
///
/// ## `IFNULL`
///
/// Executes instructions iff a cell's value is zero.
///
/// The arguments are the condition cell, and the instructions to execute.
///
/// Note that this instruction has the side effect of resetting the value of the condition cell
/// *before executing the body*. To prevent this behavior, prefix the cell with a `@`.
///
/// ### Example
///
/// The following example writes `max(1, v(x))`, where `v(x)` is the value of `x`.
///
/// ```gasm
/// IFNULL @x [
///     INCR x;
/// ];
/// WRITE x;
/// ```
///
/// ## `READ`
///
/// Reads a value into a cell. See [`Generator::generate_read`].
///
/// The only argument is the cell to read into.
///
/// ## `WRITE`
///
/// Writes a value.
///
/// You can write the value of a cell by specifying the cell to write the value of. See
/// [`Generator::generate_write`].
///
/// Alternatively, you can write a literal value by specifying a string literal. See
/// [`Generator::generate_static_write`].
///
/// ### Example
///
///The following example writes the value of a cell `x`, followed by a newline.
///
/// ```gasm
/// WRITE x;
/// WRITE "\n";
/// ```
///
/// ## `MOVE`
///
/// Moves the value of a cell to another cell. See [`Generator::generate_move`].
///
/// The arguments are the cell to move the value of, and the cell to move the value into.
/// Alternatively, you can move the value of adjacent cells to distinct adjacent cells by providing
/// the length (in cells) of the block to move as an additional argument.
///
/// Note that this instruction has the side effect of resetting the value of the first cell. To copy
/// the value instead, use `COPY`.
///
/// ### Example
///
/// The following example moves the values of three adjacent cells to another block of three
/// adjacent cells.
///
/// ```gasm
/// MOVE src dst 3;
/// ```
///
/// This is equivalent to:
///
/// ```gasm
/// let src0 = src.offset(0);
/// let src1 = src.offset(1);
/// let src2 = src.offset(2);
/// let dst0 = dst.offset(0);
/// let dst1 = dst.offset(1);
/// let dst2 = dst.offset(2);
/// MOVE src0 dst0;
/// MOVE src1 dst1;
/// MOVE src2 dst2;
/// ```
///
/// ## `COPY`
///
/// Copies the value of a cell into another cell. See [`Generator::generate_copy`].
///
/// The arguments are the cell to copy the value of, and the cell to copy the value into.
///
/// ### Example
///
/// The following example copies the value of `src` into `dst`.
///
/// ```gasm
/// COPY src dst;
/// ```
///
/// # Meta commands
///
/// Some commands do not generate any Brainfuck code. Instead, they let you declare and interact
/// with Rust variables, acting as a sort of macro.
///
/// ## `let` bindings
///
/// You can declare a new Rust variable, and execute arbitrary Rust code during the generation
/// process, by using a `let` binding.
///
/// ### Examples
///
/// The following example allocates memory, and creates a new Rust variable for the cell at index 1
/// of the allocated memory.
///
/// ```gasm
/// ALLOC x;
/// let y = x.offset(1);
/// ```
///
/// The following example prints "Hello, World!" during the generation process.
///
/// ```gasm
/// let _ = println!("Hello, World!");
/// ```
#[macro_export]
macro_rules! generate {
    { ($generator:expr) } => {};

    // Let binding
    {
        ($generator:expr)
        let $pattern:pat = $value:expr;
        $( $t:tt )*
    } => {
        let $pattern = $value;
        generate! { ($generator) $( $t )* }
    };

    // Allocate
    {
        ($generator:expr)
        ALLOC $name:ident;
        $( $t:tt )*
    } => {{
        let $name = $generator.allocate();
        generate! { ($generator) $( $t )* }
    }};

    // Allocate with initial value from cell
    {
        ($generator:expr)
        ALLOC $name:ident @$source:ident;
        $( $t:tt )*
    } => {{
        let $name = $generator.allocate();
        $generator.generate_copy($source, $name);
        generate! { ($generator) $( $t )* }
    }};

    // Allocate with initial value
    {
        ($generator:expr)
        ALLOC $name:ident $value:expr;
        $( $t:tt )*
    } => {{
        // Evaluate before to prevent declaring `$name`.
        let value = $value;
        let $name = $generator.allocate();
        $generator.generate_set($name, value);
        generate! { ($generator) $( $t )* }
    }};

    // Decrement
    {
        ($generator:expr)
        DECR $cell:ident;
        $( $t:tt )*
    } => {{
        $generator.generate_decrement($cell);
        generate! { ($generator) $( $t )* }
    }};

    // Increment
    {
        ($generator:expr)
        INCR $cell:ident;
        $( $t:tt )*
    } => {{
        $generator.generate_increment($cell);
        generate! { ($generator) $( $t )* }
    }};

    // Set from cell
    {
        ($generator:expr)
        SET $cell:ident @$source:ident;
        $( $t:tt )*
    } => {{
        $generator.generate_reset($cell);
        $generator.generate_copy($source, $cell);
        generate! { ($generator) $( $t )* }
    }};

    // Set
    {
        ($generator:expr)
        SET $cell:ident $value:expr;
        $( $t:tt )*
    } => {{
        $generator.generate_set($cell, $value);
        generate! { ($generator) $( $t )* }
    }};

    // Reset
    {
        ($generator:expr)
        RESET $cell:ident;
        $( $t:tt )*
    } => {{
        $generator.generate_reset($cell);
        generate! { ($generator) $( $t )* }
    }};

    // Reset multiple
    {
        ($generator:expr)
        RESET { $( $cell:ident ), + };
        $( $t:tt )*
    } => {{
        $(
            $generator.generate_reset($cell);
        )+
        generate! { ($generator) $( $t )* }
    }};

    // While loop
    {
        ($generator:expr)
        WHILE $origin:ident [
            $( $body:tt )*
        ];
        $( $t:tt )*
    } => {{
        $generator.generate_loop($origin, |generator| {
            generate! { (generator) $( $body )* }
        });
        generate! { ($generator) $( $t )* }
    }};

    // Loop
    {
        ($generator:expr)
        LOOP $origin:ident [
            $( $body:tt )*
        ];
        $( $t:tt )*
    } => {{
        $generator.generate_loop($origin, |generator| {
            generate! { (generator) $( $body )* }
            generator.generate_decrement($origin)
        });
        generate! { ($generator) $( $t )* }
    }};

    // Conditional branch
    {
        ($generator:expr)
        IF $condition:ident [
            $( $body:tt )*
        ];
        $( $t:tt )*
    } => {{
        $generator.generate_branch($condition, |generator| {
            generate! { (generator) $( $body )* }
        });
        generate! { ($generator) $( $t )* }
    }};

    // Non-destructive conditional branch
    {
        ($generator:expr)
        IF @$condition:ident [
            $( $body:tt )*
        ];
        $( $t:tt )*
    } => {{
        // Copy the condition
        let condition = $generator.allocate();
        $generator.generate_copy($condition, condition);
        // Execute the body
        $generator.generate_branch(condition, |generator| {
            generate! { (generator) $( $body )* }
        });
        generate! { ($generator) $( $t )* }
    }};

    // Inverted conditional branch
    {
        ($generator:expr)
        IFNULL $condition:ident [
            $( $body:tt )*
        ];
        $( $t:tt )*
    } => {{
        // Invert the condition
        let inverted_condition = $generator.allocate();
        $generator.generate_increment(inverted_condition);
        $generator.generate_branch($condition, |generator| {
            generator.generate_decrement(inverted_condition);
        });
        // Execute the body
        $generator.generate_branch(inverted_condition, |generator| {
            generate! { (generator) $( $body )* }
        });
        generate! { ($generator) $( $t )* }
    }};

    // Inverted non-destructive conditional branch
    {
        ($generator:expr)
        IFNULL @$condition:ident [
            $( $body:tt )*
        ];
        $( $t:tt )*
    } => {{
        // Copy the condition
        let condition = $generator.allocate();
        $generator.generate_copy($condition, condition);
        // Invert the condition
        let inverted_condition = $generator.allocate();
        $generator.generate_increment(inverted_condition);
        $generator.generate_branch(condition, |generator| {
            generator.generate_decrement(inverted_condition);
        });
        // Execute the body
        $generator.generate_branch(inverted_condition, |generator| {
            generate! { (generator) $( $body )* }
        });
        generate! { ($generator) $( $t )* }
    }};

    // Read
    {
        ($generator:expr)
        READ $destination:ident;
        $( $t:tt )*
    } => {{
        $generator.generate_read($destination);
        generate! { ($generator) $( $t )* }
    }};

    // Write
    {
        ($generator:expr)
        WRITE $source:ident;
        $( $t:tt )*
    } => {{
        $generator.generate_write($source);
        generate! { ($generator) $( $t )* }
    }};

    // Static write
    {
        ($generator:expr)
        WRITE $str:literal;
        $( $t:tt )*
    } => {{
        $generator.generate_static_write($str);
        generate! { ($generator) $( $t )* }
    }};

    // Move
    {
        ($generator:expr)
        MOVE $source:ident $destination:ident;
        $( $t:tt )*
    } => {{
        $generator.generate_move($source, $destination);
        generate! { ($generator) $( $t )* }
    }};

    // Move block
    {
        ($generator:expr)
        MOVE $source:ident $destination:ident $count:expr;
        $( $t:tt )*
    } => {{
        let count = $count;
        for i in 0..count {
            $generator.generate_move($source.offset(i), $destination.offset(i));
        }
        generate! { ($generator) $( $t )* }
    }};

    // Copy
    {
        ($generator:expr)
        COPY $source:ident $destination:ident;
        $( $t:tt )*
    } => {{
        $generator.generate_copy($source, $destination);
        generate! { ($generator) $( $t )* }
    }};
}
