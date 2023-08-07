use std::fmt;
use std::fmt::{Display, Formatter};

use crate::lexer::tokens::{PUBLIC_KEYWORD, Symbol};
use crate::location::Location;
use crate::reference::Reference;
use crate::type_checker::types::Type;

#[derive(Debug)]
enum ExceptionType {
    Generic,
    Unimplemented,
    Syntax,
    Type,
    ConstantEvaluation,
}

impl Display for ExceptionType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Generic => write!(f, "Error"),
            Self::Unimplemented => write!(f, "Unimplemented feature"),
            Self::Syntax => write!(f, "Syntax error"),
            Self::Type => write!(f, "Type error"),
            Self::ConstantEvaluation => write!(f, "Constant evaluation error"),
        }
    }
}


#[derive(Debug)]
struct ExceptionBuilder<S> {
    source: S,
    r#type: ExceptionType,
    message: String,
}

impl<S> ExceptionBuilder<S> {
    fn new(source: S, message: impl Into<String>) -> Self {
        Self {
            source,
            r#type: ExceptionType::Generic,
            message: message.into(),
        }
    }

    fn new_unimplemented_error(source: S, message: impl Into<String>) -> Self {
        Self {
            source,
            r#type: ExceptionType::Unimplemented,
            message: message.into(),
        }
    }

    fn new_syntax_error(source: S, message: impl Into<String>) -> Self {
        Self {
            source,
            r#type: ExceptionType::Syntax,
            message: message.into(),
        }
    }

    fn new_type_error(source: S, message: impl Into<String>) -> Self {
        Self {
            source,
            r#type: ExceptionType::Type,
            message: message.into(),
        }
    }

    /// This is a syntax error by default.
    fn new_expected(source: S, expected: impl Display) -> Self {
        ExceptionBuilder::new_syntax_error(source, format!("Expected {}", expected))
    }

    /// This is a type error by default.
    fn new_expected_found(source: S, expected: impl Display, found: impl Display) -> Self {
        ExceptionBuilder::new_syntax_error(source, format!("Expected {} but found {}", expected, found))
    }

    fn with_type(mut self, r#type: ExceptionType) -> Self {
        self.r#type = r#type;
        self
    }

    fn build_without_hint(self) -> CompilationException<S> {
        CompilationException {
            source: self.source,
            r#type: self.r#type,
            message: self.message,
            hint: None,
        }
    }

    fn build(self, hint: impl Into<String>) -> CompilationException<S> {
        let hint_string = hint.into();
        debug_assert!(hint_string.ends_with('.') || hint_string.ends_with('?'), "Hint should be a full sentence");
        CompilationException {
            source: self.source,
            r#type: self.r#type,
            message: self.message,
            hint: Some(hint_string),
        }
    }
}


/// An exception that corresponds to an error in the input file.
#[derive(Debug)]
pub struct CompilationException<S> {
    source: S,
    r#type: ExceptionType,
    message: String,
    hint: Option<String>,
}

impl<S> CompilationException<S> {
    pub fn unimplemented_arrays(source: S) -> Self {
        ExceptionBuilder::new_unimplemented_error(source, "Arrays are not yet implemented")
            .build_without_hint()
    }

    pub fn unexpected_character(source: S) -> Self {
        ExceptionBuilder::new_syntax_error(source, "Unexpected character in input")
            .build_without_hint()
    }

    pub fn empty_character_literal(source: S) -> Self {
        ExceptionBuilder::new_syntax_error(source, "Empty character literal")
            .build("Try specifying a character literal here.")
    }

    pub fn unterminated_character_literal(source: S) -> Self {
        ExceptionBuilder::new_syntax_error(source, "Unterminated character literal")
            .build("Character literals may only contain a single character (or an escape sequence).")
    }

    pub fn unterminated_string_literal(source: S, delimiter: char) -> Self {
        ExceptionBuilder::new_syntax_error(source, "Unterminated string or character literal")
            .build(format!("Try closing the literal with `{}` here.", delimiter))
    }

    pub fn unterminated_native_code_block(source: S, delimiter: &str) -> Self {
        ExceptionBuilder::new_syntax_error(source, "Unterminated native code block")
            .build(format!("Close the code block using '{}'.", delimiter))
    }

    pub fn invalid_escape_sequence(source: S) -> Self {
        ExceptionBuilder::new_syntax_error(source, "Invalid escape sequence")
            .build_without_hint()
    }

    pub fn invalid_character_in_literal(source: S) -> Self {
        ExceptionBuilder::new_syntax_error(source, "Invalid character in literal")
            .build("Only non-control ASCII characters are allowed in literals.")
    }

    /// This constructor is for highly generic cases. For more specific cases, it is preferable to
    /// use a more semantically accurate `expected_*` constructor.
    pub fn expected(source: S, expected: impl Display) -> Self {
        ExceptionBuilder::new_expected(source, expected)
            .build_without_hint()
    }

    pub fn expected_digit(source: S, radix: u32) -> Self {
        ExceptionBuilder::new_expected(source, format!("base {} digit", radix))
            .build_without_hint()
    }

    pub fn expected_identifier(source: S) -> Self {
        ExceptionBuilder::new_expected(source, "identifier")
            .build_without_hint()
    }

    pub fn expected_string(source: S) -> Self {
        ExceptionBuilder::new_expected(source, "string")
            .build_without_hint()
    }

    pub fn expected_number(source: S) -> Self {
        ExceptionBuilder::new_expected(source, "number")
            .build_without_hint()
    }

    pub fn expected_character(source: S) -> Self {
        ExceptionBuilder::new_expected(source, "character")
            .build_without_hint()
    }

    pub fn expected_native_code(source: S) -> Self {
        ExceptionBuilder::new_expected(source, "native code block")
            .build_without_hint()
    }

    pub fn invalid_char_literal(source: S) -> Self {
        ExceptionBuilder::new_syntax_error(source, "Invalid char literal: value must be between 0 and 255")
            .build_without_hint()
    }

    pub fn element_name_is_already_used(source: S, element_type: &str, initial_definition_location: Location) -> Self {
        ExceptionBuilder::new_type_error(source, format!("A {} already exists with this name", element_type))
            .build(format!("Initially defined at {}", initial_definition_location))
    }

    pub fn unknown_element(source: S, element_type: &str, reference: &Reference) -> Self {
        ExceptionBuilder::new_type_error(source, format!("Unknown {}: {}", element_type, reference))
            .build_without_hint()
    }

    pub fn inaccessible_element(source: S, element_type: &str, reference: &Reference) -> Self {
        ExceptionBuilder::new_type_error(source, format!("{} {} is inaccessible from here", element_type, reference))
            .build(format!("If possible, try making {} public using the {} keyword.", reference, PUBLIC_KEYWORD))
    }

    pub fn negative_offset(source: S) -> Self {
        ExceptionBuilder::new(source, "Offset cannot be negative")
            .build("This is because the pointer is not allowed to ever go to the left of its initial position.")
    }

    pub fn increment_non_char(source: S) -> Self {
        ExceptionBuilder::new_type_error(source, "Cannot increment non-char variable")
            .build_without_hint()
    }

    pub fn decrement_non_char(source: S) -> Self {
        ExceptionBuilder::new_type_error(source, "Cannot decrement non-char variable")
            .build_without_hint()
    }

    pub fn wrong_arity(source: S, reference: &Reference, expected_arity: usize, found_arity: usize) -> Self {
        let expected_arity_text = if expected_arity < 2 {
            format!("{} argument", expected_arity)
        } else {
            format!("{} arguments", expected_arity)
        };
        ExceptionBuilder::new_type_error(source, format!("Subroutine {} accepts {} arguments, but found {}", reference, expected_arity_text, found_arity))
            .build_without_hint()
    }

    pub fn expected_constant_value(source: S, found_type: &Type) -> Self {
        ExceptionBuilder::new_expected_found(source, "constant", found_type)
            .with_type(ExceptionType::ConstantEvaluation)
            .build("The only types that can be used as a constant value are: bool, char, <integer> and tuples.")
    }

    pub fn expected_constant_integer(source: S, found_type: &Type) -> Self {
        ExceptionBuilder::new_expected_found(source, "constant integer", found_type)
            .with_type(ExceptionType::ConstantEvaluation)
            .build_without_hint()
    }

    pub fn does_not_fit_in_char(source: S, value: i32) -> Self {
        ExceptionBuilder::new_type_error(source, format!("Value is too large to fit un `char`: {}", value))
            .build(format!("The value of a `char` must be an integer between {} and {} (both included).", u8::MIN, u8::MAX))
    }

    pub fn wrong_type(source: S, expected_type: &Type, found_type: &Type) -> Self {
        ExceptionBuilder::new_expected_found(source, expected_type, found_type)
            .build_without_hint()
    }

    pub fn unexpected_return(source: S, returned_type: &Type) -> Self {
        ExceptionBuilder::new_type_error(source, "Unexpected `return` in procedure")
            .build(format!("Try changing the procedure to a function returning {}.", returned_type))
    }

    pub fn invalid_operator(source: S, operator: Symbol, operand_types: &[Type]) -> Self {
        let formatted_operand_types = operand_types.iter()
            .enumerate()
            .fold(String::new(), |accumulator, (i, operand)| {
                if i == 0 {
                    operand.to_string()
                } else {
                    accumulator + ", " + &operand.to_string()
                }
            });
        ExceptionBuilder::new_type_error(source, format!(
            "`{}` is not defined for ({})",
            operator,
            formatted_operand_types,
        ))
            .build_without_hint()
    }

    pub fn not_a_function(source: S, reference: &Reference) -> Self {
        ExceptionBuilder::new_type_error(source, format!("Subroutine {} is not a function", reference))
            .build(format!("A function is a subroutine that has a return value. {} does not return anything.", reference))
    }

    pub fn expected_string_like(source: S, found_type: &Type) -> Self {
        ExceptionBuilder::new_expected_found(source, "string-like", found_type)
            .build(format!("String-like values are of type {}, or arrays of string-like values.", Type::CHAR))
    }

    pub fn unpack_non_tuple(source: S, value_type: &Type) -> Self {
        ExceptionBuilder::new_type_error(source, format!("Unable to unpack value of type {}", value_type))
            .build("You can only unpack tuples.")
    }

    pub fn invalid_unpack_size(source: S, expected_size: usize, found_size: usize) -> Self {
        ExceptionBuilder::new_type_error(source, "Wrong number of unpacked values")
            .build(format!("There are {} values to unpack, but you unpacked {}.", expected_size, found_size))
    }

    pub fn identifier_appears_multiple_times_in_target(source: S, identifier: String, initial_location: Location) -> Self {
        ExceptionBuilder::new(source, format!("Identifier {} appears multiple times in target", identifier))
            .build(format!("The identifier initially appeared at {}", initial_location))
    }
}


/// A [`CompilationException`] whose source is a [`Location`].
pub type LocatedException = CompilationException<Location>;

impl Display for LocatedException {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}: {}", self.source, self.r#type, self.message)
    }
}

impl LocatedException {
    /// Prints a formatted error message to standard error, highlighting the relevant part of the
    /// input.
    pub fn print_with_input(&self, input: &str) {
        eprintln!("{}", self);
        self.source.span().highlight(input);
        if let Some(hint) = &self.hint {
            eprintln!("Hint: {}", hint)
        }
    }
}


impl CompilationException<()> {
    /// Attaches a [`Location`] to this [`CompilationException`].
    pub fn locate(self, location: Location) -> LocatedException {
        LocatedException {
            source: location,
            r#type: self.r#type,
            message: self.message,
            hint: self.hint,
        }
    }
}


/// The result of an operation in the compilation process.
///
/// This is a [`Result`] whose error type is a [`LocatedException`].
pub type CompilationResult<T> = Result<T, LocatedException>;
