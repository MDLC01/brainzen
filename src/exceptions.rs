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


struct ExceptionBuilder {
    location: Location,
    r#type: ExceptionType,
    message: String,
}

impl ExceptionBuilder {
    fn new(location: Location, message: impl Into<String>) -> Self {
        Self {
            location,
            r#type: ExceptionType::Generic,
            message: message.into(),
        }
    }

    fn new_unimplemented_error(location: Location, message: impl Into<String>) -> Self {
        Self {
            location,
            r#type: ExceptionType::Unimplemented,
            message: message.into(),
        }
    }

    fn new_syntax_error(location: Location, message: impl Into<String>) -> Self {
        Self {
            location,
            r#type: ExceptionType::Syntax,
            message: message.into(),
        }
    }

    fn new_type_error(location: Location, message: impl Into<String>) -> Self {
        Self {
            location,
            r#type: ExceptionType::Type,
            message: message.into(),
        }
    }

    /// This is a syntax error by default.
    fn new_expected(location: Location, expected: impl Display) -> Self {
        ExceptionBuilder::new_syntax_error(location, format!("Expected {}", expected))
    }

    /// This is a type error by default.
    fn new_expected_found(location: Location, expected: impl Display, found: impl Display) -> Self {
        ExceptionBuilder::new_syntax_error(location, format!("Expected {} but found {}", expected, found))
    }

    fn with_type(mut self, r#type: ExceptionType) -> Self {
        self.r#type = r#type;
        self
    }

    fn build_without_hint(self) -> CompilationException {
        CompilationException {
            location: self.location,
            r#type: self.r#type,
            message: self.message,
            hint: None,
        }
    }

    fn build(self, hint: impl Into<String>) -> CompilationException {
        let hint_string = hint.into();
        debug_assert!(hint_string.ends_with('.') || hint_string.ends_with('?'), "Hint should be a full sentence");
        CompilationException {
            location: self.location,
            r#type: self.r#type,
            message: self.message,
            hint: Some(hint_string),
        }
    }
}


/// An exception that corresponds to an error in the input file.
#[derive(Debug)]
pub struct CompilationException {
    location: Location,
    r#type: ExceptionType,
    message: String,
    hint: Option<String>,
}

impl CompilationException {
    pub fn unimplemented_arrays(location: Location) -> Self {
        ExceptionBuilder::new_unimplemented_error(location, "Arrays are not yet implemented")
            .build_without_hint()
    }

    pub fn unexpected_character(location: Location) -> Self {
        ExceptionBuilder::new_syntax_error(location, "Unexpected character in input")
            .build_without_hint()
    }

    pub fn empty_character_literal(location: Location) -> Self {
        ExceptionBuilder::new_syntax_error(location, "Empty character literal")
            .build("Try specifying a character literal here.")
    }

    pub fn unterminated_character_literal(location: Location) -> Self {
        ExceptionBuilder::new_syntax_error(location, "Unterminated character literal")
            .build("Character literals may only contain a single character (or an escape sequence).")
    }

    pub fn unterminated_string_literal(location: Location, delimiter: char) -> Self {
        ExceptionBuilder::new_syntax_error(location, "Unterminated string or character literal")
            .build(format!("Try closing the literal with `{}` here.", delimiter))
    }

    pub fn unterminated_native_code_block(location: Location, delimiter: &str) -> Self {
        ExceptionBuilder::new_syntax_error(location, "Unterminated native code block")
            .build(format!("Close the code block using '{}'.", delimiter))
    }

    pub fn invalid_escape_sequence(location: Location) -> Self {
        ExceptionBuilder::new_syntax_error(location, "Invalid escape sequence")
            .build_without_hint()
    }

    pub fn invalid_character_in_literal(location: Location) -> Self {
        ExceptionBuilder::new_syntax_error(location, "Invalid character in literal")
            .build("Only non-control ASCII characters are allowed in literals.")
    }

    /// This constructor is for highly generic cases. For more specific cases, it is preferable to
    /// use a more semantically accurate `expected_*` constructor.
    pub fn expected(location: Location, expected: impl Display) -> Self {
        ExceptionBuilder::new_expected(location, expected)
            .build_without_hint()
    }

    pub fn expected_digit(location: Location, radix: u32) -> Self {
        ExceptionBuilder::new_expected(location, format!("base {} digit", radix))
            .build_without_hint()
    }

    pub fn expected_identifier(location: Location) -> Self {
        ExceptionBuilder::new_expected(location, "identifier")
            .build_without_hint()
    }

    pub fn expected_string(location: Location) -> Self {
        ExceptionBuilder::new_expected(location, "string")
            .build_without_hint()
    }

    pub fn expected_number(location: Location) -> Self {
        ExceptionBuilder::new_expected(location, "number")
            .build_without_hint()
    }

    pub fn expected_character(location: Location) -> Self {
        ExceptionBuilder::new_expected(location, "character")
            .build_without_hint()
    }

    pub fn expected_native_code(location: Location) -> Self {
        ExceptionBuilder::new_expected(location, "native code block")
            .build_without_hint()
    }

    pub fn invalid_char_literal(location: Location) -> Self {
        ExceptionBuilder::new_syntax_error(location, "Invalid char literal: value must be between 0 and 255")
            .build_without_hint()
    }

    pub fn element_name_is_already_used(location: Location, element_type: &str, initial_definition_location: Location) -> Self {
        ExceptionBuilder::new_type_error(location, format!("A {} already exists with this name", element_type))
            .build(format!("Initially defined at {}", initial_definition_location))
    }

    pub fn unknown_element(location: Location, element_type: &str, reference: &Reference) -> Self {
        ExceptionBuilder::new_type_error(location, format!("Unknown {}: {}", element_type, reference))
            .build_without_hint()
    }

    pub fn inaccessible_element(location: Location, element_type: &str, reference: &Reference) -> Self {
        ExceptionBuilder::new_type_error(location, format!("{} {} is inaccessible from here", element_type, reference))
            .build(format!("If possible, try making {} public using the {} keyword.", reference, PUBLIC_KEYWORD))
    }

    pub fn negative_offset(location: Location) -> Self {
        ExceptionBuilder::new(location, "Offset cannot be negative")
            .build("This is because the pointer is not allowed to ever go to the left of its initial position.")
    }

    pub fn increment_non_char(location: Location) -> Self {
        ExceptionBuilder::new_type_error(location, "Cannot increment non-char variable")
            .build_without_hint()
    }

    pub fn decrement_non_char(location: Location) -> Self {
        ExceptionBuilder::new_type_error(location, "Cannot decrement non-char variable")
            .build_without_hint()
    }

    pub fn wrong_arity(location: Location, reference: &Reference, expected_arity: usize, found_arity: usize) -> Self {
        let expected_arity_text = if expected_arity < 2 {
            format!("{} argument", expected_arity)
        } else {
            format!("{} arguments", expected_arity)
        };
        ExceptionBuilder::new_type_error(location, format!("Subroutine {} accepts {} arguments, but found {}", reference, expected_arity_text, found_arity))
            .build_without_hint()
    }

    pub fn expected_constant_value(location: Location, found_type: &Type) -> Self {
        ExceptionBuilder::new_expected_found(location, "constant", found_type)
            .with_type(ExceptionType::ConstantEvaluation)
            .build("The only types that can be used as a constant value are: bool, char, <integer> and tuples.")
    }

    pub fn expected_constant_integer(location: Location, found_type: &Type) -> Self {
        ExceptionBuilder::new_expected_found(location, "constant integer", found_type)
            .with_type(ExceptionType::ConstantEvaluation)
            .build_without_hint()
    }

    pub fn does_not_fit_in_char(location: Location, value: i32) -> Self {
        ExceptionBuilder::new_type_error(location, format!("Value is too large to fit un `char`: {}", value))
            .build(format!("The value of a `char` must be an integer between {} and {} (both included).", u8::MIN, u8::MAX))
    }

    pub fn wrong_type(location: Location, expected_type: &Type, found_type: &Type) -> Self {
        ExceptionBuilder::new_expected_found(location, expected_type, found_type)
            .build_without_hint()
    }

    pub fn unexpected_return(location: Location, returned_type: &Type) -> Self {
        ExceptionBuilder::new_type_error(location, "Unexpected `return` in procedure")
            .build(format!("Try changing the procedure to a function returning {}.", returned_type))
    }

    pub fn invalid_operator(location: Location, operator: Symbol, operand_types: &[Type]) -> Self {
        let formatted_operand_types = operand_types.iter()
            .enumerate()
            .fold(String::new(), |accumulator, (i, operand)| {
                if i == 0 {
                    operand.to_string()
                } else {
                    accumulator + ", " + &operand.to_string()
                }
            });
        ExceptionBuilder::new_type_error(location, format!(
            "`{}` is not defined for ({})",
            operator,
            formatted_operand_types,
        ))
            .build_without_hint()
    }

    pub fn not_a_function(location: Location, reference: &Reference) -> Self {
        ExceptionBuilder::new_type_error(location, format!("Subroutine {} is not a function", reference))
            .build(format!("A function is a subroutine that has a return value. {} does not return anything.", reference))
    }

    pub fn expected_string_like(location: Location, found_type: &Type) -> Self {
        ExceptionBuilder::new_expected_found(location, "string-like", found_type)
            .build(format!("String-like values are of type {}, or arrays of string-like values.", Type::CHAR))
    }

    pub fn unpack_non_tuple(location: Location, value_type: &Type) -> Self {
        ExceptionBuilder::new_type_error(location, format!("Unable to unpack value of type {}", value_type))
            .build("You can only unpack tuples.")
    }

    pub fn invalid_unpack_size(location: Location, expected_size: usize, found_size: usize) -> Self {
        ExceptionBuilder::new_type_error(location, "Wrong number of unpacked values")
            .build(format!("There are {} values to unpack, but you unpacked {}.", expected_size, found_size))
    }

    pub fn identifier_appears_multiple_times_in_target(location: Location, identifier: String, initial_location: Location) -> Self {
        ExceptionBuilder::new(location, format!("Identifier {} appears multiple times in target", identifier))
            .build(format!("The identifier initially appeared at {}", initial_location))
    }

    /// Prints a formatted error message to standard error, highlighting the relevant part of the
    /// input.
    pub fn print_with_input(&self, input: &str) {
        eprintln!("{}", self);
        self.location.span().highlight(input);
        if let Some(hint) = &self.hint {
            eprintln!("Hint: {}", hint)
        }
    }
}

impl Display for CompilationException {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}: {}", self.location, self.r#type, self.message)
    }
}


/// The result of an operation in the compilation process.
///
/// This is a [`Result`] whose error type is a [`CompilationException`].
pub type CompilationResult<T> = Result<T, CompilationException>;
