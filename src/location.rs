use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::exceptions::CompilationResult;

/// Represents a conceptual file.
#[derive(Clone, Eq, PartialEq, Debug)]
enum File {
    /// Unknown file.
    Unknown,
    /// Comment line arguments.
    Args,
    /// Language primitive.
    Primitive,
    /// Standard library.
    StandardLibrary,
    /// Input file.
    Path(Rc<PathBuf>),
}

impl File {
    #[inline]
    fn new(path: impl AsRef<Path>) -> Self {
        Self::Path(Rc::new(path.as_ref().to_path_buf()))
    }
}

impl Display for File {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Unknown => write!(f, "<unknown>"),
            Self::Args => write!(f, "<args>"),
            Self::Primitive => write!(f, "<primitive>"),
            Self::StandardLibrary => write!(f, "<stdlib>"),
            Self::Path(name) => write!(f, "{}", name.display()),
        }
    }
}


#[derive(Copy, Clone)]
pub struct Span {
    /// The (included) zero-indexed line of the start of this span.
    start_line: usize,
    /// The (included) zero-indexed column of the start of this span.
    start_column: usize,
    /// The number of lines this span extends to. That is, one less than the total number of lines
    /// this span covers.
    height: usize,
    /// The (included) zero-indexed column of the end of this span.
    ///
    /// If `height == 0`, then `end_column >= start_column`.
    end_column: usize,
}

impl Span {
    const START: Self = Self {
        start_line: 0,
        start_column: 0,
        height: 0,
        end_column: 0,
    };

    /// Creates a new span that starts at a specific position and covers `width + 1` columns.
    #[inline]
    pub fn new(line: usize, column: usize, width: usize) -> Self {
        Self {
            start_line: line,
            start_column: column,
            height: 0,
            end_column: column + width,
        }
    }

    /// Creates a new span that starts at a specific position and covers a single column.
    #[inline]
    pub fn new_at(line: usize, column: usize) -> Self {
        Self::new(line, column, 0)
    }

    /// Creates a new span that starts and ends on the first column of the line after the first line
    /// of this span.
    #[inline]
    pub fn next_line(self) -> Self {
        Self::new_at(self.start_line + 1, 0)
    }

    /// Creates a new span that starts and ends on the column right after the first column of this
    /// span.
    #[inline]
    pub fn next_column(self) -> Self {
        Self::new_at(self.start_line, self.start_column + 1)
    }

    /// Creates a new span that starts at the start of this span, but covers `width + 1` columns.
    #[inline]
    pub fn with_length(self, width: usize) -> Self {
        Self::new(self.start_line, self.start_column, width)
    }

    /// Creates a new span that starts at the start of this span, but covers `amount` more columns
    /// than this span.
    #[inline]
    pub fn extended_by(self, amount: usize) -> Self {
        Self {
            start_line: self.start_line,
            start_column: self.start_column,
            height: self.height,
            end_column: self.end_column + amount,
        }
    }

    /// Creates a new span that starts at the start of this span, and ends at the end of the passed
    /// span.
    ///
    /// Panics if `end` ends before the start of this span.
    #[inline]
    pub fn extended_to(self, end: Self) -> Self {
        let height = end.start_line + end.height - self.start_line;
        assert!(height != 0 || end.end_column >= self.start_column);
        Self {
            start_line: self.start_line,
            start_column: self.start_column,
            height,
            end_column: end.end_column,
        }
    }

    /// Creates a new span that starts and ends at the start of this span.
    #[inline]
    pub fn collapse(self) -> Self {
        Self::new_at(self.start_line, self.start_column)
    }

    /// Creates a new span that starts and ends on the column right after the end of this span.
    #[inline]
    pub fn after(self) -> Self {
        Self::new_at(self.start_line + self.height, self.end_column + 1)
    }

    /// Prints the span corresponding to this position in the input to standard error.
    pub fn highlight(&self, input: &str) {
        let total_height = self.height + 1;
        let mut lines = input.lines()
            .skip(self.start_line)
            .take(total_height)
            .collect::<Vec<_>>();
        lines.resize(total_height, "");
        for (i, line) in lines.into_iter().enumerate() {
            eprintln!("{}", line);
            if i == 0 && i == self.height {
                eprintln!("{}{}",
                          " ".repeat(self.start_column),
                          "^".repeat(self.end_column - self.start_column + 1),
                )
            } else if i == 0 {
                eprintln!("{}{}",
                          " ".repeat(self.start_column),
                          "^".repeat(line.len() - self.start_column),
                )
            } else if i == self.height {
                eprintln!("{}", "^".repeat(self.end_column))
            } else {
                eprintln!("{}", "^".repeat(line.len()))
            }
        }
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}:{}-{}:{}",
               self.start_line + 1,
               self.start_column + 1,
               self.start_line + self.height + 1,
               self.end_column + 1
        )
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.start_line + 1, self.start_column + 1)
    }
}


/// A location in a file.
///
/// A `Location` is very cheap to clone.
#[derive(Clone)]
pub struct Location {
    file: File,
    span: Span,
}

impl Location {
    /// An unknown location.
    pub const UNKNOWN: Self = Self {
        file: File::Unknown,
        span: Span::START,
    };

    /// The location for all things that come from a command line argument.
    pub const ARGS: Self = Self {
        file: File::Args,
        span: Span::START,
    };

    /// The location for all language primitives.
    pub const PRIMITIVE: Self = Self {
        file: File::Primitive,
        span: Span::START,
    };

    /// The location for all things that come from the standard library.
    pub const STDLIB: Self = Self {
        file: File::StandardLibrary,
        span: Span::START,
    };

    /// Creates a new location that is at the start of a file.
    #[inline]
    pub fn start_of_file(file_path: impl AsRef<Path>) -> Self {
        Self {
            file: File::new(file_path),
            span: Span::START,
        }
    }

    #[inline]
    pub fn new(file_path: PathBuf, line: usize, column: usize, length: usize) -> Self {
        Self {
            file: File::Path(Rc::new(file_path)),
            span: Span::new(line, column, length),
        }
    }

    /// Creates a new location that is at the line after this location.
    ///
    /// The new location is created with a length of 1.
    #[inline]
    pub fn next_line(&self) -> Self {
        Self {
            file: self.file.clone(),
            span: self.span.next_line(),
        }
    }

    /// Creates a new location that is at the column right after the start of this location.
    ///
    /// The new location is created with a length of 1.
    #[inline]
    pub fn next_column(&self) -> Self {
        Self {
            file: self.file.clone(),
            span: self.span.next_column(),
        }
    }

    /// Creates a new location by changing the length of this location.
    #[inline]
    pub fn with_length(&self, length: usize) -> Self {
        Self {
            file: self.file.clone(),
            span: self.span.with_length(length),
        }
    }

    /// Creates a new location by extending this one by the specified amount.
    #[inline]
    pub fn extended_by(&self, amount: usize) -> Self {
        Self {
            file: self.file.clone(),
            span: self.span.extended_by(amount),
        }
    }

    /// Creates a new location by extending this one to the end of the passed location.
    #[inline]
    pub fn extended_to(&self, location: &Location) -> Self {
        assert_eq!(location.file, self.file);
        Self {
            file: self.file.clone(),
            span: self.span.extended_to(location.span),
        }
    }

    /// Creates a location that is at the start of this location.
    #[inline]
    pub fn collapse(&self) -> Self {
        Self {
            file: self.file.clone(),
            span: self.span.collapse(),
        }
    }

    /// Creates a new location that is right after this location.
    #[inline]
    pub fn after(&self) -> Self {
        Self {
            file: self.file.clone(),
            span: self.span.after(),
        }
    }

    /// Returns the span associated with this location.
    #[inline]
    pub fn span(&self) -> Span {
        self.span
    }

    /// Boxes a value and locates it.
    #[inline]
    pub fn attach<T>(self, value: T) -> Located<Box<T>> {
        Located::new_boxed(self, value)
    }
}

impl Debug for Location {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Location[{:?} @ {:?}]", self.file, self.span)
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.file, self.span)
    }
}


/// Attaches a [`Location`] to a value.
#[derive(Clone)]
pub struct Located<T> {
    pub location: Location,
    pub value: T,
}

impl<T> Located<T> {
    /// Constructs a new located value.
    #[inline]
    pub fn new(location: Location, value: T) -> Self {
        Self { location, value }
    }

    /// Constructs a new located value with an [unknown location][`Location::UNKNOWN`].
    #[inline]
    pub fn new_unknown(value: T) -> Self {
        Self::new(Location::UNKNOWN, value)
    }

    /// Returns an owned location.
    #[inline]
    pub fn location(&self) -> Location {
        self.location.to_owned()
    }

    /// Applies a function to this value and returns the result with the same location.
    #[inline]
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Located<U> {
        Located::new(self.location, f(self.value))
    }

    /// Tries to apply a function to this value.
    ///
    /// If the function returns an error, it is propagated. Otherwise, the result is returned, with
    /// the same location.
    #[inline]
    pub fn try_map<R, E>(self, f: impl FnOnce(T) -> Result<R, E>) -> Result<Located<R>, E> {
        let mapped_value = f(self.value)?;
        Ok(Located::new(self.location, mapped_value))
    }

    /// Returns a located box containing this value (location is unchanged).
    #[inline]
    pub fn boxed(self) -> Located<Box<T>> {
        self.map(Box::new)
    }

    /// Converts a `Located<T>` to a `Located<U>` using `<U as From<T>>::from`.
    #[inline]
    pub fn into<U: From<T>>(self) -> Located<U> {
        self.map(U::from)
    }

    /// Returns a located reference to this value.
    #[inline]
    pub fn as_ref(&self) -> Located<&T> {
        Located::new(self.location(), &self.value)
    }
}

impl<T> Located<Box<T>> {
    /// Constructs a new located [`Box<T>`].
    #[inline]
    pub fn new_boxed(location: Location, value: T) -> Self {
        Self::new(location, Box::new(value))
    }

    /// Returns the unboxed value.
    #[inline]
    pub fn unbox(self) -> T {
        *self.value
    }

    /// Converts this located box to a located value.
    #[inline]
    pub fn unboxed(self) -> Located<T> {
        Located::new(self.location, *self.value)
    }

    /// Returns a new located reference to the value inside this box.
    #[inline]
    pub fn box_as_ref(&self) -> Located<&T> {
        Located::new(self.location(), &self.value)
    }
}

impl<T: Eq> PartialEq for Located<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<T: Eq> Eq for Located<T> {}

impl<T: Hash> Hash for Located<T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state)
    }
}

impl<T: Debug> Debug for Located<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{}:{:#?}", self.location, self.value)
        } else {
            write!(f, "{}:{:?}", self.location, self.value)
        }
    }
}

impl<T: Display> Display for Located<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}


/// A [`CompilationResult`] whose [`Ok`] variant contains a [`Located`] value.
pub type LocatedResult<T> = CompilationResult<Located<T>>;


/// A [`Vec`] of [`Located`] elements.
pub type Sequence<T> = Vec<Located<T>>;
