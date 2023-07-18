use std::fmt::Debug;

pub mod extensions;
pub mod with_owned;

/// Used to generate unique values using [`UniquenessProvider::next`].
#[derive(Default, Debug)]
pub struct UniquenessProvider {
    next_value: usize,
}

impl UniquenessProvider {
    /// Creates and initializes a new `UniquenessProvider`.
    pub const fn new() -> Self {
        Self { next_value: usize::MIN }
    }

    /// Returns a new unique `usize`.
    ///
    /// This value was never returned before, and won't ever be returned again by this provider.
    ///
    /// You should not rely on any pattern (other than uniqueness) in the sequence of values
    /// returned by this method.
    pub fn next(&mut self) -> usize {
        let value = self.next_value;
        self.next_value = self.next_value.checked_add(1).unwrap();
        value
    }
}


/// A visibility describes in what context an element can be accessed.
#[derive(Hash, Copy, Clone, Eq, PartialEq, Debug)]
pub enum Visibility {
    /// A public element can be accessed from anywhere.
    Public,
    /// A private element can only be accessed from within the scope it was defined in.
    Private,
}


macro_rules! write_iterator {
    ($destination:expr, $iterator:expr, $separator:expr, $prefix:expr, $suffix:expr) => {
        {
            let destination = $destination;
            let separator = $separator;
            write!(destination, "{}", $prefix)?;
            let mut is_first = true;
            for element in $iterator {
                if is_first {
                    is_first = false
                } else {
                    write!(destination, "{}", separator)?
                }
                write!(destination, "{}", element)?
            }
            write!(destination, "{}", $suffix)
        }
    };
    ($destination:expr, $iterator:expr, $separator:expr) => {
        write_iterator!($destination, $iterator, $separator, "", "")
    };
    ($destination:expr, $iterator:expr) => {
        write_iterator!($destination, $iterator, "", "", "")
    };
}

pub(crate) use {write_iterator};
