use std::fmt;
use std::fmt::{Display, Formatter};

use crate::location::Located;

/// A reference consists of an identifier, optionally within a namespace, which is itself identified
/// using a reference.
#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct Reference {
    pub identifier: String,
    pub namespace: Option<Located<Box<Reference>>>,
}

impl Reference {
    /// Creates a new reference to an identifier within a namespace.
    pub fn new(identifier: String, namespace: Located<Reference>) -> Self {
        Self {
            identifier,
            namespace: Some(namespace.boxed()),
        }
    }

    /// Creates a new reference that consists of an identifier with no namespace.
    ///
    /// To create a reference from a borrowed string instead, use [`Reference::with_identifier`].
    pub fn new_identifier(identifier: String) -> Self {
        Self {
            identifier,
            namespace: None,
        }
    }

    /// Creates a new reference that consists of an identifier with no namespace.
    ///
    /// To create a reference from an owned [`String`] instead, use [`Reference::new_identifier`].
    pub fn with_identifier(identifier: &str) -> Self {
        Self {
            identifier: identifier.to_owned(),
            namespace: None,
        }
    }
}

impl PartialEq<&str> for Reference {
    /// Tests if a reference consists of a single specific identifier with no namespace.
    fn eq(&self, identifier: &&str) -> bool {
        self.namespace.is_none() && &self.identifier == identifier
    }
}

impl PartialEq<&str> for &Reference {
    /// Tests if a reference consists of a single specific identifier with no namespace.
    fn eq(&self, other: &&str) -> bool {
        self.namespace.is_none() && &self.identifier == other
    }
}

impl Display for Reference {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.namespace {
            None => write!(f, "{}", self.identifier),
            Some(namespace) => write!(f, "{}::{}", namespace, self.identifier),
        }
    }
}
