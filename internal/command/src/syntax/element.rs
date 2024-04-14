use crate::object::qualifier::ObjectQualifier;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

/// An element of a syntax pattern.
pub enum SyntaxElement {
  /// Any verb matching the command.
  Command,
  /// A direct object element.
  DirectObject(Box<dyn ObjectQualifier>),
  /// An indirect object element.
  IndirectObject(Box<dyn ObjectQualifier>),
}

impl Debug for SyntaxElement {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    match self {
      SyntaxElement::Command => write!(f, "command"),
      SyntaxElement::DirectObject(qualifier) => {
        write!(f, "direct object: {:?}", qualifier)
      },
      SyntaxElement::IndirectObject(qualifier) => {
        write!(f, "indirect object: {:?}", qualifier)
      },
    }
  }
}

impl Display for SyntaxElement {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    match self {
      SyntaxElement::Command => write!(f, "command"),
      SyntaxElement::DirectObject(qualifier) => {
        write!(f, "direct object: {}", qualifier)
      },
      SyntaxElement::IndirectObject(qualifier) => {
        write!(f, "indirect object: {}", qualifier)
      },
    }
  }
}
