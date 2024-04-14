use crate::object::qualifier::ObjectQualifier;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

/// An element of a syntax pattern.
pub enum SyntaxElement {
  /// Any verb matching the command.
  AnyVerb,
  /// Any command from a list.
  VerbFromList(Vec<String>),
  /// Any string.
  AnyString,
  /// Any string from a list.
  StringFromList(Vec<String>),
  /// A direct object element.
  DirectObject(Box<dyn ObjectQualifier>),
  /// An indirect object element.
  IndirectObject(Box<dyn ObjectQualifier>),
}

impl Debug for SyntaxElement {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    match self {
      SyntaxElement::AnyVerb => write!(f, "command"),
      SyntaxElement::VerbFromList(verbs) => {
        write!(f, "verb from list: {:?}", verbs)
      },
      SyntaxElement::AnyString => write!(f, "any string"),
      SyntaxElement::StringFromList(pattern) => {
        write!(f, "string from list: {:?}", pattern)
      },
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
      SyntaxElement::AnyVerb => write!(f, "any verb"),
      SyntaxElement::VerbFromList(verbs) => {
        write!(f, "verb matching: {}", verbs.join(", "))
      },
      SyntaxElement::AnyString => write!(f, "any string"),
      SyntaxElement::StringFromList(pattern) => {
        write!(f, "string matching: {}", pattern.join(", "))
      },
      SyntaxElement::DirectObject(qualifier) => {
        write!(f, "direct object: {}", qualifier)
      },
      SyntaxElement::IndirectObject(qualifier) => {
        write!(f, "indirect object: {}", qualifier)
      },
    }
  }
}
