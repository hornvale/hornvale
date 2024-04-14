use super::element::SyntaxElement;
use std::fmt::{Display, Formatter, Result as FmtResult};

/// A pattern for syntax.
#[derive(Debug)]
pub struct SyntaxPattern {
  /// The elements of the pattern.
  pub elements: Vec<SyntaxElement>,
}

impl SyntaxPattern {
  /// Print usage information for the pattern.
  pub fn usage(&self, command_name: &str) -> String {
    let mut result = String::new();
    for element in &self.elements {
      match element {
        SyntaxElement::Command => result.push_str(&format!("{} ", command_name)),
        _ => result.push_str(&format!("{} ", element)),
      }
    }
    result
  }
}

impl Display for SyntaxPattern {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    let mut first = true;
    for element in &self.elements {
      if first {
        first = false;
      } else {
        write!(f, ", ")?;
      }
      write!(f, "{}", element)?;
    }
    Ok(())
  }
}
