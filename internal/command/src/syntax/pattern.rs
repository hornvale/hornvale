use super::element::SyntaxElement;
use std::fmt::{Display, Formatter, Result as FmtResult};

/// A pattern for syntax.
#[derive(Debug)]
pub struct SyntaxPattern {
  /// The elements of the pattern.
  pub elements: Vec<SyntaxElement>,
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
