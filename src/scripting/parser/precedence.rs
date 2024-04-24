use derive_more::Display;
use serde::{Deserialize, Serialize};

/// The `Precedence` enum.
///
/// This allows controlling the parser flow by setting the precedence of the
/// current operator.
#[derive(Clone, Copy, Debug, Display, Deserialize, Eq, Hash, PartialEq, PartialOrd, Serialize)]
pub enum Precedence {
  /// No precedence.
  None,
  /// Assignment operator.
  Assignment, // =
  /// Logical OR operator.
  Or, // ||
  /// Logical AND operator.
  And, // &&
  /// Equality/inequality operators.
  Equality, // == !=
  /// Comparison operators.
  Comparison, // < > <= >=
  /// Addition/subtraction operators.
  Term, // + -
  /// Multiplication/division operators.
  Factor, // * /
  /// Unary operators.
  Unary, // ! -
  /// Function call operators.
  Call, // . ()
  /// Primary operators.
  Primary,
}

impl Precedence {
  /// Get the next precedence.
  pub fn next(&self) -> Precedence {
    use Precedence::*;
    match self {
      None => Assignment,
      Assignment => Or,
      Or => And,
      And => Equality,
      Equality => Comparison,
      Comparison => Term,
      Term => Factor,
      Factor => Unary,
      Unary => Call,
      Call => Primary,
      Primary => None,
    }
  }
}

#[cfg(test)]
pub mod test {

  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  pub fn test_precedence() {
    init();
    let mut precedence = Precedence::Assignment;
    while precedence != Precedence::None {
      precedence = precedence.next();
    }
  }
}
