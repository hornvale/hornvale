use serde::{Deserialize, Serialize};
use strum::Display;

/// The "arity" of a command; how many parameters it has.
#[derive(Clone, Copy, Debug, Display, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum CommandArity {
  /// A command that has no parameters.
  Nullary,
  /// A command that has one parameter (a direct object).
  Unary,
  /// A command that has two parameters (a direct and an indirect object).
  Binary,
}
