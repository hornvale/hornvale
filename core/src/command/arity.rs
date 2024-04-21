use serde::{Deserialize, Serialize};
use strum::Display;

/// The arity, or the number of arguments taken by a command.
#[derive(Clone, Copy, Debug, Default, Deserialize, Display, Eq, PartialEq, Serialize)]
pub enum CommandArity {
  /// A command with no arguments.
  #[default]
  Nullary,
  /// A command with one argument (a direct object).
  Unary,
  /// A command with two arguments (a direct and an indirect object).
  Binary,
}
