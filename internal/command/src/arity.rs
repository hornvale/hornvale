use crate::prelude::CommandParameter;
use serde::{Deserialize, Serialize};

/// The "arity" of a command; how many parameters it has.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum CommandArity {
  /// A command that has no parameters.
  Nullary,
  /// A command that has one parameter.
  Unary(CommandParameter),
  /// A command that has two parameters (a direct and an indirect object).
  Binary(CommandParameter, CommandParameter),
}
