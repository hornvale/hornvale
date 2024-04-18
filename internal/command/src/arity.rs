use crate::prelude::CommandArgument;
use serde::{Deserialize, Serialize};

/// The "arity" of a command; how many arguments it takes.
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum CommandArity {
  /// A command that takes no arguments.
  Nullary,
  /// A command that takes one argument.
  Unary(CommandArgument),
  /// A command that takes two arguments (a direct and an indirect object).
  Binary(CommandArgument, CommandArgument),
}
