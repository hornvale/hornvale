use crate::prelude::CommandArgument;
use serde::{Deserialize, Serialize};

/// A parameter for a command.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Hash, Serialize)]
pub enum CommandParameter {
  /// A direction.
  Direction,
  /// An entity.
  Entity,
  /// A string literal.
  StringLiteral,
  /// A raw string.
  RawString,
}

impl From<CommandArgument> for CommandParameter {
  fn from(argument: CommandArgument) -> Self {
    match argument {
      CommandArgument::Direction(_) => CommandParameter::Direction,
      CommandArgument::Entity(_) => CommandParameter::Entity,
      CommandArgument::StringLiteral(_) => CommandParameter::StringLiteral,
      CommandArgument::RawString(_) => CommandParameter::RawString,
      CommandArgument::Entities(_) => CommandParameter::Entity,
    }
  }
}
