use crate::prelude::SyntaxPattern;
use crate::prelude::{CommandContext, CommandError};
use hecs::World;
use std::fmt::{Debug, Formatter, Result as FmtResult};

/// The context of the command.
pub mod context;

/// A command that can be executed.
pub trait Command {
  /// Get the name of the command.
  fn name(&self) -> &str;
  /// Get the aliases of the command.
  fn aliases(&self) -> Vec<&str> {
    vec![]
  }
  /// Execute the command.
  fn execute(&self, world: &World, context: &CommandContext) -> Result<(), CommandError>;
  /// Get the syntax patterns of the command.
  fn syntax_patterns(&self) -> Vec<SyntaxPattern>;
  /// Get the description of the command for online help.
  fn description(&self) -> &str;
  /// Get the usage of the command for online help.
  fn usage(&self) -> String {
    let pattern_lines = self
      .syntax_patterns()
      .iter()
      .map(|pattern| pattern.usage(self.name()))
      .collect::<Vec<_>>();
    let aliases = match self.aliases().len() {
      len if len > 0 => format!(" (aliases: {})", self.aliases().join(", ")),
      _ => "".to_string(),
    };
    format!("Usage:{}\n\t{}", aliases, pattern_lines.join("\n\t"))
  }
}

impl Debug for dyn Command {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.name())
  }
}
