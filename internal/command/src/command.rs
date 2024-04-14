use crate::prelude::CommandError;
use crate::prelude::{SyntaxElement, SyntaxPattern};
use hecs::World;
use std::fmt::{Debug, Formatter, Result as FmtResult};

/// A command that can be executed.
pub trait Command {
  /// Get the name of the command.
  fn name(&self) -> &str;
  /// Get the aliases of the command.
  fn aliases(&self) -> Vec<&str> {
    vec![self.name()]
  }
  /// Execute the command.
  fn execute(&self, world: &World) -> Result<(), CommandError>;
  /// Get the syntax patterns of the command.
  fn syntax_patterns(&self) -> Vec<SyntaxPattern>;
  /// Get the description of the command for online help.
  fn description(&self) -> &str;
  /// Get the usage of the command for online help.
  fn usage(&self) -> String {
    let pattern_lines = self
      .syntax_patterns()
      .iter()
      .map(|pattern| {
        let mut result = String::new();
        for element in &pattern.elements {
          match element {
            SyntaxElement::Command => result.push_str(&format!("{} ", self.name())),
            _ => result.push_str(&format!("{} ", element)),
          }
        }
        result
      })
      .collect::<Vec<_>>();
    format!("Usage:\n\t{}", pattern_lines.join("\n\t"))
  }
}

impl Debug for dyn Command {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.name())
  }
}
