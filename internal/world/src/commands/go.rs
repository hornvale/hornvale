use hecs::World;
use hornvale_command::prelude::{Command, CommandContext, CommandError};

/// Attempt to walk in a given direction.
#[derive(Clone, Copy, Debug)]
pub struct GoCommand;

impl Command for GoCommand {
  const NAME: &'static str = "go";
  const SYNONYMS: &'static [&'static str] = &["go", "move", "walk", "enter", "exit"];
  const BRIEF: &'static str = "Attempt to walk in a given direction.";
  const DESCRIPTION: &'static str = r#"
    Attempt to walk in a given direction. The player will attempt to move in the
    direction specified by the first argument. If the player is unable to move in
    that direction, they will be informed of the reason why.
  "#;
  fn execute(_world: &mut World, _context: &CommandContext) -> Result<(), CommandError> {
    Err(CommandError::UnknownError)
  }
}
