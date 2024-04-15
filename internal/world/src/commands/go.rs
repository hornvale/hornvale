use hecs::World;
use hornvale_command::prelude::{Command, CommandContext, CommandError, SyntaxElement, SyntaxPattern};

/// A command that attempts to move the player in a given direction.
#[derive(Clone, Copy, Debug)]
pub struct GoCommand;

impl Command for GoCommand {
  fn name(&self) -> &str {
    "go"
  }
  fn aliases(&self) -> Vec<&str> {
    vec![
      "move",
      "walk",
      "north",
      "n",
      "northeast",
      "ne",
      "east",
      "e",
      "southeast",
      "se",
      "south",
      "s",
      "southwest",
      "sw",
      "west",
      "w",
      "northwest",
      "nw",
      "up",
      "down",
      "in",
      "out",
    ]
  }
  fn description(&self) -> &str {
    "A command that always fails."
  }
  fn syntax_patterns(&self) -> Vec<SyntaxPattern> {
    vec![
      SyntaxPattern {
        elements: vec![SyntaxElement::VerbFromList(vec![
          "north".to_string(),
          "n".to_string(),
          "northeast".to_string(),
          "ne".to_string(),
          "east".to_string(),
          "e".to_string(),
          "southeast".to_string(),
          "se".to_string(),
          "south".to_string(),
          "s".to_string(),
          "southwest".to_string(),
          "sw".to_string(),
          "west".to_string(),
          "w".to_string(),
          "northwest".to_string(),
          "nw".to_string(),
          "up".to_string(),
          "down".to_string(),
          "in".to_string(),
          "out".to_string(),
        ])],
      },
      SyntaxPattern {
        elements: vec![SyntaxElement::VerbFromList(vec![
          "move".to_string(),
          "walk".to_string(),
        ])],
      },
    ]
  }
  fn execute(&self, _world: &World, _context: &CommandContext) -> Result<(), CommandError> {
    Err(CommandError::UnknownError)
  }
}
