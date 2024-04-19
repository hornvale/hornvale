use hecs::World;
use hornvale_command::prelude::*;

/// Look at here.
#[derive(Clone, Copy, Debug)]
pub struct LookHereCommand;

impl Command for LookHereCommand {
  const NAME: &'static str = "look";
  const SYNONYMS: &'static [&'static str] = &["look"];
  const BRIEF: &'static str = "Look at your surroundings.";
  const DESCRIPTION: &'static str = r#"
    Look at your surroundings. This command will provide a description of the
    player's current location, including any items or characters that are present.
  "#;
  const FORM: CommandForm = CommandForm::Here;
  const ARITY: CommandArity = CommandArity::Nullary;

  fn execute(_world: &mut World, _context: &CommandContext) -> Result<(), CommandError> {
    unimplemented!()
  }
}
