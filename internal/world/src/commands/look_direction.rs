use hecs::World;
use hornvale_command::prelude::*;

/// Attempt to look in a given direction.
#[derive(Clone, Copy, Debug)]
pub struct LookDirectionCommand;

impl Command for LookDirectionCommand {
  const NAME: &'static str = "look";
  const SYNONYMS: &'static [&'static str] = &["look"];
  const BRIEF: &'static str = "Attempt to look in a given direction.";
  const DESCRIPTION: &'static str = r#"
    The actor will attempt to move in the direction specified by the first argument; if they are unable to move in
    that direction, they will be informed why.
  "#;
  const FORM: CommandForm = CommandForm::Direction;
  const ARITY: CommandArity = CommandArity::Unary(CommandParameter::Direction);

  fn execute(_world: &mut World, _context: &CommandContext) -> Result<(), CommandError> {
    unimplemented!()
  }
}
