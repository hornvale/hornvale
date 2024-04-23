use crate::prelude::{Command, CommandArity, CommandError, CommandModifier};
use hecs::{Entity, World};

/// A command that always fails.
#[derive(Clone, Copy, Debug)]
pub struct FailCommand;

impl Command for FailCommand {
  const NAME: &'static str = "fail";
  const SYNONYMS: &'static [&'static str] = &[];
  const BRIEF: &'static str = "A command that always fails.";
  const DESCRIPTION: &'static str = "A command that always fails; useful for testing.";
  const ARITY: CommandArity = CommandArity::Nullary;
  const DIRECT_OBJECT_MODIFIER: Option<CommandModifier> = None;
  const INDIRECT_OBJECT_MODIFIER: Option<CommandModifier> = None;

  fn execute(
    _world: &mut World,
    _actor: Entity,
    _direct_object: Option<Entity>,
    _indirect_object: Option<Entity>,
  ) -> Result<(), CommandError> {
    Err(CommandError::UnknownError)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::prelude::CommandError;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_execute() {
    init();
    let mut world = World::new();
    let entity = world.spawn(());
    let result = FailCommand::execute(&mut world, entity, None, None);
    assert_eq!(result, Err(CommandError::UnknownError));
  }
}
