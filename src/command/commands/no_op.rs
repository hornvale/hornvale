use crate::core::prelude::*;
use anyhow::Error as AnyError;
use hecs::{Entity, World};

/// A command that does nothing at all.
#[derive(Clone, Copy, Debug)]
pub struct NoOpCommand;

impl Command for NoOpCommand {
  const NAME: &'static str = "no-op";
  const SYNONYMS: &'static [&'static str] = &[];
  const BRIEF: &'static str = "A command that always succeeds but does nothing.";
  const DESCRIPTION: &'static str = "A command that always succeeds but does nothing; useful for testing.";
  const ARITY: CommandArity = CommandArity::Nullary;
  const DIRECT_OBJECT_MODIFIER: Option<CommandModifier> = None;
  const INDIRECT_OBJECT_MODIFIER: Option<CommandModifier> = None;

  /// Do nothing.
  fn execute(
    _world: &mut World,
    _actor: Entity,
    _direct_object: Option<Entity>,
    _indirect_object: Option<Entity>,
  ) -> Result<(), AnyError> {
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_execute() {
    init();
    let mut world = World::new();
    let entity = world.spawn(());
    let result = NoOpCommand::execute(&mut world, entity, None, None);
    assert!(result.is_ok());
  }
}
