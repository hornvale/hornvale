use crate::command::prelude::*;
use crate::database::prelude::*;
use anyhow::Error as AnyError;
use hecs::Entity;

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
    _database: &mut Database,
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
    let mut database = Database::default();
    let entity = database.world.spawn(());
    let result = NoOpCommand::execute(&mut database, entity, None, None);
    assert!(result.is_ok());
  }
}
