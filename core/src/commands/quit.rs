use crate::prelude::*;
use anyhow::Error as AnyError;
use hecs::{Entity, World};

/// A command that sets the quit flag on the world.
#[derive(Clone, Copy, Debug)]
pub struct QuitCommand;

impl Command for QuitCommand {
  const NAME: &'static str = "quit";
  const SYNONYMS: &'static [&'static str] = &[];
  const BRIEF: &'static str = "Quit the game.";
  const DESCRIPTION: &'static str = "Quits the game.";
  const ARITY: CommandArity = CommandArity::Nullary;
  const DIRECT_OBJECT_MODIFIER: CommandModifier = CommandModifier::None;
  const INDIRECT_OBJECT_MODIFIER: CommandModifier = CommandModifier::None;

  /// Do nothing.
  fn execute(world: &mut World, _context: &Entity) -> Result<(), AnyError> {
    world.spawn((QuitFlag,));
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_execute() {
    init();
    let mut world = World::new();
    let entity = world.spawn(());
    let result = QuitCommand::execute(&mut world, &entity);
    let quit_flag = world.query_mut::<&mut QuitFlag>().into_iter().next().unwrap().1;
    assert_eq!(*quit_flag, QuitFlag);
    assert!(result.is_ok());
  }
}
