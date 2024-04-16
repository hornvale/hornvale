use crate::prelude::{Command, CommandContext, CommandError, QuitFlag};
use hecs::World;

/// A command that sets the quit flag on the world.
#[derive(Clone, Copy, Debug)]
pub struct QuitCommand;

impl Command for QuitCommand {
  const NAME: &'static str = "quit";
  const SYNONYMS: &'static [&'static str] = &[];
  const BRIEF: &'static str = "Quit the game.";
  const DESCRIPTION: &'static str = "Quits the game.";

  /// Do nothing.
  fn execute(world: &mut World, _context: &CommandContext) -> Result<(), CommandError> {
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
    let result = QuitCommand::execute(&mut world, &Default::default());
    let quit_flag = world.query_mut::<&mut QuitFlag>().into_iter().next().unwrap().1;
    assert_eq!(*quit_flag, QuitFlag);
    assert_eq!(result, Ok(()));
  }
}
