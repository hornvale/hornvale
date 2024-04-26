use crate::world::prelude::*;
use hecs::World;

/// A trait for setting the quit flag.
pub trait SetQuitFlag {
  /// Set the quit flag.
  fn set_quit_flag(&mut self);
}

impl SetQuitFlag for World {
  /// Set the quit flag.
  fn set_quit_flag(&mut self) {
    self.spawn((QuitFlag,));
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::database::Database;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_set_quit_flag() {
    init();
    let mut database = Database::default();
    assert!(!database.world.is_quit_flag_set());
    database.world.set_quit_flag();
    assert!(database.world.is_quit_flag_set());
  }
}
