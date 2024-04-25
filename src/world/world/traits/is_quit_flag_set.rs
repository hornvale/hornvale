use crate::world::prelude::*;
use hecs::World;

/// A trait for checking if the quit flag is set.
pub trait IsQuitFlagSet {
  /// Check to see if the quit flag is set.
  fn is_quit_flag_set(&self) -> bool;
}

/// A trait for checking if the quit flag is set.
impl IsQuitFlagSet for World {
  /// Check to see if the quit flag is set.
  fn is_quit_flag_set(&self) -> bool {
    let mut query = self.query::<&QuitFlag>();
    let query_result = query.iter().find(|(_, _)| true).map(|(_, _)| ());
    query_result.is_some()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_is_quit_flag_set() {
    init();
    let mut world = World::new();
    assert!(!world.is_quit_flag_set());
    world.spawn((QuitFlag,));
    assert!(world.is_quit_flag_set());
  }
}
