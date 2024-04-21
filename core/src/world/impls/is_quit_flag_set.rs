use crate::prelude::*;
use hecs::World;

/// A trait for checking if the quit flag is set.
impl IsQuitFlagSet for World {
  /// Check to see if the quit flag is set.
  fn is_quit_flag_set(&self) -> bool {
    let mut query = self.query::<&QuitFlag>();
    let query_result = query.iter().find(|(_, _)| true).map(|(_, _)| ());
    query_result.is_some()
  }
}
