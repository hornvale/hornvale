use crate::prelude::*;
use hecs::World;

/// Check to see if the quit flag is set.
pub fn is_quit_flag_set(world: &World) -> bool {
  let mut query = world.query::<&QuitFlag>();
  let query_result = query.iter().find(|(_, _)| true).map(|(_, _)| ());
  query_result.is_some()
}
