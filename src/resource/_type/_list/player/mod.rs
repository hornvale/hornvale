use specs::prelude::*;

/// The `Player` resource.
#[derive(Debug, Default)]
pub struct Player {
  pub entity: Option<Entity>,
}
