use specs::prelude::*;

use crate::entity_uuid::PlayerUuid;

/// The `Player` resource.
#[derive(Debug, Default)]
pub struct Player {
  pub uuid: PlayerUuid,
  pub entity: Option<Entity>,
}
