use std::collections::HashMap;
use uuid::Uuid;

use crate::entity_uuid::BaseUuidWrapperTrait;
use crate::entity_uuid::RoomUuid;
use crate::passage::Passage;
use crate::passage::PassageDirection;
use crate::room::RoomCoordinates;
use crate::room::RoomStatus;

/// The `Room` struct.
#[derive(Builder, Clone, Debug, Deserialize, PartialEq, Serialize)]
#[builder(derive(Debug))]
pub struct Room {
  /// The `Room`'s coordinates within the `Chunk`.
  pub coordinates: RoomCoordinates,
  /// The `Room`'s seed string.
  pub seed_string: String,
  /// The UUID of the room.
  #[builder(default = "RoomUuid::new(Uuid::new_v4().to_string())")]
  pub uuid: RoomUuid,
  /// The `Room`'s status.
  pub status: RoomStatus,
  /// The `Room`'s name.
  pub name: String,
  /// The `Room`'s description.
  pub description: String,
  /// Whether this room is startable.
  pub is_startable: bool,
  /// The `Room`'s passages.
  pub passages: HashMap<PassageDirection, Passage>,
}
