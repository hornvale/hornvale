use specs::prelude::*;

/// The `IsStartableChunk` component.
#[derive(Clone, Component, Debug)]
pub struct IsStartableChunk {
  /// The `Chunk`'s starting room ID.
  pub room_id: Entity,
}
