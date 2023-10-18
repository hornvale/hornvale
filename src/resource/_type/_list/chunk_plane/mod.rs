use specs::prelude::*;

/// The `ChunkPlane` resource.
#[derive(Debug, Default)]
pub struct ChunkPlane {
  pub entity: Option<Entity>,
}
