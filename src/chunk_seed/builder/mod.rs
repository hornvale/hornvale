use crate::chunk_seed::ChunkSeed;
use crate::chunk_seed::ChunkSeedType;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::entity_id::ChunkSeedId;

/// The `ChunkSeedBuilder` type.
#[derive(Clone, Debug, Default)]
pub struct Builder {
  /// The `ChunkSeed`'s ID.
  pub id: Option<ChunkSeedId>,
  /// The `ChunkSeed`'s `ChunkPlane`'s ID.
  pub chunk_plane_id: Option<ChunkPlaneId>,
  /// The `ChunkSeed`'s `Chunk`'s ID.
  pub chunk_id: Option<ChunkId>,
  /// The coordinates in (i64, i64) plane.
  pub coordinates: Option<(i64, i64)>,
  /// The `ChunkSeed`'s type.
  pub r#type: Option<ChunkSeedType>,
}

impl Builder {
  /// Creates a new `ChunkSeedBuilder`.
  pub fn new() -> Self {
    Self::default()
  }

  /// Sets the `ChunkSeed`'s ID.
  pub fn id(mut self, id: &ChunkSeedId) -> Self {
    self.id = Some(id.clone());
    self
  }

  /// Sets the `ChunkSeed`'s `ChunkPlane`'s ID.
  pub fn chunk_plane_id(mut self, chunk_plane_id: &ChunkPlaneId) -> Self {
    self.chunk_plane_id = Some(chunk_plane_id.clone());
    self
  }

  /// Sets the `ChunkSeed`'s `Chunk`'s ID.
  pub fn chunk_id(mut self, chunk_id: &ChunkId) -> Self {
    self.chunk_id = Some(chunk_id.clone());
    self
  }

  /// Sets the coordinates in (i64, i64) plane.
  pub fn coordinates(mut self, coordinates: (i64, i64)) -> Self {
    self.coordinates = Some(coordinates);
    self
  }

  /// Sets the `ChunkSeed`'s type.
  pub fn r#type(mut self, r#type: &ChunkSeedType) -> Self {
    self.r#type = Some(*r#type);
    self
  }

  /// Builds the `ChunkSeed`.
  pub fn build(self) -> ChunkSeed {
    let id = self.id.unwrap_or_default();
    let chunk_plane_id = self.chunk_plane_id.unwrap_or_default();
    let chunk_id = self.chunk_id;
    let coordinates = self.coordinates.unwrap_or((0, 0));
    let r#type = self.r#type.unwrap_or_default();
    let mut result = ChunkSeed::new(id, &chunk_plane_id, coordinates, r#type);
    result.chunk_id = chunk_id;
    result
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;

  #[test]
  fn test_chunk_seed_builder() {
    init();
    let chunk_seed = Builder::new().build();
    assert_eq!(chunk_seed.id.to_string().is_empty(), false);
    assert_eq!(chunk_seed.chunk_plane_id.to_string().is_empty(), false);
    assert_eq!(chunk_seed.chunk_id.is_none(), true);
    assert_eq!(chunk_seed.coordinates, (0, 0));
    assert_eq!(chunk_seed.r#type.to_string().is_empty(), false);
  }
}
