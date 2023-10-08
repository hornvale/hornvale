use anyhow::Error as AnyError;

use crate::chunk_world::ChunkWorld;

/// The `ChunkWorldCreatorService` service.
#[derive(Clone, Debug, Default)]
pub struct ChunkWorldCreatorService {}

impl ChunkWorldCreatorService {
  /// Creates a new `ChunkWorld`.
  pub fn create_chunk_world(&self) -> Result<ChunkWorld, AnyError> {
    let chunk_world = ChunkWorld::default();
    Ok(chunk_world)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_create_chunk_world() -> Result<(), AnyError> {
    let chunk_world_creator_service = ChunkWorldCreatorService::default();
    let _chunk_world = chunk_world_creator_service.create_chunk_world()?;
    Ok(())
  }
}
