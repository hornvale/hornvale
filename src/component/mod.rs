use anyhow::Error as AnyError;
use specs::prelude::*;

pub mod chunk;
pub use chunk::Chunk as ChunkComponent;
pub mod chunk_plane;
pub use chunk_plane::ChunkPlane as ChunkPlaneComponent;
pub mod chunk_seed;
pub use chunk_seed::ChunkSeed as ChunkSeedComponent;
pub use chunk_seed::ChunkSeedStatus;

/// Register all components.
pub fn register_components(ecs: &mut World) -> Result<(), AnyError> {
  ecs.register::<ChunkComponent>();
  ecs.register::<ChunkPlaneComponent>();
  ecs.register::<ChunkSeedComponent>();
  Ok(())
}
