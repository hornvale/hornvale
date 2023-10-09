use anyhow::Error as AnyError;

use crate::chunk_world::ChunkWorld;

/// The `ChunkWorldProcessorType` enum.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub enum ChunkWorldProcessorType {
  /// Do nothing.
  #[default]
  NoOp,
  GeneratePrimaryChunkPlane,
}

impl ChunkWorldProcessorType {
  /// Processes the `ChunkWorld`.
  pub fn process(&self, _chunk_world: &mut ChunkWorld) -> Result<(), AnyError> {
    debug!("ChunkWorldProcessorType::process");
    use ChunkWorldProcessorType::*;
    match self {
      NoOp => {
        debug!("NoOp");
        Ok(())
      },
      GeneratePrimaryChunkPlane => {
        debug!("GeneratePrimaryChunkPlane");
        Ok(())
      },
    }
  }
}
