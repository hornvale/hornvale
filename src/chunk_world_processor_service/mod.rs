use anyhow::Error as AnyError;

use crate::chunk_world::ChunkWorld;
use crate::chunk_world::ChunkWorldProcessor;

/// The `ChunkWorldProcessorService` service.
///
/// This service processes a `ChunkWorld` in various ways, such as:
/// - generating a `ChunkPlane` if one does not already exist.
///
/// These operations should be idempotent.
#[derive(Clone, Debug, Default)]
pub struct ChunkWorldProcessorService {
  /// The seed string.
  pub seed_string: String,
  /// The processors.
  pub processors: Vec<ChunkWorldProcessor>,
}

impl ChunkWorldProcessorService {
  /// Creates a new `ChunkWorldProcessorService`.
  pub fn new(seed_string: &str, processors: Vec<ChunkWorldProcessor>) -> Self {
    Self {
      seed_string: seed_string.to_string(),
      processors,
    }
  }

  /// Processes the `ChunkWorld`.
  pub fn process(&self, chunk_world: &mut ChunkWorld) -> Result<(), AnyError> {
    for processor in &self.processors {
      processor.process(chunk_world)?;
    }
    Ok(())
  }
}
