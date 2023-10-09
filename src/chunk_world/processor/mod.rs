use anyhow::Error as AnyError;

use crate::chunk_world::ChunkWorld;

pub mod processor_type;
pub use processor_type::ChunkWorldProcessorType;

/// The `ChunkWorldProcessor` struct.
///
/// This struct processes a `ChunkWorld` in various ways, such as:
/// - generating a `ChunkPlane` if one does not already exist.
/// - setting the primary `ChunkPlaneId` if one does not already exist.
/// - saving the `ChunkWorld` to disk.
///
/// These operations should be idempotent.
#[derive(Clone, Debug, Default)]
pub struct ChunkWorldProcessor {
  /// The seed string.
  pub seed_string: String,
  /// The type of this processor.
  pub processor_type: ChunkWorldProcessorType,
}

impl ChunkWorldProcessor {
  /// Creates a new `ChunkWorldProcessor`.
  pub fn new(seed_string: &str, processor_type: ChunkWorldProcessorType) -> Self {
    Self {
      seed_string: seed_string.to_string(),
      processor_type,
    }
  }

  /// Processes the `ChunkWorld`.
  pub fn process(&self, chunk_world: &mut ChunkWorld) -> Result<(), AnyError> {
    self.processor_type.process(chunk_world)?;
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;
  use crate::test::TEMPORARY_TEST_DATA_DIRECTORY;

  #[test]
  fn test_new_chunk_world_processor() {
    init();
    let seed_string = "test_new_chunk_world_processor";
    let processor_type = ChunkWorldProcessorType::NoOp;
    let chunk_world_processor = ChunkWorldProcessor::new(seed_string, processor_type);
    assert_eq!(chunk_world_processor.seed_string, seed_string);
    assert_eq!(chunk_world_processor.processor_type, processor_type);
  }

  #[test]
  fn test_process() -> Result<(), AnyError> {
    init();
    let seed_string = "test_process";
    let processor_type = ChunkWorldProcessorType::NoOp;
    let chunk_world_processor = ChunkWorldProcessor::new(seed_string, processor_type);
    let mut chunk_world = ChunkWorld::default();
    chunk_world_processor.process(&mut chunk_world)?;
    Ok(())
  }
}
