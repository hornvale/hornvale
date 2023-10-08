use anyhow::Context;
use anyhow::Error as AnyError;
use serde_yaml::from_reader as serde_yaml_from_reader;
use serde_yaml::to_string as serde_yaml_to_string;
use std::fs::create_dir_all;
use std::fs::File;
use std::io::Write;

use crate::chunk_plane::ChunkPlane;
use crate::entity_id::ChunkPlaneId;

/// The `ChunkPlaneFileManager` struct.
///
/// This struct is responsible for:
/// - loading chunk planes from disk
/// - saving chunk planes to disk
#[derive(Clone, Debug, Default)]
pub struct FileManager {
  /// The base path.
  pub base_path: String,
}

impl FileManager {
  /// Creates a new `ChunkPlaneFileManager`.
  pub fn new(base_path: &str) -> Self {
    Self {
      base_path: base_path.to_string(),
    }
  }

  /// Opens a `ChunkPlane` from disk.
  pub fn open(&self, chunk_plane_id: &ChunkPlaneId) -> Result<ChunkPlane, AnyError> {
    let file_path = format!("{}/{}.yaml", self.base_path, chunk_plane_id);
    let file =
      File::open(file_path.clone()).with_context(|| format!("Unable to read chunk plane file at {}", file_path))?;
    let chunk_plane: ChunkPlane = serde_yaml_from_reader(file)?;
    Ok(chunk_plane)
  }

  /// Saves the `ChunkPlane` in a serialized form.
  pub fn save(&self, chunk_plane: &ChunkPlane) -> Result<(), AnyError> {
    create_dir_all(&self.base_path).with_context(|| format!("Unable to create directory at {}", self.base_path))?;
    let file_path = format!("{}/{}.yaml", self.base_path, chunk_plane.id);
    let yaml_string = serde_yaml_to_string(chunk_plane)?;
    let mut file =
      File::create(file_path.clone()).with_context(|| format!("Unable to create file at {}", file_path))?;
    file
      .write_all(yaml_string.as_bytes())
      .with_context(|| format!("Unable to write chunk plane at {}", file_path))?;
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use std::fs::remove_dir_all;

  use crate::test::init;
  use crate::test::TEMPORARY_TEST_DATA_DIRECTORY;

  #[test]
  fn test_chunk_plane_file_manager() -> Result<(), AnyError> {
    init();
    let chunk_plane_file_manager = FileManager::new(&format!("{}/{}", TEMPORARY_TEST_DATA_DIRECTORY, "chunk_planes"));
    remove_dir_all(&chunk_plane_file_manager.base_path).ok();
    let mut chunk_plane = ChunkPlane::default();
    chunk_plane.id = ChunkPlaneId::default();
    chunk_plane.generate_initial_chunks()?;
    chunk_plane_file_manager.save(&chunk_plane)?;
    let _chunk_plane = chunk_plane_file_manager.open(&chunk_plane.id)?;
    Ok(())
  }
}
