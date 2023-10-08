use anyhow::Context;
use anyhow::Error as AnyError;
use serde_yaml::from_reader as serde_yaml_from_reader;
use serde_yaml::to_string as serde_yaml_to_string;
use std::fs::create_dir_all;
use std::fs::File;
use std::io::Write;

use crate::chunk::Chunk;
use crate::entity_id::ChunkId;

/// The `ChunkFileManager` struct.
///
/// This struct is responsible for:
/// - loading chunks from disk
/// - saving chunks to disk
#[derive(Clone, Debug, Default)]
pub struct FileManager {
  /// The base path.
  pub base_path: String,
}

impl FileManager {
  /// Creates a new `ChunkFileManager`.
  pub fn new(base_path: &str) -> Self {
    Self {
      base_path: base_path.to_string(),
    }
  }

  /// Open a `Chunk` from disk.
  pub fn open(&self, chunk_id: &ChunkId) -> Result<Chunk, AnyError> {
    let file_path = format!("{}/{}.yaml", self.base_path, chunk_id);
    let file = File::open(file_path.clone()).with_context(|| format!("Unable to read chunk file at {}", file_path))?;
    let chunk: Chunk = serde_yaml_from_reader(file)?;
    Ok(chunk)
  }

  /// Saves the `Chunk` in a serialized form.
  pub fn save(&self, chunk: &Chunk) -> Result<(), AnyError> {
    create_dir_all(&self.base_path).with_context(|| format!("Unable to create directory at {}", self.base_path))?;
    let file_path = format!("{}/{}.yaml", self.base_path, chunk.id);
    let yaml_string = serde_yaml_to_string(chunk)?;
    let mut file =
      File::create(file_path.clone()).with_context(|| format!("Unable to create file at {}", file_path))?;
    file
      .write_all(yaml_string.as_bytes())
      .with_context(|| format!("Unable to write chunk at {}", file_path))?;
    Ok(())
  }

  /// Open multiple `Chunk`s from disk.
  pub fn open_multiple(&self, chunk_ids: &Vec<ChunkId>) -> Result<Vec<Chunk>, AnyError> {
    let mut chunks = Vec::new();
    for chunk_id in chunk_ids {
      let chunk = self.open(chunk_id)?;
      chunks.push(chunk);
    }
    Ok(chunks)
  }

  /// Saves multiple `Chunk`s to disk.
  pub fn save_multiple(&self, chunks: &Vec<Chunk>) -> Result<(), AnyError> {
    for chunk in chunks {
      self.save(chunk)?;
    }
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
  fn test_chunk_file_manager() -> Result<(), AnyError> {
    init();
    let chunk_file_manager = FileManager::new(&format!(
      "{}/{}",
      TEMPORARY_TEST_DATA_DIRECTORY, "test_chunk_file_manager"
    ));
    remove_dir_all(&chunk_file_manager.base_path).ok();
    let mut chunk = Chunk::default();
    chunk.id = ChunkId::default();
    chunk_file_manager.save(&chunk)?;
    let _chunk = chunk_file_manager.open(&chunk.id)?;
    Ok(())
  }
}
