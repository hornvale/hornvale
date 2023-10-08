use anyhow::Context;
use anyhow::Error as AnyError;
use std::fs::create_dir_all;
use std::fs::remove_dir_all;

/// The `GameFileService` service.
///
/// This service is responsible for loading and saving game files.
///
/// At present, we're going to destroy and recreate the game file directory on
/// every launch.
#[derive(Debug, Default)]
pub struct GameFileService {
  /// The base directory.
  base_dir: String,
}

impl GameFileService {
  /// Creates a new `GameFileService`.
  pub fn new(base_dir: &str) -> Self {
    Self {
      base_dir: base_dir.to_string(),
    }
  }

  /// Creates the game file directory.
  pub fn create_game_file_directory(&self) -> Result<(), AnyError> {
    remove_dir_all(&self.base_dir).ok();
    create_dir_all(&self.base_dir)
      .with_context(|| format!("Failed to create game file directory: {}", &self.base_dir))?;
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;
  use crate::test::TEMPORARY_TEST_DATA_DIRECTORY;

  #[test]
  fn test_create_game_file_dir() {
    init();
    let base_dir = format!("{}/{}", TEMPORARY_TEST_DATA_DIRECTORY, "test_game_file_dir");
    let game_file_service = GameFileService::new(&base_dir);
    assert!(game_file_service.create_game_file_directory().is_ok());
  }
}
