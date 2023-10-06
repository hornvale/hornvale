use anyhow::Context;
use anyhow::Error as AnyError;
use std::fs::create_dir_all;
use std::fs::remove_dir_all;

use crate::game_state::FileManagerTrait;
use crate::game_state::GameState;

/// Implementation of the `FileManager` trait.
impl FileManagerTrait for GameState {
  /// Clear the directory.
  fn clear_directory(&mut self, path: &str) -> Result<(), AnyError> {
    remove_dir_all(path).with_context(|| format!("Failed to remove directory: {}", &path))?;
    create_dir_all(path).with_context(|| format!("Failed to create directory: {}", &path))?;
    Ok(())
  }
}
