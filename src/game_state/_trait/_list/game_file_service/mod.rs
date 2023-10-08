use anyhow::Error as AnyError;

/// The `GameFileService` trait.
pub trait GameFileService {
  /// Create the local game file directory.
  fn create_game_file_directory(&mut self) -> Result<(), AnyError>;
}
