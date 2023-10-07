use anyhow::Error as AnyError;

/// The `FileManager` trait.
pub trait FileManager {
  /// Clear the directory.
  fn clear_directory(&mut self, path: &str) -> Result<(), AnyError>;
}
