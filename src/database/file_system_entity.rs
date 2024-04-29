use std::path::{Path, PathBuf};

/// Defines how a file system entity is read, written, and deleted.
pub trait DatabaseFileSystemEntity {
  /// Defines the base directory for the type of entity.
  fn directory(base_path: &Path) -> PathBuf;
  /// Defines the filename for an instance.
  fn filename(&self) -> String;
}
