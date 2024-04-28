use crate::database::prelude::*;
use anyhow::Error as AnyError;
use std::fs;
use std::path::Path;

/// Trait to define methods for dealing with files.
pub trait FileExt {
  /// Get the contents of a file.
  fn get_file_contents(&self, path: &Path) -> Result<String, AnyError>;

  /// Write the contents of a file.
  fn write_file_contents(&self, path: &Path, contents: &str) -> Result<(), AnyError>;
}

impl FileExt for Database {
  fn get_file_contents(&self, path: &Path) -> Result<String, AnyError> {
    Ok(fs::read_to_string(path)?)
  }

  fn write_file_contents(&self, path: &Path, contents: &str) -> Result<(), AnyError> {
    fs::write(path, contents)?;
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;
  use tempfile::tempdir;

  #[test]
  fn test_get_file_contents() {
    init();
    let path = tempdir().unwrap().path().join("test.json");
    let database = Database::at_path(&path.parent().unwrap());
    fs::write(&path, "test").unwrap();
    let contents = database.get_file_contents(&path).unwrap();
    assert_eq!(contents, "test");
    fs::remove_file(&path).unwrap();
  }

  #[test]
  fn test_write_file_contents() {
    init();
    let path = tempdir().unwrap().path().join("test.json");
    let database = Database::at_path(&path.parent().unwrap());
    database.write_file_contents(&path, "test").unwrap();
    let contents = fs::read_to_string(&path).unwrap();
    assert_eq!(contents, "test");
    fs::remove_file(&path).unwrap();
  }
}
