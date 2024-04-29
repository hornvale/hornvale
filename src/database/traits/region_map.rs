use crate::database::prelude_internal::*;
use crate::region::prelude::*;
use anyhow::Error as AnyError;
use std::path::PathBuf;

/// Trait to define methods for dealing with the region map:
/// - Load a region map from a file.
/// - Unload the region map.
/// - Save a region map to a file.
/// - Delete the region map file.
pub trait RegionMapExt {
  /// Get the path to the region map file.
  fn get_region_map_path(&self) -> PathBuf;

  /// Read a region map from a file.
  fn read_region_map(&self) -> Result<RegionMap, AnyError>;

  /// Write a region map to a file.
  fn write_region_map(&self, region_map: &RegionMap) -> Result<(), AnyError>;

  /// Delete the region map file.
  fn delete_region_map(&mut self) -> Result<(), AnyError>;

  /// Load a region map from a file.
  fn load_region_map(&mut self) -> Result<(), AnyError>;

  /// Unload the region map.
  fn unload_region_map(&mut self) -> Result<Option<RegionMap>, AnyError>;
}

impl RegionMapExt for Database {
  /// Get the path to the region map file.
  fn get_region_map_path(&self) -> PathBuf {
    let filename = FilenameFormatter::RegionMap.filename();
    self.data_dir().join(filename)
  }

  /// Read a region map from a file.
  fn read_region_map(&self) -> Result<RegionMap, AnyError> {
    let path = self.get_region_map_path();
    let region_map = serde_json::from_str(&self.read_file_contents(&path)?)?;
    Ok(region_map)
  }

  /// Write a region map to a file.
  fn write_region_map(&self, region_map: &RegionMap) -> Result<(), AnyError> {
    let path = self.get_region_map_path();
    self.write_file_contents(&path, &serde_json::to_string(region_map)?)?;
    Ok(())
  }

  /// Delete the region map file.
  fn delete_region_map(&mut self) -> Result<(), AnyError> {
    let path = self.get_region_map_path();
    self.delete_file(&path)?;
    Ok(())
  }

  /// Load a region map from a file.
  fn load_region_map(&mut self) -> Result<(), AnyError> {
    self.region_map = Some(self.read_region_map()?);
    Ok(())
  }

  /// Unload the region map.
  fn unload_region_map(&mut self) -> Result<Option<RegionMap>, AnyError> {
    Ok(self.region_map.take())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;
  use tempfile::tempdir;

  #[test]
  fn test_read_region_map() {
    init();
    let path = tempdir().unwrap();
    let mut database = Database::at_path(&path.path());
    let region_map = RegionMap::default();
    database.write_region_map(&region_map).unwrap();
    let loaded_region_map = database.read_region_map().unwrap();
    assert_eq!(loaded_region_map, region_map);
  }

  #[test]
  fn test_unload_region_map() {
    init();
    let path = tempdir().unwrap();
    let mut database = Database::at_path(&path.path());
    let region_map = RegionMap::default();
    database.write_region_map(&region_map).unwrap();
    let loaded_region_map = database.load_region_map().unwrap();
    assert_eq!(loaded_region_map, region_map);
    database.unload_region_map().unwrap();
    let loaded_region_map = database.load_region_map().unwrap();
    assert_eq!(loaded_region_map, region_map);
  }

  #[test]
  fn test_save_region_map() {
    init();
    let path = tempdir().unwrap();
    let mut database = Database::at_path(&path.path());
    let region_map = RegionMap::default();
    database.save_region_map(&region_map).unwrap();
    let loaded_region_map = database.load_region_map().unwrap();
    assert_eq!(loaded_region_map, region_map);
  }

  #[test]
  fn test_delete_region_map() {
    init();
    let path = tempdir().unwrap();
    let mut database = Database::at_path(&path.path());
    let region_map = RegionMap::default();
    database.save_region_map(&region_map).unwrap();
    database.delete_region_map().unwrap();
    let result = database.load_region_map();
    assert!(result.is_err());
  }
}
