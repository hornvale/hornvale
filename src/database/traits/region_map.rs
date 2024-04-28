use crate::database::prelude::*;
use crate::region::prelude::*;
use anyhow::Error as AnyError;
use std::path::PathBuf;

/// Trait to define methods for dealing with the region map:
/// - Load a region map from a file.
/// - Save a region map to a file.
/// - Get a region from the region map.
pub trait RegionMapExt {
  /// Get the path to the region map file.
  fn get_region_map_path(&self) -> PathBuf;

  /// Load a region map from a file.
  fn load_region_map(&mut self) -> Result<RegionMap, AnyError>;

  /// Save a region map to a file.
  fn save_region_map(&self, region_map: &RegionMap) -> Result<(), AnyError>;

  /// Unload the region map.
  fn unload_region_map(&mut self) -> Result<(), AnyError>;
}

impl RegionMapExt for Database {
  /// Get the path to the region map file.
  fn get_region_map_path(&self) -> PathBuf {
    self.data_dir().join("region_map.json").to_path_buf()
  }

  /// Load a region map from a file.
  fn load_region_map(&mut self) -> Result<RegionMap, AnyError> {
    let path = self.get_region_map_path();
    let region_map = serde_json::from_str(&self.get_file_contents(&path)?)?;
    Ok(region_map)
  }

  /// Save a region map to a file.
  fn save_region_map(&self, region_map: &RegionMap) -> Result<(), AnyError> {
    let path = self.get_region_map_path();
    self.write_file_contents(&path, &serde_json::to_string(region_map)?)?;
    Ok(())
  }

  /// Unload the region map.
  fn unload_region_map(&mut self) -> Result<(), AnyError> {
    if let Some(region_map) = self.region_map.take() {
      self.save_region_map(&region_map)?;
    }
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;
  use tempfile::tempdir;

  #[test]
  fn test_load_region_map() {
    init();
    let path = tempdir().unwrap();
    let mut database = Database::at_path(&path.path());
    let region_map = RegionMap::default();
    database.save_region_map(&region_map).unwrap();
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
  fn test_unload_region_map() {
    init();
    let path = tempdir().unwrap();
    let mut database = Database::at_path(&path.path());
    let region_map = RegionMap::default();
    database.save_region_map(&region_map).unwrap();
    let loaded_region_map = database.load_region_map().unwrap();
    assert_eq!(loaded_region_map, region_map);
    database.unload_region_map().unwrap();
    let loaded_region_map = database.load_region_map().unwrap();
    assert_eq!(loaded_region_map, region_map);
  }
}
