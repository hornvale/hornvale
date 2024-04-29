use crate::database::prelude_internal::*;
use crate::region::prelude::*;
use anyhow::Error as AnyError;
use std::path::PathBuf;

/// Trait to define methods for dealing with individual region adjacency maps:
/// - Load a region adjacency map from a file.
/// - Unload a specific region adjacency map.
/// - Save the region adjacency map to a file.
/// - Delete the region adjacency map.
pub trait RegionAdjacencyMapExt {
  /// Get the path to the region adjacency map file.
  fn get_region_adjacency_map_file_path(&self, region_identifier: &RegionIdentifier) -> PathBuf;

  /// Read a region adjacency map from a file.
  fn read_region_adjacency_map(&self, region_identifier: &RegionIdentifier) -> Result<RegionAdjacencyMap, AnyError>;

  /// Save a region adjacency map to a file.
  fn save_region_adjacency_map(
    &self,
    region_identifier: &RegionIdentifier,
    region_adjacency_map: &RegionAdjacencyMap,
  ) -> Result<(), AnyError>;

  /// Delete the region adjacency map file.
  fn delete_region_adjacency_map(&mut self, region_identifier: &RegionIdentifier) -> Result<(), AnyError>;

  /// Load a region adjacency map.
  fn load_region_adjacency_map(&mut self, region_identifier: &RegionIdentifier)
    -> Result<RegionAdjacencyMap, AnyError>;

  /// Unload the region adjacency map.
  fn unload_region_adjacency_map(&mut self, region_identifier: &RegionIdentifier) -> Result<(), AnyError>;
}

impl RegionAdjacencyMapExt for Database {
  /// Get the path to the region adjacency map file.
  fn get_region_adjacency_map_file_path(&self, region_identifier: &RegionIdentifier) -> PathBuf {
    self.get_region_dir_path(region_identifier).join("adjacency_map.json")
  }

  /// Load a region adjacency map.
  fn load_region_adjacency_map(
    &mut self,
    region_identifier: &RegionIdentifier,
  ) -> Result<RegionAdjacencyMap, AnyError> {
    let path = self.get_region_adjacency_map_file_path(region_identifier);
    let region_adjacency_map = serde_json::from_str(&self.read_file_contents(&path)?)?;
    Ok(region_adjacency_map)
  }

  /// Unload the region adjacency map.
  fn unload_region_adjacency_map(&mut self, region_identifier: &RegionIdentifier) -> Result<(), AnyError> {
    if let Some(region_adjacency_maps) = &self.region_adjacency_maps {
      if let Some(region_adjacency_map) = region_adjacency_maps.get(region_identifier) {
        self.save_region_adjacency_map(region_identifier, region_adjacency_map)?;
      }
      region_adjacency_maps.remove(region_identifier);
    }
    Ok(())
  }

  /// Save a region adjacency map to a file.
  fn save_region_adjacency_map(
    &self,
    region_identifier: &RegionIdentifier,
    region_adjacency_map: &RegionAdjacencyMap,
  ) -> Result<(), AnyError> {
    let path = self.get_region_adjacency_map_file_path(region_identifier);
    let region_adjacency_map = self
      .region_adjacency_maps
      .as_ref()
      .unwrap()
      .get(region_identifier)
      .unwrap();
    self.write_file_contents(&path, &serde_json::to_string(region_adjacency_map)?)?;
    Ok(())
  }

  /// Delete the region adjacency map.
  fn delete_region_adjacency_map(&mut self, region_identifier: &RegionIdentifier) -> Result<(), AnyError> {
    self.unload_region_adjacency_map(region_identifier)?;
    let path = self.get_region_adjacency_map_file_path(region_identifier);
    self.delete_file(&path)?;
    Ok(())
  }
}
