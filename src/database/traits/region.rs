use crate::database::prelude_internal::*;
use crate::region::prelude::*;
use anyhow::Error as AnyError;
use std::path::PathBuf;

/// Trait to define methods for dealing with individual regions:
/// - Load a region from a file.
/// - Unload the region.
/// - Save the region to a file.
/// - Delete the region.
pub trait RegionExt {
  /// Get the path to the region file.
  fn get_region_file_path(&self, region_identifier: &RegionIdentifier) -> PathBuf;

  /// Get the path to the region directory.
  fn get_region_dir_path(&self, region_identifier: &RegionIdentifier) -> PathBuf;

  /// Load a region from a file.
  fn load_region(&mut self, region_identifier: &RegionIdentifier) -> Result<Region, AnyError>;

  /// Unload the region.
  fn unload_region(&mut self, region_identifier: &RegionIdentifier) -> Result<(), AnyError>;

  /// Save a region to a file.
  fn save_region(&self, region_identifier: &RegionIdentifier, region: &Region) -> Result<(), AnyError>;

  /// Delete the region map file.
  fn delete_region(&mut self, region_identifier: &RegionIdentifier) -> Result<(), AnyError>;
}

impl RegionExt for Database {
  /// Get the path to the region file.
  fn get_region_file_path(&self, region_identifier: &RegionIdentifier) -> PathBuf {
    self
      .data_dir()
      .join("regions")
      .join(region_identifier.to_string())
      .with_extension("json")
  }

  /// Get the path to the region directory.
  fn get_region_dir_path(&self, region_identifier: &RegionIdentifier) -> PathBuf {
    self.data_dir().join("regions").join(region_identifier.to_string())
  }

  /// Load a region from a file.
  fn load_region(&mut self, region_identifier: &RegionIdentifier) -> Result<Region, AnyError> {
    let path = self.get_region_file_path(region_identifier);
    let region = serde_json::from_str(&self.read_file_contents(&path)?)?;
    Ok(region)
  }

  /// Unload the region.
  fn unload_region(&mut self, region_identifier: &RegionIdentifier) -> Result<(), AnyError> {
    if let Some(region_adjacency_maps) = &self.region_adjacency_maps {
      if let Some(region) = region_adjacency_maps.get(region_identifier) {
        self.save_region(region_identifier, region)?;
      }
    }
    Ok(())
  }

  /// Save a region to a file.
  fn save_region(&self, region_identifier: &RegionIdentifier, region: &Region) -> Result<(), AnyError> {
    let path = self.get_region_file_path(region_identifier);
    self.write_file_contents(&path, &serde_json::to_string(region)?)?;
    Ok(())
  }

  /// Delete the region map file.
  fn delete_region(&mut self, region_identifier: &RegionIdentifier) -> Result<(), AnyError> {
    self.unload_region(region_identifier)?;
    let path = self.get_region_dir_path(region_identifier);
    self.delete_file(&path)?;
    Ok(())
  }
}
