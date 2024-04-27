use crate::database::prelude::*;
use anyhow::Error as AnyError;

/// A spawner is just a factory that creates Regions.
pub trait RegionSpawner {
  /// Spawn a region.
  fn spawn_region(&self, point: RegionPoint, database: &mut Database) -> Result<RegionEntity, AnyError>;
}
