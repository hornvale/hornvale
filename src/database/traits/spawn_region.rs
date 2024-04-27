use crate::database::prelude::*;
use anyhow::Error as AnyError;

/// Spawn a region.
pub trait SpawnRegion {
  /// Spawn a region.
  fn spawn_region(
    &mut self,
    point: RegionPoint,
    region_spawner: Box<dyn RegionSpawner>,
    database: &mut Database,
  ) -> Result<RegionEntity, AnyError>;
}
