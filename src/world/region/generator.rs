use crate::database::prelude::*;
use crate::world::prelude::Region;
use crate::world::prelude::WorldError;

/// The Region Generator Manager manages the generation of regions.
pub mod manager;
/// The Region Generator Registry is a collection of region generators.
pub mod registry;

/// The Region Generator trait defines the interface for region generators.
pub trait RegionGenerator {
  /// Generate a region within the world.
  fn generate(&self, region: Region, database: &mut Database) -> Result<(), WorldError>;
}
