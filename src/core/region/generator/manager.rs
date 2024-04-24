use super::registry::RegionGeneratorRegistry;
use super::RegionGenerator;
use crate::core::prelude::Region;
use crate::core::prelude::WorldError;
use hecs::World;

/// The Region Generator Manager manages the generation of regions.
#[derive(Default, Debug)]
pub struct RegionGeneratorManager {
  /// The Region Generator Registry.
  registry: RegionGeneratorRegistry,
}

impl RegionGeneratorManager {
  /// Create a new region generator manager.
  pub fn new() -> Self {
    Self::default()
  }

  /// Register a region generator with the manager.
  pub fn register(&mut self, name: &str, generator: Box<dyn RegionGenerator>) {
    self.registry.register(name, generator);
  }

  /// Generate a region within the world.
  pub fn generate(&self, name: &str, region: Region, world: &mut World) -> Result<(), WorldError> {
    self.registry.generate(name, region, world)
  }
}
