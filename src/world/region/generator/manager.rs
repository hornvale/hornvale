use super::registry::RegionGeneratorRegistry;
use super::RegionGenerator;
use crate::world::prelude::Region;
use crate::world::prelude::WorldError;
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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;
  use crate::world::prelude::*;

  #[test]
  fn test_register() {
    init();
    let mut manager = RegionGeneratorManager::new();
    let generator = Box::new(CompassRoseRegionGenerator);
    manager.register("compass_rose", generator);
    assert_eq!(manager.registry.generators.len(), 1);
  }

  #[test]
  fn test_generate() {
    init();
    let mut world = World::new();
    let mut manager = RegionGeneratorManager::new();
    let generator = Box::new(CompassRoseRegionGenerator);
    manager.register("compass_rose", generator);
    assert!(manager.generate("compass_rose", Region::default(), &mut world).is_ok());
  }
}
