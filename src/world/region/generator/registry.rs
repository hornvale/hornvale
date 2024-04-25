use super::RegionGenerator;
use crate::world::prelude::Region;
use crate::world::prelude::WorldError;
use hecs::World;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter, Result as FmtResult};

/// The Region Generator Registry is a collection of region generators.
#[derive(Default)]
pub struct RegionGeneratorRegistry {
  /// The collection of region generators.
  pub generators: HashMap<String, Box<dyn RegionGenerator>>,
}

impl RegionGeneratorRegistry {
  /// Create a new region generator registry.
  pub fn new() -> Self {
    Self::default()
  }

  /// Register a region generator with the registry.
  pub fn register(&mut self, name: &str, generator: Box<dyn RegionGenerator>) {
    self.generators.insert(name.to_string(), generator);
  }

  /// Generate a region within the world.
  pub fn generate(&self, name: &str, region: Region, world: &mut World) -> Result<(), WorldError> {
    if let Some(generator) = self.generators.get(name) {
      generator.generate(region, world)
    } else {
      Err(WorldError::UnknownRegionGenerator(name.to_string()))
    }
  }
}

impl Debug for RegionGeneratorRegistry {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    f.debug_struct("RegionGeneratorRegistry")
      .field("generators", &self.generators.keys())
      .finish()
  }
}
