use crate::database::prelude::*;
use crate::region::prelude_internal::*;
use anyhow::Error as AnyError;
use std::collections::HashMap;

/// The Region Generator Registry is a collection of region generators.
#[derive(Debug, Default)]
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
  pub fn register(&mut self, generator: Box<dyn RegionGenerator>) {
    self.generators.insert(generator.name().to_string(), generator);
  }

  /// De-register a region generator from the registry.
  pub fn deregister(&mut self, name: &str) {
    self.generators.remove(name);
  }

  /// Check if a region generator is registered.
  pub fn is_registered(&self, name: &str) -> bool {
    self.generators.contains_key(name)
  }

  /// Generate a region within the world.
  pub fn generate(
    &self,
    name: &str,
    region_point: &RegionPoint,
    database: &mut Database,
  ) -> Result<RegionIdentifier, AnyError> {
    if let Some(generator) = self.generators.get(name) {
      generator.generate(region_point, database)
    } else {
      anyhow::bail!("Unknown region generator: {}", name);
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[derive(Clone, Copy, Debug, Default)]
  struct TestRegionGenerator;

  impl RegionGenerator for TestRegionGenerator {
    fn name(&self) -> &str {
      "test"
    }

    fn generate(&self, _region_point: &RegionPoint, _database: &mut Database) -> Result<RegionIdentifier, AnyError> {
      Ok(RegionIdentifier::default())
    }
  }

  #[test]
  fn test_register() {
    init();
    let mut registry = RegionGeneratorRegistry::new();
    let generator = Box::new(TestRegionGenerator::default());
    registry.register(generator);
    assert_eq!(registry.generators.len(), 1);
  }

  #[test]
  fn test_generate() {
    init();
    let mut registry = RegionGeneratorRegistry::new();
    let generator = Box::new(TestRegionGenerator::default());
    registry.register(generator);
    let region_point = RegionPoint::default();
    let mut database = Database::default();
    let region_identifier = registry.generate("test", &region_point, &mut database).unwrap();
    assert_eq!(region_identifier, RegionIdentifier::default());
  }

  #[test]
  fn test_generate_unknown() {
    init();
    let registry = RegionGeneratorRegistry::new();
    let region_point = RegionPoint::default();
    let mut database = Database::default();
    let result = registry.generate("unknown", &region_point, &mut database);
    assert!(result.is_err());
  }
}
