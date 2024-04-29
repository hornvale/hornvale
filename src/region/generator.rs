use crate::database::prelude::*;
use crate::region::prelude::*;
use anyhow::Error as AnyError;
use std::fmt::{Debug, Formatter, Result as FmtResult};

/// The Region Generator trait defines the interface for region generators.
pub trait RegionGenerator {
  /// Get the name of the region generator.
  fn name(&self) -> &str;
  /// Generate a region within the world.
  fn generate(&self, region_point: &RegionPoint, database: &mut Database) -> Result<RegionIdentifier, AnyError>;
}

impl Debug for dyn RegionGenerator {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    f.debug_struct("RegionGenerator").field("name", &self.name()).finish()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[derive(Default)]
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
  fn test_name() {
    init();
    let generator = Box::new(TestRegionGenerator::default());
    assert_eq!(generator.name(), "test");
  }

  #[test]
  fn test_generate() {
    init();
    let generator = Box::new(TestRegionGenerator::default());
    let region_point = RegionPoint::default();
    let mut database = Database::default();
    let region_identifier = generator.generate(&region_point, &mut database).unwrap();
    assert_eq!(region_identifier, RegionIdentifier::default());
  }
}
