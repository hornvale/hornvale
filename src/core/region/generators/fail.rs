use crate::core::prelude::Region;
use crate::core::prelude::RegionGenerator;
use crate::core::prelude::WorldError;
use hecs::World;
use serde::{Deserialize, Serialize};

/// The Fail Region Generator fails to generate a region.
///
/// This is useful for testing and debugging purposes.
#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct FailRegionGenerator;

impl RegionGenerator for FailRegionGenerator {
  fn generate(&self, _region: Region, _world: &mut World) -> Result<(), WorldError> {
    Err(WorldError::UnknownError)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_generate() {
    init();
    let mut world = World::new();
    let generator = FailRegionGenerator;
    assert!(generator.generate(Region::default(), &mut world).is_err());
  }
}