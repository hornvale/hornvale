use crate::core::prelude::Region;
use crate::core::prelude::RegionGenerator;
use crate::core::prelude::WorldError;
use hecs::World;
use serde::{Deserialize, Serialize};

/// The Empty Region Generator generates an empty region.
///
/// This generator does not generate any entities within the region, but is
/// otherwise coherent. It is useful for testing and debugging purposes.
#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct EmptyRegionGenerator;

impl RegionGenerator for EmptyRegionGenerator {
  fn generate(&self, _region: Region, _world: &mut World) -> Result<(), WorldError> {
    Ok(())
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
    let generator = EmptyRegionGenerator;
    assert!(generator.generate(Region::default(), &mut world).is_ok());
  }
}
