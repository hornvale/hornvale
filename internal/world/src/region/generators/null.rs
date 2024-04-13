use crate::prelude::Region;
use crate::prelude::RegionGenerator;
use crate::prelude::WorldError;
use hecs::World;
use serde::{Deserialize, Serialize};

/// The Null Region Generator generates absolutely nothing.
///
/// This generator does not generate any entities at all, and is useful for
/// testing and debugging purposes.
#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct NullRegionGenerator;

impl RegionGenerator for NullRegionGenerator {
  fn generate(&self, _region: Region, _world: &mut World) -> Result<(), WorldError> {
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_generate() {
    init();
    let mut world = World::new();
    let generator = NullRegionGenerator;
    assert!(generator.generate(Region::default(), &mut world).is_ok());
  }
}
