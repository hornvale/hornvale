use crate::database::prelude::*;
use crate::world::prelude::Region;
use crate::world::prelude::RegionGenerator;
use crate::world::prelude::WorldError;
use serde::{Deserialize, Serialize};

/// The Empty Region Generator generates an empty region.
///
/// This generator does not generate any entities within the region, but is
/// otherwise coherent. It is useful for testing and debugging purposes.
#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct EmptyRegionGenerator;

impl RegionGenerator for EmptyRegionGenerator {
  fn generate(&self, _region: Region, _database: &mut Database) -> Result<(), WorldError> {
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
    let mut database = Database::default();
    let generator = EmptyRegionGenerator;
    assert!(generator.generate(Region::default(), &mut database).is_ok());
  }
}
