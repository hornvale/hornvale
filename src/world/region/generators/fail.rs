use crate::database::prelude::*;
use crate::world::prelude::Region;
use crate::world::prelude::RegionGenerator;
use crate::world::prelude::WorldError;
use serde::{Deserialize, Serialize};

/// The Fail Region Generator fails to generate a region.
///
/// This is useful for testing and debugging purposes.
#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct FailRegionGenerator;

impl RegionGenerator for FailRegionGenerator {
  fn generate(&self, _region: Region, _database: &mut Database) -> Result<(), WorldError> {
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
    let mut database = Database::default();
    let generator = FailRegionGenerator;
    assert!(generator.generate(Region::default(), &mut database).is_err());
  }
}
