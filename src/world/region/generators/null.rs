use crate::database::prelude::*;
use crate::world::prelude::Region;
use crate::world::prelude::RegionGenerator;
use crate::world::prelude::WorldError;
use serde::{Deserialize, Serialize};

/// The Null Region Generator generates absolutely nothing.
///
/// This generator does not generate any entities at all, and is useful for
/// testing and debugging purposes.
#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct NullRegionGenerator;

impl RegionGenerator for NullRegionGenerator {
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
    let generator = NullRegionGenerator;
    assert!(generator.generate(Region::default(), &mut database).is_ok());
  }
}
