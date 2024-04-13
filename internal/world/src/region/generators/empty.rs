use crate::prelude::Region;
use crate::prelude::RegionGenerator;
use crate::prelude::WorldError;
use hecs::World;
use serde::{Deserialize, Serialize};

/// The Empty Region Generator generates an empty region.
#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct EmptyRegionGenerator;

impl RegionGenerator for EmptyRegionGenerator {
  fn generate(&self, _region: Region, _world: &mut World) -> Result<(), WorldError> {
    Ok(())
  }
}
