use crate::core::prelude::*;

impl From<Region> for CorridorKind {
  fn from(region: Region) -> Self {
    if region.z > 0 {
      CorridorKind::Ascend(region)
    } else {
      CorridorKind::Default(region)
    }
  }
}
