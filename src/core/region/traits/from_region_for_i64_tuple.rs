use crate::core::prelude::*;

impl From<Region> for (i64, i64, i64) {
  fn from(region: Region) -> Self {
    (region.x, region.y, region.z)
  }
}

impl From<Region> for (i64, i64, i64, i64) {
  fn from(region: Region) -> Self {
    (region.w, region.x, region.y, region.z)
  }
}
