use crate::core::prelude::*;

impl From<Region> for Vector4D {
  fn from(region: Region) -> Self {
    let Region { w, x, y, z } = region;
    Vector4D { w, x, y, z }
  }
}
