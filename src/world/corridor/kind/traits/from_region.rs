use crate::world::prelude::*;

impl From<Region> for CorridorKind {
  fn from(region: Region) -> Self {
    if region.z > 0 {
      CorridorKind::Ascend(region)
    } else {
      CorridorKind::Default(region)
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_from() {
    init();
    assert_eq!(
      CorridorKind::from(Region::from((0, 0, 0))),
      CorridorKind::Default(Region::from((0, 0, 0)))
    );
    assert_eq!(
      CorridorKind::from(Region::from((0, 0, 1))),
      CorridorKind::Ascend(Region::from((0, 0, 1)))
    );
  }
}
