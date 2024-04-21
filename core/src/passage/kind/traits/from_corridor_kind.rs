use crate::prelude::*;

impl From<CorridorKind> for PassageKind {
  fn from(corridor_kind: CorridorKind) -> Self {
    PassageKind::Corridor(corridor_kind)
  }
}
