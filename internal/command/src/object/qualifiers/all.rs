use crate::prelude::ObjectQualifier;
use hecs::{Entity, World};
use std::fmt::{Debug, Formatter, Result as FmtResult};

/// All of the qualifiers are required.
pub struct AllQualifier {
  qualifiers: Vec<Box<dyn ObjectQualifier>>,
}

impl ObjectQualifier for AllQualifier {
  fn name(&self) -> &str {
    "all"
  }
  fn is_qualified(&self, world: &World, entity: Entity) -> bool {
    self
      .qualifiers
      .iter()
      .all(|qualifier| qualifier.is_qualified(world, entity))
  }
}

impl Debug for AllQualifier {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{} ({})",
      self.name(),
      self
        .qualifiers
        .iter()
        .map(|qualifier| qualifier.name())
        .collect::<Vec<_>>()
        .join(", ")
    )
  }
}
