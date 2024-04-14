use crate::prelude::ObjectQualifier;
use hecs::{Entity, World};
use std::fmt::{Debug, Formatter, Result as FmtResult};

/// Any of the qualifiers is sufficient.
pub struct AnyQualifier {
  qualifiers: Vec<Box<dyn ObjectQualifier>>,
}

impl ObjectQualifier for AnyQualifier {
  fn name(&self) -> &str {
    "any"
  }
  fn is_qualified(&self, world: &World, entity: Entity) -> bool {
    self
      .qualifiers
      .iter()
      .any(|qualifier| qualifier.is_qualified(world, entity))
  }
}

impl Debug for AnyQualifier {
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
