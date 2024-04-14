use crate::prelude::ObjectQualifier;
use hecs::{Entity, World};
use std::fmt::{Debug, Formatter, Result as FmtResult};

/// None of the qualifiers are allowed.
pub struct NoneQualifier {
  qualifiers: Vec<Box<dyn ObjectQualifier>>,
}

impl ObjectQualifier for NoneQualifier {
  fn name(&self) -> &str {
    "none"
  }
  fn is_qualified(&self, world: &World, entity: Entity) -> bool {
    !self
      .qualifiers
      .iter()
      .any(|qualifier| qualifier.is_qualified(world, entity))
  }
}

impl Debug for NoneQualifier {
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
