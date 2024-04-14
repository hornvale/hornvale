use hecs::{Entity, World};
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

/// A qualifier restricting an object.
pub trait ObjectQualifier {
  /// Get the name of the qualifier.
  fn name(&self) -> &str;
  /// Check if the object is qualified.
  fn is_qualified(&self, world: &World, entity: Entity) -> bool;
}

impl Debug for dyn ObjectQualifier {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.name())
  }
}

impl Display for dyn ObjectQualifier {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{:?}", self)
  }
}
