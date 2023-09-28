use crate::entity_id::BaseId;

/// Trait for types that are convertible into a BaseId.
pub trait IntoBaseId {
  /// Converts into a BaseId.
  fn into_base_id(self) -> BaseId;
}
