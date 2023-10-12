use crate::entity_uuid::BaseUuid;

/// Trait for types that are convertible into a BaseUuid.
pub trait IntoBaseUuid {
  /// Converts into a BaseUuid.
  fn into_base_uuid(self) -> BaseUuid;
}
