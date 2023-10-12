use crate::entity_uuid::IntoBaseUuidTrait;

/// Trait for types that are convertible into an EntityUuid.
pub trait IntoEntityUuid: IntoBaseUuidTrait {}
