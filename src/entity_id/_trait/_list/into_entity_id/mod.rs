use crate::entity_id::IntoBaseIdTrait;

/// Trait for types that are convertible into an EntityId.
pub trait IntoEntityId: IntoBaseIdTrait {}
