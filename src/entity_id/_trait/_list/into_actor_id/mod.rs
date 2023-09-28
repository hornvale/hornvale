use crate::entity_id::IntoBaseIdTrait;

/// Trait for types that are convertible into an ActorId.
pub trait IntoActorId: IntoBaseIdTrait {}
