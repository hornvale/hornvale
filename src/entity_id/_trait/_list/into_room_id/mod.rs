use crate::entity_id::IntoBaseIdTrait;

/// Trait for types that are convertible into a RoomId.
pub trait IntoRoomId: IntoBaseIdTrait {}
