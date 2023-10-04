use crate::entity_id::IntoBaseIdTrait;

/// Trait for types that are convertible into a ChunkId.
pub trait IntoChunkSeedId: IntoBaseIdTrait {}
