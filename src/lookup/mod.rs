use std::collections::HashMap;
use std::collections::HashSet;

use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;

pub mod _impl;
pub use _impl::*;
pub mod _trait;
pub use _trait::*;

/// The `Lookup` service.
///
/// This provides a centralized lookup service for all game data.
///
/// Examples:
/// - In what $Room is $Player?
/// - In what $Chunk is $Entity?
/// - What $Actor, if any, is currently carrying $Object?
/// - What $Actors are in $Room?
///
/// This data should be populated by events, not persisted in any way.
///
/// These should use only IDs, not objects.
///
/// The relationships here are generally one-to-many. For example, a Room may
/// contain many Actors, but an Actor may only be in one Room at a time.
///
/// So there are two general questions we can answer with that data:
/// - In what $Room is $Actor?
/// - What $Actors are in $Room?
///
/// But also, we can ask some indirect questions:
/// - What $Actors are in the same $Room as $Actor?
///
/// For the first question, "In what $Room is $Actor?", we can use a HashMap
/// with ActorId as key and RoomId as value.
///
/// For the second question, "What $Actors are in $Room?", we can use a HashMap
/// with RoomId as key and a HashSet of ActorIds as value.
///
/// For the third question, "What $Actors are in the same $Room as $Actor?", we
/// can use the first structure to get the RoomId of $Actor, then use the
/// second structure to get the HashSet of ActorIds in that Room.
///
/// Fields should be named:
/// - $entity2$entity: HashMap<$entity, $entity>,
/// - $entity2$entities: HashMap<$entity, HashSet<$entity>>,
///
/// Traits should be named:
/// - $EntityLookup
///
/// Trait methods should be named:
///   // This is a one-to-one lookup.
/// - get_$entity_of_$entity
///   // This is a one-to-many lookup.
/// - get_$entities_in_$entity
#[derive(Debug, Default)]
pub struct Lookup {
  /// ChunkId -> ChunkPlaneId.
  pub chunk2chunk_plane: HashMap<ChunkId, ChunkPlaneId>,
  /// ChunkPlaneId -> ChunkIds.
  pub chunk_plane2chunks: HashMap<ChunkPlaneId, HashSet<ChunkId>>,
}
