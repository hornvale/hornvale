use std::collections::HashMap;
use std::collections::HashSet;

use crate::entity_id::ActorId;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::entity_id::EntityId;
use crate::entity_id::ObjectId;
use crate::entity_id::PlayerId;
use crate::entity_id::RoomId;

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
pub struct LookupService {
  // The following data structures follow this graph, with each containing
  // the IDs of the entities in the next level down:
  //
  // Chunk Planes -> Chunks -> Rooms -> Entities.
  //
  /// ChunkPlaneId -> ChunkIds.
  pub chunk_plane2chunks: HashMap<ChunkPlaneId, HashSet<ChunkId>>,
  /// ChunkId -> ChunkPlaneId.
  pub chunk2chunk_plane: HashMap<ChunkId, ChunkPlaneId>,
  /// ChunkId -> RoomIds.
  pub chunk2rooms: HashMap<ChunkId, HashSet<RoomId>>,
  /// RoomId -> ChunkId.
  pub room2chunk: HashMap<RoomId, ChunkId>,
  /// RoomId -> EntityIds.
  pub room2entities: HashMap<RoomId, HashSet<EntityId>>,
  /// EntityId -> RoomId.
  pub entity2room: HashMap<EntityId, RoomId>,

  // Entities can be one of multiple types:
  // - Actor
  // - Object
  // - ???
  //
  // Consequently, we want to be able to perform lookups by type:
  // - ActorId -> RoomId
  // - ObjectId -> RoomId
  // - ???
  //
  /// RoomId -> ActorIds.
  pub room2actors: HashMap<RoomId, HashSet<ActorId>>,
  /// ActorId -> RoomId.
  pub actor2room: HashMap<ActorId, RoomId>,
  /// RoomId -> ObjectId.
  pub room2objects: HashMap<RoomId, HashSet<ObjectId>>,
  /// ObjectId -> RoomId.
  pub object2room: HashMap<ObjectId, RoomId>,
  /// RoomId -> PlayerId.
  pub room2players: HashMap<RoomId, HashSet<PlayerId>>,
  /// PlayerId -> RoomId.
  pub player2room: HashMap<PlayerId, RoomId>,
}
