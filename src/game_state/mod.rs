use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::path::PathBuf;
use std::time::Instant;

use crate::chunk::Chunk;
use crate::command::Command;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::entity_id::EntityId;
use crate::entity_id::PlayerId;
use crate::entity_id::RoomId;
use crate::event::Event;
use crate::room::Room;

pub mod _constant;
pub use _constant::*;
pub mod _impl;
pub use _impl::*;
pub mod _trait;
pub use _trait::*;
pub mod _type;
pub use _type::*;

/// The `GameState` struct.
///
/// This is an object holding the state of the game.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct GameState {
  // Flags.
  pub diegetic_flag: bool,
  pub input_ready_flag: bool,
  pub quit_flag: bool,
  // Counters.
  pub tick_counter: TickCounter,
  // Times and timers.
  pub loop_timer: Instant,
  // Entities.
  pub rooms: HashMap<RoomId, Room>,
  pub current_room_id: RoomId,
  pub player_id: PlayerId,
  // Filesystem.
  pub local_data_dir: PathBuf,
  // Queues.
  pub input_queue: VecDeque<String>,
  #[derivative(Debug = "ignore")]
  pub command_queue: VecDeque<Command>,
  #[derivative(Debug = "ignore")]
  pub event_queue: BinaryHeap<Event>,
  pub output_queue: VecDeque<String>,
  // Chunking system.
  pub loaded_chunks: HashMap<ChunkId, Chunk>,
  // Lookups.
  pub entity_id_to_room_id: HashMap<EntityId, RoomId>,
  pub room_id_to_entity_ids: HashMap<RoomId, Vec<EntityId>>,
  pub chunk_id_to_chunk_plane_id: HashMap<ChunkId, ChunkPlaneId>,
  pub chunk_plane_id_to_chunk_ids: HashMap<ChunkPlaneId, Vec<ChunkId>>,
}

impl GameState {
  /// Creates a new `GameState`.
  pub fn new() -> Self {
    Self {
      // Flags.
      diegetic_flag: false,
      input_ready_flag: false,
      quit_flag: false,
      // Counters.
      tick_counter: 0,
      // Times and timers.
      loop_timer: Instant::now(),
      // Entities.
      rooms: HashMap::new(),
      current_room_id: RoomId::default(),
      player_id: PlayerId::default(),
      // Filesystem.
      local_data_dir: LOCAL_DATA_DIR
        .as_ref()
        .expect("Unable to construct local data directory.")
        .to_path_buf(),
      // Queues.
      input_queue: VecDeque::new(),
      command_queue: VecDeque::new(),
      event_queue: BinaryHeap::new(),
      output_queue: VecDeque::new(),
      // Chunking system.
      loaded_chunks: HashMap::new(),
      // Lookups.
      entity_id_to_room_id: HashMap::new(),
      room_id_to_entity_ids: HashMap::new(),
      chunk_id_to_chunk_plane_id: HashMap::new(),
      chunk_plane_id_to_chunk_ids: HashMap::new(),
    }
  }
}

impl Default for GameState {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;

  #[test]
  fn test_new() {
    init();
    let _game_state = GameState::new();
  }
}
