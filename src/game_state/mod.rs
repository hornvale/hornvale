use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::time::Instant;
use uuid::Uuid;

use crate::chunk_creator_service::ChunkCreatorService;
use crate::chunk_file_service::ChunkFileService;
use crate::chunk_loader_service::ChunkLoaderService;
use crate::command::Command;
use crate::entity_id::PlayerId;
use crate::entity_id::RoomId;
use crate::event::Event;
use crate::game_file_service::GameFileService;
use crate::lookup_service::LookupService;
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
  pub seed_string: String,
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
  pub current_room_id: Option<RoomId>,
  pub player_id: PlayerId,
  // Filesystem.
  pub local_data_dir: String,
  // Queues.
  pub input_queue: VecDeque<String>,
  #[derivative(Debug = "ignore")]
  pub command_queue: VecDeque<Command>,
  #[derivative(Debug = "ignore")]
  pub event_queue: BinaryHeap<Event>,
  pub output_queue: VecDeque<String>,
  // Services.
  chunk_creator_service: ChunkCreatorService,
  chunk_file_service: ChunkFileService,
  chunk_loader_service: ChunkLoaderService,
  game_file_service: GameFileService,
  lookup_service: LookupService,
}

impl GameState {
  /// Creates a new `GameState`.
  pub fn new() -> Self {
    let seed_string = Uuid::new_v4().to_string();
    let local_data_dir = LOCAL_DATA_DIR.as_ref().unwrap();
    let game_file_dir = format!("{}/{}", local_data_dir, "game_file");
    let chunk_data_dir = format!("{}/{}", game_file_dir, "chunk_data");
    Self {
      // Seed string.
      seed_string: seed_string.clone(),
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
      current_room_id: None,
      player_id: PlayerId::default(),
      // Filesystem.
      local_data_dir: local_data_dir.to_string(),
      // Queues.
      input_queue: VecDeque::new(),
      command_queue: VecDeque::new(),
      event_queue: BinaryHeap::new(),
      output_queue: VecDeque::new(),
      // Services.
      chunk_creator_service: ChunkCreatorService::new(&chunk_data_dir, &seed_string),
      chunk_file_service: ChunkFileService::new(&chunk_data_dir),
      chunk_loader_service: ChunkLoaderService::default(),
      game_file_service: GameFileService::new(&game_file_dir),
      lookup_service: LookupService::default(),
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
