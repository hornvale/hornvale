use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::time::Instant;

use crate::command::Command;
use crate::entity_id::PlayerId;
use crate::entity_id::RoomId;
use crate::event::Event;
use crate::room::Room;

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
  /// Flags.
  pub diegetic_flag: bool,
  pub input_ready_flag: bool,
  pub quit_flag: bool,
  /// Counters.
  pub tick_counter: TickCounter,
  /// Times and timers.
  pub loop_timer: Instant,
  /// Entities.
  pub rooms: HashMap<RoomId, Room>,
  pub current_room_id: RoomId,
  pub player_id: PlayerId,
  /// Queues.
  pub input_queue: VecDeque<String>,
  #[derivative(Debug = "ignore")]
  pub command_queue: VecDeque<Command>,
  #[derivative(Debug = "ignore")]
  pub event_queue: BinaryHeap<Event>,
  pub output_queue: VecDeque<String>,
}

impl GameState {
  /// Creates a new `GameState`.
  pub fn new() -> Self {
    Self {
      diegetic_flag: false,
      input_ready_flag: false,
      quit_flag: false,
      tick_counter: 0,
      loop_timer: Instant::now(),
      rooms: HashMap::new(),
      current_room_id: RoomId::default(),
      player_id: PlayerId::default(),
      input_queue: VecDeque::new(),
      command_queue: VecDeque::new(),
      event_queue: BinaryHeap::new(),
      output_queue: VecDeque::new(),
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
