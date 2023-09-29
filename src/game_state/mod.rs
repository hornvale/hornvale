use std::collections::VecDeque;
use std::time::Instant;

use crate::command::Command;
use crate::event::Event;

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
  /// Times.
  pub loop_timer: Instant,
  /// Queues.
  pub input_queue: VecDeque<String>,
  #[derivative(Debug = "ignore")]
  pub command_queue: VecDeque<Command>,
  #[derivative(Debug = "ignore")]
  pub event_queue: VecDeque<Event>,
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
      input_queue: VecDeque::new(),
      command_queue: VecDeque::new(),
      event_queue: VecDeque::new(),
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
