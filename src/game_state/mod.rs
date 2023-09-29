use std::collections::VecDeque;

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
#[derive(Derivative, Default)]
#[derivative(Debug)]
pub struct GameState {
  /// Flags.
  pub diegetic_flag: bool,
  pub input_ready_flag: bool,
  pub quit_flag: bool,
  /// Counters.
  pub tick_counter: TickCounter,
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
    Self::default()
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
