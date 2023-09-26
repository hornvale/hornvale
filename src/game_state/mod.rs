use std::collections::VecDeque;

pub mod _impl;
pub use _impl::*;
pub mod _trait;
pub use _trait::*;

/// The `GameState` struct.
///
/// This is an object holding the state of the game.
#[derive(Debug, Default)]
pub struct GameState {
  /// Flags.
  pub quit_flag: bool,
  pub input_ready_flag: bool,
  /// Output queue.
  pub output: VecDeque<String>,
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

  #[test]
  fn test_new() {
    let _game_state = GameState::new();
  }
}
