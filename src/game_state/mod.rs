use std::collections::VecDeque;

pub mod _trait;
pub use _trait::*;

/// The `GameState` struct.
///
/// This is an object holding the state of the game.
#[derive(Debug, Default)]
pub struct GameState {
  /// Flags.
  pub quit_flag: bool,
  /// Output queue.
  pub output: VecDeque<String>,
}

impl GameState {
  /// Creates a new `GameState`.
  pub fn new() -> Self {
    Self::default()
  }
}

impl GameStateTrait for GameState {
  /// Enqueue a string for output.
  fn enqueue_output(&mut self, output: String) {
    self.output.push_back(output);
  }

  /// Dequeue a string for output.
  fn dequeue_output(&mut self) -> Option<String> {
    self.output.pop_front()
  }

  /// Get quit-game flag.
  fn get_quit_flag(&self) -> bool {
    self.quit_flag
  }

  /// Set quit-game flag.
  fn set_quit_flag(&mut self, quit_flag: bool) {
    self.quit_flag = quit_flag;
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    let _game_state = GameState::new();
  }

  #[test]
  fn test_enqueue_output() {
    let mut game_state = GameState::new();
    game_state.enqueue_output("Hello, world!".to_string());
    assert_eq!(game_state.output.len(), 1);
  }

  #[test]
  fn test_dequeue_output() {
    let mut game_state = GameState::new();
    game_state.enqueue_output("Hello, world!".to_string());
    assert_eq!(game_state.output.len(), 1);
    let output = game_state.dequeue_output();
    assert_eq!(output, Some("Hello, world!".to_string()));
    assert_eq!(game_state.output.len(), 0);
  }

  #[test]
  fn test_get_quit_flag() {
    let game_state = GameState::new();
    assert_eq!(game_state.get_quit_flag(), false);
  }

  #[test]
  fn test_set_quit_flag() {
    let mut game_state = GameState::new();
    game_state.set_quit_flag(true);
    assert_eq!(game_state.get_quit_flag(), true);
  }
}
