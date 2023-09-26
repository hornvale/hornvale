pub mod _trait;
pub use _trait::*;

/// The `GameState` struct.
///
/// This is an object holding the state of the game.
#[derive(Debug, Default)]
pub struct GameState {}

impl GameStateTrait for GameState {
  /// Creates a new `GameState`.
  fn new() -> Self {
    Self {}
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
