use crate::game_state::GameState;
use crate::system::SystemTrait;

/// The `Command` struct.
///
/// This system translates parsed input into commands.
#[derive(Debug, Default)]
pub struct Command {}

impl Command {
  /// Creates a new `Command`.
  pub fn new() -> Self {
    Self {}
  }
}

impl SystemTrait<GameState> for Command {
  /// Runs the `Command`.
  fn run(&self, _game_state: &mut GameState) {}
}
