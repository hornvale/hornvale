use crate::game_state::GameState;
use crate::system::SystemTrait;

/// The `Parser` struct.
///
/// This system parses input.
#[derive(Debug, Default)]
pub struct Parser {}

impl Parser {
  /// Creates a new `Parser`.
  pub fn new() -> Self {
    Self {}
  }
}

impl SystemTrait<GameState> for Parser {
  /// Runs the `Parser`.
  fn run(&self, _game_state: &mut GameState) {}
}
