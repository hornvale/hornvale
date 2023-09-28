use crate::game_state::GameState;
use crate::game_state::OutputQueueTrait;
use crate::system::SystemTrait;

/// The `Output` struct.
///
/// This system handles output.
#[derive(Debug, Default)]
pub struct Output {}

impl Output {
  /// Creates a new `Output`.
  pub fn new() -> Self {
    Self {}
  }
}

impl SystemTrait<GameState> for Output {
  /// Runs the `Output`.
  fn run(&self, game_state: &mut GameState) {
    debug!("Running output system.");
    while let Some(output) = game_state.dequeue_output() {
      println!("{}", output);
    }
  }
}
