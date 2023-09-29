use std::io::{stdin, stdout, Write};

use crate::game_state::GameState;
use crate::game_state::InputQueueTrait;
use crate::system::SystemTrait;

/// The `Input` struct.
///
/// This system reads input from the user and enqueues input.
#[derive(Debug, Default)]
pub struct Input {}

impl Input {
  /// Creates a new `Input`.
  pub fn new() -> Self {
    Self {}
  }
}

impl Input {
  /// Read the input from the user.
  fn read_input(&self) -> String {
    debug!("Running input system.");
    print!("> ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    input.trim().to_string()
  }
}

impl SystemTrait<GameState> for Input {
  /// Runs the `Input`.
  fn run(&mut self, game_state: &mut GameState) {
    let input = self.read_input();
    game_state.enqueue_input(input);
  }
}
