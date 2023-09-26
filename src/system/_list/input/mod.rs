use std::io::{stdin, stdout, Write};

use crate::game_state::GameState;
use crate::game_state::OutputQueueTrait;
use crate::game_state::QuitFlagTrait;
use crate::system::SystemTrait;

/// The `Input` struct.
///
/// This system handles input from the user.
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
    print!("> ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    input.trim().to_string()
  }
}

impl SystemTrait<GameState> for Input {
  /// Runs the `Input`.
  fn run(&self, game_state: &mut GameState) {
    let input = self.read_input();
    // Process Input
    if input == "quit" {
      game_state.set_quit_flag(true);
    } else {
      game_state.enqueue_output(format!("You entered: {}", input));
    }
  }
}
