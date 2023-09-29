use crate::command::Command;
use crate::command::CommandType;
use crate::game_state::CommandQueueTrait;
use crate::game_state::GameState;
use crate::game_state::InputQueueTrait;
use crate::system::SystemTrait;

/// The `Parser` struct.
///
/// This system parses input and enqueues commands.
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
  fn run(&self, game_state: &mut GameState) {
    debug!("Running parser system.");
    while let Some(input) = game_state.dequeue_input() {
      let command: Command = match input.as_str() {
        "quit" => Command::new(CommandType::QuitGame),
        "" => Command::new(CommandType::NoOp),
        _ => Command::new(CommandType::NoOp),
      };
      game_state.enqueue_command(command);
    }
  }
}
