use crate::command::CommandTrait;
use crate::command::NoOpCommand;
use crate::command::QuitCommand;
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
      let command: Box<dyn CommandTrait<GameState>> = match input.as_str() {
        "quit" => Box::new(QuitCommand::new()),
        "" => Box::new(NoOpCommand::new()),
        _ => Box::new(NoOpCommand::new()),
      };
      game_state.enqueue_command(command);
    }
  }
}
