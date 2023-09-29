use crate::command::Command;
use crate::game_state::CommandQueueTrait;
use crate::game_state::GameState;

impl CommandQueueTrait for GameState {
  /// Enqueue a command.
  fn enqueue_command(&mut self, command: Command) {
    self.command_queue.push_back(command);
  }

  /// Dequeue a command.
  fn dequeue_command(&mut self) -> Option<Command> {
    self.command_queue.pop_front()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::command::CommandType;
  use crate::test::init;

  #[test]
  fn test_enqueue_command() {
    init();
    let mut game_state = GameState::new();
    let command = Command::new(CommandType::NoOp);
    game_state.enqueue_command(command);
    assert_eq!(game_state.command_queue.len(), 1);
  }

  #[test]
  fn test_dequeue_command() {
    init();
    let mut game_state = GameState::new();
    let command = Command::new(CommandType::NoOp);
    game_state.enqueue_command(command);
    game_state.dequeue_command().unwrap();
    assert_eq!(game_state.command_queue.len(), 0);
  }
}
