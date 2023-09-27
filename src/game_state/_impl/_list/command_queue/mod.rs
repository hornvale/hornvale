use crate::command::CommandTrait;
use crate::game_state::CommandQueueTrait;
use crate::game_state::GameState;

impl CommandQueueTrait for GameState {
  /// Enqueue a command.
  fn enqueue_command(&mut self, command: Box<dyn CommandTrait<GameState>>) {
    self.command_queue.push_back(command);
  }

  /// Dequeue a command.
  fn dequeue_command(&mut self) -> Option<Box<dyn CommandTrait<GameState>>> {
    self.command_queue.pop_front()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::command::NoOpCommand;
  use crate::test::init;

  #[test]
  fn test_enqueue_command() {
    init();
    let mut game_state = GameState::new();
    let command = Box::new(NoOpCommand::new());
    game_state.enqueue_command(command);
    assert_eq!(game_state.command_queue.len(), 1);
  }

  #[test]
  fn test_dequeue_command() {
    init();
    let mut game_state = GameState::new();
    let command = Box::new(NoOpCommand::new());
    game_state.enqueue_command(command);
    let _command = game_state.dequeue_command();
  }
}
