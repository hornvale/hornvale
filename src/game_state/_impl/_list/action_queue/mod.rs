use crate::action::ActionTrait;
use crate::game_state::ActionQueueTrait;
use crate::game_state::GameState;

impl ActionQueueTrait for GameState {
  /// Enqueue an action.
  fn enqueue_action(&mut self, action: Box<dyn ActionTrait<GameState>>) {
    self.action_queue.push_back(action);
  }

  /// Dequeue an action.
  fn dequeue_action(&mut self) -> Option<Box<dyn ActionTrait<GameState>>> {
    self.action_queue.pop_front()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::action::NoOpAction;
  use crate::test::init;

  #[test]
  fn test_enqueue_action() {
    init();
    let mut game_state = GameState::new();
    let action = Box::new(NoOpAction::new());
    game_state.enqueue_action(action);
    assert_eq!(game_state.action_queue.len(), 1);
  }

  #[test]
  fn test_dequeue_command() {
    init();
    let mut game_state = GameState::new();
    let action = Box::new(NoOpAction::new());
    game_state.enqueue_action(action);
    let _action = game_state.dequeue_action();
  }
}
