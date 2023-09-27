use crate::game_state::GameState;
use crate::game_state::InputQueueTrait;

impl InputQueueTrait for GameState {
  /// Enqueue a string for output.
  fn enqueue_input(&mut self, input: String) {
    self.input_queue.push_back(input);
  }

  /// Dequeue a string for output.
  fn dequeue_input(&mut self) -> Option<String> {
    self.input_queue.pop_front()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;

  #[test]
  fn test_enqueue_input() {
    init();
    let mut game_state = GameState::new();
    game_state.enqueue_input("Hello, world!".to_string());
    assert_eq!(game_state.input_queue.len(), 1);
  }

  #[test]
  fn test_dequeue_input() {
    init();
    let mut game_state = GameState::new();
    game_state.enqueue_input("Hello, world!".to_string());
    assert_eq!(game_state.input_queue.len(), 1);
    let input = game_state.dequeue_input();
    assert_eq!(input, Some("Hello, world!".to_string()));
    assert_eq!(game_state.input_queue.len(), 0);
  }
}
