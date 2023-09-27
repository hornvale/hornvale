use crate::game_state::GameState;
use crate::game_state::OutputQueueTrait;

impl OutputQueueTrait for GameState {
  /// Enqueue a string for output.
  fn enqueue_output(&mut self, output: String) {
    self.output_queue.push_back(output);
  }

  /// Dequeue a string for output.
  fn dequeue_output(&mut self) -> Option<String> {
    self.output_queue.pop_front()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;

  #[test]
  fn test_enqueue_output() {
    init();
    let mut game_state = GameState::new();
    game_state.enqueue_output("Hello, world!".to_string());
    assert_eq!(game_state.output_queue.len(), 1);
  }

  #[test]
  fn test_dequeue_output() {
    init();
    let mut game_state = GameState::new();
    game_state.enqueue_output("Hello, world!".to_string());
    assert_eq!(game_state.output_queue.len(), 1);
    let output = game_state.dequeue_output();
    assert_eq!(output, Some("Hello, world!".to_string()));
    assert_eq!(game_state.output_queue.len(), 0);
  }
}
