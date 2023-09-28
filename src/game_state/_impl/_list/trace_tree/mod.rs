use crate::game_state::GameState;
use crate::game_state::TraceTreeTrait;
use crate::trace_tree::TraceTree;

impl TraceTreeTrait for GameState {
  /// Get the trace tree.
  fn get_trace_tree(&self) -> &TraceTree {
    &self.trace_tree
  }
  /// Set the trace tree.
  fn set_trace_tree(&mut self, value: TraceTree) {
    self.trace_tree = value;
  }
  /// Get a mutable reference to the trace tree.
  fn get_trace_tree_mut(&mut self) -> &mut TraceTree {
    &mut self.trace_tree
  }
  /// Prune the trace tree, removing events older than the given timestamp.
  fn prune_trace_tree(&mut self, timestamp: u64) {
    self.trace_tree.prune(timestamp);
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;

  #[test]
  fn test_get_trace_tree() {
    init();
    let game_state = GameState::new();
    let _trace_tree = game_state.get_trace_tree();
  }

  #[test]
  fn test_set_trace_tree() {
    init();
    let mut game_state = GameState::new();
    let trace_tree = TraceTree::new();
    game_state.set_trace_tree(trace_tree);
  }

  #[test]
  fn test_get_trace_tree_mut() {
    init();
    let mut game_state = GameState::new();
    let _trace_tree = game_state.get_trace_tree_mut();
  }

  #[test]
  fn test_prune_trace_tree() {
    init();
    let mut game_state = GameState::new();
    game_state.prune_trace_tree(0);
  }
}
