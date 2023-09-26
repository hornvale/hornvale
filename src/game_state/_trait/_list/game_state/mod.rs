/// The `GameState` trait.
///
/// This is a trait describing the `GameState` struct.
pub trait GameState {
  /// Enqueue a string for output.
  fn enqueue_output(&mut self, output: String);
  /// Dequeue a string for output.
  fn dequeue_output(&mut self) -> Option<String>;
  /// Get quit-game flag.
  fn get_quit_flag(&self) -> bool;
  /// Set quit-game flag.
  fn set_quit_flag(&mut self, quit_flag: bool);
}
