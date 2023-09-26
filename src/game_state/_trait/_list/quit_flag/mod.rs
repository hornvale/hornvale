/// The `QuitFlag` trait.
pub trait QuitFlag {
  /// Get quit-game flag.
  fn get_quit_flag(&self) -> bool;
  /// Set quit-game flag.
  fn set_quit_flag(&mut self, value: bool);
}
