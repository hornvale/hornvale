/// Sets the quit flag to some Option<String>.
pub trait SetQuitFlag {
  /// Sets the quit flag to some Option<String>.
  fn set_quit_flag(&mut self, value: Option<String>);
}
