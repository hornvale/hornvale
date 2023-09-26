/// The `InputReadyFlag` trait.
pub trait InputReadyFlag {
  /// Get input-ready flag.
  fn get_input_ready_flag(&self) -> bool;
  /// Set input-ready flag.
  fn set_input_ready_flag(&mut self, value: bool);
}
