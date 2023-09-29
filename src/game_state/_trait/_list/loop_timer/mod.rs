use std::time::Duration;

/// The `LoopTimer` trait.
pub trait LoopTimer {
  /// Reset the timer.
  fn reset_loop_timer(&mut self);
  /// Get the time since the last reset.
  fn get_loop_timer(&self) -> Duration;
}
