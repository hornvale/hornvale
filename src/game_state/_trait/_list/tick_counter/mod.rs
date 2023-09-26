use crate::game_state::TickCounter as TickCounterType;

/// The `TickCounter` trait.
pub trait TickCounter {
  /// Get tick counter.
  fn get_tick_counter(&self) -> TickCounterType;
  /// Set tick counter.
  fn set_tick_counter(&mut self, value: TickCounterType);
  /// Increment tick counter.
  fn increment_tick_counter(&mut self);
}
