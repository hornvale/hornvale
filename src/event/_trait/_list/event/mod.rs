use anyhow::Error as AnyError;

use crate::game_state::GameStateTrait;

/// The `Event` trait.
pub trait Event<T: GameStateTrait> {
  /// Get the name of this event.
  fn get_name(&self) -> &'static str;

  /// Processes the `Event`.
  fn process(&self, _game_state: &mut T) -> Result<(), AnyError> {
    debug!("Processing event: {}", self.get_name());
    Ok(())
  }
}
