use anyhow::Error as AnyError;
use uuid::Uuid;

use crate::event::EventMetadata;
use crate::game_state::GameStateTrait;

/// The `Event` trait.
pub trait Event<T: GameStateTrait> {
  /// Get the name of this event.
  fn get_name(&self) -> &'static str;

  /// Get the UUID for this event.
  fn get_uuid(&self) -> Uuid {
    self.get_metadata().get_uuid()
  }

  /// Get the metadata for this event.
  fn get_metadata(&self) -> &EventMetadata;

  /// Get (mutably) the metadata for this event.
  fn get_metadata_mut(&mut self) -> &mut EventMetadata;

  /// Processes the `Event`.
  fn process(&self, _game_state: &mut T) -> Result<(), AnyError> {
    debug!("Applying event: {}", self.get_name());
    Ok(())
  }
}
