use anyhow::Error as AnyError;
use uuid::Uuid;

use crate::effect::EffectTrait;
use crate::effect::NoOpEffect;
use crate::event::EventTrait;
use crate::game_state::GameState;

/// The `NoOp` event.
#[derive(Clone, Debug, Default)]
pub struct NoOp {
  pub uuid: Uuid,
}

impl NoOp {
  /// Creates a new `NoOp`.
  pub fn new() -> Self {
    Self { uuid: Uuid::new_v4() }
  }
}

impl EventTrait<GameState> for NoOp {
  /// Get the name of this event.
  fn get_name(&self) -> &'static str {
    "NoOp"
  }

  /// Get the UUID of this event.
  fn get_uuid(&self) -> uuid::Uuid {
    self.uuid
  }

  /// Processes the `NoOp` event.
  fn process(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Applying no-op event.");
    NoOpEffect::new().apply(game_state)?;
    Ok(())
  }
}
