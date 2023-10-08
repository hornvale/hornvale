use anyhow::Error as AnyError;
use uuid::Uuid;

use crate::game_state::GameState;

pub mod effect_type;
pub use effect_type::Type as EffectType;

/// The `Effect` struct.
#[derive(Clone, Debug, PartialEq)]
pub struct Effect {
  /// The `Effect` type.
  pub effect_type: EffectType,
  /// The `Effect`'s UUID.
  pub uuid: Uuid,
  /// A backtrace.
  pub backtrace: Vec<String>,
}

impl Effect {
  pub fn new(effect_type: EffectType, backtrace: Vec<String>) -> Self {
    let uuid = Uuid::new_v4();
    let mut backtrace = backtrace;
    backtrace.push(format!("Effect {:?}:{}", effect_type, uuid));
    Self {
      effect_type,
      uuid,
      backtrace,
    }
  }

  pub fn apply(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Applying {:#?} event.", self.effect_type);
    self.effect_type.apply(self, game_state)?;
    Ok(())
  }
}
