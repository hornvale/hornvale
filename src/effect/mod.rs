use anyhow::Error as AnyError;
use uuid::Uuid;

use crate::game_state::GameState;

pub mod r#type;
pub use r#type::Type as EffectType;

/// The `Effect` struct.
#[derive(Clone, Debug, PartialEq)]
pub struct Effect {
  /// The `Effect` type.
  pub r#type: EffectType,
  /// The `Effect`'s UUID.
  pub uuid: Uuid,
  /// A backtrace.
  pub backtrace: Vec<String>,
}

impl Effect {
  pub fn new(r#type: EffectType, backtrace: Vec<String>) -> Self {
    let uuid = Uuid::new_v4();
    let mut backtrace = backtrace;
    backtrace.push(format!("Effect {:?}:{}", r#type, uuid));
    Self {
      r#type,
      uuid,
      backtrace,
    }
  }

  pub fn apply(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Applying {:#?} event.", self.r#type);
    self.r#type.apply(self, game_state)?;
    Ok(())
  }
}
