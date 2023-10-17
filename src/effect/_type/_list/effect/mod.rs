use anyhow::Error as AnyError;

use crate::effect::EffectData;
use crate::effect::EffectType;

/// The `Effect` type.
#[derive(Builder, Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct Effect {
  /// The `Effect` type.
  pub effect_type: EffectType,
  /// A backtrace.
  pub backtrace: Vec<String>,
}

impl Effect {
  pub fn apply(&self, data: &mut EffectData) -> Result<(), AnyError> {
    debug!("Applying {:#?} event.", self.effect_type);
    self.effect_type.apply(self, data)?;
    Ok(())
  }
}
