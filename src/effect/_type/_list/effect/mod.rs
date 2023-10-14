use anyhow::Error as AnyError;

use crate::effect::EffectData;
use crate::effect::EffectType;
use crate::entity_uuid::EffectUuid;

/// The `Effect` type.
#[derive(Builder, Clone, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Effect {
  /// The `Effect` type.
  pub effect_type: EffectType,
  /// The `Effect`'s UUID.
  #[builder(default = "EffectUuid::default()")]
  pub uuid: EffectUuid,
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
