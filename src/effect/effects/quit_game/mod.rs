use crate::effect::EffectContext;
use crate::effect::EffectError;
use crate::effect::EffectTrait;

/// The `QuitGame` effect struct.
#[derive(Clone, Debug, Default)]
pub struct QuitGame {
  pub message: String,
}

impl EffectTrait for QuitGame {
  fn apply(&self, context: &mut EffectContext) -> Result<(), EffectError> {
    context.data.quit_flag_resource.0 = Some(self.message.to_string());
    Ok(())
  }
}
