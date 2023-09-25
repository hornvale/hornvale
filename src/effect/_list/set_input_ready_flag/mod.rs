use crate::effect::EffectContext;
use crate::effect::EffectContextTrait;
use crate::effect::EffectError;
use crate::effect::EffectTrait;
use crate::system_data::SetInputReadyFlagTrait;

/// The `SetInputReadyFlag` effect struct.
#[derive(Clone, Debug, Default)]
pub struct SetInputReadyFlag {
  pub value: bool,
}

impl EffectTrait for SetInputReadyFlag {
  fn apply(&self, context: &mut EffectContext) -> Result<(), EffectError> {
    context.get_data_mut().set_input_ready_flag(self.value);
    Ok(())
  }
}
