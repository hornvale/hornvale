use std::sync::Arc;

use crate::effect::EffectContext;
use crate::effect::EffectContextTrait;
use crate::effect::EffectError;
use crate::effect::EffectTrait;

/// The `DelayedEffect` effect struct.
///
/// This effect delays the application of another effect.
#[derive(Clone, Debug, Default)]
pub struct Delayed<EffectType: EffectTrait + Clone + 'static> {
  pub effect: EffectType,
  pub delay: u32,
}

impl<EffectType: EffectTrait + Clone + 'static> Delayed<EffectType> {
  pub fn new(effect: EffectType, delay: u32) -> Self {
    Self { effect, delay }
  }
}

impl<EffectType: EffectTrait + Clone + 'static> EffectTrait for Delayed<EffectType> {
  fn apply(&self, context: &mut EffectContext) -> Result<(), EffectError> {
    match self.delay {
      0 => {
        write_effect_event!(context.get_data_mut(), Arc::new(self.effect.clone()));
      },
      _ => {
        let mut effect = self.clone();
        effect.delay -= 1;
        write_effect_event!(context.get_data_mut(), Arc::new(effect));
      },
    }
    Ok(())
  }
}
