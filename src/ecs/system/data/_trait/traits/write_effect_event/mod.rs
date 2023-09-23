use std::sync::Arc;

use crate::effect::EffectTrait;

/// The `WriteEffectEvent` trait, which allows for writing an effect event.
pub trait WriteEffectEvent {
  fn write_effect_event(&mut self, effect: Arc<dyn EffectTrait>);
}
