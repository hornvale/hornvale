use std::sync::Arc;

use crate::effect::Effect as EffectTrait;

/// The `EffectEvent` type.
///
/// This represents an effect applied to the game world.
#[derive(Clone, Debug)]
pub struct Effect {
  pub effect: Arc<dyn EffectTrait>,
}
