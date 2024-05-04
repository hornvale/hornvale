use crate::effect_event::prelude_internal::*;
use bevy::prelude::*;

/// The Effect Event plugin.
#[derive(Clone, Copy, Debug, Default)]
pub struct EffectEventPlugin;

impl EffectEventPlugin {
  /// Setup the Effect Event plugin.
  pub fn on_startup() {}
}

impl Plugin for EffectEventPlugin {
  /// Build the Action Event plugin.
  fn build(&self, app: &mut App) {
    app
      .add_event::<UpdateEntityTransformEffect>()
      .add_systems(Startup, Self::on_startup)
      .add_systems(Update, UpdateEntityTransformEffect::apply);
  }
}
