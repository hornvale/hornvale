use crate::action_event::prelude_internal::*;
use bevy::prelude::*;

/// The Action Event plugin.
#[derive(Clone, Copy, Debug, Default)]
pub struct ActionEventPlugin;

impl ActionEventPlugin {
  /// Setup the Action Event plugin.
  pub fn on_startup() {}
}

impl Plugin for ActionEventPlugin {
  /// Build the Action Event plugin.
  fn build(&self, app: &mut App) {
    app
      .add_event::<GoDirectionAction>()
      .add_systems(Startup, Self::on_startup)
      .add_systems(Update, GoDirectionAction::apply);
  }
}
