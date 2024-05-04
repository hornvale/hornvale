use crate::command_event::prelude_internal::*;
use bevy::prelude::*;

/// The Command plugin.
#[derive(Clone, Copy, Debug, Default)]
pub struct CommandEventPlugin;

impl CommandEventPlugin {
  /// Setup the Command plugin.
  pub fn on_startup() {}
}

impl Plugin for CommandEventPlugin {
  /// Build the Command Event plugin.
  fn build(&self, app: &mut App) {
    app
      .add_event::<GoDirectionCommand>()
      .add_systems(Startup, Self::on_startup)
      .add_systems(Update, GoDirectionCommand::apply);
  }
}
