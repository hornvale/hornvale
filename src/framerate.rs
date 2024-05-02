use bevy::prelude::*;

/// My development plugin.
#[derive(Clone, Copy, Debug, Default)]
pub struct FrameratePlugin;

impl Plugin for FrameratePlugin {
  fn build(&self, _app: &mut App) {
    #[cfg(debug_assertions)]
    {
      _app.add_plugins(bevy::diagnostic::FrameTimeDiagnosticsPlugin);
      _app.add_plugins(bevy::diagnostic::LogDiagnosticsPlugin::default());
    }
  }
}
