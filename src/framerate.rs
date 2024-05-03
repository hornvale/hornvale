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
      _app.add_systems(Update, bevy::window::close_on_esc);
    }
  }
}

/// The prelude.
pub mod prelude {
  pub use super::*;
}

/// The internal prelude.
pub mod prelude_internal {
  pub use super::prelude::*;
}
