use bevy::prelude::*;
use bevy::window::PresentMode;

/// My development plugin.
#[derive(Clone, Copy, Debug, Default)]
pub struct FrameratePlugin;

impl FrameratePlugin {
  /// This system toggles the vsync mode when pressing the button V.
  /// You'll see fps increase displayed in the console.
  fn toggle_vsync(input: Res<ButtonInput<KeyCode>>, mut windows: Query<&mut Window>) {
    if input.just_pressed(KeyCode::KeyV) {
      let mut window = windows.single_mut();
      window.present_mode = if matches!(window.present_mode, PresentMode::AutoVsync) {
        PresentMode::AutoNoVsync
      } else {
        PresentMode::AutoVsync
      };
      info!("PRESENT_MODE: {:?}", window.present_mode);
    }
  }
}

impl Plugin for FrameratePlugin {
  fn build(&self, _app: &mut App) {
    #[cfg(debug_assertions)]
    {
      _app.add_plugins(bevy::diagnostic::FrameTimeDiagnosticsPlugin);
      _app.add_plugins(bevy::diagnostic::LogDiagnosticsPlugin::default());
      _app.add_systems(Update, (bevy::window::close_on_esc, Self::toggle_vsync));
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
