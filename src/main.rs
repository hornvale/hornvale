//! The actual _Hornvale_ binary.

use bevy::core::FrameCount;
use bevy::prelude::*;
use bevy::window::{EnabledButtons, PresentMode, WindowTheme};
use hornvale::prelude::*;

fn main() {
  App::new()
    .add_plugins((
      DefaultPlugins.set(WindowPlugin {
        primary_window: Some(Window {
          title: "Hornvale".into(),
          name: Some("Hornvale".into()),
          resolution: (800., 600.).into(),
          present_mode: PresentMode::AutoVsync,
          prevent_default_event_handling: false,
          window_theme: Some(WindowTheme::Dark),
          enabled_buttons: EnabledButtons {
            maximize: false,
            ..Default::default()
          },
          visible: false,
          ..default()
        }),
        ..default()
      }),
      BouncerPlugin,
      TilemapPlugin,
      PlayerPlugin,
      FrameratePlugin,
    ))
    .add_systems(Update, (make_visible, toggle_vsync))
    .run();
}

fn make_visible(mut window: Query<&mut Window>, frames: Res<FrameCount>) {
  if frames.0 == 3 {
    window.single_mut().visible = true;
  }
}

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
