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
      CameraPlugin,
      PlayerPlugin,
    ))
    .add_systems(Update, (hello_world, make_visible))
    .run();
}

fn make_visible(mut window: Query<&mut Window>, frames: Res<FrameCount>) {
  // The delay may be different for your app or system.
  if frames.0 == 3 {
    // At this point the gpu is ready to show the app so we can make the window visible.
    // Alternatively, you could toggle the visibility in Startup.
    // It will work, but it will have one white frame before it starts rendering
    window.single_mut().visible = true;
  }
}
