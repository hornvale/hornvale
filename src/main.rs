//! The actual _Hornvale_ binary.

use bevy::core::FrameCount;
use bevy::prelude::*;
use bevy::window::{EnabledButtons, PresentMode, WindowTheme};
use hornvale::prelude::*;

fn main() {
  App::new()
    .add_plugins((
      DefaultPlugins
        .set(WindowPlugin {
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
        })
        .set(ImagePlugin::default_nearest()),
      RogueViewPlugin,
      CommandEventPlugin,
      ActionEventPlugin,
      EffectEventPlugin,
      InputPlugin,
      PlayerPlugin,
      FrameratePlugin,
      SpriteSheetsPlugin,
      TileKindPlugin,
    ))
    .add_systems(Update, make_visible)
    .run();
}

fn make_visible(mut window: Query<&mut Window>, frames: Res<FrameCount>) {
  if frames.0 == 3 {
    window.single_mut().visible = true;
  }
}
