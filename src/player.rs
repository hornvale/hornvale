//! # Player
//!
//! The player.

use crate::sprite_sheets::prelude::*;
use bevy::input::{keyboard::KeyboardInput, ButtonState};
use bevy::prelude::*;

/// A player.
#[derive(Debug, Clone, Copy)]
pub struct PlayerPlugin;

/// Player component.
#[derive(Debug, Clone, Copy, Component)]
pub struct Player;

impl PlayerPlugin {
  /// The startup system.
  pub fn setup(mut commands: Commands, sprite_atlas: Res<HexanyRt16x16>, asset_server: Res<AssetServer>) {
    let sprite: Handle<Image> = asset_server.load("hexany_roguelike_tiles_16x16.png");
    commands.spawn((
      SpriteSheetBundle {
        atlas: TextureAtlas {
          layout: sprite_atlas.0.clone(),
          index: 161,
        },
        transform: Transform::from_translation(Vec3::new(0.0, 0.0, 10.0)),
        texture: sprite,
        ..default()
      },
      Player,
    ));
  }

  /// Control the player.
  fn control_player(
    mut keyboard_input_events: EventReader<KeyboardInput>,
    mut query: Query<&mut Transform, With<Player>>,
  ) {
    let mut player_transform = query.single_mut();
    let mut direction = (0.0, 0.0);
    for event in keyboard_input_events.read() {
      if event.state == ButtonState::Pressed {
        match event.key_code {
          KeyCode::ArrowUp => direction.1 = 1.0,
          KeyCode::ArrowDown => direction.1 = -1.0,
          KeyCode::ArrowLeft => direction.0 = -1.0,
          KeyCode::ArrowRight => direction.0 = 1.0,
          _ => {},
        }
      }
      let new_position = (
        player_transform.translation.x + direction.0 * 16.0,
        player_transform.translation.y + direction.1 * 16.0,
      );
      player_transform.translation = Vec3::new(new_position.0, new_position.1, 10.0);
    }
  }
}

impl Plugin for PlayerPlugin {
  fn build(&self, app: &mut App) {
    app
      .add_systems(Startup, Self::setup)
      .add_systems(Update, Self::control_player);
  }
}

/// The prelude.
pub mod prelude {
  pub use super::PlayerPlugin;
}

/// The internal prelude.
pub mod prelude_internal {
  pub use super::prelude::*;

  pub use super::Player;
}
