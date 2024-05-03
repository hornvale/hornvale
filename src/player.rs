//! # Player
//!
//! The player.

use crate::sprite_sheets::prelude::*;
use bevy::prelude::*;

/// A player.
#[derive(Debug, Clone, Copy)]
pub struct PlayerPlugin;

/// Player component.
#[derive(Debug, Clone, Copy, Component)]
pub struct Player;

impl PlayerPlugin {
  /// The startup system.
  pub fn setup(mut commands: Commands, sprite_atlas_layout: Res<HexanyRt16x16>, asset_server: Res<AssetServer>) {
    let sprite: Handle<Image> = asset_server.load("hexany_roguelike_tiles_16x16.png");
    commands.spawn((
      SpriteSheetBundle {
        atlas: TextureAtlas {
          layout: sprite_atlas_layout.0.clone(),
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
  pub fn control_player(
    time: Res<Time>,
    keyboard_input: Res<ButtonInput<KeyCode>>,
    mut query: Query<(&mut Transform, &mut Sprite), With<Player>>,
  ) {
    let (mut transform, mut player_sprite) = query.single_mut();
    let mut direction = Vec3::ZERO;
    if keyboard_input.pressed(KeyCode::KeyA) {
      direction.x = -1.0;
    }
    if keyboard_input.pressed(KeyCode::KeyD) {
      direction.x = 1.0;
    }
    if keyboard_input.pressed(KeyCode::KeyW) {
      direction.y = 1.0;
    }
    if keyboard_input.pressed(KeyCode::KeyS) {
      direction.y = -1.0;
    }
    // Flip the sprite if we are heading right or if we're heading up or
    // down and already flipped.
    player_sprite.flip_x = direction.x > 0.0 || (direction.x == 0.0 && player_sprite.flip_x);
    let z = transform.translation.z;
    transform.translation += time.delta_seconds() * direction * 500.;
    // Important! We need to restore the Z values when moving the camera around.
    // Bevy has a specific camera setup and this can mess with how our layers are shown.
    transform.translation.z = z;
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
