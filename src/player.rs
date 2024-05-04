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
    let texture: Handle<Image> = asset_server.load("hexany_roguelike_tiles_16x16.png");
    commands.spawn((
      SpriteSheetBundle {
        atlas: TextureAtlas {
          layout: sprite_atlas_layout.0.clone(),
          index: 161,
        },
        transform: Transform::from_translation(Vec3::new(0.0, 0.0, 10.0)),
        texture,
        sprite: Sprite {
          color: Color::rgb(0.6, 0.2, 0.8),
          ..Default::default()
        },
        ..default()
      },
      Player,
    ));
  }
}

impl Plugin for PlayerPlugin {
  fn build(&self, app: &mut App) {
    app.add_systems(Startup, Self::setup);
  }
}

/// The prelude.
pub mod prelude {
  pub use super::Player;
  pub use super::PlayerPlugin;
}

/// The internal prelude.
pub mod prelude_internal {
  pub use super::prelude::*;
}
