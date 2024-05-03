//! # Tile Map
//!
//! The tile map is a collection of tiles that make up the game world.
//!
//! This is normally what will be displayed to the player in the "rogue view".

use crate::sprite_sheets::prelude::*;
use bevy::prelude::*;

/// The Sprite Sheets plugin.
#[derive(Debug, Clone, Copy)]
pub struct TileMapPlugin;

impl TileMapPlugin {
  /// Setup the plugin.
  pub fn setup(mut commands: Commands, sprite_atlas: Res<HexanyRt16x16>, asset_server: Res<AssetServer>) {
    let mut x = -256.0;
    const COLUMNS: usize = 32;
    const ROWS: usize = 32;
    const WIDTH: f32 = 16.0;
    const HEIGHT: f32 = 16.0;
    for columns in 0..COLUMNS {
      let mut y = -256.0;
      for rows in 0..ROWS {
        let i = (columns * ROWS) + rows;
        let sprite: Handle<Image> = asset_server.load("hexany_roguelike_tiles_16x16.png");
        commands.spawn(SpriteSheetBundle {
          atlas: TextureAtlas {
            layout: sprite_atlas.0.clone(),
            index: i,
          },
          transform: Transform::from_translation(Vec3::new(x, y, 0.0)),
          texture: sprite,
          ..default()
        });
        y += WIDTH;
      }
      x += HEIGHT;
    }
  }
}

impl Plugin for TileMapPlugin {
  fn build(&self, app: &mut App) {
    app.add_systems(Startup, Self::setup);
  }
}

/// The prelude.
pub mod prelude {
  pub use super::TileMapPlugin;
}

/// The internal prelude.
pub mod prelude_internal {
  pub use super::prelude::*;
}
