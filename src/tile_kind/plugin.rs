use crate::tile_kind::prelude_internal::*;
use bevy::prelude::*;
use bevy_ecs_tilemap::prelude::*;
use rand::prelude::*;
use std::collections::HashMap;

/// The TileKind plugin.
#[derive(Clone, Copy, Debug, Default)]
pub struct TileKindPlugin;

impl TileKindPlugin {
  /// Setup the TileKind plugin.
  pub fn on_startup(mut commands: Commands, asset_server: ResMut<AssetServer>) {
    let mut map = vec![TileKind::Floor; 80 * 50];
    fn xy_idx(x: usize, y: usize) -> usize {
      y * 80 + x
    }

    // Make the boundaries walls
    for x in 0..80 {
      map[xy_idx(x, 0)] = TileKind::Wall;
      map[xy_idx(x, 49)] = TileKind::Wall;
    }
    for y in 0..50 {
      map[xy_idx(0, y)] = TileKind::Wall;
      map[xy_idx(79, y)] = TileKind::Wall;
    }
    let mut rng = thread_rng();
    for _i in 0..400 {
      let x = rng.gen_range(1..79);
      let y = rng.gen_range(1..49);
      let idx = xy_idx(x, y);
      if idx != xy_idx(40, 25) {
        map[idx] = TileKind::Wall;
      }
    }

    commands.insert_resource(TileKindDataSource::Matrix(TileKindMatrix {
      width: 80,
      height: 50,
      kinds: map,
    }));

    commands.insert_resource(TileKindTheme {
      sprite: asset_server.load("consolidated_tiles_16x16.png"),
      map: {
        let mut map = HashMap::new();
        map.insert(TileKind::Floor, TileTextureIndex(0));
        map.insert(TileKind::Wall, TileTextureIndex(1));
        map.insert(TileKind::Door, TileTextureIndex(2));
        map
      },
    });
  }
}

impl Plugin for TileKindPlugin {
  /// Build the TileKind plugin.
  fn build(&self, app: &mut App) {
    app.add_systems(Startup, TileKindPlugin::on_startup);
  }
}
