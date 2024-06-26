use crate::rogue_view::prelude_internal::*;
use crate::tile_kind::prelude::*;
use bevy::{math::Vec3Swizzles, prelude::*, utils::HashSet};
use bevy_ecs_tilemap::prelude::*;

/// The size of our individual tiles.
const TILE_SIZE: TilemapTileSize = TilemapTileSize { x: 16.0, y: 16.0 };
// For this example, don't choose too large a chunk size.
const CHUNK_SIZE: UVec2 = UVec2 { x: 4, y: 4 };
// Render chunk sizes are set to 4 render chunks per user-specified chunk.
const RENDER_CHUNK_SIZE: UVec2 = UVec2 {
  x: CHUNK_SIZE.x * 2,
  y: CHUNK_SIZE.y * 2,
};

/// The chunk manager.
#[derive(Default, Debug, Resource)]
pub struct RogueViewChunkManager {
  /// Currently spawned chunks.
  pub spawned_chunks: HashSet<IVec2>,
}

impl RogueViewChunkManager {
  /// Spawn a specific chunk.
  pub fn spawn_chunk(
    commands: &mut Commands,
    chunk_pos: &IVec2,
    tile_kind_data_source: &Res<TileKindDataSource>,
    tile_kind_theme: &Res<TileKindTheme>,
  ) {
    let tilemap_entity = commands.spawn_empty().id();
    let mut tile_storage = TileStorage::empty(CHUNK_SIZE.into());
    // Spawn the elements of the tilemap.
    for x in 0..CHUNK_SIZE.x {
      for y in 0..CHUNK_SIZE.y {
        let tile_x = chunk_pos.x * CHUNK_SIZE.x as i32 + x as i32;
        let tile_y = chunk_pos.y * CHUNK_SIZE.y as i32 + y as i32;
        let tile_pos = TilePos { x, y };
        let kind_option = tile_kind_data_source.get_tile_kind(tile_x, tile_y);
        if kind_option.is_none() {
          continue;
        }
        let texture_index_option = tile_kind_theme.get_texture_index(kind_option.unwrap());
        if texture_index_option.is_none() {
          continue;
        }
        let tile_entity = commands
          .spawn(TileBundle {
            position: tile_pos,
            tilemap_id: TilemapId(tilemap_entity),
            texture_index: texture_index_option.unwrap(),
            ..Default::default()
          })
          .id();
        commands.entity(tilemap_entity).add_child(tile_entity);
        tile_storage.set(&tile_pos, tile_entity);
      }
    }

    let real_x = chunk_pos.x as f32 * CHUNK_SIZE.x as f32 * TILE_SIZE.x;
    let real_y = chunk_pos.y as f32 * CHUNK_SIZE.y as f32 * TILE_SIZE.y;
    let transform = Transform::from_translation(Vec3::new(real_x, real_y, 0.0));

    commands.entity(tilemap_entity).insert(TilemapBundle {
      grid_size: TILE_SIZE.into(),
      size: CHUNK_SIZE.into(),
      storage: tile_storage,
      texture: TilemapTexture::Single(tile_kind_theme.sprite.clone()),
      tile_size: TILE_SIZE,
      transform,
      render_settings: TilemapRenderSettings {
        render_chunk_size: RENDER_CHUNK_SIZE,
        ..Default::default()
      },
      ..Default::default()
    });
  }

  /// The startup system.
  pub fn on_startup(mut _commands: Commands) {}

  /// Convert camera position to chunk position.
  pub fn camera_pos_to_chunk_pos(camera_pos: &Vec2) -> IVec2 {
    let camera_pos = camera_pos.as_ivec2();
    let chunk_size: IVec2 = IVec2::new(CHUNK_SIZE.x as i32, CHUNK_SIZE.y as i32);
    let tile_size: IVec2 = IVec2::new(TILE_SIZE.x as i32, TILE_SIZE.y as i32);
    camera_pos / (chunk_size * tile_size)
  }

  /// Spawn chunks that are within the render distance.
  pub fn spawn_chunks(
    mut commands: Commands,
    camera_query: Query<&Transform, With<RogueViewCamera>>,
    mut chunk_manager: ResMut<RogueViewChunkManager>,
    render_distance: Res<RogueViewRenderDistance>,
    tile_kind_data_source: Res<TileKindDataSource>,
    tile_kind_theme: Res<TileKindTheme>,
  ) {
    let x_chunk_limit = 2 + (render_distance.0.x / (CHUNK_SIZE.x as f32 * TILE_SIZE.x)) as i32;
    let y_chunk_limit = 2 + (render_distance.0.y / (CHUNK_SIZE.y as f32 * TILE_SIZE.y)) as i32;
    for transform in camera_query.iter() {
      let camera_chunk_pos = Self::camera_pos_to_chunk_pos(&transform.translation.xy());
      for y in (camera_chunk_pos.y - y_chunk_limit)..(camera_chunk_pos.y + y_chunk_limit) {
        for x in (camera_chunk_pos.x - x_chunk_limit)..(camera_chunk_pos.x + x_chunk_limit) {
          let xy = IVec2::new(x, y);
          if !chunk_manager.spawned_chunks.contains(&xy) {
            chunk_manager.spawned_chunks.insert(xy);
            Self::spawn_chunk(&mut commands, &xy, &tile_kind_data_source, &tile_kind_theme);
          }
        }
      }
    }
  }

  /// Despawn chunks that are too far away from the camera.
  pub fn despawn_chunks(
    mut commands: Commands,
    camera_query: Query<&Transform, With<RogueViewCamera>>,
    chunks_query: Query<(Entity, &Transform)>,
    mut chunk_manager: ResMut<RogueViewChunkManager>,
    render_distance: Res<RogueViewRenderDistance>,
  ) {
    let x_distance_limit = (render_distance.0.x + CHUNK_SIZE.x as f32 * TILE_SIZE.x) as i32;
    let y_distance_limit = (render_distance.0.y + CHUNK_SIZE.y as f32 * TILE_SIZE.y) as i32;
    for camera_transform in camera_query.iter() {
      for (entity, chunk_transform) in chunks_query.iter() {
        let chunk_pos = chunk_transform.translation.xy();
        let x_distance = (chunk_pos.x - camera_transform.translation.x).abs();
        let y_distance = (chunk_pos.y - camera_transform.translation.y).abs();
        if x_distance > x_distance_limit as f32 || y_distance > y_distance_limit as f32 {
          let x = (chunk_pos.x / (CHUNK_SIZE.x as f32 * TILE_SIZE.x)).floor() as i32;
          let y = (chunk_pos.y / (CHUNK_SIZE.y as f32 * TILE_SIZE.y)).floor() as i32;
          chunk_manager.spawned_chunks.remove(&IVec2::new(x, y));
          commands.entity(entity).despawn_recursive();
        }
      }
    }
  }
}
