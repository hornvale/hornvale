use bevy::prelude::*;

/// The dimensions of the tilemap view.
#[derive(Debug, Clone, Copy, Resource)]
pub struct TilemapDimensions(pub Vec2);
