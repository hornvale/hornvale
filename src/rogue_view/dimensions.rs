use bevy::prelude::*;

/// The dimensions of the "rogue view."
#[derive(Debug, Clone, Copy, Resource)]
pub struct RogueViewDimensions(pub Vec2);
