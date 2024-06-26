//! # The Rogue View
//!
//! This module contains the top-down, tilemap view of the game, which is what
//! most of us think of when we think of a "roguelike."
//!
//! The tilemap is a grid of tiles, each of which corresponds to a position in
//! 2D space. The tiles can be of different types, such as walls, floors, or
//! doors. The player can move around the tilemap, and the camera follows the
//! player until the player reaches the edge of the tilemap.

/// The rogue view camera.
pub mod camera;
/// A chunk manager that spawns and despawns chunks of the tilemap as needed.
pub mod chunk_manager;
/// The dimensions of the rogue view.
pub mod dimensions;
/// The Bevy plugin for the rogue view.
pub mod plugin;
/// The render distance of the rogue view.
pub mod render_distance;

/// The prelude.
pub mod prelude {
  pub use super::camera::RogueViewCamera;
  pub use super::chunk_manager::RogueViewChunkManager;
  pub use super::dimensions::RogueViewDimensions;
  pub use super::plugin::RogueViewPlugin;
  pub use super::render_distance::RogueViewRenderDistance;
}

/// The internal prelude.
#[allow(unused_imports)]
pub mod prelude_internal {
  pub use super::prelude::*;
}
