//! # Tilemap
//!
//! This module contains the tilemap view of the game, which is what most of us
//! think of when we think of a "roguelike."
//!
//! The tilemap is a grid of tiles, each of which corresponds to a position in
//! 2D space. The tiles can be of different types, such as walls, floors, or
//! doors. The player can move around the tilemap, and the camera follows the
//! player.

/// The tilemap camera.
pub mod camera;
/// The dimensions of the tilemap.
pub mod dimensions;
/// The Bevy plugin for the tilemap.
pub mod plugin;

/// The prelude.
pub mod prelude {
  pub use super::plugin::*;
}

/// The internal prelude.
#[allow(unused_imports)]
pub mod prelude_internal {
  pub use super::camera::*;
  pub use super::dimensions::*;
  pub use super::prelude::*;
}
