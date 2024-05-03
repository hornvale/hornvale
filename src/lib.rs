//! # Hornvale
//!
//! This is the main library for the _Hornvale_ project.

/// A dumb bouncing text plugin.
pub mod bouncer;
/// Development utilities re: framerate.
pub mod framerate;
/// The player.
pub mod player;
/// The "roguelike" tilemap view of the game.
pub mod rogue_view;
/// Sprite sheet plugin.
pub mod sprite_sheets;
/// Tiles used to build the tilemap view.
pub mod tile;
/// Tile map plugin.
pub mod tile_map;

/// The prelude for the _Hornvale_ library.
pub mod prelude {
  pub use super::bouncer::prelude::*;
  pub use super::framerate::prelude::*;
  pub use super::player::prelude::*;
  pub use super::rogue_view::prelude::*;
  pub use super::sprite_sheets::prelude::*;
  pub use super::tile::prelude::*;
  pub use super::tile_map::prelude::*;
}

/// The internal prelude for the _Hornvale_ library.
pub mod prelude_internal {
  pub use super::prelude::*;
}
