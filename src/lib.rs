//! # Hornvale
//!
//! This is the main library for the _Hornvale_ project.

/// A dumb bouncing text plugin.
pub mod bouncer;
/// Development utilities re: framerate.
pub mod framerate;
/// The player.
pub mod player;
/// Tiles used to build the tilemap view.
pub mod tile;
/// The tilemap view of the game.
pub mod tilemap;

/// The prelude for the _Hornvale_ library.
pub mod prelude {
  pub use super::bouncer::*;
  pub use super::framerate::*;
  pub use super::player::*;
  pub use super::tile::*;
  pub use super::tilemap::prelude::*;
}

/// The internal prelude for the _Hornvale_ library.
pub mod prelude_internal {
  pub use super::prelude::*;
}
