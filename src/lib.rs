//! # Hornvale
//!
//! This is the main library for the _Hornvale_ project.

/// Action events indicate an entity's intent.
pub mod action_event;
/// A dumb bouncing text plugin.
pub mod bouncer;
/// Command events indicate the player's intent.
pub mod command_event;
/// Directions in which to travel.
pub mod direction;
/// Effect events are changes to the world.
pub mod effect_event;
/// Development utilities re: framerate.
pub mod framerate;
/// Input handling for the player.
pub mod input;
/// The player.
pub mod player;
/// The region concept.
pub mod region;
/// The "roguelike" tilemap view of the game.
pub mod rogue_view;
/// Sprite sheet plugin.
pub mod sprite_sheets;
/// Tiles used to build the tilemap view.
pub mod tile_kind;

/// The prelude for the _Hornvale_ library.
pub mod prelude {
  pub use super::action_event::prelude::*;
  pub use super::bouncer::prelude::*;
  pub use super::command_event::prelude::*;
  pub use super::direction::prelude::*;
  pub use super::effect_event::prelude::*;
  pub use super::framerate::prelude::*;
  pub use super::input::prelude::*;
  pub use super::player::prelude::*;
  pub use super::rogue_view::prelude::*;
  pub use super::sprite_sheets::prelude::*;
  pub use super::tile_kind::prelude::*;
}

/// The internal prelude for the _Hornvale_ library.
pub mod prelude_internal {
  pub use super::prelude::*;
}
