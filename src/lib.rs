#![allow(unused_macros)]
#![allow(unknown_lints)]
#![allow(ambiguous_glob_reexports)]
#![allow(clippy::field_reassign_with_default)]

// Crate-wide configuration.
#[allow(unused_imports)]
#[macro_use]
extern crate all_asserts;
#[allow(unused_imports)]
#[macro_use]
extern crate anyhow;
#[allow(unused_imports)]
#[macro_use]
extern crate derivative;
#[allow(unused_imports)]
#[macro_use]
extern crate derive_builder;
#[allow(unused_imports)]
#[macro_use]
extern crate derive_more;
#[allow(unused_imports)]
#[macro_use]
extern crate lazy_static;
#[allow(unused_imports)]
#[macro_use]
extern crate log;
extern crate pretty_env_logger;
#[allow(unused_imports)]
#[macro_use]
extern crate serde;
extern crate serde_json;
extern crate specs;
#[allow(unused_imports)]
#[macro_use]
extern crate specs_derive;
#[allow(unused_imports)]
#[macro_use]
extern crate strum;
#[allow(unused_imports)]
#[macro_use]
extern crate thiserror;

// Derive macros from hornvale_derive.
#[allow(unused_imports)]
#[macro_use]
extern crate hornvale_derive;

// Utilities shared and relied upon by all systems.
#[macro_use]
pub mod _macro;
pub use _macro::*;

// Remaining modules.

/// Actions represent attempted actions by any actor.
pub mod action;
/// Actors represent characters and monsters.
pub mod actor;
/// Chunks subdivide the game world and its contents.
pub mod chunk;
/// Commands are reified player input.
pub mod command;
/// Components store data in the ECS.
pub mod component;
/// Dispatchers run systems.
pub mod dispatcher;
/// Effects are reified changes to the game world.
pub mod effect;
/// Entity IDs are used to identify entities.
pub mod event;
/// Where the core game loop lives.
pub mod game;
/// Markers for entities.
pub mod marker;
/// Output items.
pub mod output;
/// Passages connect rooms.
pub mod passage;
/// The player character.
pub mod player;
/// ECS resources shared by all systems.
pub mod resource;
/// Rooms, which are used to subdivide chunks into individual play areas.
pub mod room;
/// Where the ECS lives.
pub mod system;

#[cfg(test)]
pub mod test {

  // Constants for paths.
  pub const TEST_ASSETS_DIRECTORY: &str = "tests/assets";
  pub const TEMPORARY_TEST_ASSETS_DIRECTORY: &str = "tests/assets/temporary";
  pub const TEST_DATA_DIRECTORY: &str = "tests/data";
  pub const TEMPORARY_TEST_DATA_DIRECTORY: &str = "tests/data/temporary";

  use pretty_env_logger::env_logger::builder;
  use std::env::set_var;

  #[allow(unused_imports)]
  use super::*;

  // Call this function at the beginning of each test module.
  pub fn init() {
    // Enable logging for tests.
    let _ = builder().is_test(true).try_init();
    // Enable backtraces.
    set_var("RUST_BACKTRACE", "1");
  }
}
