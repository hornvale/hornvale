//! # Hornvale
//!
//! This is the main library for the _Hornvale_ project. It contains the core
//! functionality for the game, including the Entity-Component-System (ECS)
//! implementation, the database, the world model, and the input/output system.

/// General actions.
pub mod action;
/// Commands and command parsing.
pub mod command;
/// The database.
pub mod database;
/// The Entity-Component-System (ECS) implementation.
pub mod ecs;
/// Geometry.
pub mod geometry;
/// Input.
pub mod input;
/// Output.
pub mod output;
/// Profile, or "save" data.
pub mod profile;
/// Region.
pub mod region;
/// Scripting.
pub mod scripting;
/// Session and session state.
pub mod session;
/// Test utilities.
pub mod test_utilities;
/// Type map.
pub mod type_map;
/// World model.
pub mod world;

/// The prelude for the _Hornvale_ library.
pub mod prelude {

  pub use super::session::prelude::*;
  pub use super::test_utilities::prelude::*;
  pub use super::type_map::prelude::*;

  /// Hello, world!
  pub fn hello() {
    println!("Hello, world!");
  }
}
