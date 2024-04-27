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
/// Input.
pub mod input;
/// Output.
pub mod output;
/// Scripting.
pub mod scripting;
/// Test utilities.
pub mod test_utilities;
/// Type map.
pub mod type_map;
/// World model.
pub mod world;

/// The prelude for the _Hornvale_ library.
pub mod prelude {

  pub use super::test_utilities::*;
  pub use super::type_map::TypeMap;

  /// Hello, world!
  pub fn hello() {
    println!("Hello, world!");
  }
}
