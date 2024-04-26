//! A library for roguelikes, MUDs, and other text adventures.
//!
//! I don't really anticipate anyone using this library, as it's very specific
//! and intensely opinionated. Nevertheless, I'm making it public in case
//! someone finds it useful or interesting.

/// General actions.
pub mod action;
/// Commands and command parsing.
pub mod command;
/// The database.
pub mod database;
/// Input.
pub mod input;
/// Output.
pub mod output;
/// Scripting.
pub mod scripting;
/// Test utilities.
pub mod test_utilities;
/// World model.
pub mod world;

/// The prelude for the _Hornvale_ library.
pub mod prelude {

  pub use super::test_utilities::*;

  /// Hello, world!
  pub fn hello() {
    println!("Hello, world!");
  }
}
