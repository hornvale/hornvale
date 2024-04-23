//! A library for roguelikes, MUDs, and other text adventures.
//!
//! I don't really anticipate anyone using this library, as it's very specific
//! and intensely opinionated. Nevertheless, I'm making it public in case
//! someone finds it useful or interesting.

/// The prelude for the _Hornvale_ library.
///
/// This prelude is similar to the standard library's prelude in that you'll
/// almost always want to have these items available. It includes a number of
/// traits, types, and functions that will be exposed to the _Hornvale_ binary,
/// examples, benches, and integration tests.
pub use hornvale_command::prelude as command;
pub use hornvale_core::prelude as core;
pub use hornvale_input::prelude as input;
pub use hornvale_output::prelude as output;
pub use hornvale_scripting::prelude as scripting;
pub use hornvale_world::prelude as world;

/// The prelude for the _Hornvale_ library.
pub mod prelude {

  pub use super::command::*;
  pub use super::core::*;
  pub use super::input::*;
  pub use super::output::*;
  pub use super::scripting::*;
  pub use super::world::*;

  /// Hello, world!
  pub fn hello() {
    println!("Hello, world!");
  }
}
