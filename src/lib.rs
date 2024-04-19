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
pub use hornvale_astronomy::prelude as astronomy;
pub use hornvale_command::prelude as command;
pub use hornvale_dictionary::prelude as dictionary;
pub use hornvale_input::prelude as input;
pub use hornvale_output::prelude as output;
pub use hornvale_parser::prelude as parser;
pub use hornvale_world::prelude as world;

/// The prelude for the _Hornvale_ library.
pub mod prelude {
  pub use super::astronomy::*;
  pub use super::command::*;
  pub use super::dictionary::*;
  pub use super::input::*;
  pub use super::output::*;
  pub use super::parser::*;
  pub use super::world::*;

  /// Hello, world!
  pub fn hello() {
    println!("Hello, world!");
  }
}
