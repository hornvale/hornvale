#![allow(unused_macros)]
#![allow(unknown_lints)]
#![allow(ambiguous_glob_reexports)]
#![allow(clippy::field_reassign_with_default)]

// Crate-wide configuration.
#[allow(unused_imports)]
#[macro_use]
extern crate anyhow;
#[allow(unused_imports)]
#[macro_use]
extern crate derivative;
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
pub mod action;
pub mod chunk;
pub mod chunk_factory;
pub mod chunk_plane;
pub mod chunk_seed;
pub mod command;
pub mod effect;
pub mod entity_id;
pub mod event;
pub mod game;
pub mod game_rule;
pub mod game_state;
pub mod parser;
pub mod passage;
pub mod room;
pub mod system;
pub mod time;

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
