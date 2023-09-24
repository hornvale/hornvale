#![allow(unused_macros)]

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
#[macro_use]
extern crate serde;
extern crate specs;
#[allow(unused_imports)]
#[macro_use]
extern crate specs_derive;
#[allow(unused_imports)]
#[macro_use]
extern crate thiserror;

// Re-exports.
pub use log::*;
pub use pretty_env_logger::env_logger::builder as pretty_env_logger_builder;
pub use pretty_env_logger::init as init_pretty_env_logger;

// Utilities shared and relied upon by all systems.
#[macro_use]
pub mod _macro;
pub use _macro::*;

// Remaining modules.
pub mod action;
pub mod command;
pub mod component;
pub mod dispatcher;
pub mod effect;
pub mod entity;
pub mod event;
pub mod event_channel;
pub mod game;
pub mod output;
pub mod resource;
pub mod system;
pub mod system_data;

#[cfg(test)]
pub mod test {

  use pretty_env_logger::env_logger::builder;
  use std::env::set_var;

  #[allow(unused_imports)]
  use super::*;

  pub fn init() {
    let _ = builder().is_test(true).try_init();
    set_var("RUST_BACKTRACE", "1");
  }
}
