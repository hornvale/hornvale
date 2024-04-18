//! This crate provides utilities for testing.

use pretty_env_logger::env_logger::builder;
use std::env::set_var;

/// Call this function at the beginning of each test.
pub fn init() {
  // Enable logging for tests.
  let _ = builder().is_test(true).try_init();
  // Enable backtraces.
  set_var("RUST_BACKTRACE", "1");
}

/// The prelude.
pub mod prelude {
  pub use super::init;
  pub use all_asserts::*;
  pub use assert_approx_eq::assert_approx_eq;
  pub use pretty_assertions::assert_eq as assert_eq_pretty;
}
