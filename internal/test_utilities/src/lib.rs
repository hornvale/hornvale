use pretty_env_logger::env_logger::builder;
use std::env::set_var;

/// Call this function at the beginning of each test.
pub fn init() {
  // Enable logging for tests.
  let _ = builder().is_test(true).try_init();
  // Enable backtraces.
  set_var("RUST_BACKTRACE", "full");
}

/// The prelude.
pub mod prelude {
  pub use super::init;
  pub use assert_approx_eq::assert_approx_eq;
}
