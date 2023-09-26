use anyhow::Error as AnyError;

/// The `Game` struct.
///
/// This is basically a wrapper around the run loop.
#[derive(Debug, Default)]
pub struct Game {}

impl Game {
  /// Creates a new `Game`.
  pub fn new() -> Self {
    Self {}
  }

  /// Runs the `Game`.
  pub fn run(&self) -> Result<(), AnyError> {
    debug!("Running game.");
    Ok(())
  }
}
