use anyhow::Error as AnyError;

/// The `Start` trait.
pub trait Start {
  /// Start the game.
  fn start(&mut self) -> Result<(), AnyError>;
}
