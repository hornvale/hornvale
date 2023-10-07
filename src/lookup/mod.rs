pub mod _impl;
pub use _impl::*;
pub mod _trait;
pub use _trait::*;

/// The `Lookup` service.
///
/// This provides a centralized lookup service for all game data.
///
/// Examples:
/// - In what Room is $Player?
/// - In what Chunk is $Entity?
/// - What Actor, if any, is currently carrying $Object?
/// - What Actors are in $Room?
///
/// This data should be populated by events, not persisted in any way.
#[derive(Debug, Default)]
pub struct Lookup {}

impl Lookup {
  /// Creates a new `Lookup`.
  pub fn new() -> Self {
    Self {}
  }
}
