//! # Database
//!
//! This module contains the database implementation, an in-memory database
//! that stores entities and components.
//!
//! This combines elements of Entity-Component-System architectures, relational
//! databases, and graph databases with various data structures and algorithms.

use derivative::Derivative;
use hecs::World; // Temporary.

/// The database.
#[derive(Default, Derivative)]
#[derivative(Debug)]
pub struct Database {
  /// The world.
  #[derivative(Debug = "ignore")]
  pub world: World,
}

impl Database {
  /// Create a new database.
  pub fn new() -> Self {
    Self::default()
  }
}

/// The prelude.
pub mod prelude {
  pub use super::Database;
}
