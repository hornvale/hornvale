//! # Database
//!
//! This module contains the database implementation, an in-memory database
//! that stores entities and components.
//!
//! This combines elements of Entity-Component-System architectures, relational
//! databases, and graph databases with various data structures and algorithms.

use derivative::Derivative;
use hecs::World; // Temporary.
use std::collections::HashMap;

/// Region entities.
pub mod region;
use region::{entity::RegionEntity, identifier::RegionIdentifier, Region};
/// Traits and trait implementations.
pub mod traits;

/// The database.
#[derive(Default, Derivative)]
#[derivative(Debug)]
pub struct Database {
  /// The world.
  #[derivative(Debug = "ignore")]
  pub world: World,
  /// Next region ID.
  pub next_region_id: u32,
  /// RegionEntity -> RegionIdentifier map.
  pub region_identifiers: HashMap<RegionEntity, RegionIdentifier>,
  /// RegionIdentifier -> RegionEntity map.
  pub region_entities: HashMap<RegionIdentifier, RegionEntity>,
  /// RegionEntity -> Region map.
  pub regions: HashMap<RegionEntity, Region>,
}

/// The prelude.
pub mod prelude {
  pub use super::region::{
    entity::RegionEntity, identifier::RegionIdentifier, name::RegionName, point::RegionPoint, spawner::RegionSpawner,
    Region, RegionBuilder,
  };
  pub use super::{
    traits::{build::Build, database_type::DatabaseType, insert::Insert},
    Database,
  };
}
