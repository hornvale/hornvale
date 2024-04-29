//! # Database
//!
//! This module contains the database implementation, an in-memory database
//! that stores entities and components.
//!
//! This combines elements of Entity-Component-System architectures, relational
//! databases, and graph databases with various data structures and algorithms.
//!
//! The database contains and maintains a number of different structures:
//!
//! - The profile, which is a save file for the game. This informs the game how
//!   to load the player character and the game state.
//! - The region map, which is a map of region identifiers to region entities.
//!   This is used to retrieve basic information about regions.
//! - The region adjacency map, which is a map of region identifiers to a set
//!   of adjacent region identifiers. This is used to determine which regions
//!   are adjacent to a given region.

use crate::profile::prelude::*;
use crate::region::prelude::*;
use derivative::Derivative;
use hecs::World; // Temporary.
use std::collections::HashMap;
use std::{
  fs,
  path::{Path, PathBuf},
};

/// Filename formatter.
pub mod filename_formatter;
/// Traits and trait implementations.
pub mod traits;

/// The database.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Database {
  /// The path to the cache dir.
  cache_dir: PathBuf,
  /// The path to the data dir.
  data_dir: PathBuf,
  /// The path to the config dir.
  config_dir: PathBuf,
  /// The path to the state dir.
  state_dir: PathBuf,
  /// The profile.
  profile: Option<Profile>,
  /// The region map.
  region_map: Option<RegionMap>,
  /// The region adjacency maps.
  region_adjacency_maps: Option<HashMap<RegionIdentifier, RegionAdjacencyMap>>,
  /// The world (temporary).
  #[derivative(Debug = "ignore")]
  pub world: World,
}

impl Database {
  /// Create a new database.
  pub fn new() -> Self {
    Self::default()
  }

  /// Operate at a specific path.
  pub fn at_path(path: &Path) -> Self {
    let cache_dir = path.join("cache");
    let data_dir = path.join("data");
    let config_dir = path.join("config");
    let state_dir = path.join("state");
    fs::create_dir_all(&cache_dir).unwrap();
    fs::create_dir_all(&data_dir).unwrap();
    fs::create_dir_all(&config_dir).unwrap();
    fs::create_dir_all(&state_dir).unwrap();
    let profile = None;
    let region_map = None;
    let region_adjacency_maps = None;

    let world = World::new();
    Self {
      cache_dir,
      data_dir,
      config_dir,
      state_dir,
      profile,
      region_map,
      region_adjacency_maps,
      // The world (temporary).
      world,
    }
  }

  /// Get the cache dir.
  ///
  /// The cache dir is where the cache is stored.
  pub fn cache_dir(&self) -> &PathBuf {
    &self.cache_dir
  }

  /// Get the data dir.
  ///
  /// The data dir is where the entities and components are stored.
  pub fn data_dir(&self) -> &PathBuf {
    &self.data_dir
  }

  /// Get the config dir.
  ///
  /// The config dir is where the configuration is stored.
  pub fn config_dir(&self) -> &PathBuf {
    &self.config_dir
  }

  /// Get the state dir.
  ///
  /// The state dir is where stateful data, such as logs, is stored.
  pub fn state_dir(&self) -> &PathBuf {
    &self.state_dir
  }
}

/// The prelude.
pub mod prelude {
  pub use super::filename_formatter::FilenameFormatter;
  pub use super::Database;
}

/// The internal prelude.
pub mod prelude_internal {
  pub use super::prelude::*;
  // pub use super::traits::file::FileExt;
  // pub use super::traits::region::RegionExt;
  // pub use super::traits::region_adjacency_map::RegionAdjacencyMapExt;
  // pub use super::traits::region_map::RegionMapExt;
}
