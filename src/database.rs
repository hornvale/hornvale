//! # Database
//!
//! This module contains the database implementation, an in-memory database
//! that stores entities and components.
//!
//! This combines elements of Entity-Component-System architectures, relational
//! databases, and graph databases with various data structures and algorithms.

use crate::region::prelude::*;
use derivative::Derivative;
use hecs::World; // Temporary.
use std::{fs, path::PathBuf};

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
  /// The region map.
  region_map: Option<RegionMap>,
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
  pub fn at_path(path: PathBuf) -> Self {
    let cache_dir = path.join("cache");
    let data_dir = path.join("data");
    let config_dir = path.join("config");
    let state_dir = path.join("state");
    fs::create_dir_all(&cache_dir).unwrap();
    fs::create_dir_all(&data_dir).unwrap();
    fs::create_dir_all(&config_dir).unwrap();
    fs::create_dir_all(&state_dir).unwrap();
    let world = World::new();
    let region_map = None;
    Self {
      cache_dir,
      data_dir,
      config_dir,
      state_dir,
      world,
      region_map,
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
  pub use super::Database;
}
