//! # Database
//!
//! This module contains the database implementation, an in-memory database
//! that stores entities and components.
//!
//! This combines elements of Entity-Component-System architectures, relational
//! databases, and graph databases with various data structures and algorithms.

use derivative::Derivative;
use hecs::World; // Temporary.
use platform_dirs::AppDirs;
use std::{fs, path::PathBuf};

/// The database.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Database {
  /// The path to the cache dir.
  pub cache_dir: PathBuf,
  /// The path to the data dir.
  pub data_dir: PathBuf,
  /// The path to the config dir.
  pub config_dir: PathBuf,
  /// The path to the state dir.
  pub state_dir: PathBuf,
  /// The world.
  #[derivative(Debug = "ignore")]
  pub world: World,
}

impl Database {
  /// Create a new database.
  pub fn new() -> Self {
    Self::default()
  }

  /// Create a test database in a temporary directory.
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
    Self {
      cache_dir,
      data_dir,
      config_dir,
      state_dir,
      world,
    }
  }
}

impl Default for Database {
  fn default() -> Self {
    let app_dirs = AppDirs::new(Some("hornvale"), true).unwrap();
    dbg!(&app_dirs);
    let cache_dir = app_dirs.cache_dir;
    let data_dir = app_dirs.data_dir;
    let config_dir = app_dirs.config_dir;
    let state_dir = app_dirs.state_dir;
    let world = World::new();
    Self {
      cache_dir,
      data_dir,
      config_dir,
      state_dir,
      world,
    }
  }
}

/// The prelude.
pub mod prelude {
  pub use super::Database;
}
