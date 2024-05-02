//! # Hornvale
//!
//! This is the main library for the _Hornvale_ project.

/// A dumb bouncing text plugin.
pub mod bouncer;
/// The _Hornvale_ camera, so I don't lose it.
pub mod camera;
/// The player.
pub mod player;

/// The prelude for the _Hornvale_ library.
pub mod prelude {
  pub use super::bouncer::*;
  pub use super::camera::*;
  pub use super::player::*;

  /// Say hello.
  pub fn hello_world() {
    //println!("Hello, world!");
  }
}

/// The internal prelude for the _Hornvale_ library.
pub mod internal_prelude {
  pub use super::prelude::*;
}
