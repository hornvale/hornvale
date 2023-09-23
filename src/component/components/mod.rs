// This file is generated (see build.rs). Please do not edit manually.
use specs::prelude::*;

pub mod is_an_object;
pub use is_an_object::IsAnObject;

pub fn register_components(ecs: &mut World) {
  ecs.register::<IsAnObject>();
}
