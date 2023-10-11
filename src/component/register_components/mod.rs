use anyhow::Error as AnyError;
use specs::prelude::*;

use crate::component::*;

/// Register all components.
pub fn register_components(ecs: &mut World) -> Result<(), AnyError> {
  ecs.register::<IsAChunkComponent>();
  ecs.register::<IsAChunkPlaneComponent>();
  Ok(())
}
