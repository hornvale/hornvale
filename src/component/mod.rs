use anyhow::Error as AnyError;
use specs::prelude::*;

/// Register all components.
pub fn register_components(_ecs: &mut World) -> Result<(), AnyError> {
  Ok(())
}
