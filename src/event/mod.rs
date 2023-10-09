use anyhow::Error as AnyError;
use specs::prelude::*;
use specs::shrev::EventChannel;

pub mod input;
pub use input::Input as InputEvent;
pub mod output;
pub use output::Output as OutputEvent;

pub fn insert_event_channels(ecs: &mut World) -> Result<(), AnyError> {
  ecs.insert(EventChannel::<InputEvent>::new());
  ecs.insert(EventChannel::<OutputEvent>::new());
  Ok(())
}
