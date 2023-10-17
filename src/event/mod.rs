use anyhow::Error as AnyError;
use specs::prelude::*;
use specs::shrev::EventChannel;

pub mod _type;
pub use _type::*;

pub fn insert_event_channels(ecs: &mut World) -> Result<(), AnyError> {
  ecs.insert(EventChannel::<InputEvent>::new());
  ecs.insert(EventChannel::<OutputEvent>::new());
  Ok(())
}
