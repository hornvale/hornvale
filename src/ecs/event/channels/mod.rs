// This file is generated (see build.rs). Please do not edit manually.
use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::ecs::event::events::*;

pub fn insert_event_channels(ecs: &mut World) {
  ecs.insert(EventChannel::<InputEvent>::new());
  ecs.insert(EventChannel::<OutputEvent>::new());
  ecs.insert(EventChannel::<EffectEvent>::new());
  ecs.insert(EventChannel::<ActionEvent>::new());
  ecs.insert(EventChannel::<CommandEvent>::new());
}
