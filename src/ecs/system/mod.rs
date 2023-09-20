use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::ecs::event::*;

pub fn run_initial_systems(ecs: &mut World) {}

/// Every tick.
pub fn get_tick_dispatcher(_ecs: &mut World) -> Dispatcher<'static, 'static> {
  let dispatcher = DispatcherBuilder::new().build();
  dispatcher
}

/// Every ten ticks.
pub fn get_deca_tick_dispatcher(ecs: &mut World) -> Dispatcher<'static, 'static> {
  let dispatcher = DispatcherBuilder::new().build();
  dispatcher
}

/// Every hundred ticks.
pub fn get_hecto_tick_dispatcher(_ecs: &mut World) -> Dispatcher<'static, 'static> {
  let dispatcher = DispatcherBuilder::new().build();
  dispatcher
}

/// Every thousand ticks.
pub fn get_kilo_tick_dispatcher(_ecs: &mut World) -> Dispatcher<'static, 'static> {
  let dispatcher = DispatcherBuilder::new().build();
  dispatcher
}
