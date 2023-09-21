use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::ecs::event::*;
use crate::ecs::resource::*;

pub mod input_processor;
pub use input_processor::InputProcessor;
pub mod output_processor;
pub use output_processor::OutputProcessor;

pub fn run_initial_systems(_ecs: &mut World) {}

/// Every tick.
pub fn get_tick_dispatcher(_ecs: &mut World) -> Dispatcher<'static, 'static> {
  let dispatcher = DispatcherBuilder::new().build();
  dispatcher
}

/// Every ten ticks.
pub fn get_deca_tick_dispatcher(ecs: &mut World) -> Dispatcher<'static, 'static> {
  let output = {
    let output_resource = ecs.read_resource::<OutputResource>();
    output_resource.0.as_ref().unwrap().clone()
  };
  let output_processor_system = {
    let reader_id = ecs.fetch_mut::<EventChannel<OutputEvent>>().register_reader();
    OutputProcessor { reader_id, output }
  };
  let input_processor_system = {
    let reader_id = ecs.fetch_mut::<EventChannel<InputEvent>>().register_reader();
    InputProcessor { reader_id }
  };
  let dispatcher = DispatcherBuilder::new()
    .with(output_processor_system, "output_processor", &[])
    .with(input_processor_system, "input_processor", &[])
    .build();
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
