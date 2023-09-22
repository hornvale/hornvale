use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::command::ParsingStrategy;
use crate::command::SimpleParsingStrategy;
use crate::ecs::event::*;
use crate::ecs::resource::*;
use crate::ecs::system::*;

pub fn run_initial_systems(_ecs: &mut World) {}

/// Every tick.
pub fn get_tick_dispatcher(ecs: &mut World) -> Dispatcher<'static, 'static> {
  let output_processor_system = {
    let output = {
      let output_resource = ecs.read_resource::<OutputResource>();
      output_resource.0.as_ref().unwrap().clone()
    };
    let reader_id = ecs.fetch_mut::<EventChannel<OutputEvent>>().register_reader();
    OutputProcessorSystem { reader_id, output }
  };
  let input_processor_system = {
    let reader_id = ecs.fetch_mut::<EventChannel<InputEvent>>().register_reader();
    let parsers: Vec<Box<dyn ParsingStrategy>> =
      vec![Box::<SimpleParsingStrategy>::default() as Box<dyn ParsingStrategy + Send + Sync>];
    InputProcessorSystem { reader_id, parsers }
  };
  let command_processor_system = {
    let reader_id = ecs.fetch_mut::<EventChannel<CommandEvent>>().register_reader();
    CommandProcessorSystem { reader_id }
  };
  let dispatcher = DispatcherBuilder::new()
    .with(output_processor_system, "output_processor", &[])
    .with(input_processor_system, "input_processor", &[])
    .with(command_processor_system, "command_processor", &[])
    .build();
  dispatcher
}

/// Every ten ticks.
pub fn get_deca_tick_dispatcher(_ecs: &mut World) -> Dispatcher<'static, 'static> {
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
