use anyhow::Error as AnyError;
use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::event::{InputEvent, OutputEvent};

pub mod input;
pub use input::Input as InputSystem;
pub mod output;
pub use output::Output as OutputSystem;
pub mod tick;
pub use tick::Tick as TickSystem;

pub fn get_initial_dispatcher(_ecs: &mut World) -> Dispatcher<'static, 'static> {
  let dispatcher = DispatcherBuilder::new().build();
  dispatcher
}

pub fn run_initial_systems(ecs: &mut World) -> Result<(), AnyError> {
  let mut dispatcher = get_initial_dispatcher(ecs);
  dispatcher.dispatch(ecs);
  Ok(())
}

pub fn get_tick_dispatcher(_ecs: &mut World) -> Dispatcher<'static, 'static> {
  let tick_system = TickSystem {};
  let dispatcher = DispatcherBuilder::new().with(tick_system, "tick", &[]).build();
  dispatcher
}

/// Every ten ticks.
pub fn get_deca_tick_dispatcher(ecs: &mut World) -> Dispatcher<'static, 'static> {
  let input_system = {
    let reader_id = ecs.fetch_mut::<EventChannel<InputEvent>>().register_reader();
    InputSystem { reader_id }
  };
  let output_system = {
    let reader_id = ecs.fetch_mut::<EventChannel<OutputEvent>>().register_reader();
    OutputSystem { reader_id }
  };
  let dispatcher = DispatcherBuilder::new()
    .with(input_system, "input", &[])
    .with(output_system, "output", &[])
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
