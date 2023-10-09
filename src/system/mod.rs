use anyhow::Error as AnyError;
use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::event::{InputEvent, OutputEvent};

pub mod output;
pub use output::Output as OutputSystem;
pub mod parser;
pub use parser::Parser as ParserSystem;
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

/// The standard dispatcher.
pub fn get_dispatcher(ecs: &mut World) -> Dispatcher<'static, 'static> {
  let tick_system = TickSystem {};
  let parser_system = {
    let reader_id = ecs.fetch_mut::<EventChannel<InputEvent>>().register_reader();
    ParserSystem { reader_id }
  };
  let output_system = {
    let reader_id = ecs.fetch_mut::<EventChannel<OutputEvent>>().register_reader();
    OutputSystem { reader_id }
  };
  let dispatcher = DispatcherBuilder::new()
    .with(tick_system, "tick", &[])
    .with(parser_system, "parser", &[])
    .with(output_system, "output", &[])
    .build();
  dispatcher
}
