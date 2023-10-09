use anyhow::Error as AnyError;
use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::event::{InputEvent, OutputEvent};

pub mod chunk_plane_creator;
pub use chunk_plane_creator::ChunkPlaneCreator as ChunkPlaneCreatorSystem;
pub mod chunk_plane_seeder;
pub use chunk_plane_seeder::ChunkPlaneSeeder as ChunkPlaneSeederSystem;
pub mod output;
pub use output::Output as OutputSystem;
pub mod parser;
pub use parser::Parser as ParserSystem;
pub mod tick;
pub use tick::Tick as TickSystem;

pub fn get_initial_dispatcher(_ecs: &mut World) -> Dispatcher<'static, 'static> {
  let chunk_plane_creator = ChunkPlaneCreatorSystem::default();
  let chunk_plane_seeder = ChunkPlaneSeederSystem::default();
  let dispatcher = DispatcherBuilder::new()
    .with(chunk_plane_creator, "chunk_plane_creator", &[])
    .with(chunk_plane_seeder, "chunk_plane_seeder", &["chunk_plane_creator"])
    .build();
  dispatcher
}

pub fn run_initial_systems(ecs: &mut World) -> Result<(), AnyError> {
  let mut dispatcher = get_initial_dispatcher(ecs);
  dispatcher.dispatch(ecs);
  Ok(())
}

/// The standard dispatcher.
pub fn get_dispatcher(ecs: &mut World) -> Dispatcher<'static, 'static> {
  let tick = TickSystem {};
  let parser = {
    let reader_id = ecs.fetch_mut::<EventChannel<InputEvent>>().register_reader();
    ParserSystem { reader_id }
  };
  let output = {
    let reader_id = ecs.fetch_mut::<EventChannel<OutputEvent>>().register_reader();
    OutputSystem { reader_id }
  };
  let dispatcher = DispatcherBuilder::new()
    .with(tick, "tick", &[])
    .with(parser, "parser", &["tick"])
    .with(output, "output", &["parser"])
    .build();
  dispatcher
}
