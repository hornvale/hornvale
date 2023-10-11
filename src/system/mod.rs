use anyhow::Error as AnyError;
use specs::prelude::*;

pub mod initial_chunk_creator;
pub use initial_chunk_creator::InitialChunkCreator as InitialChunkCreatorSystem;
pub mod initial_chunk_plane_creator;
pub use initial_chunk_plane_creator::InitialChunkPlaneCreator as InitialChunkPlaneCreatorSystem;
pub mod output;
pub use output::Output as OutputSystem;
pub mod parser;
pub use parser::Parser as ParserSystem;
pub mod tick;
pub use tick::Tick as TickSystem;

pub fn run_initial_systems(ecs: &mut World) -> Result<(), AnyError> {
  InitialChunkPlaneCreatorSystem::default().run_now(ecs);
  InitialChunkCreatorSystem::default().run_now(ecs);
  Ok(())
}

/// The standard dispatcher.
pub fn get_simulation_dispatcher(ecs: &mut World) -> Dispatcher<'static, 'static> {
  let parser_system = {
    let mut parser_system = ParserSystem::default();
    System::setup(&mut parser_system, ecs);
    parser_system
  };
  let output_system = {
    let mut output_system = OutputSystem::default();
    System::setup(&mut output_system, ecs);
    output_system
  };
  let dispatcher = DispatcherBuilder::new()
    .with(TickSystem::default(), "tick", &[])
    .with(parser_system, "parser", &["tick"])
    .with(output_system, "output", &["parser"])
    .build();
  dispatcher
}
