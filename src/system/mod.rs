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

pub fn get_initial_dispatcher(ecs: &mut World) -> Dispatcher<'static, 'static> {
  let mut dispatcher = DispatcherBuilder::new()
    .with(
      InitialChunkPlaneCreatorSystem::default(),
      "initial_chunk_plane_creator",
      &[],
    )
    .with(
      InitialChunkCreatorSystem::default(),
      "initial_chunk_creator",
      &["initial_chunk_plane_creator"],
    )
    .build();
  dispatcher.setup(ecs);
  dispatcher
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
  let mut dispatcher = DispatcherBuilder::new()
    .with(TickSystem::default(), "tick", &[])
    .with(parser_system, "parser", &["tick"])
    .with(output_system, "output", &["parser"])
    .build();
  dispatcher.setup(ecs);
  dispatcher
}
