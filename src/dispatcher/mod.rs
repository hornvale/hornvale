use specs::prelude::*;

use crate::system::*;

pub fn get_initial_dispatcher(ecs: &mut World) -> Dispatcher<'static, 'static> {
  let mut dispatcher = DispatcherBuilder::new()
    .with(ChunkPlaneCreatorSystem::default(), "chunk_plane_creator", &[])
    .with(ChunkCreatorSystem::default(), "chunk_creator", &["chunk_plane_creator"])
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

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_get_initial_dispatcher() {
    let mut ecs = World::new();
    let _dispatcher = get_initial_dispatcher(&mut ecs);
  }

  #[test]
  fn test_get_simulation_dispatcher() {
    let mut ecs = World::new();
    let _dispatcher = get_simulation_dispatcher(&mut ecs);
  }
}
