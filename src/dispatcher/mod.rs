use specs::prelude::*;

use crate::system::*;

pub fn get_initial_dispatcher(ecs: &mut World) -> Dispatcher<'static, 'static> {
  let mut dispatcher = DispatcherBuilder::new()
    .with(ChunkPlaneCreatorSystem::default(), "chunk_plane_creator", &[])
    .with(ChunkCreatorSystem::default(), "chunk_creator", &["chunk_plane_creator"])
    .with(RoomCreatorSystem::default(), "room_creator", &["chunk_creator"])
    .with(InsertPlayerSystem::default(), "insert_player", &["room_creator"])
    .build();
  dispatcher.setup(ecs);
  dispatcher
}

/// The standard dispatcher.
pub fn get_simulation_dispatcher(ecs: &mut World) -> Dispatcher<'static, 'static> {
  let mut dispatcher = DispatcherBuilder::new()
    .with(TickSystem::default(), "tick", &[])
    .with(ChunkPlaneCreatorSystem::default(), "chunk_plane_creator", &[])
    .with(ChunkCreatorSystem::default(), "chunk_creator", &["chunk_plane_creator"])
    .with(RoomCreatorSystem::default(), "room_creator", &["chunk_creator"])
    .with(InsertPlayerSystem::default(), "insert_player", &["room_creator"])
    .with(ParserSystem::default(), "parser", &["tick"])
    .with(CommandSystem::default(), "command", &["parser"])
    .with(OutputSystem::default(), "output", &["command"])
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
