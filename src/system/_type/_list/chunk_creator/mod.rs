use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::component::*;
use crate::event::ChunkRequestEvent;
use crate::resource::SeedStringResource;

/// The `ChunkCreator` system.
///
/// This system creates each chunk.
#[derive(Debug, Default)]
pub struct ChunkCreator {
  /// The event reader ID.
  pub reader_id: Option<ReaderId<ChunkRequestEvent>>,
}

#[derive(SystemData)]
pub struct Data<'data> {
  pub entities: Entities<'data>,
  pub seed_string_resource: Read<'data, SeedStringResource>,
  pub cp_channel: Read<'data, EventChannel<ChunkRequestEvent>>,
  pub has_a_chunk_plane: WriteStorage<'data, IsInChunkPlaneComponent>,
  pub is_a_chunk_plane: ReadStorage<'data, IsAChunkPlaneComponent>,
  pub is_a_chunk: WriteStorage<'data, IsAChunkComponent>,
}

impl<'data> System<'data> for ChunkCreator {
  type SystemData = Data<'data>;

  /// Run the system.
  fn run(&mut self, mut data: Self::SystemData) {
    debug!("Running ChunkCreator system.");
    for event in data.cp_channel.read(self.reader_id.as_mut().unwrap()) {
      debug!("Creating chunk.");
      let chunk_plane_uuid = event.chunk_plane_uuid.clone();
      let chunk_plane_entity = (&data.entities, &data.is_a_chunk_plane)
        .join()
        .find(|(_, is_a_chunk_plane)| is_a_chunk_plane.0.uuid == chunk_plane_uuid)
        .map(|(entity, _)| entity)
        .unwrap();
      let chunk_entity = data.entities.create();
      data
        .has_a_chunk_plane
        .insert(chunk_entity, IsInChunkPlaneComponent(chunk_plane_entity))
        .unwrap();
      let chunk = event.chunk.clone();
      data.is_a_chunk.insert(chunk_entity, IsAChunkComponent(chunk)).unwrap();
      let _chunk_factory = event.chunk_factory.clone();
      debug!("Created chunk.");
    }
  }

  fn setup(&mut self, world: &mut World) {
    Self::SystemData::setup(world);
    self.reader_id = Some(world.fetch_mut::<EventChannel<ChunkRequestEvent>>().register_reader());
  }
}
