use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::component::*;
use crate::event::ChunkPlaneRequestEvent;
use crate::event::ChunkRequestEvent;
use crate::resource::ChunkPlaneResource;
use crate::resource::SeedStringResource;

/// The `ChunkPlaneCreator` system.
///
/// This system creates the initial `ChunkPlane`, which is mostly empty.
///
/// We kick off an aditional request event to build the initial `Chunk` for the
/// `ChunkPlane`.
#[derive(Debug, Default)]
pub struct ChunkPlaneCreator {
  /// The event reader ID.
  pub reader_id: Option<ReaderId<ChunkPlaneRequestEvent>>,
}

#[derive(SystemData)]
pub struct Data<'data> {
  pub entities: Entities<'data>,
  pub cpr_channel: Read<'data, EventChannel<ChunkPlaneRequestEvent>>,
  pub cr_channel: Write<'data, EventChannel<ChunkRequestEvent>>,
  pub is_a_chunk_plane_component: WriteStorage<'data, IsAChunkPlaneComponent>,
  pub seed_string_resource: Read<'data, SeedStringResource>,
  pub chunk_plane_resource: Write<'data, ChunkPlaneResource>,
}

impl<'data> System<'data> for ChunkPlaneCreator {
  type SystemData = Data<'data>;

  /// Run the system.
  fn run(&mut self, mut data: Self::SystemData) {
    debug!("Running ChunkPlaneCreator system.");
    for event in data.cpr_channel.read(self.reader_id.as_mut().unwrap()) {
      debug!("Creating chunk plane.");
      let chunk_plane = event.chunk_plane.clone();
      let chunk_plane_entity = data.entities.create();
      data
        .is_a_chunk_plane_component
        .insert(chunk_plane_entity, IsAChunkPlaneComponent(chunk_plane.clone()))
        .unwrap();
      if data.chunk_plane_resource.entity.is_none() {
        data.chunk_plane_resource.entity = Some(chunk_plane_entity);
      }
      debug!("Created chunk plane.");
    }
  }

  fn setup(&mut self, world: &mut World) {
    Self::SystemData::setup(world);
    self.reader_id = Some(
      world
        .fetch_mut::<EventChannel<ChunkPlaneRequestEvent>>()
        .register_reader(),
    );
  }
}
