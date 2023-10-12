use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::chunk::ChunkBuilder;
use crate::chunk::ChunkStatus;
use crate::component::*;
use crate::event::ChunkPlaneRequestEvent;
use crate::event::ChunkRequestEvent;
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
  pub seed_string_resource: Read<'data, SeedStringResource>,
  pub is_a_chunk_plane: WriteStorage<'data, IsAChunkPlaneComponent>,
}

impl<'data> System<'data> for ChunkPlaneCreator {
  type SystemData = Data<'data>;

  /// Run the system.
  fn run(&mut self, mut data: Self::SystemData) {
    debug!("Running InitialChunkPlaneCreator system.");
    for event in data.cpr_channel.read(self.reader_id.as_mut().unwrap()) {
      debug!("Creating chunk plane.");
      let chunk_plane = event.chunk_plane.clone();
      data
        .is_a_chunk_plane
        .insert(data.entities.create(), IsAChunkPlaneComponent(chunk_plane.clone()))
        .unwrap();
      data.cr_channel.single_write(ChunkRequestEvent {
        chunk_plane_uuid: chunk_plane.uuid.clone(),
        chunk: ChunkBuilder::default()
          .name("default".to_string())
          .description("The primary chunk.".to_string())
          .coordinates((0, 0).into())
          .seed_string(format!("{}::(0, 0)", chunk_plane.seed_string.clone()))
          .status(ChunkStatus::Unknown)
          .build()
          .expect("Failed to build chunk."),
      });
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
