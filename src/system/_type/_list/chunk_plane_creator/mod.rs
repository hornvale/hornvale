use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::component::*;
use crate::event::ChunkPlaneRequestEvent;
use crate::resource::SeedStringResource;

/// The `ChunkPlaneCreator` system.
///
/// This system creates the initial `ChunkPlane`.
#[derive(Debug, Default)]
pub struct ChunkPlaneCreator {
  /// The seed string.
  pub seed_string: Option<String>,
  /// The event reader ID.
  pub reader_id: Option<ReaderId<ChunkPlaneRequestEvent>>,
}

#[derive(SystemData)]
pub struct Data<'data> {
  pub entities: Entities<'data>,
  pub cpr_channel: Read<'data, EventChannel<ChunkPlaneRequestEvent>>,
  pub seed_string_resource: Read<'data, SeedStringResource>,
  pub is_a_chunk_plane: WriteStorage<'data, IsAChunkPlaneComponent>,
}

impl<'data> System<'data> for ChunkPlaneCreator {
  type SystemData = Data<'data>;

  /// Run the system.
  fn run(&mut self, mut data: Self::SystemData) {
    debug!("Running InitialChunkPlaneCreator system.");
    // If the seed string is not set, set it.
    self.set_seed_string(&data.seed_string_resource.0);
    for event in data.cpr_channel.read(self.reader_id.as_mut().unwrap()) {
      data
        .is_a_chunk_plane
        .insert(
          data.entities.create(),
          IsAChunkPlaneComponent(event.chunk_plane.clone()),
        )
        .unwrap();
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

impl ChunkPlaneCreator {
  /// Sets the seed string, if not already set.
  pub fn set_seed_string(&mut self, seed_string: &str) {
    if self.seed_string.is_none() {
      let seed_string = format!("{}::{}", seed_string, "initial_chunk_plane_creator");
      debug!("Setting seed string to {}.", seed_string);
      self.seed_string = Some(seed_string.to_string());
    }
  }
}
