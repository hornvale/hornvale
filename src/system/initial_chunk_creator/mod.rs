use specs::prelude::*;

use crate::chunk::ChunkBuilder;
use crate::chunk::ChunkStatus;
use crate::component::*;
use crate::resource::SeedStringResource;

/// The `InitialChunkCreator` system.
///
/// This system creates the initial `Chunk`.
#[derive(Debug, Default)]
pub struct InitialChunkCreator {
  /// The seed string.
  seed_string: Option<String>,
}

#[derive(SystemData)]
pub struct Data<'data> {
  pub entities: Entities<'data>,
  pub seed_string_resource: Read<'data, SeedStringResource>,
  pub is_a_chunk_plane: ReadStorage<'data, IsAChunkPlaneComponent>,
  pub is_a_chunk: WriteStorage<'data, IsAChunkComponent>,
}

impl<'data> System<'data> for InitialChunkCreator {
  type SystemData = Data<'data>;

  /// Run the system.
  fn run(&mut self, mut data: Self::SystemData) {
    debug!("Running InitialChunkCreator system.");
    // If there is not already a chunk plane, panic.
    if !self.has_chunk_plane(&data) {
      panic!("No chunk plane exists.");
    }
    // Otherwise, set the seed string.
    self.set_seed_string(&mut data);
    // Otherwise, create the initial chunk...
    let _chunk = ChunkBuilder::default()
      .name("default".to_string())
      .seed_string(self.seed_string.as_ref().unwrap().to_string())
      .description("The initial chunk.".to_string())
      .status(ChunkStatus::Unknown)
      .coordinates((0, 0).into())
      .build()
      .expect("Failed to build chunk.");
  }
}

impl<'data> InitialChunkCreator {
  /// Sets the seed string, if not already set.
  pub fn set_seed_string(&mut self, data: &mut Data<'data>) {
    if self.seed_string.is_none() {
      // Otherwise, let's get the primary chunk plane.
      let chunk_plane_seed_string = self.get_chunk_plane_seed_string(data);
      let seed_string = format!("{}::{}", chunk_plane_seed_string, "initial_chunk_generator");
      debug!("Setting seed string to {}.", seed_string);
      self.seed_string = Some(seed_string.to_string());
    }
  }

  /// Determine whether there is at least one chunk plane in existence.
  pub fn has_chunk_plane(&self, data: &Data<'data>) -> bool {
    let is_a_chunk_plane = &data.is_a_chunk_plane;
    !is_a_chunk_plane.is_empty()
  }

  /// Retrieve the primary chunk plane.
  pub fn get_chunk_plane_seed_string(&self, data: &Data<'data>) -> String {
    let is_a_chunk_plane = &data.is_a_chunk_plane;
    let result = is_a_chunk_plane.join().next().expect("Failed to get chunk plane.");
    result.0.seed_string.clone()
  }
}
