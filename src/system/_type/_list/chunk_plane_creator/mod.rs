use specs::prelude::*;

use crate::chunk::ChunkPlaneBuilder;
use crate::component::*;
use crate::resource::SeedStringResource;

/// The `ChunkPlaneCreator` system.
///
/// This system creates the initial `ChunkPlane`.
#[derive(Debug, Default)]
pub struct ChunkPlaneCreator {
  /// The seed string.
  seed_string: Option<String>,
}

#[derive(SystemData)]
pub struct Data<'data> {
  pub entities: Entities<'data>,
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
    // If there is already a chunk plane, do nothing.
    if self.has_chunk_plane(&data) {
      return;
    }
    // Otherwise, create the chunk plane...
    debug!("Creating chunk plane.");
    let chunk_plane = ChunkPlaneBuilder::default()
      .name("default".to_string())
      .seed_string(format!("{}{}", self.seed_string.as_ref().unwrap(), "primary"))
      .description("The primary chunk plane.".to_string())
      .build()
      .expect("Failed to build chunk plane.");
    // ... and insert it.
    data
      .is_a_chunk_plane
      .insert(data.entities.create(), IsAChunkPlaneComponent(chunk_plane))
      .unwrap();
    debug!("Created chunk plane.");
  }
}

impl<'data> ChunkPlaneCreator {
  /// Sets the seed string, if not already set.
  pub fn set_seed_string(&mut self, seed_string: &str) {
    if self.seed_string.is_none() {
      let seed_string = format!("{}::{}", seed_string, "initial_chunk_plane_creator");
      debug!("Setting seed string to {}.", seed_string);
      self.seed_string = Some(seed_string.to_string());
    }
  }

  /// Determine whether there is at least one chunk plane in existence.
  pub fn has_chunk_plane(&self, data: &Data<'data>) -> bool {
    let is_a_chunk_plane = &data.is_a_chunk_plane;
    !is_a_chunk_plane.is_empty()
  }
}
