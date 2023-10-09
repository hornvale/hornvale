use specs::prelude::*;
use uuid::Uuid;

use crate::component::ChunkPlaneComponent;
use crate::resource::SeedStringResource;

/// The `ChunkPlaneCreator` system.
///
/// This system creates the `ChunkPlane` component.
#[derive(Debug, Default)]
pub struct ChunkPlaneCreator {
  /// The seed string.
  seed_string: Option<String>,
}

impl ChunkPlaneCreator {}

#[derive(SystemData)]
pub struct Data<'data> {
  pub entities: Entities<'data>,
  pub seed_string_resource: Read<'data, SeedStringResource>,
  pub chunk_plane: WriteStorage<'data, ChunkPlaneComponent>,
}

impl<'data> System<'data> for ChunkPlaneCreator {
  type SystemData = Data<'data>;

  /// Run the system.
  fn run(&mut self, mut data: Self::SystemData) {
    debug!("Running chunk plane creator system...");
    if self.seed_string.is_none() {
      debug!("Setting seed string...");
      self.seed_string = Some(format!("{}/{}", data.seed_string_resource.0, "chunk_plane_creator"));
    }
    debug!("Counting chunk planes...");
    let chunk_planes = data.chunk_plane.join().collect::<Vec<&ChunkPlaneComponent>>();
    if !chunk_planes.is_empty() {
      debug!("Chunk plane already created; skipping.");
      return;
    }
    debug!("Creating chunk plane...");
    let entity = data.entities.create();
    let uuid = Uuid::new_v4().to_string();
    let seed_string = format!("{}/{}", self.seed_string.as_ref().unwrap(), uuid);
    let chunk_plane = ChunkPlaneComponent {
      seed_string,
      upper_left_corner: (-50, -50),
      lower_right_corner: (50, 50),
      is_seeded: false,
    };
    data.chunk_plane.insert(entity, chunk_plane).unwrap();
  }
}
