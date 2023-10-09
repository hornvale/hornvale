use specs::prelude::*;
use uuid::Uuid;

use crate::component::ChunkPlaneComponent;

/// The `ChunkPlaneCreator` system.
///
/// This system creates the `ChunkPlane` component.
#[derive(Debug, Default)]
pub struct ChunkPlaneCreator {}

impl ChunkPlaneCreator {}

#[derive(SystemData)]
pub struct Data<'data> {
  pub entities: Entities<'data>,
  pub chunk_plane: WriteStorage<'data, ChunkPlaneComponent>,
}

impl<'data> System<'data> for ChunkPlaneCreator {
  type SystemData = Data<'data>;

  /// Run the system.
  fn run(&mut self, mut data: Self::SystemData) {
    debug!("Running chunk plane creator system...");
    debug!("Counting chunk planes...");
    let chunk_planes = data.chunk_plane.join().collect::<Vec<&ChunkPlaneComponent>>();
    if !chunk_planes.is_empty() {
      debug!("Chunk plane already created; skipping.");
      return;
    }
    debug!("Creating chunk plane...");
    let entity = data.entities.create();
    let chunk_plane = ChunkPlaneComponent {
      id: Uuid::new_v4().to_string(),
      upper_left_corner: (-50, -50),
      lower_right_corner: (50, 50),
      is_seeded: false,
    };
    data.chunk_plane.insert(entity, chunk_plane).unwrap();
  }
}
