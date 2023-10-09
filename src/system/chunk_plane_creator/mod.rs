use specs::prelude::*;

use crate::resource::SeedStringResource;

/// The `ChunkPlaneCreator` system.
///
/// This system creates the `ChunkPlane` component.
#[derive(Debug, Default)]
pub struct ChunkPlaneCreator {
  /// The seed string.
  pub seed_string: Option<String>,
}

#[derive(SystemData)]
pub struct Data<'data> {
  pub entities: Entities<'data>,
  pub seed_string_resource: Read<'data, SeedStringResource>,
}

impl<'data> System<'data> for ChunkPlaneCreator {
  type SystemData = Data<'data>;

  /// Run the system.
  fn run(&mut self, mut _data: Self::SystemData) {}
}
