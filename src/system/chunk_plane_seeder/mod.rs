use rand::prelude::*;
use rand_seeder::SipHasher;
use specs::prelude::*;
use std::collections::HashSet;

use crate::component::ChunkPlaneComponent;
use crate::component::ChunkSeedComponent;
use crate::component::ChunkSeedStatus;
use crate::resource::SeedStringResource;

/// The `ChunkPlaneSeeder` system.
///
/// This system seeds the `ChunkPlane` component with ChunkSeeds.
#[derive(Debug, Default)]
pub struct ChunkPlaneSeeder {
  /// The seed string.
  pub seed_string: Option<String>,
}

impl ChunkPlaneSeeder {}

#[derive(SystemData)]
pub struct Data<'data> {
  pub entities: Entities<'data>,
  pub seed_string_resource: Read<'data, SeedStringResource>,
  pub chunk_plane: WriteStorage<'data, ChunkPlaneComponent>,
  pub chunk_seed: WriteStorage<'data, ChunkSeedComponent>,
}

impl<'data> System<'data> for ChunkPlaneSeeder {
  type SystemData = Data<'data>;

  /// Run the system.
  fn run(&mut self, mut data: Self::SystemData) {
    debug!("Running chunk plane creator system...");
    if self.seed_string.is_none() {
      debug!("Setting seed string...");
      self.seed_string = Some(format!("{}/{}", data.seed_string_resource.0, "chunk_plane_seeder"));
    }
    debug!("Counting chunk planes...");
    let chunk_planes = data.chunk_plane.join().collect::<Vec<&ChunkPlaneComponent>>();
    if chunk_planes.is_empty() {
      debug!("Chunk plane not yet created; skipping.");
      return;
    }
    debug!("Looking for unseeded chunk planes...");
    if chunk_planes.iter().all(|&chunk_plane| chunk_plane.is_seeded) {
      debug!("Chunk planes already seeded; skipping.");
      return;
    }
    // For each of the unseeded chunk planes...
    for (entity, chunk_plane) in (&data.entities, &mut data.chunk_plane).join() {
      if chunk_plane.is_seeded {
        continue;
      }
      debug!("Creating chunk seeds for chunk plane...");
      let mut chunk_seeds = HashSet::new();
      let mut rng = SipHasher::from(&chunk_plane.seed_string).into_rng();
      while chunk_seeds.len() < 10 {
        let x = rng.gen_range(chunk_plane.upper_left_corner.0..=chunk_plane.lower_right_corner.0);
        let y = rng.gen_range(chunk_plane.upper_left_corner.1..=chunk_plane.lower_right_corner.1);
        chunk_seeds.insert((x, y));
      }
      debug!("Inserting chunk seeds...");
      for (x, y) in chunk_seeds {
        let chunk_seed = ChunkSeedComponent {
          chunk_plane: entity,
          coordinates: (x, y),
          points: HashSet::new(),
          neighbors: Vec::new(),
          status: ChunkSeedStatus::Unknown,
        };
        data.chunk_seed.insert(data.entities.create(), chunk_seed).unwrap();
      }
      debug!("Marking chunk plane as seeded...");
      chunk_plane.is_seeded = true;
    }
  }
}
