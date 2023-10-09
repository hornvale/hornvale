use rand::prelude::*;
use rand_seeder::SipHasher;
use specs::prelude::*;
use std::collections::HashSet;
use uuid::Uuid;

use crate::component::ChunkPlaneComponent;
use crate::component::ChunkSeedComponent;
use crate::component::ChunkSeedType;
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
    let unseeded_chunk_planes = chunk_planes
      .iter()
      .filter(|&chunk_plane| !chunk_plane.is_seeded)
      .cloned()
      .collect::<Vec<&ChunkPlaneComponent>>();
    if unseeded_chunk_planes.is_empty() {
      debug!("Chunk planes already seeded; skipping.");
      return;
    }
    debug!("Seeding chunk plane...");
    let seed_count = 150;
    for chunk_plane in unseeded_chunk_planes {
      let seed_string = format!("{}/{}", self.seed_string.as_ref().unwrap(), chunk_plane.id.clone());
      let mut rng = SipHasher::from(&seed_string).into_rng();
      for _ in 0..seed_count {
        let (x, y) = (
          rng.gen_range(chunk_plane.upper_left_corner.0..=chunk_plane.lower_right_corner.0),
          rng.gen_range(chunk_plane.upper_left_corner.1..=chunk_plane.lower_right_corner.1),
        );
        let entity = data.entities.create();
        let chunk_seed = ChunkSeedComponent {
          chunk_plane_id: chunk_plane.id.clone(),
          chunk_id: None,
          id: Uuid::new_v4().to_string(),
          coordinates: (x, y),
          points: HashSet::new(),
          adjacent_chunk_seed_ids: Vec::new(),
          chunk_seed_type: ChunkSeedType::Unknown,
        };
        data.chunk_seed.insert(entity, chunk_seed).unwrap();
      }
    }
  }
}

/*
use anyhow::Error as AnyError;
use rand::prelude::*;
use rand_seeder::SipHasher;
use std::collections::HashMap;
use std::collections::HashSet;
use uuid::Uuid;

use crate::chunk::Chunk;
use crate::chunk::ChunkBuilder;
use crate::chunk_seed::ChunkSeed;
use crate::chunk_seed::ChunkSeedBuilder;
use crate::chunk_seed::ChunkSeedType;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::entity_id::ChunkSeedId;

pub mod export_png;
pub use export_png::export_chunk_plane_png;
pub mod file_manager;
pub use file_manager::FileManager as ChunkPlaneFileManager;

/// The `ChunkPlane` struct.
///
/// Chunks are generated from chunk seeds, which are distributed randomly on an
/// infinite plane. We'll start with a 100x100 square of chunk seeds, and then
/// expand outwards as needed.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ChunkPlane {
  /// The ID.
  pub id: ChunkPlaneId,
  /// The `Chunk` IDs.
  pub chunk_ids: HashSet<ChunkId>,
  /// The `ChunkSeed`s.
  pub chunk_seeds: HashMap<ChunkSeedId, ChunkSeed>,
  /// The upper-left corner of the `ChunkPlane` in (i64, i64) plane.
  pub upper_left_corner: (i64, i64),
  /// The lower-right corner of the `ChunkPlane` in (i64, i64) plane.
  pub lower_right_corner: (i64, i64),
  /// The seed string.
  pub seed_string: String,
}

impl ChunkPlane {
  /// Creates a new `ChunkPlane`.
  pub fn new(id: &ChunkPlaneId, seed_string: &str) -> Self {
    let id = id.clone();
    let upper_left_corner = (-50, -50);
    let lower_right_corner = (50, 50);
    let seed_string = seed_string.to_string();
    Self {
      id,
      chunk_ids: HashSet::new(),
      chunk_seeds: HashMap::new(),
      upper_left_corner,
      lower_right_corner,
      seed_string,
    }
  }

  /// Generates the `ChunkSeed`s.
  pub fn generate_initial_chunk_seeds(&mut self) -> Result<(), AnyError> {
    if !self.chunk_seeds.is_empty() {
      return Err(anyhow!("Chunk seeds already generated."));
    }
    let mut chunk_seeds = HashMap::new();
    let count = 150;
    let mut rng = SipHasher::from(&self.seed_string).into_rng();
    // Randomly generate the chunk seeds.
    for _ in 0..count {
      let (x, y) = (
        rng.gen_range(self.upper_left_corner.0..=self.lower_right_corner.0),
        rng.gen_range(self.upper_left_corner.1..=self.lower_right_corner.1),
      );
      let chunk_seed = ChunkSeedBuilder::new()
        .chunk_plane_id(&self.id)
        .coordinates((x, y))
        .build();
      chunk_seeds.insert(chunk_seed.id.clone(), chunk_seed);
    }
    self.chunk_seeds = chunk_seeds.clone();
    Ok(())
  }

  /// Generates the `Chunk`s.
  pub fn generate_initial_chunks(&mut self) -> Result<Vec<Chunk>, AnyError> {
    if !self.chunk_ids.is_empty() {
      return Err(anyhow!("Chunks already generated."));
    }
    // Generate the initial chunk seeds.
    self.generate_initial_chunk_seeds()?;
    // For every coordinate pair in the range, we'll determine which chunk seed
    // it belongs to. That means determining the distances between the pair and
    // each chunk seed. The coordinate pair belongs to the chunk seed with the
    // smallest distance. We'll then add the  pair to the chunk seed's points.
    let chunk_adjacencies = self.calculate_chunk_adjacencies()?;
    // Now, we'll insert the adjacent chunk seeds into each chunk seed.
    self.update_adjacencies(&chunk_adjacencies)?;
    // Now, we'll determine which chunk seeds are "open", "half-open", and
    // "closed". These terms refer to the vulnerability of the seeds to
    // modification as the diagram is expanded outwards.
    // Any seed whose corner touches a point along the edge of the diagram
    // is considered "open". Any seed next to an "open" seed is considered
    // "half-open". All remaining seeds are considered "closed".
    self.mark_open_chunk_seeds()?;
    // Now, we'll iterate through the chunk seeds and determine which ones are
    // "half-open", that is, which ones are next to an "open" chunk seed.
    self.mark_half_open_chunk_seeds()?;
    // Now, we'll iterate through the chunk seeds and determine which ones are
    // "closed", that is, which ones are not "open" or "half-open".
    self.mark_closed_chunk_seeds()?;
    // Now, we'll generate the chunks from the closed chunk seeds.
    let chunks = self.generate_chunks()?;
    Ok(chunks)
  }

  /// Calculate adjacencies between `ChunkSeed`s.
  pub fn calculate_chunk_adjacencies(&mut self) -> Result<HashSet<(ChunkSeedId, ChunkSeedId)>, AnyError> {
    let mut chunk_adjacencies = HashSet::<(ChunkSeedId, ChunkSeedId)>::new();
    let mut last_columns_chunk_seed_ids =
      vec![ChunkSeedId::default(); (self.lower_right_corner.1 - self.upper_left_corner.1 + 1) as usize];
    for x in self.upper_left_corner.0..=self.lower_right_corner.0 {
      let col_index = (x - self.upper_left_corner.0) as usize;
      let mut last_chunk_seed_id = ChunkSeedId::default();
      for y in self.upper_left_corner.1..=self.lower_right_corner.1 {
        let row_index = (y - self.upper_left_corner.1) as usize;
        let current_coordinates = (x, y);
        let mut closest_chunk_seed = None;
        for chunk_seed in self.chunk_seeds.values() {
          let chunk_seed_coordinates = chunk_seed.coordinates;
          let distance = (((chunk_seed_coordinates.0 - current_coordinates.0).pow(2)
            + (chunk_seed_coordinates.1 - current_coordinates.1).pow(2)) as f64)
            .sqrt();
          if closest_chunk_seed.is_none() {
            closest_chunk_seed = Some((chunk_seed, distance));
          } else {
            let (_, closest_distance) = closest_chunk_seed.unwrap();
            if distance < closest_distance {
              closest_chunk_seed = Some((chunk_seed, distance));
            }
          }
        }
        let (chunk_seed, _) = closest_chunk_seed.unwrap();
        let chunk_seed_id = chunk_seed.id.clone();
        let chunk_seed = self.chunk_seeds.get_mut(&chunk_seed_id).unwrap();
        if row_index == 0 && col_index == 0 {
          last_chunk_seed_id = chunk_seed_id.clone();
        }
        if chunk_seed_id != last_chunk_seed_id {
          chunk_adjacencies.insert((last_chunk_seed_id.clone(), chunk_seed_id.clone()));
          chunk_adjacencies.insert((chunk_seed_id.clone(), last_chunk_seed_id.clone()));
        }
        last_chunk_seed_id = chunk_seed_id.clone();
        if col_index > 0 {
          let last_columns_chunk_seed_id = last_columns_chunk_seed_ids.get(row_index).unwrap();
          if chunk_seed_id != *last_columns_chunk_seed_id {
            chunk_adjacencies.insert((last_columns_chunk_seed_id.clone(), chunk_seed_id.clone()));
            chunk_adjacencies.insert((chunk_seed_id.clone(), last_columns_chunk_seed_id.clone()));
          }
        }
        last_columns_chunk_seed_ids[row_index] = chunk_seed_id.clone();
        chunk_seed.points.insert(current_coordinates);
      }
    }
    Ok(chunk_adjacencies)
  }

  /// Update adjacencies between `ChunkSeed`s.
  pub fn update_adjacencies(&mut self, adjacencies: &HashSet<(ChunkSeedId, ChunkSeedId)>) -> Result<(), AnyError> {
    for (chunk_seed_id, chunk_seed) in self.chunk_seeds.iter_mut() {
      let mut adjacent_chunk_seed_ids = HashSet::<ChunkSeedId>::new();
      for (chunk_seed_id_1, chunk_seed_id_2) in adjacencies.iter() {
        if chunk_seed_id_1 == chunk_seed_id {
          adjacent_chunk_seed_ids.insert(chunk_seed_id_2.clone());
        } else if chunk_seed_id_2 == chunk_seed_id {
          adjacent_chunk_seed_ids.insert(chunk_seed_id_1.clone());
        }
      }
      chunk_seed.adjacent_chunk_seed_ids = adjacent_chunk_seed_ids.iter().cloned().collect();
    }
    Ok(())
  }

  /// Mark "open" `ChunkSeed`s.
  pub fn mark_open_chunk_seeds(&mut self) -> Result<(), AnyError> {
    for x in self.upper_left_corner.0..=self.lower_right_corner.0 {
      for y in self.upper_left_corner.1..=self.lower_right_corner.1 {
        if x == self.upper_left_corner.0
          || x == self.lower_right_corner.0
          || y == self.upper_left_corner.1
          || y == self.lower_right_corner.1
        {
          let chunk_seed_id = self.get_chunk_seed_id((x, y)).unwrap();
          let chunk_seed = self.chunk_seeds.get_mut(&chunk_seed_id).unwrap();
          chunk_seed.chunk_seed_type = ChunkSeedType::Open;
        }
      }
    }
    Ok(())
  }

  /// Mark "half-open" `ChunkSeed`s.
  pub fn mark_half_open_chunk_seeds(&mut self) -> Result<(), AnyError> {
    let mut chunk_seed_ids = Vec::<ChunkSeedId>::new();
    for chunk_seed in self.chunk_seeds.values() {
      if chunk_seed.chunk_seed_type == ChunkSeedType::Unknown {
        for adjacent_chunk_seed_id in chunk_seed.adjacent_chunk_seed_ids.iter() {
          let adjacent_chunk_seed = self.chunk_seeds.get(adjacent_chunk_seed_id).unwrap();
          if adjacent_chunk_seed.chunk_seed_type == ChunkSeedType::Open {
            chunk_seed_ids.push(chunk_seed.id.clone());
            break;
          }
        }
      }
    }
    for chunk_seed_id in chunk_seed_ids {
      let chunk_seed = self.chunk_seeds.get_mut(&chunk_seed_id).unwrap();
      chunk_seed.chunk_seed_type = ChunkSeedType::HalfOpen;
    }
    Ok(())
  }

  /// Mark "closed" `ChunkSeed`s.
  pub fn mark_closed_chunk_seeds(&mut self) -> Result<(), AnyError> {
    for chunk_seed in self.chunk_seeds.values_mut() {
      if chunk_seed.chunk_seed_type == ChunkSeedType::Unknown {
        chunk_seed.chunk_seed_type = ChunkSeedType::Closed;
      }
    }
    Ok(())
  }

  /// Get the `ChunkSeedId`s of the `ChunkSeed`s bordering a given `ChunkSeed`.
  pub fn get_chunk_seed_neighbor_ids(&self, chunk_seed_id: &ChunkSeedId) -> Vec<ChunkSeedId> {
    let chunk_seed = self.chunk_seeds.get(chunk_seed_id).unwrap();
    chunk_seed.adjacent_chunk_seed_ids.clone()
  }

  /// Get the `ChunkSeedId` for the given coordinates.
  pub fn get_chunk_seed_id(&self, coordinates: (i64, i64)) -> Option<ChunkSeedId> {
    for chunk_seed in self.chunk_seeds.values() {
      if chunk_seed.contains(coordinates) {
        return Some(chunk_seed.id.clone());
      }
    }
    None
  }

  /// Generate chunks from the given `ChunkSeed`s.
  pub fn generate_chunks(&mut self) -> Result<Vec<Chunk>, AnyError> {
    let chunk_seeds = self
      .chunk_seeds
      .values()
      .filter(|&chunk_seed| chunk_seed.chunk_seed_type == ChunkSeedType::Closed && chunk_seed.chunk_id.is_none())
      .cloned()
      .collect::<Vec<ChunkSeed>>();
    let mut chunks = Vec::<Chunk>::new();
    for chunk_seed in chunk_seeds {
      let chunk_seed_id = chunk_seed.id.clone();
      let chunk_seed = self.chunk_seeds.get_mut(&chunk_seed_id).unwrap();
      let chunk_id = ChunkId::default();
      chunk_seed.chunk_id = Some(chunk_id.clone());
      let chunk = ChunkBuilder::new()
        .id(&chunk_id)
        .chunk_plane_id(&self.id)
        .chunk_seed(chunk_seed)
        .name(&format!(
          "Chunk ({}, {})",
          chunk_seed.coordinates.0, chunk_seed.coordinates.1
        ))
        .description(&format!(
          "This is a chunk at ({}, {}).",
          chunk_seed.coordinates.0, chunk_seed.coordinates.1
        ))
        .build();
      self.chunk_ids.insert(chunk_id);
      chunks.push(chunk);
    }
    Ok(chunks)
  }

  /// Get the `ChunkSeed` for the given coordinates.
  pub fn get_chunk_seed(&self, coordinates: (i64, i64)) -> Option<&ChunkSeed> {
    self
      .chunk_seeds
      .values()
      .find(|&chunk_seed| chunk_seed.contains(coordinates))
  }
}

impl Default for ChunkPlane {
  fn default() -> Self {
    let id = ChunkPlaneId::default();
    let seed_string = Uuid::new_v4().to_string();
    Self::new(&id, &seed_string)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use anyhow::Error as AnyError;

  use crate::test::init;

  #[test]
  fn test_chunk_plane() -> Result<(), AnyError> {
    init();
    let mut chunk_plane = ChunkPlane::default();
    chunk_plane.id = ChunkPlaneId::default();
    chunk_plane.generate_initial_chunks()?;
    Ok(())
  }
}
*/
