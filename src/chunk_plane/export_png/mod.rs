use anyhow::Error as AnyError;
use image::{ImageBuffer, Rgb};

use crate::chunk_plane::ChunkPlane;
use crate::chunk_seed::ChunkSeedType;

/// To export a PNG from a chunk plane.
pub fn export_chunk_plane_png(chunk_plane: &ChunkPlane, file_path: &str) -> Result<(), AnyError> {
  let ulc = chunk_plane.upper_left_corner;
  let lrc = chunk_plane.lower_right_corner;
  let width = (lrc.0 - ulc.0).try_into().unwrap();
  let height = (lrc.1 - ulc.1).try_into().unwrap();
  let mut imgbuf = ImageBuffer::new(width, height);
  for (x, y, pixel) in imgbuf.enumerate_pixels_mut() {
    let (local_x, local_y) = (ulc.0 + x as i64, ulc.1 + y as i64);
    let chunk_seed = chunk_plane.get_chunk_seed((local_x, local_y));
    if chunk_seed.is_none() {
      continue;
    }
    let color = {
      let chunk_seed = chunk_seed.unwrap();
      if chunk_seed.coordinates == (local_x, local_y) {
        match chunk_seed.chunk_seed_type {
          ChunkSeedType::Open => (255, 255, 255),
          ChunkSeedType::HalfOpen => (192, 192, 192),
          ChunkSeedType::Closed => (64, 64, 64),
          ChunkSeedType::Unknown => (0, 0, 0),
        }
      } else {
        (
          ((chunk_seed.coordinates.0.pow(2) + lrc.0.pow(2)) % 256) as u8,
          ((chunk_seed.coordinates.1.pow(2) + ulc.1.pow(2)) % 256) as u8,
          ((chunk_seed.coordinates.0.pow(2) + lrc.0.pow(2) + chunk_seed.coordinates.1.pow(2) + ulc.1.pow(2)) % 256)
            as u8,
        )
      }
    };
    *pixel = Rgb([color.0, color.1, color.2]);
  }
  imgbuf.save(file_path)?;
  Ok(())
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::chunk_plane::ChunkPlane;
  use crate::chunk_plane::ChunkPlaneId;

  use crate::test::init;
  use crate::test::TEMPORARY_TEST_DATA_DIRECTORY;

  #[test]
  fn test_export_chunk_plane_png() -> Result<(), AnyError> {
    init();
    let mut chunk_plane = ChunkPlane::default();
    chunk_plane.id = ChunkPlaneId::default();
    chunk_plane.generate_initial_chunks()?;
    export_chunk_plane_png(
      &chunk_plane,
      &format!("{}/{}", TEMPORARY_TEST_DATA_DIRECTORY, "test_chunk_plane.png"),
    )?;
    Ok(())
  }
}
