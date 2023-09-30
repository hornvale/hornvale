use crate::chunk::Chunk;
use crate::chunk::ChunkBuilder;
use crate::chunk_factory::ChunkFactoryStrategyTrait;
use crate::room::RoomBuilder;

/// The `CompassRoseStrategy` struct.
#[derive(Clone)]
pub struct CompassRose {}

impl ChunkFactoryStrategyTrait for CompassRose {
  /// Creates a new `Chunk`.
  fn create_chunk(&self) -> Chunk {
    let mut chunk = ChunkBuilder::new()
      .name("Compass Rose")
      .description("A compass rose.")
      .build();
    let nw_room = RoomBuilder::new()
      .name("Northwest Room")
      .description("A room to the northwest.")
      .build();
    let n_room = RoomBuilder::new()
      .name("North Room")
      .description("A room to the north.")
      .build();
    let ne_room = RoomBuilder::new()
      .name("Northeast Room")
      .description("A room to the northeast.")
      .build();
    let w_room = RoomBuilder::new()
      .name("West Room")
      .description("A room to the west.")
      .build();
    let e_room = RoomBuilder::new()
      .name("East Room")
      .description("A room to the east.")
      .build();
    let sw_room = RoomBuilder::new()
      .name("Southwest Room")
      .description("A room to the southwest.")
      .build();
    let s_room = RoomBuilder::new()
      .name("South Room")
      .description("A room to the south.")
      .build();
    let se_room = RoomBuilder::new()
      .name("Southeast Room")
      .description("A room to the southeast.")
      .build();
    let center_room = RoomBuilder::new()
      .name("Center Room")
      .description("A room in the center.")
      .build();
    chunk.rooms.insert(nw_room.id.clone(), nw_room);
    chunk.rooms.insert(n_room.id.clone(), n_room);
    chunk.rooms.insert(ne_room.id.clone(), ne_room);
    chunk.rooms.insert(w_room.id.clone(), w_room);
    chunk.rooms.insert(center_room.id.clone(), center_room);
    chunk.rooms.insert(e_room.id.clone(), e_room);
    chunk.rooms.insert(sw_room.id.clone(), sw_room);
    chunk.rooms.insert(s_room.id.clone(), s_room);
    chunk.rooms.insert(se_room.id.clone(), se_room);
    chunk
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;

  #[test]
  fn test_create_chunk() {
    init();
    let strategy = CompassRose {};
    let chunk = strategy.create_chunk();
    assert_eq!(chunk.name, "Compass Rose".to_string());
    assert_eq!(chunk.description, "A compass rose.".to_string());
    assert_eq!(chunk.rooms.len(), 9);
    chunk
      .write_to_file(&format!(
        "{}/{}",
        &crate::test::TEMPORARY_TEST_DATA_DIRECTORY,
        "test_compass_rose.yaml"
      ))
      .unwrap();
    let chunk2 = Chunk::load_from_file(&format!(
      "{}/{}",
      &crate::test::TEMPORARY_TEST_DATA_DIRECTORY,
      "test_compass_rose.yaml"
    ))
    .unwrap();
    assert_eq!(chunk2.name, "Compass Rose".to_string());
  }
}
