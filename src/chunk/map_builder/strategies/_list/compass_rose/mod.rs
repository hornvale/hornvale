use anyhow::Error as AnyError;

use crate::chunk::Chunk;
use crate::passage::PassageBuilder;
use crate::passage::PassageDirection;
use crate::passage::PassageType;
use crate::room::RoomBuilder;

/// The `CompassRoseStrategy` struct.
#[derive(Clone)]
pub struct CompassRose {}

impl CompassRose {
  /// Maps a new `Chunk`.
  pub fn map_chunk(&self, chunk: &mut Chunk) -> Result<(), AnyError> {
    let mut nw_room = RoomBuilder::new()
      .name("Northwest Room")
      .description("A room to the northwest.")
      .build();
    let mut n_room = RoomBuilder::new()
      .name("North Room")
      .description("A room to the north.")
      .add_passage(
        &PassageDirection::West,
        &PassageBuilder::new()
          .r#type(&PassageType::Path)
          .name("West Path")
          .description("A path to the west.")
          .destination(&nw_room.id)
          .build(),
      )
      .build();
    nw_room.add_passage(
      &PassageDirection::East,
      &PassageBuilder::new()
        .r#type(&PassageType::Path)
        .name("East Path")
        .description("A path to the east.")
        .destination(&n_room.id)
        .build(),
    );
    let mut ne_room = RoomBuilder::new()
      .name("Northeast Room")
      .description("A room to the northeast.")
      .add_passage(
        &PassageDirection::West,
        &PassageBuilder::new()
          .r#type(&PassageType::Path)
          .name("West Path")
          .description("A path to the west.")
          .destination(&n_room.id)
          .build(),
      )
      .build();
    n_room.add_passage(
      &PassageDirection::East,
      &PassageBuilder::new()
        .r#type(&PassageType::Path)
        .name("East Path")
        .description("A path to the east.")
        .destination(&ne_room.id)
        .build(),
    );
    let mut w_room = RoomBuilder::new()
      .name("West Room")
      .description("A room to the west.")
      .add_passage(
        &PassageDirection::North,
        &PassageBuilder::new()
          .r#type(&PassageType::Path)
          .name("North Path")
          .description("A path to the north.")
          .destination(&nw_room.id)
          .build(),
      )
      .build();
    nw_room.add_passage(
      &PassageDirection::South,
      &PassageBuilder::new()
        .r#type(&PassageType::Path)
        .name("South Path")
        .description("A path to the south.")
        .destination(&w_room.id)
        .build(),
    );
    let mut e_room = RoomBuilder::new()
      .name("East Room")
      .description("A room to the east.")
      .add_passage(
        &PassageDirection::North,
        &PassageBuilder::new()
          .r#type(&PassageType::Path)
          .name("North Path")
          .description("A path to the north.")
          .destination(&ne_room.id)
          .build(),
      )
      .build();
    ne_room.add_passage(
      &PassageDirection::South,
      &PassageBuilder::new()
        .r#type(&PassageType::Path)
        .name("South Path")
        .description("A path to the south.")
        .destination(&e_room.id)
        .build(),
    );
    let mut sw_room = RoomBuilder::new()
      .name("Southwest Room")
      .description("A room to the southwest.")
      .add_passage(
        &PassageDirection::North,
        &PassageBuilder::new()
          .r#type(&PassageType::Path)
          .name("North Path")
          .description("A path to the north.")
          .destination(&w_room.id)
          .build(),
      )
      .build();
    w_room.add_passage(
      &PassageDirection::South,
      &PassageBuilder::new()
        .r#type(&PassageType::Path)
        .name("South Path")
        .description("A path to the south.")
        .destination(&sw_room.id)
        .build(),
    );
    let mut s_room = RoomBuilder::new()
      .name("South Room")
      .description("A room to the south.")
      .add_passage(
        &PassageDirection::West,
        &PassageBuilder::new()
          .r#type(&PassageType::Path)
          .name("West Path")
          .description("A path to the west.")
          .destination(&sw_room.id)
          .build(),
      )
      .build();
    sw_room.add_passage(
      &PassageDirection::East,
      &PassageBuilder::new()
        .r#type(&PassageType::Path)
        .name("East Path")
        .description("A path to the east.")
        .destination(&s_room.id)
        .build(),
    );
    let mut se_room = RoomBuilder::new()
      .name("Southeast Room")
      .description("A room to the southeast.")
      .add_passage(
        &PassageDirection::West,
        &PassageBuilder::new()
          .r#type(&PassageType::Path)
          .name("West Path")
          .description("A path to the west.")
          .destination(&s_room.id)
          .build(),
      )
      .build();
    s_room.add_passage(
      &PassageDirection::East,
      &PassageBuilder::new()
        .r#type(&PassageType::Path)
        .name("East Path")
        .description("A path to the east.")
        .destination(&se_room.id)
        .build(),
    );
    se_room.add_passage(
      &PassageDirection::North,
      &PassageBuilder::new()
        .r#type(&PassageType::Path)
        .name("North Path")
        .description("A path to the north.")
        .destination(&e_room.id)
        .build(),
    );
    e_room.add_passage(
      &PassageDirection::South,
      &PassageBuilder::new()
        .r#type(&PassageType::Path)
        .name("South Path")
        .description("A path to the south.")
        .destination(&se_room.id)
        .build(),
    );
    let center_room = RoomBuilder::new()
      .name("Center Room")
      .description("A room in the center.")
      .add_passage(
        &PassageDirection::North,
        &PassageBuilder::new()
          .r#type(&PassageType::Path)
          .name("North Path")
          .description("A path to the north.")
          .destination(&n_room.id)
          .build(),
      )
      .add_passage(
        &PassageDirection::South,
        &PassageBuilder::new()
          .r#type(&PassageType::Path)
          .name("South Path")
          .description("A path to the south.")
          .destination(&s_room.id)
          .build(),
      )
      .add_passage(
        &PassageDirection::West,
        &PassageBuilder::new()
          .r#type(&PassageType::Path)
          .name("West Path")
          .description("A path to the west.")
          .destination(&w_room.id)
          .build(),
      )
      .add_passage(
        &PassageDirection::East,
        &PassageBuilder::new()
          .r#type(&PassageType::Path)
          .name("East Path")
          .description("A path to the east.")
          .destination(&e_room.id)
          .build(),
      )
      .add_passage(
        &PassageDirection::Northwest,
        &PassageBuilder::new()
          .r#type(&PassageType::Path)
          .name("Northwest Path")
          .description("A path to the northwest.")
          .destination(&nw_room.id)
          .build(),
      )
      .add_passage(
        &PassageDirection::Northeast,
        &PassageBuilder::new()
          .r#type(&PassageType::Path)
          .name("Northeast Path")
          .description("A path to the northeast.")
          .destination(&ne_room.id)
          .build(),
      )
      .add_passage(
        &PassageDirection::Southwest,
        &PassageBuilder::new()
          .r#type(&PassageType::Path)
          .name("Southwest Path")
          .description("A path to the southwest.")
          .destination(&sw_room.id)
          .build(),
      )
      .add_passage(
        &PassageDirection::Southeast,
        &PassageBuilder::new()
          .r#type(&PassageType::Path)
          .name("Southeast Path")
          .description("A path to the southeast.")
          .destination(&se_room.id)
          .build(),
      )
      .build();
    nw_room.passages.insert(
      PassageDirection::Southeast,
      PassageBuilder::new()
        .r#type(&PassageType::Path)
        .name("Southeast Path")
        .description("A path to the southeast.")
        .destination(&center_room.id)
        .build(),
    );
    ne_room.passages.insert(
      PassageDirection::Southwest,
      PassageBuilder::new()
        .r#type(&PassageType::Path)
        .name("Southwest Path")
        .description("A path to the southwest.")
        .destination(&center_room.id)
        .build(),
    );
    sw_room.passages.insert(
      PassageDirection::Northeast,
      PassageBuilder::new()
        .r#type(&PassageType::Path)
        .name("Northeast Path")
        .description("A path to the northeast.")
        .destination(&center_room.id)
        .build(),
    );
    se_room.passages.insert(
      PassageDirection::Northwest,
      PassageBuilder::new()
        .r#type(&PassageType::Path)
        .name("Northwest Path")
        .description("A path to the northwest.")
        .destination(&center_room.id)
        .build(),
    );
    n_room.passages.insert(
      PassageDirection::South,
      PassageBuilder::new()
        .r#type(&PassageType::Path)
        .name("South Path")
        .description("A path to the south.")
        .destination(&center_room.id)
        .build(),
    );
    s_room.passages.insert(
      PassageDirection::North,
      PassageBuilder::new()
        .r#type(&PassageType::Path)
        .name("North Path")
        .description("A path to the north.")
        .destination(&center_room.id)
        .build(),
    );
    w_room.passages.insert(
      PassageDirection::East,
      PassageBuilder::new()
        .r#type(&PassageType::Path)
        .name("East Path")
        .description("A path to the east.")
        .destination(&center_room.id)
        .build(),
    );
    e_room.passages.insert(
      PassageDirection::West,
      PassageBuilder::new()
        .r#type(&PassageType::Path)
        .name("West Path")
        .description("A path to the west.")
        .destination(&center_room.id)
        .build(),
    );
    chunk.rooms.insert(nw_room.id.clone(), nw_room);
    chunk.rooms.insert(n_room.id.clone(), n_room);
    chunk.rooms.insert(ne_room.id.clone(), ne_room);
    chunk.rooms.insert(w_room.id.clone(), w_room);
    chunk.rooms.insert(center_room.id.clone(), center_room);
    chunk.rooms.insert(e_room.id.clone(), e_room);
    chunk.rooms.insert(sw_room.id.clone(), sw_room);
    chunk.rooms.insert(s_room.id.clone(), s_room);
    chunk.rooms.insert(se_room.id.clone(), se_room);
    chunk.name = "Compass Rose".to_string();
    chunk.description = "A compass rose.".to_string();
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::chunk::ChunkFileManager;
  use crate::test::init;
  use crate::test::TEMPORARY_TEST_DATA_DIRECTORY;

  #[test]
  fn test_create_chunk() {
    init();
    let strategy = CompassRose {};
    let mut chunk = Chunk::default();
    strategy.map_chunk(&mut chunk).unwrap();
    assert_eq!(chunk.name, "Compass Rose".to_string());
    assert_eq!(chunk.description, "A compass rose.".to_string());
    assert_eq!(chunk.rooms.len(), 9);
    let base_dir = format!("{}/{}", TEMPORARY_TEST_DATA_DIRECTORY, "test_compass_rose_create_chunk");
    let chunk_file_manager = ChunkFileManager::new(&base_dir);
    chunk_file_manager.store(&chunk).unwrap();
    let chunk2 = chunk_file_manager.load(&chunk.id).unwrap();
    assert_eq!(chunk2.name, "Compass Rose".to_string());
  }
}
