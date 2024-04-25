use crate::world::prelude::*;
use anyhow::Error as AnyError;
use hecs::{Entity, World};

/// A trait for describing the room passages.
pub trait DescribeRoomPassages {
  /// Describe the room passages.
  fn describe_room_passages(&self, entity: Entity) -> Result<String, AnyError>;
}

impl DescribeRoomPassages for World {
  fn describe_room_passages(&self, entity: Entity) -> Result<String, AnyError> {
    let (region, room) = self.get_region_and_room_containing_entity(entity)?;
    let passages = self.get_room_passage_directions(&region, &room)?;
    let result = if !passages.is_empty() {
      let passages = passages
        .iter()
        .map(|direction| direction.to_string().to_lowercase())
        .collect::<Vec<_>>();
      match passages.len() {
        1 => format!("There is an exit to the {}.", passages[0]),
        2 => format!("There are exits to the {} and {}.", passages[0], passages[1]),
        _ => format!(
          "There are exits to the {}, and {}.",
          passages[..passages.len() - 1].join(", "),
          passages[passages.len() - 1]
        ),
      }
    } else {
      "There are no exits.".to_string()
    };
    Ok(result)
  }
}
