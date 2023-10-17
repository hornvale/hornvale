use std::collections::HashMap;

use crate::passage::Passage;
use crate::passage::PassageDirection;

/// The `RoomPassagesDescriber` type.
#[derive(Clone, Debug)]
pub struct RoomPassagesDescriber;

impl RoomPassagesDescriber {
  /// Describe a room's passages.
  pub fn describe(&self, passages: &HashMap<PassageDirection, Passage>) -> String {
    // This will become a rather complicated algorithm.
    //
    // Our goal is to be able to generate natural-sounding sentences that
    // describe the passages in a room.
    //
    // A few examples:
    // - "There are no obvious exits."
    // - "A path leads north."
    // - "A shadowy path leads north."
    // - "A shadowy path winds north."
    // - "Paths lead north and east."
    // - "Paths lead north, east, and west."
    // - "A shadowy path leads north, and another winds south."
    // - "Shadowy paths lead north and east, and a road winds south."
    // - "Shadowy paths lead north and east, and a road winds south. A dark
    //   cave gapes to the northeast."
    // - "Paths lead north and east. A road winds south to where the city of
    //   Alondil glimmers faintly across the desert sands. A dark cave gapes to
    //   the northeast."
    //
    // This should be accomplished by providing a small amount of customization
    // for each passage:
    // - The passage's kind (e.g. path, road, cave, tunnel, door, gate, etc.)
    // - The passage's adjective (e.g. shadowy, dark, winding, etc.)
    // - The passage's verb (e.g. leads, winds, goes, etc.)
    // - The passage's direction (e.g. north, northeast, etc.)
    // - An optional distinct description as a complete sentence (e.g. "A dark
    //   cave gapes to the northeast.")
    match passages.len() {
      0 => String::from("There are no obvious exits."),
      1 => String::from("There is one passage."),
      _ => format!("There are {} passages.", passages.len()),
    }
  }
}
