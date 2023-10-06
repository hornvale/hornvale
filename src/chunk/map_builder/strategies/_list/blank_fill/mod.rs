use anyhow::Error as AnyError;

use crate::chunk::Chunk;
// use crate::passage::PassageBuilder;
// use crate::passage::PassageDirection;
// use crate::passage::PassageType;
// use crate::room::RoomBuilder;

/// The `BlankFillStrategy` struct.
#[derive(Clone)]
pub struct BlankFill {}

impl BlankFill {
  /// Maps a new `Chunk`.
  pub fn map_chunk(&self, _chunk: &mut Chunk) -> Result<(), AnyError> {
    Ok(())
  }
}
