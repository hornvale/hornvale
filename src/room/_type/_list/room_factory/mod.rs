use crate::room::RoomFactoryStrategy;

/// The `RoomFactory` struct.
#[derive(Clone, Debug, Default)]
pub struct RoomFactory {
  pub strategy: RoomFactoryStrategy,
}

impl RoomFactory {}
