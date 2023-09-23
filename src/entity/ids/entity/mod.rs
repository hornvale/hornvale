use specs::prelude::*;
use specs::world::Index;

use crate::entity::ids::*;

/// The `EntityId` type.
///
/// We do this so that we can perform some compile-time type-checking with IDs.
#[derive(
  Clone, Component, Copy, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize,
)]
#[repr(transparent)]
pub struct Id(pub Index);

impl From<ObjectId> for Id {
  fn from(id: ObjectId) -> Self {
    Self(id.0)
  }
}
