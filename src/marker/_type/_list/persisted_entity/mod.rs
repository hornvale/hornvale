use specs::prelude::*;
use specs::saveload::SimpleMarker;

/// The `PersistedEntity` marker.
#[derive(Clone, Component, Debug, Deserialize, PartialEq, Serialize)]
pub struct PersistedEntity;

pub type PersistedEntityMarker = SimpleMarker<PersistedEntity>;
