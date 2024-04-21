use hecs::Entity;
use serde::{Deserialize, Serialize};

/// An indirect object.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CommandIndirectObject(pub Entity);
