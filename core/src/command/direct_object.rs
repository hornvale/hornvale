use hecs::Entity;
use serde::{Deserialize, Serialize};

/// A direct object.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CommandDirectObject(pub Entity);
