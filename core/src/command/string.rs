use serde::{Deserialize, Serialize};

/// A raw command string, such as we might've received from the player.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Ord, PartialOrd, Serialize)]
pub struct CommandString(pub String);
