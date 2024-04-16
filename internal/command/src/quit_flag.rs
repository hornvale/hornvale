use serde::{Deserialize, Serialize};

/// A flag to set when the game should quit.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct QuitFlag;
