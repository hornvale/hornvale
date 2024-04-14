use serde::{Deserialize, Serialize};

/// Mark a room.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub struct IsARoom;
