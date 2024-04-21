use serde::{Deserialize, Serialize};

/// Mark a corridor terminus.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub struct IsCorridorTerminus;
