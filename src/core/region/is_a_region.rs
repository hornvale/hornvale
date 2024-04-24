use serde::{Deserialize, Serialize};

/// Mark a region.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub struct IsARegion;
