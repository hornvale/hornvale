use derive_more::Display;
use serde::{Deserialize, Serialize};

/// Region name.
#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize, Serialize)]
#[repr(transparent)]
pub struct RegionName(pub String);
