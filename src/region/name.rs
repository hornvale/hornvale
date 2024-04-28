use derive_more::Display;
use serde::{Deserialize, Serialize};

/// Region name.
#[derive(Clone, Debug, Display, Deserialize, Eq, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct RegionName(pub String);
