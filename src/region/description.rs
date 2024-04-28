use derive_more::Display;
use serde::{Deserialize, Serialize};

/// Region description.
#[derive(Clone, Debug, Display, Deserialize, Eq, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct RegionDescription(pub String);
