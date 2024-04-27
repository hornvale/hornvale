use derive_more::Display;
use serde::{Deserialize, Serialize};

/// Region entity.
#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Deserialize, Serialize)]
#[repr(transparent)]
pub struct RegionEntity(pub u32);
