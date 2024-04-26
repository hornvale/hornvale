use derive_more::Display;
use serde::{Deserialize, Serialize};

/// Region entity.
#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Deserialize, Serialize)]
pub struct RegionEntity(u32);
