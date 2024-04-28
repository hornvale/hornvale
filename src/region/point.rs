use crate::geometry::prelude::*;
use derive_more::Display;
use serde::{Deserialize, Serialize};

/// Region point.
#[derive(Clone, Copy, Debug, Display, Deserialize, Eq, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct RegionPoint(pub Point4D);
