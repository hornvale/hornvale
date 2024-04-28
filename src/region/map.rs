use crate::region::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A region map.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct RegionMap(pub HashMap<RegionIdentifier, Region>);
