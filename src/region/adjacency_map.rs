use crate::geometry::prelude::*;
use crate::region::prelude_internal::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// The adjacency map.
#[derive(Debug, Default, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct RegionAdjacencyMap(pub HashMap<Direction, RegionIdentifier>);
