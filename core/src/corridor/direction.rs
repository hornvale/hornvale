use crate::prelude::*;
use serde::{Deserialize, Serialize};

/// Trait implementations for `CorridorDirection`.
pub mod traits;

/// The direction of a corridor.
#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[repr(transparent)]
pub struct CorridorDirection(pub Direction);
