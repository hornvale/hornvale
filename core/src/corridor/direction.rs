use crate::prelude::*;
use derive_more::Display;
use serde::{Deserialize, Serialize};

/// Trait implementations for `CorridorDirection`.
pub mod traits;

/// The direction of a corridor.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
#[repr(transparent)]
pub struct CorridorDirection(pub Direction);
