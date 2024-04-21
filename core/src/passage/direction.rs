use crate::prelude::*;
use derive_more::Display;
use serde::{Deserialize, Serialize};

/// Trait implementations.
pub mod traits;

/// A `PassageDirection` is a direction in which a passage can be built.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub struct PassageDirection(pub Direction);
