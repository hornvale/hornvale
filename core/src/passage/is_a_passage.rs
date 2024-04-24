use serde::{Deserialize, Serialize};

/// Mark a passage.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub struct IsAPassage;
