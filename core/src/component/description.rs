use serde::{Deserialize, Serialize};

/// A component providing a simple descriptive phrase.
///
/// e.g. "rusty knife", "shiny sword", "old book"
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
#[repr(transparent)]
pub struct Description(pub String);
