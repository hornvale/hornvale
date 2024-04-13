use serde::{Deserialize, Serialize};

/// A component providing a name.
///
/// e.g. "rusty knife", "shiny sword", "old book"
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
#[repr(transparent)]
pub struct Name(pub String);
