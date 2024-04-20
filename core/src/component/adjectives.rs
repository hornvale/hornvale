use serde::{Deserialize, Serialize};

/// A component listing adjectives relevant to a noun.
///
/// e.g. "rusty", "shiny", "old"
#[derive(Clone, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Adjectives(pub Vec<String>);
