use serde::{Deserialize, Serialize};

/// A component listing adjectives relevant to a noun.
#[derive(Clone, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Adjectives(pub Vec<String>);
