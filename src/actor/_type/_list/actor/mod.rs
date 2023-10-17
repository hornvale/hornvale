use derive_more::Display;

/// The `Actor` type.
#[derive(Builder, Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Serialize)]
pub struct Actor {}
