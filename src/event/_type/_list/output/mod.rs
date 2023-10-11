/// The `Output` type.
///
/// This represents a piece of textual output to present to the player.
#[derive(Builder, Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Output {
  pub output: String,
}
