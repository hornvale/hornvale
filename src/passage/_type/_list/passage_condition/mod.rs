/// The `PassageCondition` enum.
#[derive(Clone, Copy, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Serialize)]
pub enum PassageCondition {
  /// This passage's condition is unknown.
  #[default]
  Unknown,
}
