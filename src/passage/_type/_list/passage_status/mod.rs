/// The `PassageStatus` enum.
#[derive(Clone, Copy, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Serialize)]
pub enum PassageStatus {
  /// This passage's status is unknown.
  #[default]
  Unknown,
}
