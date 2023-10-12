/// The `PassageStatus` enum.
#[derive(Clone, Copy, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Serialize)]
pub enum PassageStatus {
  /// This passage's status is unknown.
  #[default]
  Unknown,
  /// This passage is open.
  Open,
  /// This passage is closed.
  Closed,
  /// This passage is locked.
  ClosedAndLocked,
  /// This passage is hidden.
  Hidden,
  /// This passage is hidden and locked.
  HiddenAndLocked,
}
