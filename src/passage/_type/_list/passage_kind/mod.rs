/// The `PassageKind` enum.
#[derive(
  Clone,
  Debug,
  Default,
  Deserialize,
  Eq,
  Hash,
  PartialEq,
  Serialize,
  strum::Display,
  strum::IntoStaticStr,
  strum::AsRefStr,
  strum::EnumString,
  strum::EnumCount,
  strum::EnumIter,
)]
pub enum PassageKind {
  /// This passage is a path.
  #[default]
  Path,
  /// It's so open, it's not even a passage.
  WideOpen,
  /// This passage is a door.
  Door,
}
