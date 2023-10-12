/// The `PassageDirection` enum.
#[derive(
  Clone,
  Copy,
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
pub enum PassageDirection {
  North,
  East,
  South,
  West,
  Northeast,
  Northwest,
  Southeast,
  Southwest,
  Up,
  Down,
  In,
  Out,
  #[default]
  Thru,
}

impl PassageDirection {
  /// Returns the opposite direction.
  pub fn opposite(&self) -> Self {
    use PassageDirection::*;
    match self {
      North => South,
      Northeast => Southwest,
      Northwest => Southeast,
      South => North,
      East => West,
      West => East,
      Southeast => Northwest,
      Southwest => Northeast,
      Up => Down,
      Down => Up,
      In => Out,
      Out => In,
      Thru => Thru,
    }
  }
}
