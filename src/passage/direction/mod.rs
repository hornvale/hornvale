use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;

/// The `PassageDirection` enum.
#[derive(Clone, Debug, Default, Deserialize, Eq, Hash, PartialEq, PartialOrd, Serialize)]
pub enum Direction {
  /// North -- the passage is to the north.
  North,
  /// Northeast -- the passage is to the northeast.
  Northeast,
  /// Northwest -- the passage is to the northwest.
  Northwest,
  /// South -- the passage is to the south.
  South,
  /// East -- the passage is to the east.
  East,
  /// West -- the passage is to the west.
  West,
  /// Southeast -- the passage is to the southeast.
  Southeast,
  /// Southwest -- the passage is to the southwest.
  Southwest,
  /// Up -- the passage is up.
  Up,
  /// Down -- the passage is down.
  Down,
  /// In -- the passage is in.
  In,
  /// Out -- the passage is out.
  Out,
  /// Thru -- the passage is thru.
  #[default]
  Thru,
}

impl Direction {
  /// Returns the opposite direction.
  pub fn opposite(&self) -> Self {
    match self {
      Self::North => Self::South,
      Self::Northeast => Self::Southwest,
      Self::Northwest => Self::Southeast,
      Self::South => Self::North,
      Self::East => Self::West,
      Self::West => Self::East,
      Self::Southeast => Self::Northwest,
      Self::Southwest => Self::Northeast,
      Self::Up => Self::Down,
      Self::Down => Self::Up,
      Self::In => Self::Out,
      Self::Out => Self::In,
      Self::Thru => Self::Thru,
    }
  }

  /// Returns a string representation of the `PassageDirection`.
  pub fn to_lower_case_string(&self) -> String {
    use Direction::*;
    match self {
      North => "north".to_string(),
      Northeast => "northeast".to_string(),
      Northwest => "northwest".to_string(),
      South => "south".to_string(),
      East => "east".to_string(),
      West => "west".to_string(),
      Southeast => "southeast".to_string(),
      Southwest => "southwest".to_string(),
      Up => "up".to_string(),
      Down => "down".to_string(),
      In => "in".to_string(),
      Out => "out".to_string(),
      Thru => "through".to_string(),
    }
  }

  /// Returns a title case string representation of the `PassageDirection`.
  pub fn to_title_case_string(&self) -> String {
    use Direction::*;
    match self {
      North => "North".to_string(),
      Northeast => "Northeast".to_string(),
      Northwest => "Northwest".to_string(),
      South => "South".to_string(),
      East => "East".to_string(),
      West => "West".to_string(),
      Southeast => "Southeast".to_string(),
      Southwest => "Southwest".to_string(),
      Up => "Up".to_string(),
      Down => "Down".to_string(),
      In => "In".to_string(),
      Out => "Out".to_string(),
      Thru => "Through".to_string(),
    }
  }
}

impl Display for Direction {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.to_lower_case_string())
  }
}
