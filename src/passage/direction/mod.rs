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
}
