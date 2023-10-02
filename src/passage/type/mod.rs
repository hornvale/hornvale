use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;

/// The `PassageType` enum.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub enum Type {
  /// None -- there is no passage in this direction.
  #[default]
  None,
  /// Path -- there is a path in this direction.
  Path,
  /// Stub -- this is a placeholder for a passage that may or may not exist.
  Stub,
  /// Placeholder -- this is a placeholder for a passage to an unloaded room.
  Placeholder,
}

impl Type {
  /// Returns a plural string representation of the `PassageType`.
  pub fn to_plural_string(&self) -> String {
    use Type::*;
    match self {
      None => "none".to_string(),
      Path => "paths".to_string(),
      Stub => "blurry paths".to_string(),
      Placeholder => "shadowy paths".to_string(),
    }
  }
}

impl Display for Type {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    use Type::*;
    let value = match self {
      None => "none",
      Path => "path",
      Stub => "blurry path",
      Placeholder => "shadowy path",
    };
    write!(f, "{}", value)
  }
}
