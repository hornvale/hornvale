use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;

use crate::output::OutputType;

/// The `Output` type.
#[derive(Builder, Clone, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Output {
  /// The `Output` type.
  pub output_type: OutputType,
}

impl Display for Output {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.output_type)
  }
}
