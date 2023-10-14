use anyhow::Error as AnyError;
use colored::*;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;

/// The `OutputType` enum.
///
/// This should be an exhaustive collection of effects.
///
/// Effects should be phrased in the imperative mood.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize)]
pub enum OutputType {
  /// No-Op -- nothing prints.
  #[default]
  NoOp,
  /// Output a blank line.
  BlankLine,
  /// Output an error.
  Error(String),
}

impl OutputType {
  pub fn format(&self) -> Result<String, AnyError> {
    debug!("Displaying {:#?} output.", self);
    use OutputType::*;
    #[allow(unreachable_patterns)]
    match self {
      NoOp => {
        debug!("Displaying no-op output.");
        Ok(String::new())
      },
      BlankLine => {
        debug!("Displaying blank-line output.");
        Ok("\n".to_string())
      },
      Error(message) => {
        debug!("Displaying error output.");
        Ok(format!("{}\n", message.red()))
      },
    }
  }
}

impl Display for OutputType {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.format().unwrap())
  }
}
