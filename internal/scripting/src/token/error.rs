use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Errors encountered during the compilation process.
#[derive(Clone, Debug, Deserialize, Eq, Error, Hash, PartialEq, Serialize)]
pub enum TokenError {
  /// Unknown keyword.
  #[error("unknown keyword ({0})")]
  UnknownKeyword(String),
}
