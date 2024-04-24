use crate::scripting::parser::error::Error as CommandError;
use crate::scripting::scanner::error::Error as ScannerError;
use thiserror::Error;

/// Errors encountered during the compilation process.
#[derive(Clone, Debug, Error)]
pub enum Error {
  /// Parser error.
  #[error("an error occurred in the parser ({0})")]
  CommandError(#[from] CommandError),
  /// Scanner error.
  #[error("an error occurred in the scanner ({0})")]
  ScannerError(#[from] ScannerError),
}
