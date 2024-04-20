use thiserror::Error;

/// Errors encountered during the parsing process.
#[derive(Clone, Copy, Debug, Eq, Error, PartialEq)]
pub enum CompilerError {
  /// Unknown error.
  #[error("an unknown error occurred")]
  UnknownError,
  /// Attempted to read the variable in its own initializer.
  #[error("attempted to read variable in its own initializer")]
  AttemptedToReadVariableInOwnInitializer,
}
