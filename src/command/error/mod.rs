use anyhow::Error as AnyError;

use crate::action::ActionError;

/// The `CommandError` type.
///
/// This will mostly be used to translate from `ActionError` into information
/// that is useful for the developer and the player.
#[derive(Debug, Error)]
pub enum Error {
  /// An error occurred while executing the command.
  #[error(transparent)]
  ActionError(#[from] ActionError),
  /// Any error occurred.
  #[error(transparent)]
  AnyError(#[from] AnyError),
}
