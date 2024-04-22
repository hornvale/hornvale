use serde::{Deserialize, Serialize};
use strum::Display;

/// The kind of a command.
#[derive(Clone, Copy, Debug, Default, Deserialize, Display, Eq, PartialEq, Serialize)]
pub enum CommandKind {
  /// A "normal" verb command.
  #[default]
  Default,
  /// A "magic" command.
  Magic,
  /// A "question" command.
  Question,
}
