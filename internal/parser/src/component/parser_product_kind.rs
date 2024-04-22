use serde::{Deserialize, Serialize};

/// An enum for indicating the kind of a parser product.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum ParserProductKind {
  /// A parser product that describes a magic command.
  MagicCommand,
  /// A parser product that describes a question command.
  QuestionCommand,
  /// A parser product that encapsulates a verb command.
  VerbCommand,
}
