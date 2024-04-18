use crate::prelude::Direction;
use hecs::Entity;
use serde::{Deserialize, Serialize};

/// An argument that a command might take.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Hash, Serialize)]
pub enum CommandArgument {
  /// A direction.
  Direction(Direction),
  /// An entity.
  Entity(Entity),
  /// A string literal.
  StringLiteral(String),
  /// A raw string.
  RawString(String),
  /// Multiple arguments.
  Entities(Vec<Entity>),
}
