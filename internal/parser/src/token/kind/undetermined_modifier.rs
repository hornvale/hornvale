use serde::{Deserialize, Serialize};
use strum::Display;

/// An undetermined modifier.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum UndeterminedModifier {
  /// Could be anything.
  AdverbOrDirectionOrPreposition,
  /// Could be an adverb or direction.
  AdverbOrDirection,
  /// Could be an adverb or preposition.
  AdverbOrPreposition,
  /// Could be a direction or preposition.
  DirectionOrPreposition,
}

impl UndeterminedModifier {
  /// Can this token follow adjectives?
  pub fn can_follow_adjective(&self) -> bool {
    false
  }

  /// Could this token be an adverb?
  pub fn could_be_adverb(&self) -> bool {
    matches!(
      self,
      Self::AdverbOrDirectionOrPreposition | Self::AdverbOrDirection | Self::AdverbOrPreposition
    )
  }

  /// Could this token be a direction?
  pub fn could_be_direction(&self) -> bool {
    matches!(
      self,
      Self::AdverbOrDirectionOrPreposition | Self::AdverbOrDirection | Self::DirectionOrPreposition
    )
  }

  /// Could this token be a preposition?
  pub fn could_be_preposition(&self) -> bool {
    matches!(
      self,
      Self::AdverbOrDirectionOrPreposition | Self::AdverbOrPreposition | Self::DirectionOrPreposition
    )
  }
}
