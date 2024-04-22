use serde::{Deserialize, Serialize};
use strum::Display;

/// A determiner.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Determiner {
  /// A noun possessive determiner, e.g. in `take thief's treasure`.
  NounPossessive,
  /// "All", e.g. in `take all`.
  All,
  /// "Any", e.g. in `take any`.
  Any,
  /// "Each", e.g. in `take each`.
  Each,
  /// "Either", e.g. in `take either`.
  Either,
  /// "Every", e.g. in `take every`.
  Every,
  /// "Half", e.g. in `take half`.
  Half,
  /// "His", e.g. in `take his sword`.
  His,
  /// "Her", e.g. in `take her sword`.
  Her,
  /// "Its", e.g. in `take its sword`.
  Its,
  /// "Many", e.g. in `take many`.
  Many,
  /// "My", e.g. in `take my sword`.
  My,
  /// "Neither", e.g. in `take neither`.
  Neither,
  /// "No", e.g. in `take no wives, father no children`.
  No,
  /// "None", e.g. in `take none`.
  None,
  /// "Our", e.g. in `take our sword`.
  Our,
  /// "Some", e.g. in `take some`.
  Some,
  /// "That", e.g. in `take that`.
  That,
  /// "Their", e.g. in `take their sword`.
  Their,
  /// "These", e.g. in `take these`.
  These,
  /// "This", e.g. in `take this`.
  This,
  /// "Those", e.g. in `take those`.
  Those,
  /// "Your", e.g. in `take your sword`.
  Your,
}

impl Determiner {
  /// Can this token follow adjectives?
  pub fn can_follow_adjective(&self) -> bool {
    self.is_noun_possessive()
  }

  /// Is this the "all" token?
  pub fn is_all(&self) -> bool {
    matches!(self, Self::All)
  }

  /// Is this token a demonstrative determiner?
  pub fn is_demonstrative(&self) -> bool {
    matches!(self, Self::This | Self::That | Self::These | Self::Those)
  }

  /// Is this token a distributive determiner?
  pub fn is_distributive(&self) -> bool {
    matches!(
      self,
      Self::All | Self::Any | Self::Each | Self::Either | Self::Every | Self::Neither | Self::Some
    )
  }

  /// Is this token a possessive determiner?
  pub fn is_possessive(&self) -> bool {
    self.is_noun_possessive() || self.is_personal_possessive()
  }

  /// Is this token a definite (or pronominal) possessive determiner?
  pub fn is_personal_possessive(&self) -> bool {
    matches!(
      self,
      Self::My | Self::Your | Self::His | Self::Her | Self::Its | Self::Our | Self::Their
    )
  }

  /// Is this token a noun possessive determiner?
  pub fn is_noun_possessive(&self) -> bool {
    matches!(self, Self::NounPossessive)
  }
}

impl TryFrom<&str> for Determiner {
  type Error = ();

  fn try_from(value: &str) -> Result<Self, Self::Error> {
    match value {
      "all" => Ok(Self::All),
      "any" => Ok(Self::Any),
      "each" => Ok(Self::Each),
      "either" => Ok(Self::Either),
      "every" => Ok(Self::Every),
      "half" => Ok(Self::Half),
      "his" => Ok(Self::His),
      "her" => Ok(Self::Her),
      "its" => Ok(Self::Its),
      "many" => Ok(Self::Many),
      "my" => Ok(Self::My),
      "neither" => Ok(Self::Neither),
      "no" => Ok(Self::No),
      "none" => Ok(Self::None),
      "our" => Ok(Self::Our),
      "some" => Ok(Self::Some),
      "that" => Ok(Self::That),
      "their" => Ok(Self::Their),
      "these" => Ok(Self::These),
      "this" => Ok(Self::This),
      "those" => Ok(Self::Those),
      "your" => Ok(Self::Your),
      string if (string.ends_with("'s") || string.ends_with("'")) => Ok(Self::NounPossessive),
      _ => Err(()),
    }
  }
}
