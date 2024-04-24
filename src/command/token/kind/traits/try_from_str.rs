use crate::command::prelude::*;
use crate::core::prelude::*;

impl TryFrom<&str> for TokenKind {
  type Error = ();

  fn try_from(string: &str) -> Result<Self, Self::Error> {
    match string {
      // directions and modifiers
      "up" => Ok(Self::Up),
      "down" => Ok(Self::Down),
      "in" => Ok(Self::In),
      "out" => Ok(Self::Out),
      string if Direction::try_from(string).is_ok() => {
        let direction = Direction::try_from(string).unwrap();
        Ok(Self::Direction(direction))
      },
      string if CommandModifier::try_from(string).is_ok() => {
        let modifier = CommandModifier::try_from(string).unwrap();
        Ok(Self::CommandModifier(modifier))
      },

      // Demonstrative determiners.
      "this" => Ok(Self::This),
      "that" => Ok(Self::That),
      "these" => Ok(Self::These),
      "those" => Ok(Self::Those),

      // Distributive determiners.
      "all" => Ok(Self::All),
      "any" => Ok(Self::Any),
      "each" => Ok(Self::Each),
      "either" => Ok(Self::Either),
      "every" => Ok(Self::Every),
      "neither" => Ok(Self::Neither),
      "some" => Ok(Self::Some),

      // Possessive determiners.
      "my" => Ok(Self::My),
      "your" => Ok(Self::Your),
      "his" => Ok(Self::His),
      "her" => Ok(Self::Her(Her::default())), // (also pronoun)
      "its" => Ok(Self::Its),
      "our" => Ok(Self::Our),
      "their" => Ok(Self::Their),

      // Pronouns.
      "me" | "myself" | "I" => Ok(Self::Me),
      "you" => Ok(Self::You),
      "him" => Ok(Self::Him),
      // "her" => Ok(Self::Her), // (also possessive determiner)
      "it" => Ok(Self::It),
      "them" => Ok(Self::Them),

      // Articles.
      "a" | "an" => Ok(Self::A),
      "the" => Ok(Self::The),

      // Conjunctions.
      "and" => Ok(Self::And),
      "or" => Ok(Self::Or),
      "but" => Ok(Self::But),

      // Yes/No.
      "yes" | "y" | "true" | "t" => Ok(Self::Yes),
      "no" | "false" | "f" => Ok(Self::No),

      // Any other word.
      _ => Err(()),
    }
  }
}
