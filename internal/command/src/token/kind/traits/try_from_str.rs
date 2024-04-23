use crate::prelude::*;

impl TryFrom<&str> for TokenKind {
  type Error = ();

  fn try_from(string: &str) -> Result<Self, Self::Error> {
    match string {
      // Directions.
      "north" | "n" => Ok(Self::North),
      "northeast" | "ne" => Ok(Self::Northeast),
      "east" | "e" => Ok(Self::East),
      "southeast" | "se" => Ok(Self::Southeast),
      "south" | "s" => Ok(Self::South),
      "southwest" | "sw" => Ok(Self::Southwest),
      "west" | "w" => Ok(Self::West),
      "northwest" | "nw" => Ok(Self::Northwest),
      "up" => Ok(Self::Up),     // (also adverb)
      "down" => Ok(Self::Down), // (also adverb)
      "in" => Ok(Self::In),     // (also adverb)
      "out" => Ok(Self::Out),   // (also adverb)

      // Prepositions.
      "about" => Ok(Self::About),
      "above" => Ok(Self::Above),
      "across" => Ok(Self::Across),
      "against" => Ok(Self::Against),
      "along" => Ok(Self::Along),
      "among" => Ok(Self::Among),
      "as" => Ok(Self::As),
      "at" => Ok(Self::At),
      "before" => Ok(Self::Before),
      "behind" => Ok(Self::Behind),
      "below" => Ok(Self::Below),
      "beside" => Ok(Self::Beside),
      "between" => Ok(Self::Between),
      "beyond" => Ok(Self::Beyond),
      "by" => Ok(Self::By),
      "for" => Ok(Self::For),
      "from" => Ok(Self::From),
      // "in" => Ok(Self::In), // (also direction)
      "into" => Ok(Self::Into),
      "of" => Ok(Self::Of),
      "off" => Ok(Self::Off), // (also adverb)
      "on" => Ok(Self::On),   // (also adverb)
      // "out" => Ok(Self::Out), // (also direction)
      "over" => Ok(Self::Over),
      "to" => Ok(Self::To),
      "toward" => Ok(Self::Toward),
      "under" => Ok(Self::Under),
      "upon" => Ok(Self::Upon),
      "with" => Ok(Self::With),
      "without" => Ok(Self::Without),

      // Adverbs.
      "around" => Ok(Self::Around),
      // "down" => Ok(Self::Down), // (also direction)
      "here" => Ok(Self::Here),
      // "in" => Ok(Self::In), // (also direction)
      // "off" => Ok(Self::Off), // (also preposition)
      // "on" => Ok(Self::On), // (also preposition)
      // "out" => Ok(Self::Out), // (also direction)
      // "up" => Ok(Self::Up), // (also direction)

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
      "her" => Ok(Self::Her), // (also pronoun)
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
