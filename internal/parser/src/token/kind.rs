use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter};

/// Different kinds of tokens for the scanner.
#[derive(Clone, Copy, Debug, Display, EnumIter, Eq, Hash, PartialEq, Serialize, Deserialize)]
#[allow(missing_docs)]
pub enum TokenKind {
  //
  // Single-character tokens.
  //
  /// A single quote, e.g. in `say 'hello'` or `take the thief's treasure`.
  SingleQuote,
  /// A comma, e.g. in `take sword, shield`.
  Comma,
  /// A period, e.g. in `east. attack troll with sword. west`.
  Period,
  /// A semicolon, e.g. in `east; attack troll with sword; west`.
  Semicolon,
  /// An exclamation point, e.g. in `!`.
  Bang,
  /// A question mark, e.g. in `?`.
  Question,
  /// At sign, e.g. in `@`.
  AtSign,
  /// Hash sign, e.g. in `#`.
  Hash,
  /// Dollar sign, e.g. in `$`.
  Dollar,
  /// Percent sign, e.g. in `%`.
  Percent,
  /// Caret, e.g. in `^`.
  Caret,
  /// Ampersand, e.g. in `&`.
  Ampersand,
  /// Asterisk, e.g. in `*`.
  Asterisk,
  /// Forward slash, e.g. in `/`.
  ForwardSlash,
  /// Backward slash, e.g. in `\`.
  BackSlash,
  /// Left parenthesis, e.g. in `(`.
  LeftParenthesis,
  /// Right parenthesis, e.g. in `)`.
  RightParenthesis,
  /// Left square bracket, e.g. in `[`.
  LeftSquareBracket,
  /// Right square bracket, e.g. in `]`.
  RightSquareBracket,
  /// Left curly brace, e.g. in `{`.
  LeftCurlyBrace,
  /// Right curly brace, e.g. in `}`.
  RightCurlyBrace,
  /// Less than sign, e.g. in `<`.
  LessThan,
  /// Greater than sign, e.g. in `>`.
  GreaterThan,
  /// Equals sign, e.g. in `=`.
  Equals,
  /// Plus sign, e.g. in `+`.
  Plus,
  /// Minus sign, e.g. in `-`.
  Minus,
  /// Pipe, e.g. in `|`.
  Pipe,
  /// Colon, e.g. in `:`.
  Colon,
  /// Underscore, e.g. in `_`.
  Underscore,
  /// Tilde, e.g. in `~`.
  Tilde,
  /// Backtick, e.g. in `\``.
  Backtick,

  //
  // Multi-character tokens.
  //
  /// A string literal, e.g. in `say "hello"`.
  StringLiteral,
  /// A number literal, e.g. in `take 5 coins`. We use `u32` for simplicity.
  NumberLiteral,
  /// An ordinal, e.g. in `take 1st coin`.
  Ordinal,

  //
  // Word tokens.
  //
  /// Directions, which might also be prepositions, adjectives, etc.
  North,
  Northeast,
  East,
  Southeast,
  South,
  Southwest,
  West,
  Northwest,
  Up,
  Down,
  In,
  Out,

  /// Prepositions.
  About,
  Above,
  Across,
  Against,
  Along,
  Among,
  At,
  Before,
  Behind,
  Below,
  Beside,
  Between,
  Beyond,
  By,
  For,
  From,
  Into,
  Off,
  On,
  Over,
  To,
  Toward,
  Under,
  Upon,
  With,
  Without,

  // Adverbs, most of which are elided.
  /// "Around", e.g. in `look around`.
  Around,
  // "Down", e.g. in `turn radio down`.
  // Down,
  /// "Here", e.g. in `look here`.
  Here,
  // "In", e.g. in `push in`.
  // In,
  // "Off", e.g. in `turn radio off`.
  // Off,
  // "On", e.g. in `turn radio on`.
  // On,
  // "Out", e.g. in `pull out`.
  // Out,
  /// Then, e.g. in `take sword then take shield`.
  Then,
  // "Up", e.g. in `turn radio up`.
  // Up,
  /// Demonstrative determiners.
  /// "This", e.g. in `take this`.
  This,
  /// "That", e.g. in `take that`.
  That,
  /// "These", e.g. in `take these`.
  These,
  /// "Those", e.g. in `take those`.
  Those,

  /// Distributive determiners.
  /// "All", e.g. in `take all`.
  All,
  Any,
  Each,
  Either,
  Every,
  Neither,
  Some,

  /// Possessive determiners.
  My,
  Your,
  His,
  Her,
  Its,
  Our,
  Their,

  /// Pronouns.
  /// `I`/`me`/`myself`.
  Me,
  /// `you`.
  You,
  /// `he`.
  Him,
  /// `her`.
  /// Her (also possessive determiner)
  /// `it`.
  It,
  /// `them`.
  Them,

  /// Articles.
  A, // Also `an`.
  The,

  // Conjunctions.
  /// "And", e.g. in `take sword and shield`.
  And,
  /// "Or", e.g. in `take sword or shield`.
  Or,
  /// "But", e.g. in `take all but shield`.
  But,

  // Yes/No.
  /// e.g. in `yes` or `y`.
  Yes,
  /// e.g. in `no` or `n`.
  No,

  /// Any other word.
  Word,

  // Kinds assigned _only_ during the classification process.
  //
  // These are open groups, as opposed to the closed groups above.
  /// A verb.
  Verb,
  /// An adjective.
  Adjective,
  /// A noun.
  Noun,
  /// A general possessive determiner ("someone's").
  PossessiveDeterminer,
  /// The direct object.
  DirectObject,
  /// The indirect object.
  IndirectObject,

  //
  // Any other tokens.
  //
  /// End of input.
  EndOfInput,
}

impl TokenKind {
  /// Is this token a single-character token?
  pub fn is_single_character(&self) -> bool {
    matches!(
      self,
      Self::SingleQuote
        | Self::Comma
        | Self::Period
        | Self::Semicolon
        | Self::Bang
        | Self::Question
        | Self::AtSign
        | Self::Hash
        | Self::Dollar
        | Self::Percent
        | Self::Caret
        | Self::Ampersand
        | Self::Asterisk
        | Self::ForwardSlash
        | Self::BackSlash
        | Self::LeftParenthesis
        | Self::RightParenthesis
        | Self::LeftSquareBracket
        | Self::RightSquareBracket
        | Self::LeftCurlyBrace
        | Self::RightCurlyBrace
        | Self::LessThan
        | Self::GreaterThan
        | Self::Equals
        | Self::Plus
        | Self::Minus
        | Self::Pipe
        | Self::Colon
        | Self::Underscore
        | Self::Tilde
        | Self::Backtick
    )
  }

  /// Is this token punctuation (that we will use in the parser)?
  ///
  /// Currently, the only valid punctuation character is the comma.
  pub fn is_punctuation(&self) -> bool {
    matches!(self, Self::Comma)
  }

  /// Is this token a special character?
  ///
  /// This may be punctuation in real life, but we treat it as a special
  /// character in the parser.
  ///
  /// So _not_:
  /// - `Comma`
  /// - `SingleQuote`
  pub fn is_special_character(&self) -> bool {
    self.is_single_character() && !matches!(self, Self::Comma | Self::SingleQuote)
  }

  /// Is this token a direction?
  pub fn is_direction(&self) -> bool {
    matches!(
      self,
      Self::North
        | Self::Northeast
        | Self::East
        | Self::Southeast
        | Self::South
        | Self::Southwest
        | Self::West
        | Self::Northwest
        | Self::Up
        | Self::Down
        | Self::In
        | Self::Out
    )
  }

  /// Is this token a preposition?
  pub fn is_preposition(&self) -> bool {
    matches!(
      self,
      Self::About
        | Self::Above
        | Self::Across
        | Self::Against
        | Self::Along
        | Self::Among
        | Self::At
        | Self::Before
        | Self::Behind
        | Self::Below
        | Self::Beside
        | Self::Between
        | Self::Beyond
        | Self::By
        | Self::For
        | Self::From
        | Self::In
        | Self::Into
        | Self::Off
        | Self::On
        | Self::Out
        | Self::Over
        | Self::To
        | Self::Toward
        | Self::Under
        | Self::Upon
        | Self::With
        | Self::Without
    )
  }

  /// Is this token an adverb?
  pub fn is_adverb(&self) -> bool {
    matches!(
      self,
      Self::Around | Self::Down | Self::Here | Self::In | Self::Off | Self::On | Self::Out | Self::Then | Self::Up
    )
  }

  /// Is this token a demonstrative determiner?
  pub fn is_demonstrative_determiner(&self) -> bool {
    matches!(self, Self::This | Self::That | Self::These | Self::Those)
  }

  /// Is this token a distributive determiner?
  pub fn is_distributive_determiner(&self) -> bool {
    matches!(
      self,
      Self::All | Self::Any | Self::Each | Self::Either | Self::Every | Self::Neither | Self::Some
    )
  }

  /// Is this token a possessive determiner?
  pub fn is_possessive_determiner(&self) -> bool {
    matches!(
      self,
      Self::My | Self::Your | Self::His | Self::Her | Self::Its | Self::Our | Self::Their | Self::PossessiveDeterminer
    )
  }

  /// Is this token a pronoun?
  pub fn is_pronoun(&self) -> bool {
    matches!(
      self,
      Self::Me | Self::You | Self::Him | Self::Her | Self::It | Self::Them
    )
  }

  /// Is this token an article?
  pub fn is_article(&self) -> bool {
    matches!(self, Self::A | Self::The)
  }

  /// Is this token a conjunction?
  pub fn is_conjunction(&self) -> bool {
    matches!(self, Self::And | Self::Or | Self::But)
  }

  /// Is this token a verb?
  pub fn is_verb(&self) -> bool {
    self.is_direction() || matches!(self, Self::Verb)
  }

  /// Is this token a noun?
  pub fn is_noun(&self) -> bool {
    self.is_direction()
      || self.is_pronoun()
      || matches!(self, Self::All | Self::Noun | Self::DirectObject | Self::IndirectObject)
  }

  /// Could this token be a noun?
  pub fn could_be_noun(&self) -> bool {
    self.is_noun() || matches!(self, Self::Word)
  }

  /// Could this token be an adjective?
  pub fn could_be_adjective(&self) -> bool {
    self.is_adverb() || matches!(self, Self::Word)
  }

  /// Could this token be a verb?
  pub fn could_be_verb(&self) -> bool {
    self.is_verb() || matches!(self, Self::Word)
  }

  /// Does this token accept adjectives?
  pub fn can_follow_adjective(&self) -> bool {
    (self.is_noun() && !self.is_pronoun() && !self.is_direction() && !self.is_distributive_determiner())
      || (self.is_possessive_determiner()
        && !matches!(
          self,
          Self::My | Self::Your | Self::Our | Self::Their | Self::His | Self::Her | Self::Its
        ))
      || matches!(self, Self::Comma | Self::Adjective)
  }

  /// Is this token a yes/no token?
  pub fn is_yes_no(&self) -> bool {
    matches!(self, Self::Yes | Self::No)
  }

  /// Get the boolean value of this token.
  pub fn as_bool(&self) -> Option<bool> {
    match self {
      Self::Yes => Some(true),
      Self::No => Some(false),
      _ => None,
    }
  }
}

impl TryFrom<char> for TokenKind {
  type Error = ();

  fn try_from(character: char) -> Result<Self, Self::Error> {
    match character {
      ',' => Ok(Self::Comma),
      '.' => Ok(Self::Period),
      ';' => Ok(Self::Semicolon),
      '!' => Ok(Self::Bang),
      '?' => Ok(Self::Question),
      '@' => Ok(Self::AtSign),
      '#' => Ok(Self::Hash),
      '$' => Ok(Self::Dollar),
      '%' => Ok(Self::Percent),
      '^' => Ok(Self::Caret),
      '&' => Ok(Self::Ampersand),
      '*' => Ok(Self::Asterisk),
      '/' => Ok(Self::ForwardSlash),
      '\\' => Ok(Self::BackSlash),
      '(' => Ok(Self::LeftParenthesis),
      ')' => Ok(Self::RightParenthesis),
      '[' => Ok(Self::LeftSquareBracket),
      ']' => Ok(Self::RightSquareBracket),
      '{' => Ok(Self::LeftCurlyBrace),
      '}' => Ok(Self::RightCurlyBrace),
      '<' => Ok(Self::LessThan),
      '>' => Ok(Self::GreaterThan),
      '=' => Ok(Self::Equals),
      '+' => Ok(Self::Plus),
      '-' => Ok(Self::Minus),
      '|' => Ok(Self::Pipe),
      ':' => Ok(Self::Colon),
      '_' => Ok(Self::Underscore),
      '~' => Ok(Self::Tilde),
      '`' => Ok(Self::Backtick),
      _ => Err(()),
    }
  }
}

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
      "then" => Ok(Self::Then),
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

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;
  use strum::IntoEnumIterator;

  #[test]
  fn test_is_punctuation() {
    init();
    assert!(TokenKind::Comma.is_punctuation());
    for kind in TokenKind::iter() {
      if kind != TokenKind::Comma {
        assert!(!kind.is_punctuation());
      }
    }
  }

  #[test]
  fn test_is_special_character() {
    init();
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::Comma | TokenKind::SingleQuote => {
          assert!(!kind.is_special_character(), "{:?} is special", kind);
        },
        kind if !kind.is_single_character() => {
          assert!(!kind.is_special_character(), "{:?} is special", kind);
        },
        _ => {
          assert!(kind.is_special_character(), "{:?} is not special", kind);
        },
      }
    }
  }

  #[test]
  fn test_is_direction() {
    init();
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::North
        | TokenKind::Northeast
        | TokenKind::East
        | TokenKind::Southeast
        | TokenKind::South
        | TokenKind::Southwest
        | TokenKind::West
        | TokenKind::Northwest
        | TokenKind::Up
        | TokenKind::Down
        | TokenKind::In
        | TokenKind::Out => {
          assert!(kind.is_direction(), "{:?} is not a direction", kind);
        },
        _ => {
          assert!(!kind.is_direction(), "{:?} is a direction", kind);
        },
      }
    }
  }

  #[test]
  fn test_is_preposition() {
    init();
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::About
        | TokenKind::Above
        | TokenKind::Across
        | TokenKind::Against
        | TokenKind::Along
        | TokenKind::Among
        | TokenKind::At
        | TokenKind::Before
        | TokenKind::Behind
        | TokenKind::Below
        | TokenKind::Beside
        | TokenKind::Between
        | TokenKind::Beyond
        | TokenKind::By
        | TokenKind::For
        | TokenKind::From
        | TokenKind::In
        | TokenKind::Into
        | TokenKind::Off
        | TokenKind::On
        | TokenKind::Out
        | TokenKind::Over
        | TokenKind::To
        | TokenKind::Toward
        | TokenKind::Under
        | TokenKind::Upon
        | TokenKind::With
        | TokenKind::Without => {
          assert!(kind.is_preposition(), "{:?} is not a preposition", kind);
        },
        _ => {
          assert!(!kind.is_preposition(), "{:?} is a preposition", kind);
        },
      }
    }
  }

  #[test]
  fn test_is_adverb() {
    init();
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::Around
        | TokenKind::Down
        | TokenKind::Here
        | TokenKind::In
        | TokenKind::Off
        | TokenKind::On
        | TokenKind::Out
        | TokenKind::Then
        | TokenKind::Up => {
          assert!(kind.is_adverb(), "{:?} is not an adverb", kind);
        },
        _ => {
          assert!(!kind.is_adverb(), "{:?} is an adverb", kind);
        },
      }
    }
  }

  #[test]
  fn test_is_demonstrative_determiner() {
    init();
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::This | TokenKind::That | TokenKind::These | TokenKind::Those => {
          assert!(
            kind.is_demonstrative_determiner(),
            "{:?} is not a demonstrative determiner",
            kind
          );
        },
        _ => {
          assert!(
            !kind.is_demonstrative_determiner(),
            "{:?} is a demonstrative determiner",
            kind
          );
        },
      }
    }
  }

  #[test]
  fn test_is_distributive_determiner() {
    init();
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::All
        | TokenKind::Any
        | TokenKind::Each
        | TokenKind::Either
        | TokenKind::Every
        | TokenKind::Neither
        | TokenKind::Some => {
          assert!(
            kind.is_distributive_determiner(),
            "{:?} is not a distributive determiner",
            kind
          );
        },
        _ => {
          assert!(
            !kind.is_distributive_determiner(),
            "{:?} is a distributive determiner",
            kind
          );
        },
      }
    }
  }

  #[test]
  fn test_is_possessive_determiner() {
    init();
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::My
        | TokenKind::Your
        | TokenKind::His
        | TokenKind::Her
        | TokenKind::Its
        | TokenKind::Our
        | TokenKind::Their
        | TokenKind::PossessiveDeterminer => {
          assert!(
            kind.is_possessive_determiner(),
            "{:?} is not a possessive determiner",
            kind
          );
        },
        _ => {
          assert!(
            !kind.is_possessive_determiner(),
            "{:?} is a possessive determiner",
            kind
          );
        },
      }
    }
  }

  #[test]
  fn test_is_pronoun() {
    init();
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::Me | TokenKind::You | TokenKind::Him | TokenKind::Her | TokenKind::It | TokenKind::Them => {
          assert!(kind.is_pronoun(), "{:?} is not a pronoun", kind);
        },
        _ => {
          assert!(!kind.is_pronoun(), "{:?} is a pronoun", kind);
        },
      }
    }
  }

  #[test]
  fn test_is_article() {
    init();
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::A | TokenKind::The => {
          assert!(kind.is_article(), "{:?} is not an article", kind);
        },
        _ => {
          assert!(!kind.is_article(), "{:?} is an article", kind);
        },
      }
    }
  }

  #[test]
  fn test_is_conjunction() {
    init();
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::And | TokenKind::Or | TokenKind::But => {
          assert!(kind.is_conjunction(), "{:?} is not a conjunction", kind);
        },
        _ => {
          assert!(!kind.is_conjunction(), "{:?} is a conjunction", kind);
        },
      }
    }
  }

  #[test]
  fn test_is_verb() {
    init();
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::North
        | TokenKind::Northeast
        | TokenKind::East
        | TokenKind::Southeast
        | TokenKind::South
        | TokenKind::Southwest
        | TokenKind::West
        | TokenKind::Northwest
        | TokenKind::Up
        | TokenKind::Down
        | TokenKind::In
        | TokenKind::Out
        | TokenKind::Verb => {
          assert!(kind.is_verb(), "{:?} is not a verb", kind);
        },
        _ => {
          assert!(!kind.is_verb(), "{:?} is a verb", kind);
        },
      }
    }
  }

  #[test]
  fn test_is_noun() {
    init();
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::North
        | TokenKind::Northeast
        | TokenKind::East
        | TokenKind::Southeast
        | TokenKind::South
        | TokenKind::Southwest
        | TokenKind::West
        | TokenKind::Northwest
        | TokenKind::Up
        | TokenKind::Down
        | TokenKind::In
        | TokenKind::Out
        | TokenKind::All
        | TokenKind::Noun
        | TokenKind::Him
        | TokenKind::Me
        | TokenKind::Her
        | TokenKind::It
        | TokenKind::Them
        | TokenKind::You
        | TokenKind::DirectObject
        | TokenKind::IndirectObject => {
          assert!(kind.is_noun(), "{:?} is not a noun", kind);
        },
        _ => {
          assert!(!kind.is_noun(), "{:?} is a noun", kind);
        },
      }
    }
  }

  #[test]
  fn test_could_be_noun() {
    init();
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::North
        | TokenKind::Northeast
        | TokenKind::East
        | TokenKind::Southeast
        | TokenKind::South
        | TokenKind::Southwest
        | TokenKind::West
        | TokenKind::Northwest
        | TokenKind::Up
        | TokenKind::Down
        | TokenKind::In
        | TokenKind::Out
        | TokenKind::All
        | TokenKind::Noun
        | TokenKind::Him
        | TokenKind::Me
        | TokenKind::Her
        | TokenKind::It
        | TokenKind::Them
        | TokenKind::You
        | TokenKind::Word
        | TokenKind::DirectObject
        | TokenKind::IndirectObject => {
          assert!(kind.could_be_noun(), "{:?} is not a noun", kind);
        },
        _ => {
          assert!(!kind.could_be_noun(), "{:?} is a noun", kind);
        },
      }
    }
  }

  #[test]
  fn test_could_be_adjective() {
    init();
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::Around
        | TokenKind::Down
        | TokenKind::Here
        | TokenKind::In
        | TokenKind::Off
        | TokenKind::On
        | TokenKind::Out
        | TokenKind::Then
        | TokenKind::Up
        | TokenKind::Word => {
          assert!(kind.could_be_adjective(), "{:?} is not an adjective", kind);
        },
        _ => {
          assert!(!kind.could_be_adjective(), "{:?} is an adjective", kind);
        },
      }
    }
  }

  #[test]
  fn test_could_be_verb() {
    init();
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::North
        | TokenKind::Northeast
        | TokenKind::East
        | TokenKind::Southeast
        | TokenKind::South
        | TokenKind::Southwest
        | TokenKind::West
        | TokenKind::Northwest
        | TokenKind::Up
        | TokenKind::Down
        | TokenKind::In
        | TokenKind::Out
        | TokenKind::Verb
        | TokenKind::Word => {
          assert!(kind.could_be_verb(), "{:?} is not a verb", kind);
        },
        _ => {
          assert!(!kind.could_be_verb(), "{:?} is a verb", kind);
        },
      }
    }
  }

  #[test]
  fn test_can_follow_adjective() {
    init();
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::Comma
        | TokenKind::Adjective
        | TokenKind::Noun
        | TokenKind::DirectObject
        | TokenKind::IndirectObject
        | TokenKind::PossessiveDeterminer => {
          assert!(kind.can_follow_adjective(), "{:?} cannot follow an adjective", kind);
        },
        _ => {
          assert!(!kind.can_follow_adjective(), "{:?} can follow an adjective", kind);
        },
      }
    }
  }

  #[test]
  fn test_is_yes_no() {
    init();
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::Yes | TokenKind::No => {
          assert!(kind.is_yes_no(), "{:?} is not a yes/no token", kind);
        },
        _ => {
          assert!(!kind.is_yes_no(), "{:?} is a yes/no token", kind);
        },
      }
    }
  }

  #[test]
  fn test_as_bool() {
    init();
    assert_eq!(TokenKind::Yes.as_bool(), Some(true));
    assert_eq!(TokenKind::No.as_bool(), Some(false));
    for kind in TokenKind::iter() {
      match kind {
        TokenKind::Yes => {
          assert_eq!(kind.as_bool(), Some(true));
        },
        TokenKind::No => {
          assert_eq!(kind.as_bool(), Some(false));
        },
        _ => {
          assert_eq!(kind.as_bool(), None);
        },
      }
    }
  }
}
