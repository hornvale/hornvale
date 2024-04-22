use hornvale_command::prelude::*;
use hornvale_core::prelude::*;
use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter};

#[cfg(test)]
mod tests;

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
  As,
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
  Of,
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

  // Demonstrative determiners.
  /// "This", e.g. in `take this`.
  This,
  /// "That", e.g. in `take that`.
  That,
  /// "These", e.g. in `take these`.
  These,
  /// "Those", e.g. in `take those`.
  Those,

  // Distributive determiners.
  /// "All", e.g. in `take all`.
  All,
  Any,
  Each,
  Either,
  Every,
  Neither,
  Some,

  // Possessive determiners.
  My,
  Your,
  His,
  Her,
  Its,
  Our,
  Their,

  // Pronouns.
  /// `I`/`me`/`myself`.
  Me,
  /// `you`.
  You,
  /// `he`.
  Him,
  /// `her`.
  // Her (also possessive determiner)
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

  //
  // Words beginning with special characters.
  //
  /// An exclamation point, e.g. in `!`.
  BangWord,
  /// A question mark, e.g. in `?`.
  QuestionWord,
  /// At sign, e.g. in `@`.
  AtSignWord,
  /// Hash sign, e.g. in `#`.
  HashWord,
  /// Dollar sign, e.g. in `$`.
  DollarWord,
  /// Percent sign, e.g. in `%`.
  PercentWord,
  /// Caret, e.g. in `^`.
  CaretWord,
  /// Ampersand, e.g. in `&`.
  AmpersandWord,
  /// Asterisk, e.g. in `*`.
  AsteriskWord,
  /// Forward slash, e.g. in `/`.
  ForwardSlashWord,
  /// Backward slash, e.g. in `\`.
  BackSlashWord,
  /// Left parenthesis, e.g. in `(`.
  LeftParenthesisWord,
  /// Right parenthesis, e.g. in `)`.
  RightParenthesisWord,
  /// Left square bracket, e.g. in `[`.
  LeftSquareBracketWord,
  /// Right square bracket, e.g. in `]`.
  RightSquareBracketWord,
  /// Left curly brace, e.g. in `{`.
  LeftCurlyBraceWord,
  /// Right curly brace, e.g. in `}`.
  RightCurlyBraceWord,
  /// Less than sign, e.g. in `<`.
  LessThanWord,
  /// Greater than sign, e.g. in `>`.
  GreaterThanWord,
  /// Equals sign, e.g. in `=`.
  EqualsWord,
  /// Plus sign, e.g. in `+`.
  PlusWord,
  /// Minus sign, e.g. in `-`.
  MinusWord,
  /// Pipe, e.g. in `|`.
  PipeWord,
  /// Colon, e.g. in `:`.
  ColonWord,
  /// Underscore, e.g. in `_`.
  UnderscoreWord,
  /// Tilde, e.g. in `~`.
  TildeWord,

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
        | Self::As
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
        | Self::Of
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
    self.is_noun_possessive_determiner() || self.is_personal_possessive_determiner()
  }

  /// Is this token a definite (or pronominal) possessive determiner?
  pub fn is_personal_possessive_determiner(&self) -> bool {
    matches!(
      self,
      Self::My | Self::Your | Self::His | Self::Her | Self::Its | Self::Our | Self::Their
    )
  }

  /// Is this token a noun possessive determiner?
  pub fn is_noun_possessive_determiner(&self) -> bool {
    matches!(self, Self::PossessiveDeterminer)
  }

  /// Is this token a pronoun?
  pub fn is_pronoun(&self) -> bool {
    matches!(
      self,
      Self::Me | Self::You | Self::Him | Self::Her | Self::It | Self::Them
    )
  }

  /// Is this token an adjective?
  pub fn is_adjective(&self) -> bool {
    matches!(self, Self::Adjective)
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
    matches!(self, Self::Word)
  }

  /// Could this token be a verb?
  pub fn could_be_verb(&self) -> bool {
    self.is_verb() || matches!(self, Self::Word)
  }

  /// Does this token accept adjectives?
  pub fn can_follow_adjective(&self) -> bool {
    (self.is_noun() && !self.is_pronoun() && !self.is_direction() && !self.is_distributive_determiner())
      || self.is_noun_possessive_determiner()
      || matches!(self, Self::Comma | Self::Adjective)
  }

  /// Is this token a magic word?
  pub fn is_magic_word(&self) -> bool {
    matches!(
      self,
      Self::BangWord
        | Self::QuestionWord
        | Self::AtSignWord
        | Self::HashWord
        | Self::DollarWord
        | Self::PercentWord
        | Self::CaretWord
        | Self::AmpersandWord
        | Self::AsteriskWord
        | Self::ForwardSlashWord
        | Self::BackSlashWord
        | Self::LeftParenthesisWord
        | Self::RightParenthesisWord
        | Self::LeftSquareBracketWord
        | Self::RightSquareBracketWord
        | Self::LeftCurlyBraceWord
        | Self::RightCurlyBraceWord
        | Self::LessThanWord
        | Self::GreaterThanWord
        | Self::EqualsWord
        | Self::PlusWord
        | Self::MinusWord
        | Self::PipeWord
        | Self::ColonWord
        | Self::UnderscoreWord
        | Self::TildeWord
    )
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

impl From<TokenKind> for Direction {
  fn from(kind: TokenKind) -> Self {
    match kind {
      TokenKind::North => Direction::North,
      TokenKind::Northeast => Direction::Northeast,
      TokenKind::East => Direction::East,
      TokenKind::Southeast => Direction::Southeast,
      TokenKind::South => Direction::South,
      TokenKind::Southwest => Direction::Southwest,
      TokenKind::West => Direction::West,
      TokenKind::Northwest => Direction::Northwest,
      TokenKind::Up => Direction::Up,
      TokenKind::Down => Direction::Down,
      TokenKind::In => Direction::In,
      TokenKind::Out => Direction::Out,
      _ => unreachable!(),
    }
  }
}

impl TryFrom<TokenKind> for CommandArgument {
  type Error = ();

  fn try_from(kind: TokenKind) -> Result<Self, Self::Error> {
    match kind {
      // Directions.
      kind if kind.is_direction() => Ok(CommandArgument::Direction(kind.into())),

      _ => Err(()),
    }
  }
}

impl TryFrom<TokenKind> for CommandForm {
  type Error = ();

  fn try_from(kind: TokenKind) -> Result<Self, Self::Error> {
    match kind {
      // Directions.
      kind if kind.is_direction() => Ok(CommandForm::Direction),
      // Adverbs and Prepositions.
      TokenKind::About => Ok(CommandForm::About),
      TokenKind::Above => Ok(CommandForm::Above),
      TokenKind::Across => Ok(CommandForm::Across),
      TokenKind::Against => Ok(CommandForm::Against),
      TokenKind::Along => Ok(CommandForm::Along),
      TokenKind::Among => Ok(CommandForm::Among),
      TokenKind::Around => Ok(CommandForm::Around),
      TokenKind::As => Ok(CommandForm::As),
      TokenKind::At => Ok(CommandForm::At),
      TokenKind::Before => Ok(CommandForm::Before),
      TokenKind::Behind => Ok(CommandForm::Behind),
      TokenKind::Below => Ok(CommandForm::Below),
      TokenKind::Beside => Ok(CommandForm::Beside),
      TokenKind::Between => Ok(CommandForm::Between),
      TokenKind::Beyond => Ok(CommandForm::Beyond),
      TokenKind::By => Ok(CommandForm::By),
      TokenKind::For => Ok(CommandForm::For),
      TokenKind::From => Ok(CommandForm::From),
      TokenKind::Here => Ok(CommandForm::Here),
      TokenKind::In => Ok(CommandForm::In),
      TokenKind::Into => Ok(CommandForm::Into),
      TokenKind::Of => Ok(CommandForm::Of),
      TokenKind::Off => Ok(CommandForm::Off),
      TokenKind::On => Ok(CommandForm::On),
      TokenKind::Out => Ok(CommandForm::Out),
      TokenKind::Over => Ok(CommandForm::Over),
      TokenKind::To => Ok(CommandForm::To),
      TokenKind::Toward => Ok(CommandForm::Toward),
      TokenKind::Under => Ok(CommandForm::Under),
      TokenKind::Upon => Ok(CommandForm::Upon),
      TokenKind::With => Ok(CommandForm::With),
      TokenKind::Without => Ok(CommandForm::Without),
      _ => Err(()),
    }
  }
}
