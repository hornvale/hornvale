use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter};

#[cfg(test)]
mod tests;

/// An enumeration of the possibilities for single-character tokens.
pub mod character_token;
use character_token::CharacterToken;
/// An enumeration of the possibilities for `Her`.
pub mod her_token;
use her_token::HerToken;
/// An enumeration of the possibilities for any given `MagicWord` token.
pub mod magic_word_token;
use magic_word_token::MagicWordToken;
/// Trait implementations.
pub mod traits;
/// An enumeration of the possibilities for any given `Word` token.
pub mod word_token;
use word_token::WordToken;

/// Different kinds of tokens for the scanner.
#[derive(Clone, Copy, Debug, Display, EnumIter, Eq, Hash, PartialEq, Serialize, Deserialize)]
#[allow(missing_docs)]
pub enum TokenKind {
  /// Single-character tokens.
  Character(CharacterToken),
  /// A string literal, e.g. in `say "hello"`.
  StringLiteral,
  /// A number literal, e.g. in `take 5 coins`. We use `u32` for simplicity.
  NumberLiteral,
  /// An ordinal, e.g. in `take 1st coin`.
  Ordinal,
  /// A general possessive determiner ("someone's").
  NounPossessiveDeterminer,
  /// Words beginning with special characters.
  MagicWord(MagicWordToken),

  //
  // Word tokens.
  //
  // Compass directions.
  /// "North", e.g. in `go north`.
  North,
  /// "Northeast", e.g. in `go northeast`.
  Northeast,
  /// "East", e.g. in `go east`.
  East,
  /// "Southeast", e.g. in `go southeast`.
  Southeast,
  /// "South", e.g. in `go south`.
  South,
  /// "Southwest", e.g. in `go southwest`.
  Southwest,
  /// "West", e.g. in `go west`.
  West,
  /// "Northwest", e.g. in `go northwest`.
  Northwest,

  // Command modifiers.
  /// "About", e.g. in `ask man about the goat`.
  About,
  /// "Above", e.g. in `look above the shelf`.
  Above,
  /// "Across", e.g. in `go across the bridge`.
  Across,
  /// "Against", e.g. in `lean against the wall`.
  Against,
  /// "Along", e.g. in `walk along the path`.
  Along,
  /// "Among", e.g. in `look among the trees`.
  Among,
  /// "Around", e.g. in `look around`.
  Around,
  /// "As", e.g. in `take it as a gift`.
  As,
  /// "At", e.g. in `look at the sky`.
  At,
  /// "Before", e.g. in `go before the king`.
  Before,
  /// "Behind", e.g. in `hide behind the tree`.
  Behind,
  /// "Below", e.g. in `look below the table`.
  Below,
  /// "Beside", e.g. in `stand beside the knight`.
  Beside,
  /// "Between", e.g. in `walk between the trees`.
  Between,
  /// "Beyond", e.g. in `go beyond the mountains`.
  Beyond,
  /// "By", e.g. in `stand by the door`.
  By,
  /// "Down", e.g. in `climb down the ladder`.
  Down,
  /// "For", e.g. in `fight for the king`.
  For,
  /// "From", e.g. in `take it from the chest`.
  From,
  /// "Here", e.g. in `look here`.
  Here,
  /// "In", e.g. in `put the pie in the oven`.
  In,
  /// "Into", e.g. in `go into the forest`.
  Into,
  /// "Of", e.g. in `take the sword of the king`.
  Of,
  /// "Off", e.g. in `take the hat off`.
  Off,
  /// "On", e.g. in `put the book on the table`.
  On,
  /// "Out", e.g. in `go out of the cave`.
  Out,
  /// "Over", e.g. in `jump over the fence`.
  Over,
  /// "To", e.g. in `go to the castle`.
  To,
  /// "Toward", e.g. in `walk toward the light`.
  Toward,
  /// "Under", e.g. in `hide under the bed`.
  Under,
  /// "Up", e.g. in `climb up the mountain`.
  Up,
  /// "Upon", e.g. in `stand upon the hill`.
  Upon,
  /// "With", e.g. in `fight with the sword`.
  With,
  /// "Without", e.g. in `go without the sword`.
  Without,

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
  /// `my`.
  My,
  /// `your`.
  Your,
  /// `his`.
  His,
  /// `her` (can also act as a pronoun).
  Her(HerToken),
  /// `its`.
  Its,
  /// `our`.
  Our,
  /// `their`.
  Their,

  // Pronouns.
  /// `I`/`me`/`myself`.
  Me,
  /// `you`.
  You,
  /// `he`.
  Him,
  // `her`.
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

  /// Any other word.
  Word(WordToken),

  //
  // Any other tokens.
  //
  /// End of input.
  EndOfInput,
}

impl TokenKind {
  /// Is this token a single-character token?
  pub fn is_single_character(&self) -> bool {
    matches!(self, Self::Character(_))
  }

  /// Is this token punctuation (that we will use in the parser)?
  ///
  /// Currently, the only valid punctuation character is the comma.
  pub fn is_punctuation(&self) -> bool {
    matches!(self, Self::Character(CharacterToken::Comma))
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
    self.is_single_character()
      && !matches!(
        self,
        Self::Character(CharacterToken::Comma | CharacterToken::SingleQuote)
      )
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
      Self::Around | Self::Down | Self::Here | Self::In | Self::Off | Self::On | Self::Out | Self::Up
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
      Self::My
        | Self::Your
        | Self::His
        | Self::Her(HerToken::PossessiveDeterminer)
        | Self::Its
        | Self::Our
        | Self::Their
    )
  }

  /// Is this token a noun possessive determiner?
  pub fn is_noun_possessive_determiner(&self) -> bool {
    matches!(self, Self::NounPossessiveDeterminer)
  }

  /// Is this token a pronoun?
  pub fn is_pronoun(&self) -> bool {
    matches!(
      self,
      Self::Me | Self::You | Self::Him | Self::Her(HerToken::Pronoun) | Self::It | Self::Them
    )
  }

  /// Is this token an adjective?
  pub fn is_adjective(&self) -> bool {
    matches!(self, Self::Word(WordToken::Adjective))
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
    matches!(self, Self::Word(WordToken::Verb))
  }

  /// Is this token a noun?
  pub fn is_noun(&self) -> bool {
    self.is_direction() || self.is_pronoun() || matches!(self, Self::All | Self::Word(WordToken::Noun))
  }

  /// Could this token be a noun?
  pub fn could_be_noun(&self) -> bool {
    self.is_noun()
      || matches!(
        self,
        Self::Word(WordToken::Unclassified | WordToken::Noun | WordToken::Ambiguous)
      )
  }

  /// Could this token be an adjective?
  pub fn could_be_adjective(&self) -> bool {
    matches!(
      self,
      Self::Word(WordToken::Unclassified | WordToken::Adjective | WordToken::Ambiguous)
    )
  }

  /// Could this token be a verb?
  pub fn could_be_verb(&self) -> bool {
    self.is_verb()
      || matches!(
        self,
        Self::Word(WordToken::Unclassified | WordToken::Verb | WordToken::Ambiguous)
      )
  }

  /// Does this token accept adjectives?
  pub fn can_follow_adjective(&self) -> bool {
    (self.is_noun() && !self.is_pronoun() && !self.is_direction() && !self.is_distributive_determiner())
      || self.is_noun_possessive_determiner()
      || matches!(
        self,
        Self::Character(CharacterToken::Comma) | Self::Word(WordToken::Adjective)
      )
  }

  /// Is this token a magic word?
  pub fn is_magic_word(&self) -> bool {
    matches!(self, Self::MagicWord(_))
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
