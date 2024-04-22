use hornvale_core::prelude::*;
use serde::{Deserialize, Serialize};
use strum::Display;

/// Article tokens.
pub mod article;
use article::Article;
/// Boolean tokens.
pub mod boolean;
use boolean::Boolean;
/// Single-character tokens.
pub mod character;
use character::Character;
/// Conjunctions.
pub mod conjunction;
use conjunction::Conjunction;
/// Determiner tokens.
pub mod determiner;
use determiner::Determiner;
/// Magic word tokens.
pub mod magic_word;
use magic_word::MagicWord;
/// Pronoun tokens.
pub mod pronoun;
use pronoun::Pronoun;
/// Question word tokens.
pub mod question_word;
use question_word::QuestionWord;
/// Trait implementations.
pub mod traits;
/// Undetermined modifiers.
pub mod undetermined_modifier;
use undetermined_modifier::UndeterminedModifier;
/// Yes and no.
pub mod yes_no;
use yes_no::YesNo;

/// Different kinds of tokens for the scanner.
#[derive(Clone, Copy, Debug, Default, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum TokenKind {
  /// A single-character token.
  Character(Character),
  /// A string literal, e.g. in `say "hello"`.
  StringLiteral,
  /// A number literal, e.g. in `take 5 coins`. We use `u32` for simplicity.
  NumberLiteral,
  /// An ordinal, e.g. in `take 1st coin`.
  Ordinal,
  /// A direction.
  Direction(Direction),
  /// Adverbs.
  Adverb(Adverb),
  /// Prepositions.
  Preposition(Preposition),
  /// Determiners.
  Determiner(Determiner),
  /// Pronouns.
  Pronoun(Pronoun),
  /// Articles.
  Article(Article),
  /// Words beginning with special characters.
  MagicWord(MagicWord),
  /// Interrogatives, or "question words".
  QuestionWord(QuestionWord),
  /// Conjunctions.
  Conjunction(Conjunction),
  /// Boolean values.
  Boolean(Boolean),
  /// Affirmatives and negatives.
  YesNo(YesNo),
  /// An undetermined command modifier.
  /// This is used by the scanner, and will be classified by the classifier.
  UndeterminedModifier(UndeterminedModifier),
  /// Any other word.
  Word,
  /// A verb (assigned during classification).
  Verb,
  /// An adjective (assigned during classification).
  Adjective,
  /// A noun (assigned during classification).
  Noun,
  /// The direct object (assigned during classification).
  DirectObject,
  /// The indirect object (assigned during classification).
  IndirectObject,
  /// End of input.
  #[default]
  EndOfInput,
}

impl TokenKind {
  /// Is this token a single-character token?
  pub fn is_character(&self) -> bool {
    matches!(self, Self::Character(_))
  }

  /// Is this token a direction?
  pub fn is_direction(&self) -> bool {
    matches!(self, Self::Direction(_))
  }

  /// Is this token a preposition?
  pub fn is_preposition(&self) -> bool {
    matches!(self, Self::Preposition(_))
  }

  /// Is this token an adverb?
  pub fn is_adverb(&self) -> bool {
    matches!(self, Self::Adverb(_))
  }

  /// Is this token a determiner?
  pub fn is_determiner(&self) -> bool {
    matches!(self, Self::Determiner(_))
  }

  /// Is this token a demonstrative determiner?
  pub fn is_demonstrative_determiner(&self) -> bool {
    match self {
      Self::Determiner(determiner) => determiner.is_demonstrative(),
      _ => false,
    }
  }

  /// Is this token a distributive determiner?
  pub fn is_distributive_determiner(&self) -> bool {
    match self {
      Self::Determiner(determiner) => determiner.is_distributive(),
      _ => false,
    }
  }

  /// Is this token a possessive determiner?
  pub fn is_possessive_determiner(&self) -> bool {
    match self {
      Self::Determiner(determiner) => determiner.is_possessive(),
      _ => false,
    }
  }

  /// Is this token a noun possessive determiner?
  pub fn is_noun_possessive_determiner(&self) -> bool {
    match self {
      Self::Determiner(determiner) => determiner.is_noun_possessive(),
      _ => false,
    }
  }

  /// Is this token a personal possessive determiner?
  pub fn is_personal_possessive_determiner(&self) -> bool {
    match self {
      Self::Determiner(determiner) => determiner.is_personal_possessive(),
      _ => false,
    }
  }

  /// Is this token a pronoun?
  pub fn is_pronoun(&self) -> bool {
    matches!(self, Self::Pronoun(_))
  }

  /// Is this token a command modifier?
  pub fn is_modifier(&self) -> bool {
    self.is_preposition() || self.is_adverb()
  }

  /// Is this token an adjective?
  pub fn is_adjective(&self) -> bool {
    matches!(self, Self::Adjective)
  }

  /// Is this token an article?
  pub fn is_article(&self) -> bool {
    matches!(self, Self::Article(_))
  }

  /// Is this token a conjunction?
  pub fn is_conjunction(&self) -> bool {
    matches!(self, Self::Conjunction(_))
  }

  /// Is this token a verb?
  pub fn is_verb(&self) -> bool {
    matches!(self, Self::Verb)
  }

  /// Is this token the "all" token?
  pub fn is_all(&self) -> bool {
    matches!(self, Self::Determiner(Determiner::All))
  }

  /// Is this token a noun?
  pub fn is_noun(&self) -> bool {
    self.is_direction()
      || self.is_pronoun()
      || self.is_all()
      || matches!(self, Self::Noun | Self::DirectObject | Self::IndirectObject)
  }

  /// Is this token undetermined?
  pub fn is_undetermined_modifier(&self) -> bool {
    matches!(self, Self::UndeterminedModifier(_))
  }

  /// Could this token be a noun?
  pub fn could_be_noun(&self) -> bool {
    self.is_noun() || matches!(self, Self::Word)
  }

  /// Could this token be an adjective?
  pub fn could_be_adjective(&self) -> bool {
    matches!(self, Self::Word)
  }

  /// Could this token be an adverb?
  pub fn could_be_adverb(&self) -> bool {
    match self {
      Self::Adverb(_) => true,
      Self::UndeterminedModifier(modifier) => modifier.could_be_adverb(),
      _ => false,
    }
  }

  /// Could this token be a direction?
  pub fn could_be_direction(&self) -> bool {
    match self {
      Self::Direction(_) => true,
      Self::UndeterminedModifier(modifier) => modifier.could_be_direction(),
      _ => false,
    }
  }

  /// Could this token be a preposition?
  pub fn could_be_preposition(&self) -> bool {
    match self {
      Self::Preposition(_) => true,
      Self::UndeterminedModifier(modifier) => modifier.could_be_preposition(),
      _ => false,
    }
  }

  /// Could this token be a verb?
  pub fn could_be_verb(&self) -> bool {
    self.is_verb() || matches!(self, Self::Word)
  }

  /// Does this token accept adjectives?
  pub fn can_follow_adjective(&self) -> bool {
    matches!(
      self,
      Self::Noun
        | Self::DirectObject
        | Self::IndirectObject
        | Self::Adjective
        | Self::Determiner(Determiner::NounPossessive)
        | Self::Character(Character::Comma)
    )
  }

  /// Is this token a magic word?
  pub fn is_magic_word(&self) -> bool {
    matches!(self, Self::MagicWord(_))
  }

  /// Is this token an interrogative?
  pub fn is_question_word(&self) -> bool {
    matches!(self, Self::QuestionWord(_))
  }

  /// Is this token a comma?
  pub fn is_comma(&self) -> bool {
    matches!(self, Self::Character(Character::Comma))
  }

  /// Is this token a yes/no token?
  pub fn is_yes_no(&self) -> bool {
    matches!(self, Self::YesNo(_))
  }

  /// Get the boolean value of this token.
  pub fn as_bool(&self) -> Option<bool> {
    match self {
      Self::Boolean(token) => Some(token.as_bool()),
      Self::YesNo(token) => Some(token.as_bool()),
      _ => None,
    }
  }
}
