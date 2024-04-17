use crate::prelude::{ParserError, Token, TokenKind};
use serde::{Deserialize, Serialize};
use std::cmp;

/// A case for the command classifier.
///
/// Cases are used to classify commands based on their grammatical structure.
///
/// Our most common words (verbs, nouns, adjectives) are dynamic and numberless
/// and need to be validated using a comparatively expensive process. Also,
/// some words can be multiple parts of speech (e.g. "mine" can be a possessive
/// pronoun, noun, verb, etc, depending on context). We can use cases to
/// disambiguate these words. This helps us parse more performantly and provide
/// better error messages.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Case {
  //
  // The following cases are for one-token commands.
  //
  /// The simplest case: a single verb.
  Verb,

  //
  // The following cases are for two-token commands.
  //
  /// A verb followed by a direction.
  VerbDirection,
  /// A verb followed by a pronoun.
  VerbPronoun,
  /// A verb followed by an adverb.
  VerbAdverb,
  /// A verb followed by a noun, which acts as the direct object.
  VerbDirectObject,
  /// A verb followed by a distributive determiner, specifically "all".
  VerbDistributiveDeterminer,

  //
  // The following cases are for three-token commands.
  //
  /// A verb followed by a direct object and an adverb.
  VerbDirectObjectAdverb,

  /// A verb followed by a preposition and an indirect object.
  VerbImpliedYouPrepositionIndirectObject,

  /// A verb followed by a demonstrative determiner and a direct object.
  VerbDemonstrativeDeterminerDirectObject,

  /// A verb followed by a distributive determiner and a direct object.
  VerbDistributiveDeterminerDirectObject,

  /// A verb followed by a possessive determiner and a direct object.
  VerbPossessiveDeterminerDirectObject,
}

/// Try to classify a slice of tokens into a case.
///
/// This is a simple pattern matcher that can then be used to alter the list of
/// tokens in the classifier.
impl TryFrom<&[Token]> for Case {
  type Error = ParserError;

  fn try_from(tokens: &[Token]) -> Result<Self, Self::Error> {
    // The index of the end of input (EOI) is the last token.
    let eoi_index = tokens.len() - 1;
    // The last index is the last token, or 0 if there are no tokens.
    let last_index = cmp::max(eoi_index, 0);
    // Match the tokens based on their position in the sentence.
    match &tokens[..last_index] {
      //
      // Some error conditions...
      //

      // If there are no tokens, there is nothing to classify.
      [] => Err(ParserError::NoInput),

      //
      // The following cases are for one-token commands.
      //

      // If there is only one token, it must be a verb.
      [Token {
        kind: TokenKind::Word, ..
      }] => Ok(Self::Verb),

      // Commands must begin with a verb.
      // We might change this in the future.
      [Token { kind, .. }, ..] if kind != &TokenKind::Word => Err(ParserError::NoVerb),

      //
      // The following cases are for two-token commands.
      //

      // The following are not valid in the second position:
      // - Adjectives
      // - Demonstrative determiners
      // - Possessive determiners
      // - Possessive pronouns
      // - Prepositions
      //
      // The following are valid in the second position:
      // - Adverbs
      // - Directions
      // - Distributive determiners
      // - Pronouns
      // - Nouns

      // It might be a direction (e.g. `go north`).
      [_, Token { kind, .. }] if kind.is_direction() => Ok(Self::VerbDirection),

      // It might be a pronoun (e.g. `attack her`).
      //
      // This catches the case where the pronoun is "her", which is also a
      // possessive determiner, but would not be appropriate in this context.
      [_, Token { kind, .. }] if kind.is_pronoun() => Ok(Self::VerbPronoun),

      // It might be an adverb (e.g. `look around`).
      [_, Token { kind, .. }] if kind.is_adverb() => Ok(Self::VerbAdverb),

      // It might be a distributive determiner (e.g. `take all`).
      [_, Token { kind, .. }] if kind.is_distributive_determiner() => Ok(Self::VerbDistributiveDeterminer),

      // Otherwise, assume it's a noun acting as the direct object (e.g.
      // `take sword`).
      [_, Token {
        kind: TokenKind::Word, ..
      }] => Ok(Self::VerbDirectObject),

      //
      // The following cases are for three-token commands.
      //

      // Essentially any token can appear in the second position.

      // If the second token is a preposition and the third is a direction,
      // the command is a verb followed by a preposition and a direction (e.g.
      // `go to north`).
      [_, Token { kind: kind2, .. }, Token { kind: kind3, .. }] if kind2.is_preposition() && kind3.is_direction() => {
        Ok(Self::VerbDirection)
      },

      // If the second token is a preposition and the third is a pronoun, the
      // command is a verb followed by a preposition and a pronoun (e.g. `look
      // at it`).
      [_, Token { kind: kind2, .. }, Token { kind: kind3, .. }] if kind2.is_preposition() && kind3.is_pronoun() => {
        Ok(Self::VerbPronoun)
      },

      // If the third token is an adverb, the command is a verb followed by a
      // direct object and an adverb (e.g. `turn lantern on`). This is the only
      // case where the direct object can be followed by an adverb.
      [_, Token {
        kind: TokenKind::Word, ..
      }, Token { kind, .. }]
        if kind.is_adverb() =>
      {
        Ok(Self::VerbDirectObjectAdverb)
      },

      // If the second token is an preposition, the command is a verb followed
      // by a preposition, the implied subject "you", and an indirect object
      // (e.g. `look behind curtain`).
      [_, Token { kind, .. }, Token {
        kind: TokenKind::Word, ..
      }] if kind.is_preposition() => Ok(Self::VerbImpliedYouPrepositionIndirectObject),

      // The second token might also be a demonstrative determiner; the third
      // token is then the direct object.
      [_, Token { kind, .. }, Token {
        kind: TokenKind::Word, ..
      }] if kind.is_demonstrative_determiner() => Ok(Self::VerbDemonstrativeDeterminerDirectObject),

      // The second token might also be a distributive determiner; the third
      // token is then the direct object.
      [_, Token { kind, .. }, Token {
        kind: TokenKind::Word, ..
      }] if kind.is_distributive_determiner() => Ok(Self::VerbDistributiveDeterminerDirectObject),

      // The second token might also be a possessive determiner.
      [_, Token { kind, .. }, Token {
        kind: TokenKind::Word, ..
      }] if kind.is_possessive_determiner() => Ok(Self::VerbPossessiveDeterminerDirectObject),

      // Otherwise, it's... a more complicated situation.
      _ => {
        // If we can't classify a command, just return it and let the parser deal
        // with it.
        Err(ParserError::CouldNotClassifyInput)
      },
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::prelude::Scanner;
  use hornvale_test_utilities::prelude::*;

  fn test_string(string: &str, expected: &Result<Case, ParserError>) {
    let tokens = Scanner::new(string).scan_tokens().unwrap();
    assert_eq!(Case::try_from(&tokens[..]), *expected, "case: {}", string);
  }

  #[test]
  fn test_try_from() {
    init();
    let cases = [
      ("", Err(ParserError::NoInput)),
      ("go north", Ok(Case::VerbDirection)),
      ("attack him", Ok(Case::VerbPronoun)),
      ("look around", Ok(Case::VerbAdverb)),
      ("take sword", Ok(Case::VerbDirectObject)),
      ("turn lantern on", Ok(Case::VerbDirectObjectAdverb)),
      ("look behind curtain", Ok(Case::VerbImpliedYouPrepositionIndirectObject)),
      ("take this sword", Ok(Case::VerbDemonstrativeDeterminerDirectObject)),
      ("take each sword", Ok(Case::VerbDistributiveDeterminerDirectObject)),
      ("take my sword", Ok(Case::VerbPossessiveDeterminerDirectObject)),
      ("quit", Ok(Case::Verb)),
      ("say 'hello'", Err(ParserError::CouldNotClassifyInput)),
      ("say \"hello\"", Err(ParserError::CouldNotClassifyInput)),
      ("say 'hello' to farmer", Err(ParserError::CouldNotClassifyInput)),
      ("say \"hello\" to farmer", Err(ParserError::CouldNotClassifyInput)),
      ("say 'hello' to farmer's cow", Err(ParserError::CouldNotClassifyInput)),
      ("say \"hello\" to farmer's cow", Err(ParserError::CouldNotClassifyInput)),
      ("say to farmer \"hello\"", Err(ParserError::CouldNotClassifyInput)),
      ("say to farmer's cow 'hello'", Err(ParserError::CouldNotClassifyInput)),
      (
        "tell farmer \"I know about you and the chicken.\"",
        Err(ParserError::CouldNotClassifyInput),
      ),
      (
        "tell farmer, \"I know about you and the pumpkin.\"",
        Err(ParserError::CouldNotClassifyInput),
      ),
      ("take sword and shield", Err(ParserError::CouldNotClassifyInput)),
      ("take 5 coins", Err(ParserError::CouldNotClassifyInput)),
      ("take 3rd coin", Err(ParserError::CouldNotClassifyInput)),
      ("take all", Ok(Case::VerbDistributiveDeterminer)),
      ("take all here", Err(ParserError::CouldNotClassifyInput)),
      ("north", Err(ParserError::NoVerb)),
      ("go", Ok(Case::Verb)),
      ("look", Ok(Case::Verb)),
      ("take", Ok(Case::Verb)),
      ("turn", Ok(Case::Verb)),
      ("attack", Ok(Case::Verb)),
      ("take sword then take shield", Err(ParserError::CouldNotClassifyInput)),
      (
        "take red cube and green cylinder",
        Err(ParserError::CouldNotClassifyInput),
      ),
      ("take red cube, green cylinder", Err(ParserError::CouldNotClassifyInput)),
      (
        "take red cube, green cylinder, and yellow prism",
        Err(ParserError::CouldNotClassifyInput),
      ),
      ("look at red-eyed goblin", Err(ParserError::CouldNotClassifyInput)),
      ("look at goblin's club", Err(ParserError::CouldNotClassifyInput)),
      (
        "look at red-eyed goblin's club",
        Err(ParserError::CouldNotClassifyInput),
      ),
      ("read print", Ok(Case::VerbDirectObject)),
      ("read print on kettle", Err(ParserError::CouldNotClassifyInput)),
      (
        "read print on underside of kettle",
        Err(ParserError::CouldNotClassifyInput),
      ),
      (
        "read print on underside of kettle on stove",
        Err(ParserError::CouldNotClassifyInput),
      ),
      (
        "read fine print on glowing underside of whistling kettle on hot stove",
        Err(ParserError::CouldNotClassifyInput),
      ),
      ("remember goblin as franklin", Err(ParserError::CouldNotClassifyInput)),
      (
        "remember red-eyed, shining-haired goblin as franklin",
        Err(ParserError::CouldNotClassifyInput),
      ),
      ("steal from elf", Ok(Case::VerbImpliedYouPrepositionIndirectObject)),
      (
        "steal gold from elf's moneybag",
        Err(ParserError::CouldNotClassifyInput),
      ),
      ("give money to elf", Err(ParserError::CouldNotClassifyInput)),
      ("give elf coin", Err(ParserError::CouldNotClassifyInput)),
      ("give elf poisoned coin", Err(ParserError::CouldNotClassifyInput)),
      (
        "give elf poisoned coin with red ribbon",
        Err(ParserError::CouldNotClassifyInput),
      ),
      (
        "give elf poisoned coin with red ribbon and green bow",
        Err(ParserError::CouldNotClassifyInput),
      ),
    ];
    for (string, case) in cases.iter() {
      test_string(string, case);
    }
  }
}
