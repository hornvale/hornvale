use super::*;

#[test]
fn test_classify_tokens01() {
  init();
  test_string_classification("quit", &[TokenKind::Verb]);
}

#[test]
fn test_classify_tokens02() {
  init();
  test_string_classification("say 'hello'", &[TokenKind::Verb, TokenKind::StringLiteral]);
}

#[test]
fn test_classify_tokens03() {
  init();
  test_string_classification("say \"hello\"", &[TokenKind::Verb, TokenKind::StringLiteral]);
}

#[test]
fn test_classify_tokens04() {
  init();
  test_string_classification(
    "say 'hello' to farmer",
    &[
      TokenKind::Verb,
      TokenKind::StringLiteral,
      TokenKind::To,
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens05() {
  init();
  test_string_classification(
    "say \"hello\" to farmer",
    &[
      TokenKind::Verb,
      TokenKind::StringLiteral,
      TokenKind::To,
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens06() {
  init();
  test_string_classification(
    "say 'hello' to farmer's cow",
    &[
      TokenKind::Verb,
      TokenKind::StringLiteral,
      TokenKind::To,
      TokenKind::PossessiveDeterminer,
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens07() {
  init();
  test_string_classification(
    "say \"hello\" to farmer's cow",
    &[
      TokenKind::Verb,
      TokenKind::StringLiteral,
      TokenKind::To,
      TokenKind::PossessiveDeterminer,
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens08() {
  init();
  test_string_classification(
    "say to farmer \"hello\"",
    &[
      TokenKind::Verb,
      TokenKind::To,
      TokenKind::Word, // Should be a noun.
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens09() {
  init();
  // Can't get much out of this pattern currently.
  test_string_classification(
    "say to nice farmer \"hello\"",
    &[
      TokenKind::Verb,
      TokenKind::To,
      TokenKind::Word, // Should be an adjective.
      TokenKind::Word, // Should be a noun.
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens10() {
  init();
  test_string_classification(
    "say to lily-livered farmer's cow \"hello\"",
    &[
      TokenKind::Verb,
      TokenKind::To,
      TokenKind::Adjective,
      TokenKind::PossessiveDeterminer,
      TokenKind::Word, // Probably fix this... should be a noun.
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
// We can't currently classify "nice" as an adjective in this context because
// we don't attempt to process adjectives before a possessive determiner.
fn test_classify_tokens11() {
  init();
  test_string_classification(
    "say to nice lily-livered farmer's cow \"hello\"",
    &[
      TokenKind::Verb,
      TokenKind::To,
      TokenKind::Word, // Should be an adjective.
      TokenKind::Adjective,
      TokenKind::PossessiveDeterminer,
      TokenKind::Word, // Probably fix this... should be a noun.
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens12() {
  init();
  test_string_classification(
    "say to nice, lily-livered farmer's cow \"hello\"",
    &[
      TokenKind::Verb,
      TokenKind::To,
      TokenKind::Word, // Should be an adjective.
      TokenKind::Comma,
      TokenKind::Adjective,
      TokenKind::PossessiveDeterminer,
      TokenKind::Word, // Probably fix this... should be a noun.
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens13() {
  init();
  test_string_classification(
    "say to lily-livered nice farmer's cow \"hello\"",
    &[
      TokenKind::Verb,
      TokenKind::To,
      TokenKind::Adjective,
      TokenKind::Word, // Should be an adjective.
      TokenKind::PossessiveDeterminer,
      TokenKind::Word, // Probably fix this... should be a noun.
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens14() {
  init();
  test_string_classification(
    "say to lily-livered, nice farmer's cow \"hello\"",
    &[
      TokenKind::Verb,
      TokenKind::To,
      TokenKind::Adjective,
      TokenKind::Comma,
      TokenKind::Word, // Should be an adjective.
      TokenKind::PossessiveDeterminer,
      TokenKind::Word, // Probably fix this... should be a noun.
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens15() {
  init();
  test_string_classification(
    "say to farmer's cow 'hello'",
    &[
      TokenKind::Verb,
      TokenKind::To,
      TokenKind::PossessiveDeterminer,
      TokenKind::Word, // Should be a noun.
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens16() {
  init();
  test_string_classification(
    "tell farmer \"I know about you and the chicken.\"",
    &[
      TokenKind::Verb,
      TokenKind::Word, // Should be a noun.
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens17() {
  init();
  test_string_classification(
    "tell farmer, \"I know about you and the pumpkin.\"",
    &[
      TokenKind::Verb,
      TokenKind::Word, // Should be a noun.
      TokenKind::Comma,
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens18() {
  init();
  test_string_classification(
    "take sword",
    &[
      TokenKind::Verb,
      TokenKind::Word, // Should be a noun.
    ],
  );
}

#[test]
fn test_classify_tokens19() {
  init();
  test_string_classification(
    "take sword and shield",
    &[
      TokenKind::Verb,
      TokenKind::Word, // Should be a noun.
      TokenKind::And,
      TokenKind::Word, // Should be a noun.
    ],
  );
}

#[test]
fn test_classify_tokens20() {
  init();
  test_string_classification(
    "take sword, shield",
    &[
      TokenKind::Verb,
      TokenKind::Word, // Should be a noun.
      TokenKind::Comma,
      TokenKind::Word, // Should be a noun.
    ],
  );
}