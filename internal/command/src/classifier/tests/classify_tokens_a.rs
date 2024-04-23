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
      TokenKind::Noun, // Should be indirect object.
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
      TokenKind::Noun, // Should be indirect object.
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
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Noun, // Should be indirect object.
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
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Noun, // Should be indirect object.
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
      TokenKind::Noun, // Should be indirect object.
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens09() {
  init();
  test_string_classification(
    "say to nice farmer \"hello\"",
    &[
      TokenKind::Verb,
      TokenKind::To,
      TokenKind::Adjective,
      TokenKind::Noun, // Should be indirect object.
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
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Noun, // Should be indirect object.
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens11() {
  init();
  test_string_classification(
    "say to nice lily-livered farmer's cow \"hello\"",
    &[
      TokenKind::Verb,
      TokenKind::To,
      TokenKind::Adjective,
      TokenKind::Adjective,
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Noun, // Should be indirect object.
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
      TokenKind::Noun, // Should be adjective.
      TokenKind::Comma,
      TokenKind::Adjective,
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Noun, // Should be direct object.
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
      TokenKind::Adjective,
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Noun, // Should be indirect object.
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
      TokenKind::Adjective,
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Noun, // Should be direct object.
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
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Noun, // Should be direct object.
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens16() {
  init();
  test_string_classification(
    "tell farmer \"I know about you and the chicken.\"",
    &[TokenKind::Verb, TokenKind::DirectObject, TokenKind::StringLiteral],
  );
}

#[test]
fn test_classify_tokens17() {
  init();
  test_string_classification(
    "tell farmer, \"I know about you and the pumpkin.\"",
    &[
      TokenKind::Verb,
      TokenKind::DirectObject,
      TokenKind::Comma,
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens18() {
  init();
  test_string_classification("take sword", &[TokenKind::Verb, TokenKind::DirectObject]);
}

#[test]
fn test_classify_tokens19() {
  init();
  test_string_classification(
    "take sword and shield",
    &[
      TokenKind::Verb,
      TokenKind::Noun,
      TokenKind::And,
      TokenKind::DirectObject,
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
      TokenKind::Noun, // Should be direct object.
      TokenKind::Comma,
      TokenKind::DirectObject,
    ],
  );
}
