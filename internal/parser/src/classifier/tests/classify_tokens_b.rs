use super::*;

#[test]
fn test_classify_tokens21() {
  init();
  test_string_classification(
    "take 5 coins",
    &[TokenKind::Verb, TokenKind::NumberLiteral, TokenKind::DirectObject],
  );
}

#[test]
fn test_classify_tokens22() {
  init();
  test_string_classification(
    "take 3rd coin",
    &[TokenKind::Verb, TokenKind::Ordinal, TokenKind::DirectObject],
  );
}

#[test]
fn test_classify_tokens23() {
  init();
  test_string_classification("take all", &[TokenKind::Verb, TokenKind::All]);
}

#[test]
fn test_classify_tokens24() {
  init();
  test_string_classification("take all here", &[TokenKind::Verb, TokenKind::All, TokenKind::Here]);
}

#[test]
fn test_classify_tokens25() {
  init();
  test_string_classification("north", &[TokenKind::Verb]);
}

#[test]
fn test_classify_tokens26() {
  init();
  test_string_classification("go north", &[TokenKind::Verb, TokenKind::North]);
}

#[test]
fn test_classify_tokens27() {
  init();
  test_string_classification("go to north", &[TokenKind::Verb, TokenKind::To, TokenKind::North]);
}

#[test]
fn test_classify_tokens28() {
  init();
  test_string_classification("look around", &[TokenKind::Verb, TokenKind::Around]);
}

#[test]
fn test_classify_tokens29() {
  init();
  test_string_classification("look north", &[TokenKind::Verb, TokenKind::North]);
}

#[test]
fn test_classify_tokens30() {
  init();
  test_string_classification("look at north", &[TokenKind::Verb, TokenKind::At, TokenKind::North]);
}

#[test]
fn test_classify_tokens31() {
  init();
  test_string_classification(
    "look behind curtain",
    &[
      TokenKind::Verb,
      TokenKind::Behind,
      TokenKind::Noun, // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens32() {
  init();
  test_string_classification(
    "look under stove",
    &[
      TokenKind::Verb,
      TokenKind::Under,
      TokenKind::Noun, // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens33() {
  init();
  test_string_classification(
    "look in box",
    &[
      TokenKind::Verb,
      TokenKind::In,
      TokenKind::Noun, // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens34() {
  init();
  test_string_classification(
    "turn lantern on",
    &[TokenKind::Verb, TokenKind::DirectObject, TokenKind::On],
  );
}

#[test]
fn test_classify_tokens35() {
  init();
  test_string_classification(
    "turn radio up",
    &[TokenKind::Verb, TokenKind::DirectObject, TokenKind::Up],
  );
}

#[test]
fn test_classify_tokens36() {
  init();
  test_string_classification(
    "turn on lantern",
    &[
      TokenKind::Verb,
      TokenKind::On,
      TokenKind::Noun, // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens37() {
  init();
  test_string_classification(
    "turn up radio",
    &[TokenKind::Verb, TokenKind::Up, TokenKind::DirectObject],
  );
}

#[test]
fn test_classify_tokens38() {
  init();
  test_string_classification("attack him", &[TokenKind::Verb, TokenKind::Him]);
}

#[test]
fn test_classify_tokens39() {
  init();
  test_string_classification("take hers", &[TokenKind::Verb, TokenKind::DirectObject]);
}

#[test]
fn test_classify_tokens40() {
  init();
  test_string_classification("get mine", &[TokenKind::Verb, TokenKind::DirectObject]);
}
