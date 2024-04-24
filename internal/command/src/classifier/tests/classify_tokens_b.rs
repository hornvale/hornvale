use super::*;

#[test]
fn test_classify_tokens21() {
  init();
  test_string_classification(
    "take 5 coins",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::NumberLiteral,
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens22() {
  init();
  test_string_classification(
    "take 3rd coin",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Ordinal,
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens23() {
  init();
  test_string_classification("take all", &[TokenKind::Word(WordToken::Verb), TokenKind::All]);
}

#[test]
fn test_classify_tokens24() {
  init();
  test_string_classification(
    "take all here",
    &[TokenKind::Word(WordToken::Verb), TokenKind::All, TokenKind::Here],
  );
}

#[test]
fn test_classify_tokens25() {
  init();
  test_string_classification("look north", &[TokenKind::Word(WordToken::Verb), TokenKind::North]);
}

#[test]
fn test_classify_tokens26() {
  init();
  test_string_classification("go north", &[TokenKind::Word(WordToken::Verb), TokenKind::North]);
}

#[test]
fn test_classify_tokens27() {
  init();
  test_string_classification(
    "go to north",
    &[TokenKind::Word(WordToken::Verb), TokenKind::To, TokenKind::North],
  );
}

#[test]
fn test_classify_tokens28() {
  init();
  test_string_classification("look around", &[TokenKind::Word(WordToken::Verb), TokenKind::Around]);
}

#[test]
fn test_classify_tokens29() {
  init();
  test_string_classification("look north", &[TokenKind::Word(WordToken::Verb), TokenKind::North]);
}

#[test]
fn test_classify_tokens30() {
  init();
  test_string_classification(
    "look at north",
    &[TokenKind::Word(WordToken::Verb), TokenKind::At, TokenKind::North],
  );
}

#[test]
fn test_classify_tokens31() {
  init();
  test_string_classification(
    "look behind curtain",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Behind,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens32() {
  init();
  test_string_classification(
    "look under stove",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Under,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens33() {
  init();
  test_string_classification(
    "look in box",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::In,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens34() {
  init();
  test_string_classification(
    "turn lantern on",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun),
      TokenKind::On,
    ],
  );
}

#[test]
fn test_classify_tokens35() {
  init();
  test_string_classification(
    "turn radio up",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun),
      TokenKind::Up,
    ],
  );
}

#[test]
fn test_classify_tokens36() {
  init();
  test_string_classification(
    "turn on lantern",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::On,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens37() {
  init();
  test_string_classification(
    "turn up radio",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Up,
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens38() {
  init();
  test_string_classification("attack him", &[TokenKind::Word(WordToken::Verb), TokenKind::Him]);
}

#[test]
fn test_classify_tokens39() {
  init();
  test_string_classification(
    "take hers",
    &[TokenKind::Word(WordToken::Verb), TokenKind::Word(WordToken::Noun)],
  );
}

#[test]
fn test_classify_tokens40() {
  init();
  test_string_classification(
    "get mine",
    &[TokenKind::Word(WordToken::Verb), TokenKind::Word(WordToken::Noun)],
  );
}
