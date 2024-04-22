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
  test_string_classification("take all", &[TokenKind::Verb, TokenKind::Determiner(Determiner::All)]);
}

#[test]
fn test_classify_tokens24() {
  init();
  test_string_classification(
    "take all here",
    &[
      TokenKind::Verb,
      TokenKind::Determiner(Determiner::All),
      TokenKind::Adverb(Adverb::Here),
    ],
  );
}

#[test]
fn test_classify_tokens25() {
  init();
  test_string_classification("north", &[TokenKind::Verb]);
}

#[test]
fn test_classify_tokens26() {
  init();
  test_string_classification("go north", &[TokenKind::Verb, TokenKind::Direction(Direction::North)]);
}

#[test]
fn test_classify_tokens27() {
  init();
  test_string_classification(
    "go to north",
    &[
      TokenKind::Verb,
      TokenKind::Preposition(Preposition::To),
      TokenKind::Direction(Direction::North),
    ],
  );
}

#[test]
fn test_classify_tokens28() {
  init();
  test_string_classification("look around", &[TokenKind::Verb, TokenKind::Adverb(Adverb::Around)]);
}

#[test]
fn test_classify_tokens29() {
  init();
  test_string_classification("look north", &[TokenKind::Verb, TokenKind::Direction(Direction::North)]);
}

#[test]
fn test_classify_tokens30() {
  init();
  test_string_classification(
    "look at north",
    &[
      TokenKind::Verb,
      TokenKind::Preposition(Preposition::At),
      TokenKind::Direction(Direction::North),
    ],
  );
}

#[test]
fn test_classify_tokens31() {
  init();
  test_string_classification(
    "look behind curtain",
    &[
      TokenKind::Verb,
      TokenKind::Preposition(Preposition::Behind),
      TokenKind::Noun,
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
      TokenKind::Preposition(Preposition::Under),
      TokenKind::Noun,
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
      TokenKind::Preposition(Preposition::In),
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens34() {
  init();
  test_string_classification(
    "turn lantern on",
    &[
      TokenKind::Verb,
      TokenKind::DirectObject,
      TokenKind::Preposition(Preposition::On),
    ],
  );
}

#[test]
fn test_classify_tokens35() {
  init();
  test_string_classification(
    "turn radio up",
    &[TokenKind::Verb, TokenKind::DirectObject, TokenKind::Adverb(Adverb::Up)],
  );
}

#[test]
fn test_classify_tokens36() {
  init();
  test_string_classification(
    "turn on lantern",
    &[
      TokenKind::Verb,
      TokenKind::Preposition(Preposition::On),
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens37() {
  init();
  test_string_classification(
    "turn up radio",
    &[TokenKind::Verb, TokenKind::Adverb(Adverb::Up), TokenKind::DirectObject],
  );
}

#[test]
fn test_classify_tokens38() {
  init();
  test_string_classification("attack him", &[TokenKind::Verb, TokenKind::Pronoun(Pronoun::Him)]);
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
