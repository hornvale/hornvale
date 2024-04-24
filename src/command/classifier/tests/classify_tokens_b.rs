use super::*;

#[test]
fn test_classify_tokens21() {
  init();
  test_string_classification(
    "take 5 coins",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::NumberLiteral,
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens22() {
  init();
  test_string_classification(
    "take 3rd coin",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::Ordinal,
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens23() {
  init();
  test_string_classification("take all", &[TokenKind::Word(Word::Verb), TokenKind::All]);
}

#[test]
fn test_classify_tokens24() {
  init();
  test_string_classification(
    "take all here",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::All,
      TokenKind::CommandModifier(CommandModifier::Here),
    ],
  );
}

#[test]
fn test_classify_tokens25() {
  init();
  test_string_classification(
    "look north",
    &[TokenKind::Word(Word::Verb), TokenKind::Direction(Direction::North)],
  );
}

#[test]
fn test_classify_tokens26() {
  init();
  test_string_classification(
    "go north",
    &[TokenKind::Word(Word::Verb), TokenKind::Direction(Direction::North)],
  );
}

#[test]
fn test_classify_tokens27() {
  init();
  test_string_classification(
    "go to north",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Direction(Direction::North),
    ],
  );
}

#[test]
fn test_classify_tokens28() {
  init();
  test_string_classification(
    "look around",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::Around),
    ],
  );
}

#[test]
fn test_classify_tokens29() {
  init();
  test_string_classification(
    "look north",
    &[TokenKind::Word(Word::Verb), TokenKind::Direction(Direction::North)],
  );
}

#[test]
fn test_classify_tokens30() {
  init();
  test_string_classification(
    "look at north",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::At),
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
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::Behind),
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens32() {
  init();
  test_string_classification(
    "look under stove",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::Under),
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens33() {
  init();
  test_string_classification(
    "look in box",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::In),
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens34() {
  init();
  test_string_classification(
    "turn lantern on",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::Word(Word::Noun),
      TokenKind::CommandModifier(CommandModifier::On),
    ],
  );
}

#[test]
fn test_classify_tokens35() {
  init();
  test_string_classification(
    "turn radio up",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::Word(Word::Noun),
      TokenKind::CommandModifier(CommandModifier::Up),
    ],
  );
}

#[test]
fn test_classify_tokens36() {
  init();
  test_string_classification(
    "turn on lantern",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::On),
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens37() {
  init();
  test_string_classification(
    "turn up radio",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::Up),
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens38() {
  init();
  test_string_classification("attack him", &[TokenKind::Word(Word::Verb), TokenKind::Him]);
}

#[test]
fn test_classify_tokens39() {
  init();
  test_string_classification("take hers", &[TokenKind::Word(Word::Verb), TokenKind::Word(Word::Noun)]);
}

#[test]
fn test_classify_tokens40() {
  init();
  test_string_classification("get mine", &[TokenKind::Word(Word::Verb), TokenKind::Word(Word::Noun)]);
}
