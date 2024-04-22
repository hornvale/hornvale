use super::*;

#[test]
fn test_classify_tokens61() {
  init();
  test_string_classification(
    "steal from elf",
    &[
      TokenKind::Verb,
      TokenKind::Preposition(Preposition::From),
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens62() {
  init();
  test_string_classification(
    "steal gold from elf's moneybag",
    &[
      TokenKind::Verb,
      TokenKind::DirectObject,
      TokenKind::Preposition(Preposition::From),
      TokenKind::Determiner(Determiner::NounPossessive),
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens63() {
  init();
  test_string_classification(
    "give elf coin",
    &[
      TokenKind::Verb,
      TokenKind::Adjective, // Should be a noun.
      TokenKind::DirectObject,
    ],
  );
}

#[test]
fn test_classify_tokens64() {
  init();
  test_string_classification(
    "give elf poisoned coin",
    &[
      TokenKind::Verb,
      TokenKind::Adjective, // Should be a noun.
      TokenKind::Adjective,
      TokenKind::DirectObject,
    ],
  );
}

#[test]
fn test_classify_tokens65() {
  init();
  test_string_classification(
    "give poisoned coin to elf",
    &[
      TokenKind::Verb,
      TokenKind::Adjective,
      TokenKind::DirectObject,
      TokenKind::Preposition(Preposition::To),
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens66() {
  init();
  test_string_classification(
    "sneak poisoned coin into elf's moneybag",
    &[
      TokenKind::Verb,
      TokenKind::Adjective,
      TokenKind::DirectObject,
      TokenKind::Preposition(Preposition::Into),
      TokenKind::Determiner(Determiner::NounPossessive),
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens67() {
  init();
  test_string_classification(
    "look at farmer's neighbor's wife's cat",
    &[
      TokenKind::Verb,
      TokenKind::Preposition(Preposition::At),
      TokenKind::Determiner(Determiner::NounPossessive),
      TokenKind::Determiner(Determiner::NounPossessive),
      TokenKind::Determiner(Determiner::NounPossessive),
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens68() {
  init();
  test_string_classification("!test", &[TokenKind::MagicWord(MagicWord::BangWord)]);
}

#[test]
fn test_classify_tokens69() {
  init();
  test_string_classification(
    "turn to page 569 in the book",
    &[
      TokenKind::Verb,
      TokenKind::Preposition(Preposition::To),
      TokenKind::Word,
      TokenKind::NumberLiteral,
      TokenKind::Preposition(Preposition::In),
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens70() {
  init();
  test_string_classification(
    "examine the writing with the magnifying glass",
    &[
      TokenKind::Verb,
      TokenKind::DirectObject,
      TokenKind::Preposition(Preposition::With),
      TokenKind::Adjective,
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens71() {
  init();
  test_string_classification(
    "examine the writing with the magnifying glass and the flashlight",
    &[
      TokenKind::Verb,
      TokenKind::DirectObject,
      TokenKind::Preposition(Preposition::With),
      TokenKind::Adjective,
      TokenKind::Noun,
      TokenKind::Conjunction(Conjunction::And),
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens72() {
  init();
  test_string_classification(
    "examine the writing with the magnifying glass, the flashlight, and the lantern",
    &[
      TokenKind::Verb,
      TokenKind::DirectObject,
      TokenKind::Preposition(Preposition::With),
      TokenKind::Adjective,
      TokenKind::Noun,
      TokenKind::Character(Character::Comma),
      TokenKind::Noun,
      TokenKind::Character(Character::Comma),
      TokenKind::Conjunction(Conjunction::And),
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens73() {
  init();
  test_string_classification(
    "examine the writing with the magnifying glass, the red shiny flashlight, and the lantern, and the torch",
    &[
      TokenKind::Verb,
      TokenKind::DirectObject,
      TokenKind::Preposition(Preposition::With),
      TokenKind::Adjective,
      TokenKind::Noun,
      TokenKind::Character(Character::Comma),
      TokenKind::Adjective,
      TokenKind::Adjective,
      TokenKind::Noun,
      TokenKind::Character(Character::Comma),
      TokenKind::Conjunction(Conjunction::And),
      TokenKind::Noun,
      TokenKind::Character(Character::Comma),
      TokenKind::Conjunction(Conjunction::And),
      TokenKind::Noun,
    ],
  );
}
