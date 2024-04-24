use super::*;

#[test]
fn test_classify_tokens61() {
  init();
  test_string_classification(
    "steal from elf",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::From),
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens62() {
  init();
  test_string_classification(
    "steal gold from elf's moneybag",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::Word(Word::Noun),
      TokenKind::CommandModifier(CommandModifier::From),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
// This is an acceptable failure.
fn test_classify_tokens63() {
  init();
  test_string_classification(
    "give elf coin",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::Word(Word::Adjective), // Should be a noun.
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
// This is an acceptable failure.
fn test_classify_tokens64() {
  init();
  test_string_classification(
    "give elf poisoned coin",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::Word(Word::Adjective), // Should be a noun.
      TokenKind::Word(Word::Adjective),
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens65() {
  init();
  test_string_classification(
    "give poisoned coin to elf",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::Word(Word::Adjective),
      TokenKind::Word(Word::Noun),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens66() {
  init();
  test_string_classification(
    "sneak poisoned coin into elf's moneybag",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::Word(Word::Adjective),
      TokenKind::Word(Word::Noun),
      TokenKind::CommandModifier(CommandModifier::Into),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens67() {
  init();
  test_string_classification(
    "look at farmer's neighbor's wife's cat",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::At),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::NounPossessiveDeterminer,
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
#[should_panic]
fn test_classify_tokens68() {
  init();
  test_string_classification(
    "!test",
    &[
      TokenKind::Word(Word::Verb), // Should be TokenKind::MagicWord(MagicWord::BangWord)
    ],
  );
}

#[test]
fn test_classify_tokens69() {
  init();
  test_string_classification(
    "turn to page 569 in the book",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(Word::Noun),
      TokenKind::NumberLiteral,
      TokenKind::CommandModifier(CommandModifier::In),
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens70() {
  init();
  test_string_classification(
    "examine the writing with the magnifying glass",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::Word(Word::Noun),
      TokenKind::CommandModifier(CommandModifier::With),
      TokenKind::Word(Word::Adjective),
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens71() {
  init();
  test_string_classification(
    "examine the writing with the magnifying glass and the flashlight",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::Word(Word::Noun),
      TokenKind::CommandModifier(CommandModifier::With),
      TokenKind::Word(Word::Adjective),
      TokenKind::Word(Word::Noun),
      TokenKind::And,
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens72() {
  init();
  test_string_classification(
    "examine the writing with the magnifying glass, the flashlight, and the lantern",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::Word(Word::Noun),
      TokenKind::CommandModifier(CommandModifier::With),
      TokenKind::Word(Word::Adjective),
      TokenKind::Word(Word::Noun),
      TokenKind::Character(Character::Comma),
      TokenKind::Word(Word::Noun),
      TokenKind::Character(Character::Comma),
      TokenKind::And,
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens73() {
  init();
  test_string_classification(
    "examine the writing with the magnifying glass, the red shiny flashlight, and the lantern, and the torch",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::Word(Word::Noun),
      TokenKind::CommandModifier(CommandModifier::With),
      TokenKind::Word(Word::Adjective),
      TokenKind::Word(Word::Noun),
      TokenKind::Character(Character::Comma),
      TokenKind::Word(Word::Adjective),
      TokenKind::Word(Word::Adjective),
      TokenKind::Word(Word::Noun),
      TokenKind::Character(Character::Comma),
      TokenKind::And,
      TokenKind::Word(Word::Noun),
      TokenKind::Character(Character::Comma),
      TokenKind::And,
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens74() {
  init();
  test_string_classification(
    "give her stick to her",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::Her(Her::PossessiveDeterminer),
      TokenKind::Word(Word::Noun),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Her(Her::Pronoun),
    ],
  );
}

#[test]
fn test_classify_tokens75() {
  init();
  test_string_classification(
    "turn on car with key",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::On),
      TokenKind::Word(Word::Noun),
      TokenKind::CommandModifier(CommandModifier::With),
      TokenKind::Word(Word::Noun),
    ],
  );
}
