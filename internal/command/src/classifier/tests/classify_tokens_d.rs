use super::*;

#[test]
fn test_classify_tokens61() {
  init();
  test_string_classification("steal from elf", &[TokenKind::Verb, TokenKind::From, TokenKind::Noun]);
}

#[test]
fn test_classify_tokens62() {
  init();
  test_string_classification(
    "steal gold from elf's moneybag",
    &[
      TokenKind::Verb,
      TokenKind::DirectObject,
      TokenKind::From,
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Noun, // Should be indirect object.
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
      TokenKind::To,
      TokenKind::Noun, // Should be indirect object.
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
      TokenKind::Into,
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Noun, // Should be indirect object.
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
      TokenKind::At,
      TokenKind::NounPossessiveDeterminer,
      TokenKind::NounPossessiveDeterminer,
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Noun, // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens68() {
  init();
  test_string_classification("!test", &[TokenKind::BangWord]);
}

#[test]
fn test_classify_tokens69() {
  init();
  test_string_classification(
    "turn to page 569 in the book",
    &[
      TokenKind::Verb,
      TokenKind::To,
      TokenKind::Word, // Should be indirect object.
      TokenKind::NumberLiteral,
      TokenKind::In,
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
      TokenKind::With,
      TokenKind::Adjective,
      TokenKind::Noun, // Should be indirect object.
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
      TokenKind::With,
      TokenKind::Adjective,
      TokenKind::Noun, // Should be indirect object.
      TokenKind::And,
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
      TokenKind::With,
      TokenKind::Adjective,
      TokenKind::Noun, // Should be indirect object.
      TokenKind::Comma,
      TokenKind::Noun,
      TokenKind::Comma,
      TokenKind::And,
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
      TokenKind::With,
      TokenKind::Adjective,
      TokenKind::Noun, // Should be indirect object.
      TokenKind::Comma,
      TokenKind::Adjective,
      TokenKind::Adjective,
      TokenKind::Noun, // Should be indirect object.
      TokenKind::Comma,
      TokenKind::And,
      TokenKind::Noun, // Should be indirect object.
      TokenKind::Comma,
      TokenKind::And,
      TokenKind::Noun, // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens74() {
  init();
  test_string_classification(
    "give her stick to her",
    &[
      TokenKind::Verb,
      TokenKind::Her,
      TokenKind::DirectObject,
      TokenKind::To,
      TokenKind::Her,
    ],
  );
}

#[test]
fn test_classify_tokens75() {
  init();
  test_string_classification(
    "turn on car with key",
    &[
      TokenKind::Verb,
      TokenKind::On,
      TokenKind::Noun, // Should be direct object.
      TokenKind::With,
      TokenKind::Noun, // Should be indirect object.
    ],
  );
}
