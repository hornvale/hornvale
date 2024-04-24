use super::*;

#[test]
fn test_classify_tokens61() {
  init();
  test_string_classification(
    "steal from elf",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::From,
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens62() {
  init();
  test_string_classification(
    "steal gold from elf's moneybag",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun),
      TokenKind::From,
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens63() {
  init();
  test_string_classification(
    "give elf coin",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Adjective), // Should be a noun.
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens64() {
  init();
  test_string_classification(
    "give elf poisoned coin",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Adjective), // Should be a noun.
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens65() {
  init();
  test_string_classification(
    "give poisoned coin to elf",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun),
      TokenKind::To,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens66() {
  init();
  test_string_classification(
    "sneak poisoned coin into elf's moneybag",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun),
      TokenKind::Into,
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens67() {
  init();
  test_string_classification(
    "look at farmer's neighbor's wife's cat",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::At,
      TokenKind::NounPossessiveDeterminer,
      TokenKind::NounPossessiveDeterminer,
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
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
      TokenKind::Word(WordToken::Verb),
      TokenKind::To,
      TokenKind::Word(WordToken::Unclassified), // Should be Word(Noun).
      TokenKind::NumberLiteral,
      TokenKind::In,
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens70() {
  init();
  test_string_classification(
    "examine the writing with the magnifying glass",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun),
      TokenKind::With,
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens71() {
  init();
  test_string_classification(
    "examine the writing with the magnifying glass and the flashlight",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun),
      TokenKind::With,
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
      TokenKind::And,
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens72() {
  init();
  test_string_classification(
    "examine the writing with the magnifying glass, the flashlight, and the lantern",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun),
      TokenKind::With,
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
      TokenKind::Comma,
      TokenKind::Word(WordToken::Noun),
      TokenKind::Comma,
      TokenKind::And,
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens73() {
  init();
  test_string_classification(
    "examine the writing with the magnifying glass, the red shiny flashlight, and the lantern, and the torch",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun),
      TokenKind::With,
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
      TokenKind::Comma,
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
      TokenKind::Comma,
      TokenKind::And,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
      TokenKind::Comma,
      TokenKind::And,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens74() {
  init();
  test_string_classification(
    "give her stick to her",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Her(HerToken::Unclassified), // Should be possessive determiner.
      TokenKind::Word(WordToken::Noun),
      TokenKind::To,
      TokenKind::Her(HerToken::Unclassified), // Should be unclassified.
    ],
  );
}

#[test]
fn test_classify_tokens75() {
  init();
  test_string_classification(
    "turn on car with key",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::On,
      TokenKind::Word(WordToken::Noun), // Should be direct object.
      TokenKind::With,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}
