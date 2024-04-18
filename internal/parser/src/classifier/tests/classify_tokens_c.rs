use super::*;

#[test]
fn test_classify_tokens41() {
  init();
  test_string_classification(
    "take this sword",
    &[TokenKind::Verb, TokenKind::This, TokenKind::DirectObject],
  );
}

#[test]
fn test_classify_tokens42() {
  init();
  test_string_classification(
    "take each sword",
    &[TokenKind::Verb, TokenKind::Each, TokenKind::DirectObject],
  );
}

#[test]
fn test_classify_tokens43() {
  init();
  test_string_classification(
    "take every sword",
    &[TokenKind::Verb, TokenKind::Every, TokenKind::DirectObject],
  );
}

#[test]
fn test_classify_tokens44() {
  init();
  test_string_classification("kill troll", &[TokenKind::Verb, TokenKind::DirectObject]);
}

#[test]
fn test_classify_tokens45() {
  init();
  test_string_classification(
    "kill troll with sword",
    &[
      TokenKind::Verb,
      TokenKind::DirectObject,
      TokenKind::With,
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens46() {
  init();
  test_string_classification(
    "give money to elf",
    &[TokenKind::Verb, TokenKind::DirectObject, TokenKind::To, TokenKind::Noun],
  );
}

#[test]
fn test_classify_tokens47() {
  init();
  test_string_classification(
    "take red cube and green cylinder",
    &[
      TokenKind::Verb,
      TokenKind::Word, // Should be an adjective.
      TokenKind::Word, // Should be a noun.
      TokenKind::And,
      TokenKind::Adjective,
      TokenKind::DirectObject,
    ],
  );
}

#[test]
fn test_classify_tokens48() {
  init();
  test_string_classification(
    "take red cube, green cylinder",
    &[
      TokenKind::Verb,
      TokenKind::Adjective,
      TokenKind::Adjective, // Should be a noun.
      TokenKind::Comma,
      TokenKind::Adjective,
      TokenKind::DirectObject,
    ],
  );
}

#[test]
fn test_classify_tokens49() {
  init();
  test_string_classification(
    "take red cube, green cylinder, and yellow prism",
    &[
      TokenKind::Verb,
      TokenKind::Word, // Should be an adjective.
      TokenKind::Word, // Should be a noun.
      TokenKind::Comma,
      TokenKind::Word, // Should be an adjective.
      TokenKind::Word, // Should be a noun.
      TokenKind::Comma,
      TokenKind::And,
      TokenKind::Adjective,
      TokenKind::DirectObject,
    ],
  );
}

#[test]
fn test_classify_tokens50() {
  init();
  test_string_classification(
    "look at red-eyed goblin",
    &[TokenKind::Verb, TokenKind::At, TokenKind::Adjective, TokenKind::Noun],
  );
}

#[test]
fn test_classify_tokens51() {
  init();
  test_string_classification(
    "look at goblin's club",
    &[
      TokenKind::Verb,
      TokenKind::At,
      TokenKind::PossessiveDeterminer,
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens52() {
  init();
  test_string_classification(
    "look at red-eyed goblin's club",
    &[
      TokenKind::Verb,
      TokenKind::At,
      TokenKind::Adjective,
      TokenKind::PossessiveDeterminer,
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens53() {
  init();
  test_string_classification("read print", &[TokenKind::Verb, TokenKind::DirectObject]);
}

#[test]
fn test_classify_tokens54() {
  init();
  test_string_classification(
    "read print on kettle",
    &[TokenKind::Verb, TokenKind::DirectObject, TokenKind::On, TokenKind::Noun],
  );
}

#[test]
fn test_classify_tokens55() {
  init();
  test_string_classification(
    "read underside of kettle",
    &[
      TokenKind::Verb,
      TokenKind::Word, // Should be a noun.
      TokenKind::Of,
      TokenKind::DirectObject,
    ],
  );
}

#[test]
fn test_classify_tokens56() {
  init();
  test_string_classification(
    "read print on underside of kettle",
    &[
      TokenKind::Verb,
      TokenKind::DirectObject,
      TokenKind::On,
      TokenKind::Word, // Should be a noun.
      TokenKind::Of,
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens57() {
  init();
  test_string_classification(
    "read print on underside of kettle on stove",
    &[
      TokenKind::Verb,
      TokenKind::DirectObject,
      TokenKind::On,
      TokenKind::Word,
      TokenKind::Of,
      TokenKind::Noun,
      TokenKind::On,
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens58() {
  init();
  test_string_classification(
    "read fine print on glowing underside of whistling kettle on hot stove",
    &[
      TokenKind::Verb,
      TokenKind::Adjective,
      TokenKind::DirectObject,
      TokenKind::On,
      TokenKind::Word, // Should be an adjective.
      TokenKind::Word, // Should be a noun.
      TokenKind::Of,
      TokenKind::Adjective,
      TokenKind::Noun,
      TokenKind::On,
      TokenKind::Adjective,
      TokenKind::Noun,
    ],
  );
}

#[test]
fn test_classify_tokens59() {
  init();
  test_string_classification(
    "remember goblin as franklin",
    &[
      TokenKind::Verb,
      TokenKind::Word, // Should be a noun.
      TokenKind::As,
      TokenKind::DirectObject, // Should be a word.
    ],
  );
}

#[test]
fn test_classify_tokens60() {
  init();
  test_string_classification(
    "remember red-eyed, shining-haired goblin as franklin",
    &[
      TokenKind::Verb,
      TokenKind::Adjective,
      TokenKind::Comma,
      TokenKind::Adjective,
      TokenKind::Word, // Should be a noun.
      TokenKind::As,
      TokenKind::DirectObject, // Should be a word.
    ],
  );
}
