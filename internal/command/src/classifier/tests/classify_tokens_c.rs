use super::*;

#[test]
fn test_classify_tokens41() {
  init();
  test_string_classification(
    "take this sword",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::This,
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens42() {
  init();
  test_string_classification(
    "take each sword",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Each,
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens43() {
  init();
  test_string_classification(
    "take every sword",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Every,
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens44() {
  init();
  test_string_classification(
    "kill troll",
    &[TokenKind::Word(WordToken::Verb), TokenKind::Word(WordToken::Noun)],
  );
}

#[test]
fn test_classify_tokens45() {
  init();
  test_string_classification(
    "kill troll with sword",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun),
      TokenKind::With,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens46() {
  init();
  test_string_classification(
    "give money to elf",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun),
      TokenKind::To,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens47() {
  init();
  test_string_classification(
    "take red cube and green cylinder",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun), // Should be direct object.
      TokenKind::And,
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens48() {
  init();
  test_string_classification(
    "take red cube, green cylinder",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun), // Should be direct object.
      TokenKind::Comma,
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens49() {
  init();
  test_string_classification(
    "take red cube, green cylinder, and yellow prism",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun), // Should be direct object.
      TokenKind::Comma,
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun), // Should be direct object.
      TokenKind::Comma,
      TokenKind::And,
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens50() {
  init();
  test_string_classification(
    "look at red-eyed goblin",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::At,
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens51() {
  init();
  test_string_classification(
    "look at goblin's club",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::At,
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens52() {
  init();
  test_string_classification(
    "look at red-eyed goblin's club",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::At,
      TokenKind::Word(WordToken::Adjective),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens53() {
  init();
  test_string_classification(
    "read print",
    &[TokenKind::Word(WordToken::Verb), TokenKind::Word(WordToken::Noun)],
  );
}

#[test]
fn test_classify_tokens54() {
  init();
  test_string_classification(
    "read print on kettle",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun),
      TokenKind::On,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens55() {
  init();
  test_string_classification(
    "read underside of kettle",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun),
      TokenKind::Of,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens56() {
  init();
  test_string_classification(
    "read print on underside of kettle",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun),
      TokenKind::On,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
      TokenKind::Of,
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens57() {
  init();
  test_string_classification(
    "read print on underside of kettle on stove",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun),
      TokenKind::On,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
      TokenKind::Of,
      TokenKind::Word(WordToken::Noun),
      TokenKind::On,
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens58() {
  init();
  test_string_classification(
    "read fine print on glowing underside of whistling kettle on hot stove",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun),
      TokenKind::On,
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
      TokenKind::Of,
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun),
      TokenKind::On,
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens59() {
  init();
  test_string_classification(
    "remember goblin as franklin",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun),
      TokenKind::As,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens60() {
  init();
  test_string_classification(
    "remember red-eyed, shining-haired goblin as franklin",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Comma,
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun),
      TokenKind::As,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}
