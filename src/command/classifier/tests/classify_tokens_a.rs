use super::*;

#[test]
fn test_classify_tokens01() {
  init();
  test_string_classification("quit", &[TokenKind::Word(Word::Verb)]);
}

#[test]
fn test_classify_tokens02() {
  init();
  test_string_classification("say 'hello'", &[TokenKind::Word(Word::Verb), TokenKind::StringLiteral]);
}

#[test]
fn test_classify_tokens03() {
  init();
  test_string_classification(
    "say \"hello\"",
    &[TokenKind::Word(Word::Verb), TokenKind::StringLiteral],
  );
}

#[test]
fn test_classify_tokens04() {
  init();
  test_string_classification(
    "say 'hello' to farmer",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::StringLiteral,
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens05() {
  init();
  test_string_classification(
    "say \"hello\" to farmer",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::StringLiteral,
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens06() {
  init();
  test_string_classification(
    "say 'hello' to farmer's cow",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::StringLiteral,
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens07() {
  init();
  test_string_classification(
    "say \"hello\" to farmer's cow",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::StringLiteral,
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens08() {
  init();
  test_string_classification(
    "say to farmer \"hello\"",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(Word::Noun),
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens09() {
  init();
  test_string_classification(
    "say to nice farmer \"hello\"",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(Word::Adjective),
      TokenKind::Word(Word::Noun),
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens10() {
  init();
  test_string_classification(
    "say to lily-livered farmer's cow \"hello\"",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(Word::Adjective),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(Word::Noun),
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens11() {
  init();
  test_string_classification(
    "say to nice lily-livered farmer's cow \"hello\"",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(Word::Adjective),
      TokenKind::Word(Word::Adjective),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(Word::Noun),
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
// This is an acceptable failure; the classifier is not perfect.
fn test_classify_tokens12() {
  init();
  test_string_classification(
    "say to nice, lily-livered farmer's cow \"hello\"",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(Word::Noun), // Should be adjective.
      TokenKind::Character(Character::Comma),
      TokenKind::Word(Word::Adjective),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(Word::Noun),
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens13() {
  init();
  test_string_classification(
    "say to lily-livered nice farmer's cow \"hello\"",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(Word::Adjective),
      TokenKind::Word(Word::Adjective),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(Word::Noun),
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens14() {
  init();
  test_string_classification(
    "say to lily-livered, nice farmer's cow \"hello\"",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(Word::Adjective),
      TokenKind::Character(Character::Comma),
      TokenKind::Word(Word::Adjective),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(Word::Noun),
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens15() {
  init();
  test_string_classification(
    "say to farmer's cow 'hello'",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(Word::Noun),
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens16() {
  init();
  test_string_classification(
    "tell farmer \"I know about you and the chicken.\"",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::Word(Word::Noun),
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens17() {
  init();
  test_string_classification(
    "tell farmer, \"I know about you and the pumpkin.\"",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::Word(Word::Noun),
      TokenKind::Character(Character::Comma),
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens18() {
  init();
  test_string_classification(
    "take sword",
    &[TokenKind::Word(Word::Verb), TokenKind::Word(Word::Noun)],
  );
}

#[test]
fn test_classify_tokens19() {
  init();
  test_string_classification(
    "take sword and shield",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::Word(Word::Noun),
      TokenKind::And,
      TokenKind::Word(Word::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens20() {
  init();
  test_string_classification(
    "take sword, shield",
    &[
      TokenKind::Word(Word::Verb),
      TokenKind::Word(Word::Noun),
      TokenKind::Character(Character::Comma),
      TokenKind::Word(Word::Noun),
    ],
  );
}
