use super::*;

#[test]
fn test_classify_tokens01() {
  init();
  test_string_classification("quit", &[TokenKind::Word(WordToken::Verb)]);
}

#[test]
fn test_classify_tokens02() {
  init();
  test_string_classification(
    "say 'hello'",
    &[TokenKind::Word(WordToken::Verb), TokenKind::StringLiteral],
  );
}

#[test]
fn test_classify_tokens03() {
  init();
  test_string_classification(
    "say \"hello\"",
    &[TokenKind::Word(WordToken::Verb), TokenKind::StringLiteral],
  );
}

#[test]
fn test_classify_tokens04() {
  init();
  test_string_classification(
    "say 'hello' to farmer",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::StringLiteral,
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens05() {
  init();
  test_string_classification(
    "say \"hello\" to farmer",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::StringLiteral,
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens06() {
  init();
  test_string_classification(
    "say 'hello' to farmer's cow",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::StringLiteral,
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens07() {
  init();
  test_string_classification(
    "say \"hello\" to farmer's cow",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::StringLiteral,
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
    ],
  );
}

#[test]
fn test_classify_tokens08() {
  init();
  test_string_classification(
    "say to farmer \"hello\"",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
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
      TokenKind::Word(WordToken::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
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
      TokenKind::Word(WordToken::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(WordToken::Adjective),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
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
      TokenKind::Word(WordToken::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Adjective),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens12() {
  init();
  test_string_classification(
    "say to nice, lily-livered farmer's cow \"hello\"",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(WordToken::Noun), // Should be adjective.
      TokenKind::Character(CharacterToken::Comma),
      TokenKind::Word(WordToken::Adjective),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(WordToken::Noun), // Should be direct object.
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
      TokenKind::Word(WordToken::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Word(WordToken::Adjective),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(WordToken::Noun), // Should be indirect object.
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
      TokenKind::Word(WordToken::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::Word(WordToken::Adjective),
      TokenKind::Character(CharacterToken::Comma),
      TokenKind::Word(WordToken::Adjective),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(WordToken::Noun), // Should be direct object.
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
      TokenKind::Word(WordToken::Verb),
      TokenKind::CommandModifier(CommandModifier::To),
      TokenKind::NounPossessiveDeterminer,
      TokenKind::Word(WordToken::Noun), // Should be direct object.
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
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun),
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
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun),
      TokenKind::Character(CharacterToken::Comma),
      TokenKind::StringLiteral,
    ],
  );
}

#[test]
fn test_classify_tokens18() {
  init();
  test_string_classification(
    "take sword",
    &[TokenKind::Word(WordToken::Verb), TokenKind::Word(WordToken::Noun)],
  );
}

#[test]
fn test_classify_tokens19() {
  init();
  test_string_classification(
    "take sword and shield",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun),
      TokenKind::And,
      TokenKind::Word(WordToken::Noun),
    ],
  );
}

#[test]
fn test_classify_tokens20() {
  init();
  test_string_classification(
    "take sword, shield",
    &[
      TokenKind::Word(WordToken::Verb),
      TokenKind::Word(WordToken::Noun), // Should be direct object.
      TokenKind::Character(CharacterToken::Comma),
      TokenKind::Word(WordToken::Noun),
    ],
  );
}
