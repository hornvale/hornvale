use super::super::*;
use hornvale_core::prelude::*;
use hornvale_test_utilities::prelude::*;

#[test]
fn test_process_presumed_noun1() {
  init();
  let mut tokens = vec![
    Token {
      kind: TokenKind::CommandModifier(CommandModifier::At),
      lexeme: "at".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::default()),
      lexeme: "table".to_string(),
    },
  ];
  let classifier = Classifier::new();
  classifier.process_presumed_noun(&mut tokens, 1).unwrap();
  assert_eq!(tokens[0].kind, TokenKind::CommandModifier(CommandModifier::At));
  assert_eq!(tokens[1].kind, TokenKind::Word(WordToken::Noun));
}

#[test]
fn test_process_presumed_noun2() {
  init();
  let mut tokens = vec![
    Token {
      kind: TokenKind::CommandModifier(CommandModifier::At),
      lexeme: "at".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::default()),
      lexeme: "table".to_string(),
    },
    Token {
      kind: TokenKind::CommandModifier(CommandModifier::In),
      lexeme: "in".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::default()),
      lexeme: "kitchen".to_string(),
    },
  ];
  let classifier = Classifier::new();
  classifier.process_presumed_noun(&mut tokens, 3).unwrap();
  assert_eq!(tokens[0].kind, TokenKind::CommandModifier(CommandModifier::At));
  assert_eq!(tokens[1].kind, TokenKind::Word(WordToken::default()));
  assert_eq!(tokens[2].kind, TokenKind::CommandModifier(CommandModifier::In));
  assert_eq!(tokens[3].kind, TokenKind::Word(WordToken::Noun));
}

#[test]
fn test_process_presumed_noun3() {
  init();
  let mut tokens = vec![
    Token {
      kind: TokenKind::CommandModifier(CommandModifier::At),
      lexeme: "at".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::default()),
      lexeme: "table".to_string(),
    },
    Token {
      kind: TokenKind::CommandModifier(CommandModifier::In),
      lexeme: "in".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::default()),
      lexeme: "kitchen".to_string(),
    },
    Token {
      kind: TokenKind::CommandModifier(CommandModifier::On),
      lexeme: "on".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::default()),
      lexeme: "stove".to_string(),
    },
  ];
  let classifier = Classifier::new();
  classifier.process_presumed_noun(&mut tokens, 5).unwrap();
  assert_eq!(tokens[0].kind, TokenKind::CommandModifier(CommandModifier::At));
  assert_eq!(tokens[1].kind, TokenKind::Word(WordToken::default()));
  assert_eq!(tokens[2].kind, TokenKind::CommandModifier(CommandModifier::In));
  assert_eq!(tokens[3].kind, TokenKind::Word(WordToken::default()));
  assert_eq!(tokens[4].kind, TokenKind::CommandModifier(CommandModifier::On));
  assert_eq!(tokens[5].kind, TokenKind::Word(WordToken::Noun));
}