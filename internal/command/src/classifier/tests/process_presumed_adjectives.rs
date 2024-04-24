use super::super::*;
use hornvale_test_utilities::prelude::*;

#[test]
fn test_process_presumed_adjectives1() {
  init();
  let mut tokens = vec![
    Token {
      kind: TokenKind::Word(WordToken::default()),
      lexeme: "red".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::Noun),
      lexeme: "cube".to_string(),
    },
  ];
  let classifier = Classifier::new();
  classifier.process_presumed_adjectives(&mut tokens, 1).unwrap();
  assert_eq!(tokens[0].kind, TokenKind::Word(WordToken::Adjective));
  assert_eq!(tokens[1].kind, TokenKind::Word(WordToken::Noun));
}

#[test]
fn test_process_presumed_adjectives2() {
  init();
  let mut tokens = vec![
    Token {
      kind: TokenKind::Word(WordToken::default()),
      lexeme: "red".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::Noun),
      lexeme: "cube".to_string(),
    },
    Token {
      kind: TokenKind::And,
      lexeme: "and".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::default()),
      lexeme: "green".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::Noun),
      lexeme: "cylinder".to_string(),
    },
  ];
  let classifier = Classifier::new();
  classifier.process_presumed_adjectives(&mut tokens, 4).unwrap();
  assert_eq!(tokens[0].kind, TokenKind::Word(WordToken::Adjective));
  assert_eq!(tokens[1].kind, TokenKind::Word(WordToken::Noun));
  assert_eq!(tokens[2].kind, TokenKind::And);
  assert_eq!(tokens[3].kind, TokenKind::Word(WordToken::Adjective));
  assert_eq!(tokens[4].kind, TokenKind::Word(WordToken::Noun));
}

#[test]
fn test_process_presumed_adjectives3() {
  init();
  let mut tokens = vec![
    Token {
      kind: TokenKind::Word(WordToken::default()),
      lexeme: "red".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::Noun),
      lexeme: "cube".to_string(),
    },
    Token {
      kind: TokenKind::Character(CharacterToken::Comma),
      lexeme: ",".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::default()),
      lexeme: "green".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::Noun),
      lexeme: "cylinder".to_string(),
    },
  ];
  let classifier = Classifier::new();
  classifier.process_presumed_adjectives(&mut tokens, 4).unwrap();
  assert_eq!(tokens[0].kind, TokenKind::Word(WordToken::Adjective));
  assert_eq!(tokens[1].kind, TokenKind::Word(WordToken::Noun));
  assert_eq!(tokens[2].kind, TokenKind::Character(CharacterToken::Comma));
  assert_eq!(tokens[3].kind, TokenKind::Word(WordToken::Adjective));
  assert_eq!(tokens[4].kind, TokenKind::Word(WordToken::Noun));
}

#[test]
fn test_process_presumed_adjectives4() {
  init();
  let mut tokens = vec![
    Token {
      kind: TokenKind::Word(WordToken::default()),
      lexeme: "red".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::Noun),
      lexeme: "cube".to_string(),
    },
    Token {
      kind: TokenKind::Character(CharacterToken::Comma),
      lexeme: ",".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::default()),
      lexeme: "green".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::Noun),
      lexeme: "cylinder".to_string(),
    },
    Token {
      kind: TokenKind::And,
      lexeme: "and".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::default()),
      lexeme: "yellow".to_string(),
    },
    Token {
      kind: TokenKind::Word(WordToken::Noun),
      lexeme: "prism".to_string(),
    },
  ];
  let classifier = Classifier::new();
  classifier.process_presumed_adjectives(&mut tokens, 7).unwrap();
  assert_eq!(tokens[0].kind, TokenKind::Word(WordToken::Adjective));
  assert_eq!(tokens[1].kind, TokenKind::Word(WordToken::Noun));
  assert_eq!(tokens[2].kind, TokenKind::Character(CharacterToken::Comma));
  assert_eq!(tokens[3].kind, TokenKind::Word(WordToken::Adjective));
  assert_eq!(tokens[4].kind, TokenKind::Word(WordToken::Noun));
  assert_eq!(tokens[5].kind, TokenKind::And);
  assert_eq!(tokens[6].kind, TokenKind::Word(WordToken::Adjective));
  assert_eq!(tokens[7].kind, TokenKind::Word(WordToken::Noun));
}
