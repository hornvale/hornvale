use super::super::*;
use hornvale_core::prelude::*;
use hornvale_test_utilities::prelude::*;

#[test]
fn test_process_presumed_noun1() {
  init();
  let mut tokens = vec![
    Token {
      kind: TokenKind::Preposition(Preposition::At),
      lexeme: "at".to_string(),
    },
    Token {
      kind: TokenKind::Word,
      lexeme: "table".to_string(),
    },
  ];
  let classifier = Classifier::new();
  classifier.process_presumed_noun(&mut tokens, 1).unwrap();
  assert_eq!(tokens[0].kind, TokenKind::Preposition(Preposition::At));
  assert_eq!(tokens[1].kind, TokenKind::Noun);
}

#[test]
fn test_process_presumed_noun2() {
  init();
  let mut tokens = vec![
    Token {
      kind: TokenKind::Preposition(Preposition::At),
      lexeme: "at".to_string(),
    },
    Token {
      kind: TokenKind::Word,
      lexeme: "table".to_string(),
    },
    Token {
      kind: TokenKind::Preposition(Preposition::In),
      lexeme: "in".to_string(),
    },
    Token {
      kind: TokenKind::Word,
      lexeme: "kitchen".to_string(),
    },
  ];
  let classifier = Classifier::new();
  classifier.process_presumed_noun(&mut tokens, 3).unwrap();
  assert_eq!(tokens[0].kind, TokenKind::Preposition(Preposition::At));
  assert_eq!(tokens[1].kind, TokenKind::Word);
  assert_eq!(tokens[2].kind, TokenKind::Preposition(Preposition::In));
  assert_eq!(tokens[3].kind, TokenKind::Noun);
}

#[test]
fn test_process_presumed_noun3() {
  init();
  let mut tokens = vec![
    Token {
      kind: TokenKind::Preposition(Preposition::At),
      lexeme: "at".to_string(),
    },
    Token {
      kind: TokenKind::Word,
      lexeme: "table".to_string(),
    },
    Token {
      kind: TokenKind::Preposition(Preposition::In),
      lexeme: "in".to_string(),
    },
    Token {
      kind: TokenKind::Word,
      lexeme: "kitchen".to_string(),
    },
    Token {
      kind: TokenKind::Preposition(Preposition::On),
      lexeme: "on".to_string(),
    },
    Token {
      kind: TokenKind::Word,
      lexeme: "stove".to_string(),
    },
  ];
  let classifier = Classifier::new();
  classifier.process_presumed_noun(&mut tokens, 5).unwrap();
  assert_eq!(tokens[0].kind, TokenKind::Preposition(Preposition::At));
  assert_eq!(tokens[1].kind, TokenKind::Word);
  assert_eq!(tokens[2].kind, TokenKind::Preposition(Preposition::In));
  assert_eq!(tokens[3].kind, TokenKind::Word);
  assert_eq!(tokens[4].kind, TokenKind::Preposition(Preposition::On));
  assert_eq!(tokens[5].kind, TokenKind::Noun);
}
