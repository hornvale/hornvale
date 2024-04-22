use super::*;

#[test]
fn test_scan_tokens() {
  let input = "take sword and shield";
  let mut scanner = Scanner::new(input);
  let tokens = scanner.scan_tokens();
  let expected = vec![
    Token {
      kind: TokenKind::Word,
      lexeme: "take".to_string(),
    },
    Token {
      kind: TokenKind::Word,
      lexeme: "sword".to_string(),
    },
    Token {
      kind: TokenKind::Conjunction(Conjunction::And),
      lexeme: "and".to_string(),
    },
    Token {
      kind: TokenKind::Word,
      lexeme: "shield".to_string(),
    },
    Token {
      kind: TokenKind::EndOfInput,
      lexeme: "".to_string(),
    },
  ];
  assert_eq!(tokens, Ok(expected));
}

#[test]
fn test_scan_tokens_with_special_characters() {
  let input = "take sword & shield";
  let mut scanner = Scanner::new(input);
  let tokens = scanner.scan_tokens();
  let expected = vec![
    Token {
      kind: TokenKind::Word,
      lexeme: "take".to_string(),
    },
    Token {
      kind: TokenKind::Word,
      lexeme: "sword".to_string(),
    },
    Token {
      kind: TokenKind::Character(Character::Ampersand),
      lexeme: "&".to_string(),
    },
    Token {
      kind: TokenKind::Word,
      lexeme: "shield".to_string(),
    },
    Token {
      kind: TokenKind::EndOfInput,
      lexeme: "".to_string(),
    },
  ];
  assert_eq!(tokens, Ok(expected));
}

#[test]
fn test_scan_tokens_with_special_characters2() {
  let input = "!take #sword";
  let mut scanner = Scanner::new(input);
  let tokens = scanner.scan_tokens();
  let expected = vec![
    Token {
      kind: TokenKind::MagicWord(MagicWord::BangWord),
      lexeme: "!take".to_string(),
    },
    Token {
      kind: TokenKind::MagicWord(MagicWord::HashWord),
      lexeme: "#sword".to_string(),
    },
    Token {
      kind: TokenKind::EndOfInput,
      lexeme: "".to_string(),
    },
  ];
  assert_eq!(tokens, Ok(expected));
}

#[test]
fn test_scan_tokens_filter_articles() {
  let input = "take a sword and the shield";
  let mut scanner = Scanner::new(input);
  let tokens = scanner.scan_tokens();
  let expected = vec![
    Token {
      kind: TokenKind::Word,
      lexeme: "take".to_string(),
    },
    Token {
      kind: TokenKind::Word,
      lexeme: "sword".to_string(),
    },
    Token {
      kind: TokenKind::Conjunction(Conjunction::And),
      lexeme: "and".to_string(),
    },
    Token {
      kind: TokenKind::Word,
      lexeme: "shield".to_string(),
    },
    Token {
      kind: TokenKind::EndOfInput,
      lexeme: "".to_string(),
    },
  ];
  assert_eq!(tokens, Ok(expected));
}

#[test]
fn test_scan_token() {
  let input = "take sword";
  let mut scanner = Scanner::new(input);
  let token = scanner.scan_token().unwrap();
  let expected = Token {
    kind: TokenKind::Word,
    lexeme: "take".to_string(),
  };
  assert_eq!(token, expected);
}

#[test]
fn test_advance() {
  let input = "take sword";
  let mut scanner = Scanner::new(input);
  let character = scanner.advance();
  assert_eq!(character, Ok('t'));
}

#[test]
fn test_is_at_end() {
  let input = "take sword";
  let scanner = Scanner::new(input);
  assert_eq!(scanner.is_at_end(), false);
}

#[test]
fn test_peek() {
  let input = "take sword";
  let scanner = Scanner::new(input);
  assert_eq!(scanner.peek(), 't');
}

#[test]
fn test_peek_next() {
  let input = "take sword";
  let scanner = Scanner::new(input);
  assert_eq!(scanner.peek_next(), 'a');
}

#[test]
fn test_peek_at_offset() {
  let input = "take sword";
  let scanner = Scanner::new(input);
  assert_eq!(scanner.peek_at_offset(1), 'a');
}

#[test]
fn test_skip_whitespace() {
  let input = "  take sword";
  let mut scanner = Scanner::new(input);
  scanner.skip_whitespace().unwrap();
  assert_eq!(scanner.peek(), 't');
}

#[test]
fn test_make_token() {
  let input = "take sword";
  let mut scanner = Scanner::new(input);
  scanner.start = 0;
  scanner.current = 4;
  let token = scanner.make_token(TokenKind::Word);
  let expected = Token {
    kind: TokenKind::Word,
    lexeme: "take".to_string(),
  };
  assert_eq!(token, expected);
}
