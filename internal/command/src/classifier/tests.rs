use super::*;
use crate::prelude::Scanner;
use hornvale_test_utilities::prelude::*;

pub mod classify_tokens_a;
pub mod classify_tokens_b;
pub mod classify_tokens_c;
pub mod classify_tokens_d;

fn test_string_classification(string: &str, expected: &[TokenKind]) {
  let mut scanner = Scanner::new(string);
  let mut tokens = scanner.scan_tokens().unwrap();
  let classifier = Classifier::new();
  classifier.classify_tokens(&mut tokens).unwrap();
  for (i, kind) in expected.iter().enumerate() {
    assert_eq!(
      tokens[i].kind, *kind,
      "Token {} ({:?}) should be a {:?}\nTokens: {:#?}\nExpected: {:#?}",
      i, tokens[i], kind, tokens, expected
    );
  }
}
