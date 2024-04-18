use super::*;
use crate::prelude::Classifier;
use crate::prelude::Scanner;
use hornvale_test_utilities::prelude::*;

pub mod parser_a;

pub fn test_string_parsing(
  string: &str,
  world: &mut World,
  (expected_function, expected_contexts): (CommandFunction, Vec<CommandContext>),
) {
  init();
  let mut scanner = Scanner::new(string);
  let mut tokens = scanner.scan_tokens().unwrap();
  let classifier = Classifier::new();
  classifier.classify_tokens(&mut *tokens).unwrap();
  let mut parser = Parser::new(&mut *tokens, world);
  let (actual_function, actual_contexts) = parser.parse().unwrap();
  assert_eq!(*actual_function, expected_function);
  assert_eq!(
    actual_contexts, expected_contexts,
    "Expected: {:#?}\nActual: {:#?}",
    expected_contexts, actual_contexts
  );
}
