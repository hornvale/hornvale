use super::*;
use crate::prelude::Classifier;
use crate::prelude::Scanner;
use hornvale_core::prelude::*;
use hornvale_test_utilities::prelude::*;

pub mod parser_a;

/// A player.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Player;

pub fn setup_world(world: &mut World) -> Entity {
  let generator = CompassRoseRegionGenerator;
  let region = Region::default();
  generator.generate(region, world).unwrap();
  let player = world.spawn((Player, region, Room::default(), Name("Player".to_string())));
  player
}

pub fn test_string_parsing(
  world: &mut World,
  command_registry: &CommandRegistry,
  string: &str,
  actor: Entity,
  expected: Result<(CommandFunction, Option<Entity>, Option<Entity>), ParserError>,
) {
  init();
  let mut scanner = Scanner::new(string);
  let mut tokens = scanner.scan_tokens().unwrap();
  let classifier = Classifier::new();
  classifier.classify_tokens(&mut *tokens).unwrap();
  let mut parser = Parser::new(world, command_registry, &mut *tokens, actor);
  let actual = parser.parse();
  match (expected, actual) {
    (Ok(expected), Ok(actual)) => {
      let (expected_function, expected_context) = expected;
      let (actual_function, actual_context) = actual;
      assert_eq!(*actual_function, expected_function);
      assert_eq!(
        actual_context, expected_context,
        "Expected: {:#?}\nActual: {:#?}",
        expected_context, actual_context
      );
    },
    (Err(expected_error), Err(actual_error)) => {
      assert_eq!(expected_error, actual_error);
    },
    (expected, actual) => {
      panic!("Expected: {:#?}\nActual: {:#?}", expected, actual);
    },
  }
}
