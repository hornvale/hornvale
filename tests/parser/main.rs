use hecs::{Entity, With, World};
use hornvale::command::prelude::*;
use hornvale::core::prelude::*;
#[cfg(test)]
use hornvale::test_utilities::prelude::*;

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
  string: &str,
  actor: Entity,
  world: &mut World,
  expected: Result<(CommandFunction, (Entity, Option<Entity>, Option<Entity>)), CommandError>,
) {
  init();
  let mut scanner = Scanner::new(string);
  let mut tokens = scanner.scan_tokens().unwrap();
  let classifier = Classifier::new();
  classifier.classify_tokens(&mut *tokens).unwrap();
  let mut parser = Parser::new(&mut *tokens, actor, world);
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
      assert_eq!(
        std::mem::discriminant(&expected_error),
        std::mem::discriminant(&actual_error)
      );
    },
    (expected, actual) => {
      panic!("Expected: {:#?}\nActual: {:#?}", expected, actual);
    },
  }
}