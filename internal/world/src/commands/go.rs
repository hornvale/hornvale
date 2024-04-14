// use hornvale_command::prelude::{Command, CommandContext, CommandError, SyntaxElement, SyntaxPattern};
// use hecs::World;
//
// /// A command that attempts to move the player in a given direction.
// #[derive(Clone, Copy, Debug)]
// pub struct GoCommand;
//
// impl Command for GoCommand {
//   fn name(&self) -> &str {
//     "go"
//   }
//   fn aliases(&self) -> Vec<&str> {
//     vec![
//       "move",
//       "walk",
//       "north",
//       "n",
//       "northeast",
//       "ne",
//       "east",
//       "e",
//       "southeast",
//       "se",
//       "south",
//       "s",
//       "southwest",
//       "sw",
//       "west",
//       "w",
//       "northwest",
//       "nw",
//       "up",
//       "u",
//       "down",
//       "d",
//       "in",
//       "i",
//       "out",
//       "o",
//     ]
//   }
//   fn description(&self) -> &str {
//     "A command that always fails."
//   }
//   fn syntax_patterns(&self) -> Vec<SyntaxPattern> {
//     vec![SyntaxPattern {
//       elements: vec![SyntaxElement::VerbFromList(vec![
//         "north".to_string(),
//         "n".to_string(),
//         "northeast".to_string(),
//         "ne".to_string(),
//         "east".to_string(),
//         "e".to_string(),
//         "southeast".to_string(),
//         "se".to_string(),
//         "south".to_string(),
//         "s".to_string(),
//         "southwest".to_string(),
//         "sw".to_string(),
//         "west".to_string(),
//         "w".to_string(),
//         "northwest".to_string(),
//         "nw".to_string(),
//         "up".to_string(),
//         "u".to_string(),
//         "down".to_string(),
//         "d".to_string(),
//         "in".to_string(),
//         "i".to_string(),
//         "out".to_string(),
//         "o".to_string(),
//       ])],
//     },
//     SyntaxPattern {
//       elements: vec![SyntaxElement::VerbFromList(vec![
//         "move".to_string(),
//         "walk".to_string(),
//       ])]
//       },
//     ]
//   }
//   fn execute(&self, _world: &World, _context: &CommandContext) -> Result<(), CommandError> {
//     Err(CommandError::UnknownError)
//   }
// }
//
// #[cfg(test)]
// mod tests {
//   use super::*;
//   use crate::prelude::CommandError;
//   use hornvale_test_utilities::prelude::*;
//
//   #[test]
//   fn test_name() {
//     init();
//     let command = FailCommand;
//     assert_eq!(command.name(), "fail");
//   }
//
//   #[test]
//   fn test_execute() {
//     init();
//     let command = FailCommand;
//     let world = World::new();
//     let result = command.execute(&world, &Default::default());
//     assert_eq!(result, Err(CommandError::UnknownError));
//   }
// }
