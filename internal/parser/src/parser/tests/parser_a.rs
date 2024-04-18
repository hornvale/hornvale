use super::*;
use hornvale_command::prelude::*;

//#[test]
fn test_parser01() {
  init();
  let mut world = World::new();
  test_string_parsing(
    "quit",
    &mut world,
    (QuitCommand::execute, vec![CommandContext::default()]),
  );
}
