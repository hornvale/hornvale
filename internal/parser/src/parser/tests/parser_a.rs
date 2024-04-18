use super::*;

#[test]
fn test_parser01() {
  init();
  let mut world = World::new();
  let mut command_registry = CommandRegistry::new();
  command_registry.register::<QuitCommand>();
  world.spawn((command_registry,));
  test_string_parsing(
    "quit",
    &mut world,
    (
      QuitCommand::execute,
      CommandContext {
        raw: "quit".to_string(),
        verb: "quit".to_string(),
        form: CommandForm::Default,
        direct_object: None,
        indirect_object: None,
      },
    ),
  );
}
