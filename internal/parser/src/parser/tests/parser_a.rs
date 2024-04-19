use super::*;

#[test]
fn test_parser01() {
  init();
  let mut world = World::new();
  let mut command_registry = CommandRegistry::new();
  command_registry.register::<QuitCommand>();
  world.spawn((command_registry,));
  let player = setup_world(&mut world);
  test_string_parsing(
    "quit",
    player,
    &mut world,
    Ok((
      QuitCommand::execute,
      CommandContext {
        raw: "quit".to_string(),
        verb: "quit".to_string(),
        actor: Some(player),
        form: CommandForm::Default,
        direct_object: None,
        indirect_object: None,
      },
    )),
  );
}

#[test]
fn test_parser02() {
  init();
  let mut world = World::new();
  let mut command_registry = CommandRegistry::new();
  command_registry.register::<LookHereCommand>();
  world.spawn((command_registry,));
  let player = setup_world(&mut world);
  let room_entity = world
    .query::<&Room>()
    .into_iter()
    .filter_map(|(e, &rm)| if rm == Room::default() { Some(e) } else { None })
    .next()
    .unwrap();
  test_string_parsing(
    "look here",
    player,
    &mut world,
    Ok((
      LookHereCommand::execute,
      CommandContext {
        raw: "look here".to_string(),
        verb: "look".to_string(),
        actor: Some(player),
        form: CommandForm::Here,
        direct_object: Some(CommandArgument::Entity(room_entity)),
        indirect_object: None,
      },
    )),
  );
}

#[test]
fn test_parser03() {
  init();
  let mut world = World::new();
  let mut command_registry = CommandRegistry::new();
  command_registry.register::<GoDirectionCommand>();
  world.spawn((command_registry,));
  let player = setup_world(&mut world);
  test_string_parsing(
    "go north",
    player,
    &mut world,
    Ok((
      GoDirectionCommand::execute,
      CommandContext {
        raw: "go north".to_string(),
        verb: "go".to_string(),
        actor: Some(player),
        form: CommandForm::Direction,
        direct_object: Some(CommandArgument::Direction(CommandDirection::North)),
        indirect_object: None,
      },
    )),
  );
}

#[test]
/// This test should panic because "go to <direction>" is not a valid command.
#[should_panic]
fn test_parser04() {
  init();
  let mut world = World::new();
  let mut command_registry = CommandRegistry::new();
  command_registry.register::<GoDirectionCommand>();
  let player = setup_world(&mut world);
  world.spawn((command_registry,));
  test_string_parsing(
    "go to north",
    player,
    &mut world,
    Ok((
      GoDirectionCommand::execute,
      CommandContext {
        raw: "go to north".to_string(),
        verb: "go".to_string(),
        actor: Some(player),
        form: CommandForm::Direction,
        direct_object: None,
        indirect_object: None,
      },
    )),
  );
}

#[test]
fn test_parser05() {
  init();
  let mut world = World::new();
  let mut command_registry = CommandRegistry::new();
  command_registry.register::<LookDirectionCommand>();
  let player = setup_world(&mut world);
  world.spawn((command_registry,));
  test_string_parsing(
    "look north",
    player,
    &mut world,
    Ok((
      LookDirectionCommand::execute,
      CommandContext {
        raw: "look north".to_string(),
        verb: "look".to_string(),
        actor: Some(player),
        form: CommandForm::Direction,
        direct_object: Some(CommandArgument::Direction(CommandDirection::North)),
        indirect_object: None,
      },
    )),
  );
}

#[test]
/// This test verifies that we can have multiple commands with the same name.
fn test_parser06() {
  init();
  let mut world = World::new();
  let mut command_registry = CommandRegistry::new();
  command_registry.register::<LookDirectionCommand>();
  command_registry.register::<LookHereCommand>();
  let player = setup_world(&mut world);
  let room_entity = world
    .query::<&Room>()
    .into_iter()
    .filter_map(|(e, &rm)| if rm == Room::default() { Some(e) } else { None })
    .next()
    .unwrap();
  world.spawn((command_registry,));
  test_string_parsing(
    "look north",
    player,
    &mut world,
    Ok((
      LookDirectionCommand::execute,
      CommandContext {
        raw: "look north".to_string(),
        verb: "look".to_string(),
        actor: Some(player),
        form: CommandForm::Direction,
        direct_object: Some(CommandArgument::Direction(CommandDirection::North)),
        indirect_object: None,
      },
    )),
  );
  test_string_parsing(
    "look here",
    player,
    &mut world,
    Ok((
      LookHereCommand::execute,
      CommandContext {
        raw: "look here".to_string(),
        verb: "look".to_string(),
        actor: Some(player),
        form: CommandForm::Here,
        direct_object: Some(CommandArgument::Entity(room_entity)),
        indirect_object: None,
      },
    )),
  );
}
