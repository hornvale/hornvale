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
      (player, None, None), // (actor, direct_object, indirect_object
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
  let _room_entity = world
    .query::<&Room>()
    .into_iter()
    .filter_map(|(e, &rm)| if rm == Room::default() { Some(e) } else { None })
    .next()
    .unwrap();
  test_string_parsing(
    "look here",
    player,
    &mut world,
    Ok((LookHereCommand::execute, (player, None, None))),
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
  let n_room = Room { w: 0, x: 0, y: 1, z: 0 };
  let n_room_entity = world
    .query::<With<&Room, &IsARoom>>()
    .into_iter()
    .filter_map(|(e, &rm)| if rm == n_room { Some(e) } else { None })
    .next()
    .unwrap();
  test_string_parsing(
    "go north",
    player,
    &mut world,
    Ok((GoDirectionCommand::execute, (player, Some(n_room_entity), None))),
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
    Ok((GoDirectionCommand::execute, (player, None, None))),
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
  let n_room = Room { w: 0, x: 0, y: 1, z: 0 };
  let n_room_entity = world
    .query::<With<&Room, &IsARoom>>()
    .into_iter()
    .filter_map(|(e, &rm)| if rm == n_room { Some(e) } else { None })
    .next()
    .unwrap();
  test_string_parsing(
    "look north",
    player,
    &mut world,
    Ok((LookDirectionCommand::execute, (player, Some(n_room_entity), None))),
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
  let _room_entity = world
    .query::<&Room>()
    .into_iter()
    .filter_map(|(e, &rm)| if rm == Room::default() { Some(e) } else { None })
    .next()
    .unwrap();
  let n_room = Room { w: 0, x: 0, y: 1, z: 0 };
  let n_room_entity = world
    .query::<With<&Room, &IsARoom>>()
    .into_iter()
    .filter_map(|(e, &rm)| if rm == n_room { Some(e) } else { None })
    .next()
    .unwrap();
  world.spawn((command_registry,));
  test_string_parsing(
    "look north",
    player,
    &mut world,
    Ok((LookDirectionCommand::execute, (player, Some(n_room_entity), None))),
  );
  test_string_parsing(
    "look here",
    player,
    &mut world,
    Ok((LookHereCommand::execute, (player, None, None))),
  );
}
