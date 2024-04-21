use crate::prelude::*;
use hecs::World;
use hornvale_command::prelude::*;

/// Look at here.
#[derive(Clone, Copy, Debug)]
pub struct LookHereCommand;

impl Command for LookHereCommand {
  const NAME: &'static str = "look";
  const SYNONYMS: &'static [&'static str] = &["look"];
  const BRIEF: &'static str = "Look at your surroundings.";
  const DESCRIPTION: &'static str = r#"
    Look at your surroundings. This command will provide a description of the
    player's current location, including any items or characters that are present.
  "#;
  const FORM: CommandForm = CommandForm::Here;
  const ARITY: CommandArity = CommandArity::Nullary;

  fn execute(world: &mut World, context: &CommandContext) -> Result<(), CommandError> {
    let actor = context.actor.unwrap();
    let actor_info = world_query::get_entity_region_and_room(world, actor);
    if actor_info.is_none() {
      return Err(CommandError::InvalidActor(
        "The actor is not in a region or room.".to_string(),
      ));
    }
    let (region, room) = actor_info.unwrap();
    let (room_name, room_description) = world_query::get_room_name_and_description(world, &region, &room).unwrap();
    println!("{}", room_name.0);
    println!("{}", room_description.0);
    println!("{}", world_query::describe_room_passages(world, actor));
    Ok(())
  }
}
