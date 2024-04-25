use crate::command::prelude::*;
use crate::world::prelude::*;
use anyhow::Error as AnyError;
use hecs::{Entity, World};

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
  const ARITY: CommandArity = CommandArity::Nullary;
  const DIRECT_OBJECT_MODIFIER: Option<CommandModifier> = Some(CommandModifier::Here);
  const INDIRECT_OBJECT_MODIFIER: Option<CommandModifier> = None;

  fn execute(
    world: &mut World,
    actor: Entity,
    _direct_object: Option<Entity>,
    _indirect_object: Option<Entity>,
  ) -> Result<(), AnyError> {
    let actor_info = world_query::get_entity_region_and_room(world, actor);
    if actor_info.is_none() {
      anyhow::bail!("The actor is not in a region or room.");
    }
    let (region, room) = actor_info.unwrap();
    let (room_name, room_description) = world_query::get_room_name_and_description(world, &region, &room).unwrap();
    println!("{}", room_name.0);
    println!("{}", room_description.0);
    println!("{}", world_query::describe_room_passages(world, actor));
    Ok(())
  }
}
