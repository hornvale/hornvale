use anyhow::Error as AnyError;
use hecs::{Entity, World};
use hornvale_core::prelude::*;

/// Attempt to look in a given direction.
#[derive(Clone, Copy, Debug)]
pub struct LookDirectionCommand;

impl Command for LookDirectionCommand {
  const NAME: &'static str = "look";
  const SYNONYMS: &'static [&'static str] = &["look"];
  const BRIEF: &'static str = "Attempt to look in a given direction.";
  const DESCRIPTION: &'static str = r#"
    The actor will attempt to move in the direction specified by the first argument; if they are unable to move in
    that direction, they will be informed why.
  "#;
  const ARITY: CommandArity = CommandArity::Unary;
  const DIRECT_OBJECT_MODIFIER: CommandModifier = CommandModifier::None;
  const INDIRECT_OBJECT_MODIFIER: CommandModifier = CommandModifier::None;

  fn execute(
    _world: &mut World,
    _direct_object: Option<Entity>,
    _indirect_object: Option<Entity>,
  ) -> Result<(), AnyError> {
    /*
    let actor = context.actor.unwrap();
    let argument = context.direct_object.clone().unwrap();
    let direction = match argument {
      CommandArgument::Direction(direction) => direction,
      _ => return Err(CommandError::InvalidArgument("Expected a direction.".to_string())),
    };
    let actor_info = world_query::get_entity_region_and_room(world, actor);
    if actor_info.is_none() {
      return Err(CommandError::InvalidActor(
        "The actor is not in a region or room.".to_string(),
      ));
    }
    let (region, room) = actor_info.unwrap();

    // Check if the player can move in the given direction.
    let passage = {
      let passage = world_query::get_room_passage_in_direction(world, &region, &room, direction.into());
      if let Some(passage) = passage {
        passage
      } else {
        PassageKind::NoExit("You can't go that way.".to_string())
      }
    };

    match passage {
      PassageKind::Corridor(next_region) => {
        let next_room = {
          let corridor_direction = CorridorDirection::try_from(-direction).unwrap();
          let next_room_result = world
            .query::<(&Region, &Room, &CorridorDirection, &CorridorTerminus)>()
            .iter()
            .find(|(_, (&rgn, _, &dir, _))| rgn == next_region && dir == corridor_direction)
            .map(|(_, (_, &rm, _, _))| rm);
          if let Some(next_room_result) = next_room_result {
            next_room_result
          } else {
            let generator = CompassRoseRegionGenerator;
            generator.generate(next_region, world).unwrap();
            let next_room_result = world
              .query::<(&Region, &Room, &CorridorDirection, &CorridorTerminus)>()
              .iter()
              .find(|(_, (&rgn, _, &dir, _))| rgn == next_region && dir == corridor_direction)
              .map(|(_, (_, &rm, _, _))| rm)
              .unwrap();
            next_room_result
          }
        };
        let (room_name, room_description) =
          world_query::get_room_name_and_description(world, &next_region, &next_room).unwrap();
        println!("{}", room_name.0);
        println!("{}", room_description.0);
      },
      PassageKind::Default(next_room) => {
        let (room_name, room_description) =
          world_query::get_room_name_and_description(world, &region, &next_room).unwrap();
        println!("{}", room_name.0);
        println!("{}", room_description.0);
      },
      PassageKind::Conditional(next_room, condition, message) => {
        if condition.is_met(world) {
          let (room_name, room_description) =
            world_query::get_room_name_and_description(world, &region, &next_room).unwrap();
          println!("{}", room_name.0);
          println!("{}", room_description.0);
        } else {
          println!("{}", message);
        }
      },
      PassageKind::NoExit(message) => {
        println!("{}", message);
      },
    }
    println!("{}", world_query::describe_room_passages(world, actor));
    */
    Ok(())
  }
}
