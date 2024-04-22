use hecs::World;
use hornvale_command::prelude::*;
use hornvale_world::prelude::*;

/// Attempt to walk in a given direction.
#[derive(Clone, Copy, Debug)]
pub struct GoDirectionCommand;

impl Command for GoDirectionCommand {
  const NAME: &'static str = "go";
  const SYNONYMS: &'static [&'static str] = &["go"];
  const BRIEF: &'static str = "Attempt to walk in a given direction.";
  const DESCRIPTION: &'static str = r#"
    The actor will attempt to move in the direction specified by the first argument; if they are unable to move in
    that direction, they will be informed why.
  "#;
  const FORM: CommandForm = CommandForm::Direction;
  const ARITY: CommandArity = CommandArity::Unary(CommandParameter::Direction);

  fn execute(world: &mut World, context: &CommandContext) -> Result<(), CommandError> {
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
        world.insert(actor, (next_region, next_room)).unwrap();
      },
      PassageKind::Default(next_room) => {
        world.insert(actor, (next_room,)).unwrap();
      },
      PassageKind::Conditional(next_room, condition, message) => {
        if condition.is_met(world) {
          world.insert(actor, (next_room,)).unwrap();
        } else {
          println!("{}", message);
        }
      },
      PassageKind::NoExit(message) => {
        println!("{}", message);
      },
    }
    Ok(())
  }
}
