use crate::prelude::*;
use hecs::World;
use hornvale_command::prelude::*;

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
    // Find the room the player is in.
    let (region, room) = {
      let mut query = world.query_one::<(&Region, &Room)>(actor).unwrap();
      let (region, room) = query.get().unwrap();
      (*region, *room)
    };

    // Check if the player can move in the given direction.
    let passage = {
      let info = world
        .query::<(&Region, &Room, &PassageDirection, &PassageKind)>()
        .iter()
        .find(|(_, (&rgn, &rm, &dir, _))| rgn == region && rm == room && dir == direction.into())
        .map(|(_, (_, _, _, kind))| kind.clone());

      if let Some(kind) = info {
        kind.clone()
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
