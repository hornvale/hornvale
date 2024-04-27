use crate::command::prelude::*;
use crate::database::prelude::*;
use crate::world::prelude::*;
use crate::world::region::Region;
use anyhow::{ensure, Error as AnyError};
use hecs::Entity;

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
  const ARITY: CommandArity = CommandArity::Unary;
  const DIRECT_OBJECT_MODIFIER: Option<CommandModifier> = None;
  const INDIRECT_OBJECT_MODIFIER: Option<CommandModifier> = None;

  fn execute(
    database: &mut Database,
    actor: Entity,
    direct_object: Option<Entity>,
    _indirect_object: Option<Entity>,
  ) -> Result<(), AnyError> {
    ensure!(direct_object.is_some(), "Expected a direction.");
    let direction_query = database
      .world
      .query_one_mut::<&PassageDirection>(direct_object.unwrap());
    if direction_query.is_err() {
      anyhow::bail!("Expected a direction.");
    }
    let direction = *direction_query.unwrap();
    let actor_info = database.world.get_region_and_room_containing_entity(actor);
    if actor_info.is_err() {
      anyhow::bail!("The actor is not in a region or room.");
    }
    let (region, room) = actor_info.unwrap();

    // Check if the player can move in the given direction.
    let passage = {
      let passage = database
        .world
        .get_room_passage_kind_in_direction(&region, &room, &direction);
      if let Ok(passage) = passage {
        passage
      } else {
        PassageKind::NoExit("You can't go that way.".to_string())
      }
    };

    match passage {
      PassageKind::Corridor(corridor_kind) => {
        let (next_region, next_room) = {
          let corridor_direction = CorridorDirection(-direction.0);
          match corridor_kind {
            CorridorKind::Default(next_region) => {
              let next_room_result = database
                .world
                .query::<(&Region, &Room, &CorridorDirection, &CorridorTerminus)>()
                .iter()
                .find(|(_, (&rgn, _, &dir, _))| rgn == next_region && dir == corridor_direction)
                .map(|(_, (_, &rm, _, _))| rm);
              if let Some(next_room_result) = next_room_result {
                (next_region, next_room_result)
              } else {
                let generator = CompassRoseRegionGenerator;
                generator.generate(next_region, database).unwrap();
                let next_room_result = database
                  .world
                  .query::<(&Region, &Room, &CorridorDirection, &CorridorTerminus)>()
                  .iter()
                  .find(|(_, (&rgn, _, &dir, _))| rgn == next_region && dir == corridor_direction)
                  .map(|(_, (_, &rm, _, _))| rm)
                  .unwrap();
                (next_region, next_room_result)
              }
            },
            CorridorKind::Ascend(next_region) => {
              let next_room_result = database
                .world
                .query::<(&Region, &Room, &CorridorDirection, &CorridorTerminus)>()
                .iter()
                .find(|(_, (&rgn, _, &dir, _))| rgn == next_region && dir == corridor_direction)
                .map(|(_, (_, &rm, _, _))| rm);
              if let Some(next_room_result) = next_room_result {
                (next_region, next_room_result)
              } else {
                let generator = CompassRoseRegionGenerator;
                generator.generate(next_region, database).unwrap();
                let next_room_result = database
                  .world
                  .query::<(&Region, &Room, &CorridorDirection, &CorridorTerminus)>()
                  .iter()
                  .find(|(_, (&rgn, _, &dir, _))| rgn == next_region && dir == corridor_direction)
                  .map(|(_, (_, &rm, _, _))| rm)
                  .unwrap();
                (next_region, next_room_result)
              }
            },
          }
        };
        database.world.insert(actor, (next_region, next_room)).unwrap();
      },
      PassageKind::Default(next_room) => {
        database.world.insert(actor, (next_room,)).unwrap();
      },
      PassageKind::Conditional(next_room, condition, message) => {
        if condition.is_met(database) {
          database.world.insert(actor, (next_room,)).unwrap();
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
