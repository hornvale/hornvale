use anyhow::Error as AnyError;
use rand_seeder::SipHasher;
use specs::prelude::*;
use std::io::stdout;

pub mod _type;
pub use _type::*;

pub fn insert_resources(ecs: &mut World, seed_string: &str) -> Result<(), AnyError> {
  ecs.insert(InputReadyFlagResource(true));
  ecs.insert(OutputResource(Some(stdout())));
  ecs.insert(QuitFlagResource(false));
  ecs.insert(SeedStringResource(seed_string.to_owned()));
  let rng = SipHasher::from(seed_string).into_rng();
  ecs.insert(RandomResource(rng));
  ecs.insert(TickResource(0));
  Ok(())
}
