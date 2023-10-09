use anyhow::Error as AnyError;
use rand_seeder::SipHasher;
use specs::prelude::*;
use std::io::stdout;

pub mod output;
pub use output::Output as OutputResource;
pub mod random;
pub use random::Random as RandomResource;
pub mod seed_string;
pub use seed_string::SeedString as SeedStringResource;
pub mod tick;
pub use tick::Tick as TickResource;

pub fn insert_resources(ecs: &mut World, seed_string: &str) -> Result<(), AnyError> {
  ecs.insert(OutputResource(Some(stdout())));
  ecs.insert(SeedStringResource(seed_string.to_owned()));
  let rng = SipHasher::from(seed_string).into_rng();
  ecs.insert(RandomResource(rng));
  ecs.insert(TickResource(0));
  Ok(())
}
