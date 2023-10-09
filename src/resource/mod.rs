use anyhow::Error as AnyError;
use rand_seeder::SipHasher;
use specs::prelude::*;

pub mod random;
pub use random::Random as RandomResource;
pub mod seed_string;
pub use seed_string::SeedString as SeedStringResource;

pub fn insert_resources(ecs: &mut World, seed_string: &str) -> Result<(), AnyError> {
  ecs.insert(SeedStringResource(seed_string.to_owned()));
  let rng = SipHasher::from(seed_string).into_rng();
  ecs.insert(RandomResource(rng));
  Ok(())
}
