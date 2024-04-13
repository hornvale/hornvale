//! Test of the world.
use hecs::World;
use hornvale_world::prelude::*;

/// Generate a world.
pub fn main() {
  let mut world = World::new();
  let generator = CompassRoseRegionGenerator;
  generator.generate(Region::default(), &mut world).unwrap();
}
