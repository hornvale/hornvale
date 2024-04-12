//! Test of the world.

/// Generate a world.
pub fn main() {
  for z in -5..5 {
    for y in -5..5 {
      for x in -5..5 {
        println!("({}, {}, {})", x, y, z);
      }
    }
  }
}
