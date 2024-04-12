//! Example of a simple world generator using a pseudo-random hash function.

use hornvale_world::point::Point;

fn main() {
  let seed = 123456789; // Initial seed value
  for y in -50..50 {
    let mut row = String::new();
    for x in -50..50 {
      let point = Point { x, y };
      let is_marked = point.get_magic_number(seed) % 100 < 50;
      if is_marked {
        row.push(' ');
      } else {
        row.push('#');
      }
    }
    println!("{}", row);
  }
}
