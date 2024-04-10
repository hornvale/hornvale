//! Example of a simple world generator using a pseudo-random hash function.

use hornvale_world::point::Point;

fn main() {
  let seed = 123456789; // Initial seed value
  for y in 0..50 {
    let mut row = String::new();
    for x in 0..50 {
      if (Point { x, y }).is_marked(seed) {
        row.push('#');
      } else {
        row.push('.');
      }
    }
    println!("{}", row);
  }
}
