//! Example of a simple world generator using a pseudo-random hash function.

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

fn hash_coords(x: i64, y: i64) -> u64 {
  let mut hasher = DefaultHasher::new();
  (x, y).hash(&mut hasher);
  hasher.finish()
}

fn is_marked(x: i64, y: i64, seed: u64) -> bool {
  let hash = hash_coords(x, y).wrapping_add(seed);
  let pseudo_random = hash % 50;
  pseudo_random < 10
}

fn main() {
  let seed = 123456789; // Initial seed value
  for y in 0..50 {
    let mut row = String::new();
    for x in 0..50 {
      if is_marked(x, y, seed) {
        row.push('#');
      } else {
        row.push('.');
      }
    }
    println!("{}", row);
  }
}
