use serde::{Deserialize, Serialize};
use std::i64;

/// A two-dimensional point in the world.
#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct Point {
  /// The x-coordinate of the point.
  pub x: i64,
  /// The y-coordinate of the point.
  pub y: i64,
}

impl Point {
  /// This calculates a "magic number" (basically a hash) for the point.
  ///
  /// This magic number should be completely stable over time, serve much of
  /// the same purpose as a hash, and be used to seed any random number
  /// generation or other deterministic calculations that need to be done for
  /// this point.
  pub fn get_magic_number(&self, seed: u64) -> u64 {
    const C: u64 = 0x9e3779b97f4a7c15;
    let mut hash = C.wrapping_mul(self.x as u64);
    hash ^= seed;
    hash = hash.rotate_left(5);
    hash = hash.wrapping_mul(C);
    hash ^= self.y as u64;
    hash = hash.rotate_left(5);
    hash
  }

  /// Determines whether this point is "marked," i.e. whether it is a region.
  pub fn is_marked(&self, seed: u64) -> bool {
    const LIMIT: u64 = 100;
    const PERCENTAGE: u64 = 10;
    let hash = self.get_magic_number(seed);
    let pseudo_random = hash % LIMIT;
    pseudo_random < PERCENTAGE
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_get_magic_number() {
    init();
    // Test the magic number calculation for a few points.
    //
    // We see here that this algorithm is deterministic and stable, but also
    // that it's fairly predictable; adding 1 to the y-coordinate decreases
    // the magic number by some fixed number. This is fine for our purposes.
    let seed = 123456789;
    assert_eq!(Point { x: 0, y: -1 }.get_magic_number(seed), 4089247486106213350);
    assert_eq!(Point { x: 0, y: 0 }.get_magic_number(seed), 14357496587603338265);
    assert_eq!(Point { x: 0, y: 1 }.get_magic_number(seed), 14357496587603338297);
    assert_eq!(Point { x: 1, y: 0 }.get_magic_number(seed), 5520928134225637864);
    assert_eq!(Point { x: 1, y: 1 }.get_magic_number(seed), 5520928134225637832);
    assert_eq!(Point { x: 1, y: 2 }.get_magic_number(seed), 5520928134225637800);
    assert_eq!(Point { x: i64::MAX, y: 1 }.get_magic_number(seed), 9042962994073280935);
    assert_eq!(
      Point { x: i64::MAX - 1, y: 1 }.get_magic_number(seed),
      8040736735683154233
    );
    assert_eq!(
      Point {
        x: i64::MAX,
        y: i64::MAX
      }
      .get_magic_number(seed),
      9403781079636270696
    );
    assert_eq!(
      Point {
        x: i64::MAX - 1,
        y: i64::MAX - 1
      }
      .get_magic_number(seed),
      10406007338026397398
    );
    assert_eq!(
      Point {
        x: i64::MAX,
        y: i64::MIN
      }
      .get_magic_number(seed),
      9042962994073280919
    );
    assert_eq!(
      Point {
        x: i64::MAX - 1,
        y: i64::MIN + 1
      }
      .get_magic_number(seed),
      8040736735683154217
    );
    assert_eq!(
      Point {
        x: i64::MIN,
        y: i64::MIN
      }
      .get_magic_number(seed),
      3905612715153100294
    );
    assert_eq!(
      Point {
        x: i64::MIN + 1,
        y: i64::MIN + 1
      }
      .get_magic_number(seed),
      15972812006675875803
    );
  }

  #[test]
  fn test_is_marked() {
    init();
    let seed = 123456789;
    assert_eq!(Point { x: 0, y: -1 }.is_marked(seed), false);
    assert_eq!(Point { x: 0, y: 0 }.is_marked(seed), false);
    assert_eq!(Point { x: 0, y: 1 }.is_marked(seed), false);
    assert_eq!(Point { x: 1, y: 0 }.is_marked(seed), false);
    assert_eq!(Point { x: 1, y: 1 }.is_marked(seed), false);
    assert_eq!(Point { x: 1, y: 2 }.is_marked(seed), true);
    assert_eq!(Point { x: i64::MAX, y: 1 }.is_marked(seed), false);
    assert_eq!(Point { x: i64::MAX - 1, y: 1 }.is_marked(seed), false);
    assert_eq!(
      Point {
        x: i64::MAX,
        y: i64::MAX
      }
      .is_marked(seed),
      false
    );
    assert_eq!(
      Point {
        x: i64::MAX - 1,
        y: i64::MAX - 1
      }
      .is_marked(seed),
      false
    );
    assert_eq!(
      Point {
        x: i64::MAX,
        y: i64::MIN
      }
      .is_marked(seed),
      false
    );
    assert_eq!(
      Point {
        x: i64::MAX - 1,
        y: i64::MIN + 1
      }
      .is_marked(seed),
      false
    );
    assert_eq!(
      Point {
        x: i64::MIN,
        y: i64::MIN
      }
      .is_marked(seed),
      false
    );
    assert_eq!(
      Point {
        x: i64::MIN + 1,
        y: i64::MIN + 1
      }
      .is_marked(seed),
      true
    );
  }
}
