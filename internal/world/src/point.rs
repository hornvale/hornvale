use serde::{Deserialize, Serialize};
use std::i64;

/// A two-dimensional point in the world.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
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

  /// Calculate the Euclidean distance this and another Point.
  pub fn get_euclidean_distance(&self, other: &Point) -> f64 {
    let x2 = self.x.wrapping_sub(other.x).pow(2) as f64;
    let y2 = self.y.wrapping_sub(other.y).pow(2) as f64;
    (x2 + y2).sqrt()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_get_magic_number() {
    init();
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
  fn test_get_euclidean_distance() {
    init();
    assert_approx_eq!(Point { x: 0, y: 0 }.get_euclidean_distance(&Point { x: 0, y: 0 }), 0.0);
    assert_approx_eq!(Point { x: 0, y: 0 }.get_euclidean_distance(&Point { x: 1, y: 0 }), 1.0);
    assert_approx_eq!(Point { x: 0, y: 0 }.get_euclidean_distance(&Point { x: 0, y: 1 }), 1.0);
    assert_approx_eq!(
      Point { x: 0, y: 0 }.get_euclidean_distance(&Point { x: 1, y: 1 }),
      1.4142135623730951
    );
    assert_approx_eq!(
      Point { x: 0, y: 0 }.get_euclidean_distance(&Point { x: 1, y: 2 }),
      2.23606797749979
    );
    assert_approx_eq!(
      Point { x: 0, y: 0 }.get_euclidean_distance(&Point { x: 2, y: 1 }),
      2.23606797749979
    );
    assert_approx_eq!(
      Point { x: 0, y: 0 }.get_euclidean_distance(&Point { x: 2, y: 2 }),
      2.8284271247461903
    );
    assert_approx_eq!(Point { x: 0, y: 0 }.get_euclidean_distance(&Point { x: 3, y: 4 }), 5.0);
    assert_approx_eq!(Point { x: 0, y: 0 }.get_euclidean_distance(&Point { x: 4, y: 3 }), 5.0);
    assert_approx_eq!(
      Point { x: 0, y: 0 }.get_euclidean_distance(&Point { x: 4, y: 4 }),
      5.656854249492381
    );
    assert_approx_eq!(
      Point { x: 0, y: 0 }.get_euclidean_distance(&Point { x: 5, y: 12 }),
      13.0
    );
  }

  #[test]
  fn test_ordering() {
    init();
    // The arrangement of points is as follows:
    //
    // ```text
    // 4 3 8
    // 7 1 2
    // 9 6 5
    // ```
    //
    // So they will be ordered thusly descending:
    // 8 2 5 3 1 6 4 7 9
    let p1 = Point { x: 0, y: 0 };
    let p2 = Point { x: 1, y: 0 };
    let p3 = Point { x: 0, y: 1 };
    let p4 = Point { x: -1, y: 1 };
    let p5 = Point { x: 1, y: -1 };
    let p6 = Point { x: 0, y: -1 };
    let p7 = Point { x: -1, y: 0 };
    let p8 = Point { x: 1, y: 1 };
    let p9 = Point { x: -1, y: -1 };
    assert!(p8 > p2); // p8 is above p2
    assert!(p2 > p5); // p2 is above p5
    assert!(p5 > p3); // p5 is below but to the right of p3
    assert!(p3 > p1); // p3 is above p1
    assert!(p1 > p6); // p1 is above p6
    assert!(p6 > p4); // p6 is below but to the right of p4
    assert!(p4 > p7); // p4 is above p7
    assert!(p7 > p9); // p7 is above p9
  }
}
