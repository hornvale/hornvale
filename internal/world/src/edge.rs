use crate::point::Point;
use serde::{Deserialize, Serialize};
use std::cmp::{self, Ordering};

/// An edge connecting two points in the world.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Edge {
  /// The two points connected by the edge.
  pub points: (Point, Point),
  /// The weight of the edge.
  pub weight: f64,
}

impl From<(Point, Point)> for Edge {
  /// Create an edge from two points.
  ///
  /// This will order the points so that the point with the smaller x-coordinate
  /// comes first. If the x-coordinates are equal, the point with the smaller
  /// y-coordinate comes first.
  ///
  /// This also calculates the weight for the Edge, based on the Euclidean
  /// distance between the two points.
  fn from(points: (Point, Point)) -> Self {
    let weight = points.0.get_euclidean_distance(&points.1);
    Edge { points, weight }
  }
}

// Implementing PartialOrd for Edge
//
// This implementation is based on the following criteria:
//
// 1. The weight of the edge
// 2. The minimum point of the edge
// 3. The maximum point of the edge
//
// If the weight of the edges is equal, the minimum point is compared first.
// If the minimum points are equal, the maximum points are compared.
//
// The goal is to provide a consistent ordering for the edges that does not
// show any bias towards any particular arrangement of points.
impl PartialOrd for Edge {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for Edge {
  fn cmp(&self, other: &Self) -> Ordering {
    let self_min_point = cmp::min(self.points.0, self.points.1);
    let self_max_point = cmp::max(self.points.0, self.points.1);
    let other_min_point = cmp::min(other.points.0, other.points.1);
    let other_max_point = cmp::max(other.points.0, other.points.1);
    self
      .weight
      .partial_cmp(&other.weight)
      .unwrap()
      .then_with(|| self_min_point.cmp(&other_min_point))
      .then_with(|| self_max_point.cmp(&other_max_point))
  }
}

impl Eq for Edge {}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::point::Point;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_edge_from() {
    init();
    let edge = Edge::from((Point { x: 0, y: 0 }, Point { x: 1, y: 1 }));
    assert_eq!(edge.points, (Point { x: 0, y: 0 }, Point { x: 1, y: 1 }));
    assert_approx_eq!(edge.weight, 1.4142135623730951);

    let edge = Edge::from((Point { x: 0, y: 0 }, Point { x: 1, y: 1 }));
    assert_eq!(edge.points, (Point { x: 0, y: 0 }, Point { x: 1, y: 1 }));
    assert_approx_eq!(edge.weight, 1.4142135623730951);
  }

  #[test]
  fn test_edge_ordering() {
    init();
    // The arrangement of points is as follows:
    //
    // ```text
    // 4 3 8
    // 7 1 2
    // 9 6 5
    // ```
    //
    // What we want to test is that the edges are ordered without any bias.
    //
    // The weight of each edge is the Euclidean distance between the two points
    // forming the edge.
    let p1 = Point { x: 0, y: 0 };
    let p2 = Point { x: 1, y: 0 };
    let p3 = Point { x: 0, y: 1 };
    let p4 = Point { x: -1, y: 1 };
    let p5 = Point { x: 1, y: -1 };
    let p6 = Point { x: 0, y: -1 };
    let p7 = Point { x: -1, y: 0 };
    let p8 = Point { x: 1, y: 1 };
    let p9 = Point { x: -1, y: -1 };
    let edge12 = Edge::from((p1, p2));
    assert_approx_eq!(edge12.weight, 1.0);
    assert_eq!(edge12.points, (p1, p2));
    let edge18 = Edge::from((p1, p8));
    assert_approx_eq!(edge18.weight, 1.4142135623730951);
    assert_eq!(edge18.points, (p1, p8));
    let edge13 = Edge::from((p1, p3));
    assert_approx_eq!(edge13.weight, 1.0);
    assert_eq!(edge13.points, (p1, p3));
    let edge14 = Edge::from((p1, p4));
    assert_approx_eq!(edge14.weight, 1.4142135623730951);
    assert_ne!(edge14.points, (p4, p1));
    let edge17 = Edge::from((p1, p7));
    assert_approx_eq!(edge17.weight, 1.0);
    assert_ne!(edge17.points, (p7, p1));
    let edge19 = Edge::from((p1, p9));
    assert_approx_eq!(edge19.weight, 1.4142135623730951);
    assert_ne!(edge19.points, (p9, p1));
    let edge16 = Edge::from((p1, p6));
    assert_approx_eq!(edge16.weight, 1.0);
    assert_ne!(edge16.points, (p6, p1));
    let edge15 = Edge::from((p1, p5));
    assert_approx_eq!(edge15.weight, 1.4142135623730951);
    assert_eq!(edge15.points, (p1, p5)); // Ordering is back to p1 -> p5.

    // The following edges are ordered by weight.
    assert!(edge12 < edge18);
    assert!(edge18 > edge13);
    assert!(edge13 < edge14);
    assert!(edge14 > edge17);
    assert!(edge17 < edge19);
    assert!(edge19 > edge16);
    assert!(edge16 < edge15);

    // The following edges are ordered by the minimum point.
    assert_approx_eq!(edge13.weight, edge17.weight); // weights are the same
    assert!(edge13 > edge17); // p1 > p7
    assert_approx_eq!(edge17.weight, edge16.weight); // weights are the same
    assert!(edge17 < edge16); // p7 < p6
    assert_approx_eq!(edge16.weight, edge12.weight); // weights are the same
    assert!(edge16 < edge12); // p6 < p2

    // The following edges are ordered by the maximum point.
    assert_approx_eq!(edge12.weight, edge13.weight); // weights are the same
    assert_eq!(edge12.points.0, edge13.points.0); // p1 == p1
    assert!(edge12.points.1 > edge13.points.1); // p2 > p3
    assert!(edge12 > edge13); // p2 > p3
    assert_approx_eq!(edge15.weight, edge18.weight); // weights are the same
    assert_eq!(edge15.points.0, edge18.points.0); // p1 == p1
    assert!(edge15.points.1 < edge18.points.1); // p5 < p8
    assert!(edge15 < edge18); // p5 < p8
  }
}
