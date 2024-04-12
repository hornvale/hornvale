use crate::edge::Edge;
use crate::point::Point;
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashSet};

/// Generates a minimum spanning tree from a given set of points.
pub fn generate_mst(context: Vec<Point>) -> Vec<(Point, Point)> {
  let mut edges = BinaryHeap::new();
  let mut in_mst = HashSet::new();
  let mut mst_edges = Vec::new();

  if context.is_empty() {
    return mst_edges;
  }

  // Initialize with the first point
  in_mst.insert(context[0]);

  // Initialize edges with reversed weight logic if necessary
  for &point in &context[1..] {
    edges.push(Reverse(Edge::from((context[0], point))));
  }

  while in_mst.len() < context.len() {
    if let Some(Reverse(edge)) = edges.pop() {
      if in_mst.insert(edge.points.1) {
        mst_edges.push(edge.points);

        for &point in &context {
          if !in_mst.contains(&point) {
            edges.push(Reverse(Edge::from((edge.points.1, point))));
          }
        }
      }
    }
  }

  mst_edges
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::point::Point;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_generate_mst() {
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
    let points = vec![p1, p2, p3, p4, p5, p6, p7, p8, p9];
    let mst = generate_mst(points.clone());
    assert_eq!(mst.len(), points.len() - 1);
    assert_eq!(mst[0], (p1, p7));
    assert_eq!(mst[1], (p7, p9));
    assert_eq!(mst[2], (p9, p6));
    assert_eq!(mst[3], (p7, p4));
    assert_eq!(mst[4], (p4, p3));
    assert_eq!(mst[5], (p6, p5));
    assert_eq!(mst[6], (p1, p2));
    assert_eq!(mst[7], (p3, p8));
  }

  #[test]
  fn test_generate_large_mst() {
    init();
    let seed = 123456789; // Initial seed value
    let mut points = Vec::new();
    points.push(Point { x: 0, y: 0 });
    for y in -50..50 {
      for x in -50..50 {
        let point = Point { x, y };
        if point.is_marked(seed) {
          points.push(point);
        }
      }
    }
    println!("{:?}", points.len());
    let mst = generate_mst(points);
    assert_eq!(mst[0], (Point { x: 0, y: 0 }, Point { x: -1, y: 2 }));
  }
}
