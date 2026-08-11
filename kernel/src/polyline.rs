//! Signed distance from a point to a polyline on the unit sphere, and the
//! banding of that scalar into ordinal zones. Pure geometry: this module
//! knows nothing about what the polyline represents — a river, a coastline,
//! a scarp and a treeline all band the same way.

use crate::math;

/// A polyline on the unit sphere: an ordered run of unit vectors joined by
/// great-circle segments.
/// type-audit: pending(wave-1: points)
#[derive(Clone, Debug, PartialEq)]
pub struct SphericalPolyline {
    /// The vertices, in travel order. Each must be a unit vector.
    pub points: Vec<[f64; 3]>,
}

/// Dot product of two 3-vectors.
/// type-audit: bare-ok(ratio: return)
fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// Cross product of two 3-vectors.
/// type-audit: bare-ok(position: return)
fn cross(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}

/// Normalize a 3-vector; returns the input unchanged if its norm is zero.
/// type-audit: bare-ok(position: return)
fn normalize(v: [f64; 3]) -> [f64; 3] {
    let n = (v[0] * v[0] + v[1] * v[1] + v[2] * v[2]).sqrt();
    if n == 0.0 {
        v
    } else {
        [v[0] / n, v[1] / n, v[2] / n]
    }
}

impl SphericalPolyline {
    /// Signed great-circle distance from `p` to the nearest point of the
    /// polyline, in radians.
    ///
    /// The sign is **left-positive** relative to travel direction: a point to
    /// the left of the nearest segment's direction of travel reads positive,
    /// to the right negative. Carrying the sign is what makes a *crossing*
    /// expressible — a ford is a path on which the sign flips — and what
    /// distinguishes the cut-bank from the point-bar of a meander.
    ///
    /// Returns `f64::INFINITY` for an empty polyline.
    /// type-audit: pending(wave-1: return), pending(wave-1: p)
    pub fn signed_distance(&self, p: [f64; 3]) -> f64 {
        if self.points.is_empty() {
            return f64::INFINITY;
        }
        if self.points.len() == 1 {
            return math::acos(dot(p, self.points[0]).clamp(-1.0, 1.0));
        }
        let mut best = f64::INFINITY;
        let mut best_sign = 1.0f64;
        for w in self.points.windows(2) {
            let (a, b) = (w[0], w[1]);
            let n = cross(a, b);
            let nn = (n[0] * n[0] + n[1] * n[1] + n[2] * n[2]).sqrt();
            // Degenerate segment (coincident endpoints): fall back to the vertex.
            let (d, side) = if nn == 0.0 {
                (math::acos(dot(p, a).clamp(-1.0, 1.0)), 1.0)
            } else {
                let nu = normalize(n);
                // Foot of the perpendicular, projected onto the segment's plane.
                let foot = normalize([
                    p[0] - nu[0] * dot(p, nu),
                    p[1] - nu[1] * dot(p, nu),
                    p[2] - nu[2] * dot(p, nu),
                ]);
                // Inside the arc iff the foot lies between a and b.
                let inside = dot(cross(a, foot), nu) >= 0.0 && dot(cross(foot, b), nu) >= 0.0;
                let d = if inside {
                    math::acos(dot(p, foot).clamp(-1.0, 1.0))
                } else {
                    let da = math::acos(dot(p, a).clamp(-1.0, 1.0));
                    let db = math::acos(dot(p, b).clamp(-1.0, 1.0));
                    if da <= db { da } else { db }
                };
                // Left of travel is the +normal hemisphere.
                let side = if dot(p, nu) >= 0.0 { 1.0 } else { -1.0 };
                (d, side)
            };
            // Deterministic tie-break: strict `<` keeps the FIRST segment on
            // an exact tie, so the result never depends on iteration accident.
            if d < best {
                best = d;
                best_sign = side;
            }
        }
        best * best_sign
    }
}

/// Index of the band `|d|` falls in, given strictly ascending `edges`.
/// Returns `edges.len()` for anything beyond the last edge.
/// type-audit: pending(wave-1: d), pending(wave-1: edges), bare-ok(index: return)
pub fn band(d: f64, edges: &[f64]) -> usize {
    let a = d.abs();
    edges.iter().position(|e| a < *e).unwrap_or(edges.len())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unit(x: f64, y: f64, z: f64) -> [f64; 3] {
        let n = (x * x + y * y + z * z).sqrt();
        [x / n, y / n, z / n]
    }

    /// A point exactly on the line has zero distance.
    #[test]
    fn a_point_on_the_polyline_is_at_distance_zero() {
        let line = SphericalPolyline {
            points: vec![unit(1.0, 0.0, 0.0), unit(0.0, 1.0, 0.0)],
        };
        let on = unit(1.0, 1.0, 0.0);
        assert!(line.signed_distance(on).abs() < 1e-12);
    }

    /// THE POINT OF THE SIGN: two points on opposite sides of the same
    /// segment, equidistant from it, must differ in sign and agree in
    /// magnitude. Without this, a ford (a side change) is inexpressible.
    #[test]
    fn opposite_sides_have_opposite_signs_and_equal_magnitude() {
        let line = SphericalPolyline {
            points: vec![unit(1.0, 0.0, 0.0), unit(0.0, 1.0, 0.0)],
        };
        let left = unit(1.0, 1.0, 0.2);
        let right = unit(1.0, 1.0, -0.2);
        let dl = line.signed_distance(left);
        let dr = line.signed_distance(right);
        assert!(dl * dr < 0.0, "expected opposite signs, got {dl} and {dr}");
        assert!((dl.abs() - dr.abs()).abs() < 1e-12);
    }

    /// Distance is to the NETWORK, so a point near the far segment of a
    /// two-segment line reads off that segment, not the first.
    #[test]
    fn distance_is_to_the_nearest_segment() {
        let line = SphericalPolyline {
            points: vec![
                unit(1.0, 0.0, 0.0),
                unit(0.0, 1.0, 0.0),
                unit(0.0, 0.0, 1.0),
            ],
        };
        let near_second = unit(0.0, 1.0, 1.0);
        assert!(line.signed_distance(near_second).abs() < 0.02);
    }

    /// Past an endpoint, distance is to the endpoint itself, not to the
    /// infinite great circle through it.
    #[test]
    fn beyond_an_endpoint_distance_is_to_the_endpoint() {
        let line = SphericalPolyline {
            points: vec![unit(1.0, 0.0, 0.0), unit(1.0, 0.1, 0.0)],
        };
        let past = unit(1.0, -0.5, 0.0);
        let d = line.signed_distance(past).abs();
        let to_endpoint = crate::math::acos(dot(past, unit(1.0, 0.0, 0.0)).clamp(-1.0, 1.0));
        assert!(
            (d - to_endpoint).abs() < 1e-9,
            "d={d} endpoint={to_endpoint}"
        );
    }

    #[test]
    fn band_indexes_ascending_edges() {
        let edges = [0.1, 0.3, 0.7];
        assert_eq!(band(0.05, &edges), 0);
        assert_eq!(band(-0.05, &edges), 0); // banding is on |d|
        assert_eq!(band(0.2, &edges), 1);
        assert_eq!(band(0.5, &edges), 2);
        assert_eq!(band(9.0, &edges), 3);
    }
}
