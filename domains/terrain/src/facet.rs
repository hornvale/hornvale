//! Addressed terrain facets and refinement-independent feature geometry.

use std::cmp::Ordering;

use hornvale_kernel::{Facet, FacetError, Vertex, math};

/// A terrain patch below one canonical macro-grid facet.
///
/// `macro_face` identifies the Level-6 facet shared by world generation and
/// clients. `child_path` refines that facet without changing any feature's
/// macro identity; its length is the refinement level below the macro grid.
/// type-audit: bare-ok(index: child_path)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FacetAddress {
    /// The canonical macro-grid facet.
    pub macro_face: Facet,
    /// Quadtree child digits (`0..=3`) below `macro_face`.
    pub child_path: Vec<u8>,
}

impl FacetAddress {
    /// Construct a validated address.
    ///
    /// Rejects an invalid macro facet, a child digit outside `0..=3`, or a
    /// combined path deeper than the kernel's address cap.
    /// type-audit: bare-ok(index: child_path)
    pub fn new(macro_face: Facet, child_path: Vec<u8>) -> Result<Self, FacetError> {
        macro_face.pack()?;
        if macro_face.depth() != crate::GLOBE_LEVEL {
            return Err(FacetError::Invalid);
        }
        let mut facet = macro_face.clone();
        for &digit in &child_path {
            facet = facet.child(digit)?;
        }
        Ok(Self {
            macro_face,
            child_path,
        })
    }

    /// Resolve the two-part terrain address to the kernel's full facet address.
    fn resolved(&self) -> Facet {
        assert_eq!(
            self.macro_face.depth(),
            crate::GLOBE_LEVEL,
            "FacetAddress macro_face must be a Level-6 facet"
        );
        let mut facet = self.macro_face.clone();
        for &digit in &self.child_path {
            facet = facet
                .child(digit)
                .expect("FacetAddress child digits and total depth must be valid");
        }
        facet
    }
}

/// The stable class of an addressed terrain feature.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FeatureKind {
    /// One directed reach of a drainage channel.
    ChannelReach,
    /// A junction where directed channel reaches meet.
    Confluence,
    /// A land-water boundary.
    Shoreline,
    /// A linear crest in the terrain surface.
    Ridge,
    /// A boundary between terrain materials.
    MaterialTransition,
}

/// Refinement-independent identity for a terrain feature.
///
/// Identity contains only a macro-grid anchor and a stable ordinal. Patch
/// addresses and realized geometry deliberately do not enter it.
/// type-audit: bare-ok(index: ordinal)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FeatureId {
    /// The feature's semantic class.
    pub kind: FeatureKind,
    /// Stable macro-grid vertex anchoring the feature.
    pub macro_anchor: Vertex,
    /// Stable ordinal among features of this kind at the anchor.
    pub ordinal: u32,
}

impl FeatureId {
    /// Construct a refinement-independent feature identity.
    /// type-audit: bare-ok(index: ordinal)
    pub fn new(kind: FeatureKind, macro_anchor: Vertex, ordinal: u32) -> Self {
        Self {
            kind,
            macro_anchor,
            ordinal,
        }
    }
}

/// Which directed end of a feature an endpoint describes.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum EndpointSide {
    /// The end from which the feature flows or is traversed.
    Upstream,
    /// The end toward which the feature flows or is traversed.
    Downstream,
}

/// Canonical location along one addressed facet edge.
/// type-audit: bare-ok(index: edge), bare-ok(ratio: t)
#[derive(Clone, Debug, PartialEq)]
pub struct BoundaryParameter {
    /// The facet whose boundary carries the point.
    pub address: FacetAddress,
    /// Edge index in the facet's counter-clockwise corner winding (`0..=3`).
    pub edge: u8,
    /// Canonically oriented position along the edge, in `[0, 1]`.
    pub t: f64,
}

/// How a feature ends at an endpoint.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TerminalKind {
    /// The source of a channel with no upstream continuation.
    Headwater,
    /// A join to another channel feature.
    Confluence,
    /// A terminal or through-flow lake.
    Lake,
    /// A terminal ocean.
    Ocean,
    /// The same feature continues beyond the current realized patch.
    Continuation,
}

/// One topological endpoint of a realized terrain feature.
#[derive(Clone, Debug, PartialEq)]
pub struct FeatureEndpoint {
    /// The feature this endpoint belongs to.
    pub feature: FeatureId,
    /// The directed side represented by this endpoint.
    pub side: EndpointSide,
    /// Canonical boundary crossing, when the feature continues across a patch.
    pub boundary: Option<BoundaryParameter>,
    /// The endpoint's topological terminal meaning.
    pub terminal: TerminalKind,
}

/// Patch-local world-space realization of a stable terrain feature.
/// type-audit: pending(wave-1: points), pending(wave-1: width)
#[derive(Clone, Debug, PartialEq)]
pub struct RealizedCurve {
    /// Stable identity shared by every realization of this feature.
    pub feature: FeatureId,
    /// Unit-sphere points in feature travel order.
    pub points: Vec<[f64; 3]>,
    /// Angular width at each corresponding point, in radians on the unit sphere.
    pub width: Vec<f64>,
    /// Upstream and downstream topology for this realization.
    pub endpoints: [FeatureEndpoint; 2],
}

/// Continuous terrain fields evaluated at one world-space position.
/// type-audit: pending(wave-1)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FacetFieldSample {
    /// Unit-sphere sample position.
    pub position: [f64; 3],
    /// Terrain height above the planetary datum, in metres.
    pub height_m: f64,
    /// Unit surface normal.
    pub normal: [f64; 3],
    /// Mixture weights for the eight terrain material channels.
    pub material_weights: [f32; 8],
    /// Signed distance to the shoreline, in metres.
    pub shoreline_distance_m: f64,
    /// Water depth, in metres.
    pub water_depth_m: f64,
    /// Unit direction of surface-water flow.
    pub flow_direction: [f64; 3],
    /// Relative strength of surface-water flow.
    pub flow_strength: f64,
    /// Signed distance to the nearest channel, in metres.
    pub channel_distance_m: f64,
    /// Width of the nearest channel, in metres.
    pub channel_width_m: f64,
    /// Continuous floodplain membership weight.
    pub floodplain_weight: f32,
    /// Continuous channel-bank membership weight.
    pub bank_weight: f32,
    /// Continuous terrace membership weight.
    pub terrace_weight: f32,
    /// Continuous delta membership weight.
    pub delta_weight: f32,
    /// Unit direction of the nearest ridge.
    pub ridge_direction: [f64; 3],
    /// Relative strength of the nearest ridge.
    pub ridge_strength: f32,
}

/// Evaluate a canonical point on an addressed facet edge in world space.
///
/// Edge endpoints are sorted into one world-space order before interpolation,
/// so adjacent facets use the same direction even when their local windings
/// oppose one another.
/// type-audit: bare-ok(index: edge), bare-ok(ratio: t), pending(wave-1: return)
pub fn canonical_edge_sample(address: &FacetAddress, edge: u8, t: f64) -> [f64; 3] {
    assert!(edge < 4, "invalid facet edge {edge}: expected 0..4");
    assert!(
        (0.0..=1.0).contains(&t),
        "invalid boundary parameter {t}: expected a finite value in [0, 1]"
    );
    let corners = address.resolved().corners();
    let mut endpoints = [corners[edge as usize], corners[(edge as usize + 1) % 4]];
    if point_cmp(&endpoints[1], &endpoints[0]) == Ordering::Less {
        endpoints.swap(0, 1);
    }
    if t == 0.0 {
        return endpoints[0];
    }
    if t == 1.0 {
        return endpoints[1];
    }
    normalize([
        endpoints[0][0] + t * (endpoints[1][0] - endpoints[0][0]),
        endpoints[0][1] + t * (endpoints[1][1] - endpoints[0][1]),
        endpoints[0][2] + t * (endpoints[1][2] - endpoints[0][2]),
    ])
}

/// Evaluate one addressed facet corner in world space.
/// type-audit: bare-ok(index: corner), pending(wave-1: return)
pub fn canonical_corner_sample(address: &FacetAddress, corner: u8) -> [f64; 3] {
    assert!(corner < 4, "invalid facet corner {corner}: expected 0..4");
    address.resolved().corners()[corner as usize]
}

/// Sample signed spherical distance and interpolated width from a curve.
///
/// Both returned values are angular: `(signed_distance_rad, width_rad)`.
/// Distance is left-positive relative to the curve's travel direction,
/// matching the kernel's spherical-polyline convention. Multiply either value
/// by the planetary radius for a length. An empty curve returns `(infinity, 0)`.
/// type-audit: pending(wave-1: position), pending(wave-1: return)
pub fn feature_sample(curve: &RealizedCurve, position: [f64; 3]) -> (f64, f64) {
    assert_eq!(
        curve.points.len(),
        curve.width.len(),
        "a realized curve must carry one width per point"
    );
    let Some(projection) = nearest_segment(&curve.points, position) else {
        return (f64::INFINITY, 0.0);
    };
    if curve.points.len() == 1 {
        return (projection.distance, curve.width[0]);
    }
    let index = projection.segment;
    let width = curve.width[index]
        + projection.interpolation * (curve.width[index + 1] - curve.width[index]);
    (projection.distance, width)
}

fn point_cmp(a: &[f64; 3], b: &[f64; 3]) -> Ordering {
    a[0].total_cmp(&b[0])
        .then_with(|| a[1].total_cmp(&b[1]))
        .then_with(|| a[2].total_cmp(&b[2]))
}

fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

fn cross(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}

fn normalize(v: [f64; 3]) -> [f64; 3] {
    let norm = dot(v, v).sqrt();
    if norm == 0.0 {
        v
    } else {
        [v[0] / norm, v[1] / norm, v[2] / norm]
    }
}

fn angle(a: [f64; 3], b: [f64; 3]) -> f64 {
    math::acos(dot(a, b).clamp(-1.0, 1.0))
}

struct NearestSegment {
    distance: f64,
    segment: usize,
    interpolation: f64,
}

fn nearest_segment(points: &[[f64; 3]], position: [f64; 3]) -> Option<NearestSegment> {
    match points.len() {
        0 => return None,
        1 => {
            return Some(NearestSegment {
                distance: angle(position, points[0]),
                segment: 0,
                interpolation: 0.0,
            });
        }
        _ => {}
    }

    let mut best_distance = f64::INFINITY;
    let mut best = None;
    for (index, segment) in points.windows(2).enumerate() {
        let (a, b) = (segment[0], segment[1]);
        let normal = cross(a, b);
        let normal_length = dot(normal, normal).sqrt();
        let (distance, t) = if normal_length == 0.0 {
            (angle(position, a), 0.0)
        } else {
            let unit_normal = [
                normal[0] / normal_length,
                normal[1] / normal_length,
                normal[2] / normal_length,
            ];
            let foot = normalize([
                position[0] - unit_normal[0] * dot(position, unit_normal),
                position[1] - unit_normal[1] * dot(position, unit_normal),
                position[2] - unit_normal[2] * dot(position, unit_normal),
            ]);
            let inside =
                dot(cross(a, foot), unit_normal) >= 0.0 && dot(cross(foot, b), unit_normal) >= 0.0;
            let side = if dot(position, unit_normal) >= 0.0 {
                1.0
            } else {
                -1.0
            };
            if inside {
                let segment_length = angle(a, b);
                let t = if segment_length == 0.0 {
                    0.0
                } else {
                    angle(a, foot) / segment_length
                };
                (side * angle(position, foot), t.clamp(0.0, 1.0))
            } else {
                let distance_a = angle(position, a);
                let distance_b = angle(position, b);
                if distance_a <= distance_b {
                    (side * distance_a, 0.0)
                } else {
                    (side * distance_b, 1.0)
                }
            }
        };
        if distance.abs() < best_distance {
            best_distance = distance.abs();
            best = Some(NearestSegment {
                distance,
                segment: index,
                interpolation: t,
            });
        }
    }
    best
}
