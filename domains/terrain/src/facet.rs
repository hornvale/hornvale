//! Addressed terrain facets and refinement-independent feature geometry.

use std::cmp::Ordering;

use hornvale_kernel::{Facet, FacetError, Geosphere, Vertex, math};

use crate::{
    CatchmentCut, ChannelNetwork, GeneratedTerrain, TectonicGlobe, WaterKind, band_edges,
    channel_half_width, local_slope, rills_of, vertex_catchment,
};

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

    /// Construct identity from stable semantics in an addressed patch.
    ///
    /// The address is realization context only: refinement must never enter
    /// the identity, which is determined by the semantic feature inputs.
    /// type-audit: bare-ok(index: ordinal)
    pub fn from_address(
        _address: &FacetAddress,
        kind: FeatureKind,
        macro_anchor: Vertex,
        ordinal: u32,
    ) -> Self {
        Self::new(kind, macro_anchor, ordinal)
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

/// Calibration for deterministic, patch-local ribbon sampling.
/// type-audit: bare-ok(count: patch_divisions), bare-ok(ratio: width_step_multiplier), bare-ok(ratio: curvature_gain), bare-ok(count: max_subdivisions)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FeatureStripSampling {
    /// Baseline longitudinal samples across one patch diameter.
    pub patch_divisions: u32,
    /// Maximum longitudinal step as a multiple of feature width.
    pub width_step_multiplier: f64,
    /// Additional sampling pressure at polyline turns.
    pub curvature_gain: f64,
    /// Per-source-segment safety bound.
    pub max_subdivisions: u32,
}

/// One signed ribbon edge vertex before worldgen evaluates its surface field.
/// type-audit: pending(wave-1)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FeatureStripLayoutVertex {
    /// Canonical unit-sphere position.
    pub position: [f64; 3],
    /// `-1` on the right and `1` on the left, facing feature travel.
    pub side: i8,
    /// Signed angular distance from the centerline.
    pub signed_distance_rad: f64,
}

/// Adaptive, render-independent layout for one patch-local feature ribbon.
/// type-audit: pending(wave-1)
#[derive(Clone, Debug, PartialEq)]
pub struct FeatureStripLayout {
    /// Stable source feature identity.
    pub feature: FeatureId,
    /// Adaptively sampled unit-sphere centerline in travel order.
    pub centerline: Vec<[f64; 3]>,
    /// Full angular width at each centerline sample.
    pub width_rad: Vec<f64>,
    /// Paired right/left edge vertices for every centerline sample.
    pub vertices: Vec<FeatureStripLayoutVertex>,
    /// Canonical triangle-list topology over `vertices`.
    pub triangles: Vec<[u32; 3]>,
    /// Source-owned material-channel mask.
    pub semantic_mask: [f32; 8],
    /// Unchanged feature topology and boundary continuation metadata.
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

/// Borrowed, already-generated hydrology used to realize a terrain patch.
#[derive(Clone, Copy)]
pub struct TerrainFacetInputs<'a> {
    /// The authoritative elevations, drainage and downhill graph.
    pub globe: &'a TectonicGlobe,
    /// The mesh against which the globe was generated.
    pub geo: &'a Geosphere,
    /// The rendered, confluence-repaired channel network.
    pub channels: &'a ChannelNetwork,
}

/// World-space borders, left then right looking downstream, at a curve point.
/// All positions are on the unit sphere; these are geometry, not new routing.
/// type-audit: pending(wave-1)
#[derive(Clone, Debug, PartialEq)]
pub struct ChannelCrossSection {
    /// Water's two edges.
    pub banks: [[f64; 3]; 2],
    /// The outer floodplain edges (coincident with the bank border in a gorge).
    pub floodplain: [[f64; 3]; 2],
    /// The outer terrace edges.
    pub terrace: [[f64; 3]; 2],
    /// A widening apron on the final reach into standing water, when present.
    pub delta: Option<[[f64; 3]; 2]>,
}

/// Classify an inherited endpoint at `vertex`, before any patch clipping.
/// Upstream land with no inflow is a headwater; a downstream land sink is a
/// lake. A nonterminal downstream endpoint joins the owning trunk. Clipping,
/// not the drainage graph, supplies `Continuation`.
/// type-audit: pending(wave-1: downstream)
pub fn channel_endpoint_kind(
    inputs: TerrainFacetInputs<'_>,
    vertex: Vertex,
    downstream: bool,
) -> TerminalKind {
    if *inputs.globe.water_kind.get(vertex) == WaterKind::Ocean {
        TerminalKind::Ocean
    } else if downstream && inputs.globe.downhill.get(vertex).is_none() {
        TerminalKind::Lake
    } else if downstream
        || inputs
            .geo
            .neighbors(vertex)
            .iter()
            .any(|&upstream| *inputs.globe.downhill.get(upstream) == Some(vertex))
    {
        TerminalKind::Confluence
    } else {
        TerminalKind::Headwater
    }
}

fn trunk_id(inputs: TerrainFacetInputs<'_>, line: usize) -> FeatureId {
    FeatureId::new(
        FeatureKind::ChannelReach,
        inputs.channels.run_vertices[line][0],
        0,
    )
}

fn endpoint(feature: FeatureId, side: EndpointSide, terminal: TerminalKind) -> FeatureEndpoint {
    FeatureEndpoint {
        feature,
        side,
        boundary: None,
        terminal,
    }
}

fn trunk_curve(inputs: TerrainFacetInputs<'_>, line: usize) -> RealizedCurve {
    let vertices = &inputs.channels.run_vertices[line];
    let feature = trunk_id(inputs, line);
    RealizedCurve {
        feature,
        points: inputs.channels.polylines[line].points.clone(),
        width: inputs.channels.band_edges[line]
            .iter()
            .map(|edges| 2.0 * edges[0])
            .collect(),
        endpoints: [
            endpoint(
                feature,
                EndpointSide::Upstream,
                channel_endpoint_kind(inputs, vertices[0], false),
            ),
            endpoint(
                feature,
                EndpointSide::Downstream,
                channel_endpoint_kind(
                    inputs,
                    *vertices.last().expect("channel has vertices"),
                    true,
                ),
            ),
        ],
    }
}

fn rill_curve(inputs: TerrainFacetInputs<'_>, rill: &crate::Rill, index: usize) -> RealizedCurve {
    // Zero is reserved for a trunk. Partition order is independent of patch
    // address and refinement and is the existing Rill identity convention.
    let ordinal = u32::try_from(index + 1).expect("rill ordinal exceeds feature identity");
    let feature = FeatureId::new(FeatureKind::ChannelReach, rill.vertex, ordinal);
    let width = 2.0
        * channel_half_width(
            rill.catchment / vertex_catchment(inputs.geo),
            crate::channel::vertex_spacing(inputs.geo, rill.vertex),
        );
    RealizedCurve {
        feature,
        points: vec![rill.head, rill.mouth],
        width: vec![width; 2],
        endpoints: [
            endpoint(feature, EndpointSide::Upstream, TerminalKind::Headwater),
            endpoint(feature, EndpointSide::Downstream, TerminalKind::Confluence),
        ],
    }
}

/// Realize inherited trunks and attached rills intersecting `address`.
///
/// Ordinal zero identifies a whole trunk by its head vertex; positive ordinals
/// identify rills by their vertex and partition index plus one. A trunk can
/// leave and re-enter a patch, yielding multiple pieces with the same ID.
/// Rill candidates use a conservative spherical cap enclosing both their
/// tangent-plane square and parent stretch. The square is an area proxy, not
/// a claim that the branches lie inside an addressed facet. Every emitted
/// segment is clipped against the actual facet boundary.
pub fn realize_channel_curves(
    inputs: TerrainFacetInputs<'_>,
    address: &FacetAddress,
) -> Vec<RealizedCurve> {
    let facet = address.resolved();
    let center = facet.centroid();
    let radius = facet
        .corners()
        .iter()
        .map(|&p| angle(center, p))
        .fold(0.0, f64::max);
    let mut curves = Vec::new();
    for line in 0..inputs.channels.polylines.len() {
        clip_curve(&trunk_curve(inputs, line), address, &mut curves);
    }
    let cut = CatchmentCut::Drawn(inputs.globe.rill_partition_seed());
    // atan(diagonal / 2) <= diagonal / 2. Include the entire neighboring
    // trunk segments as well: a root mouth may lie beyond the square.
    let square_radius = (vertex_catchment(inputs.geo) / 2.0).sqrt();
    for vertex in inputs.geo.vertices() {
        let Some((line, index)) = inputs.channels.trunk_vertex(vertex) else {
            continue;
        };
        let points = &inputs.channels.polylines[line].points;
        let origin = inputs.geo.position(vertex);
        let reach_radius = points[index.saturating_sub(1)..=index + 1]
            .iter()
            .map(|&p| angle(origin, p))
            .fold(square_radius, f64::max);
        if angle(origin, center) > reach_radius + radius {
            continue;
        }
        for (index, rill) in rills_of(vertex, inputs.channels, inputs.geo, &cut)
            .iter()
            .enumerate()
        {
            clip_curve(&rill_curve(inputs, rill, index), address, &mut curves);
        }
    }
    curves
}

/// Build deterministic patch-local ribbons independently of terrain vertices.
///
/// Longitudinal spacing responds to patch scale, feature width and polyline
/// turning. Refinement may change the number of samples, but never feature or
/// endpoint identity. Curves have already been clipped by the canonical facet
/// boundary evaluator, so strip endpoints retain those exact continuation
/// tokens.
pub fn adaptive_feature_strips(
    curves: &[RealizedCurve],
    address: &FacetAddress,
    sampling: FeatureStripSampling,
) -> Vec<FeatureStripLayout> {
    assert!(sampling.patch_divisions > 0);
    assert!(sampling.width_step_multiplier.is_finite() && sampling.width_step_multiplier > 0.0);
    assert!(sampling.curvature_gain.is_finite() && sampling.curvature_gain >= 0.0);
    assert!(sampling.max_subdivisions > 0);
    let corners = address.resolved().corners();
    let patch_scale = (0..4)
        .map(|edge| angle(corners[edge], corners[(edge + 1) % 4]))
        .fold(0.0, f64::max);
    let patch_step = patch_scale / f64::from(sampling.patch_divisions);
    let minimum_step = patch_step * 0.5;
    let mut layouts = curves
        .iter()
        .filter(|curve| curve.points.len() >= 2 && curve.points.len() == curve.width.len())
        .map(|curve| {
            let mut centerline = Vec::new();
            let mut width_rad = Vec::new();
            for (segment_index, segment) in curve.points.windows(2).enumerate() {
                let segment_length = angle(segment[0], segment[1]);
                let width = curve.width[segment_index]
                    .min(curve.width[segment_index + 1])
                    .max(f64::EPSILON);
                let turn = endpoint_turn(&curve.points, segment_index)
                    .max(endpoint_turn(&curve.points, segment_index + 1));
                let width_step = (width * sampling.width_step_multiplier).max(minimum_step);
                let target_step = patch_step.min(width_step)
                    / (1.0 + sampling.curvature_gain * turn / std::f64::consts::PI);
                let subdivisions = ((segment_length / target_step).ceil() as u32)
                    .clamp(1, sampling.max_subdivisions);
                if centerline.is_empty() {
                    centerline.push(segment[0]);
                    width_rad.push(curve.width[segment_index]);
                }
                for step in 1..=subdivisions {
                    let t = f64::from(step) / f64::from(subdivisions);
                    centerline.push(interpolate(segment[0], segment[1], t));
                    width_rad.push(
                        curve.width[segment_index]
                            + t * (curve.width[segment_index + 1] - curve.width[segment_index]),
                    );
                }
            }
            let vertices = centerline
                .iter()
                .enumerate()
                .flat_map(|(index, &center)| {
                    let before = centerline[index.saturating_sub(1)];
                    let after = centerline[(index + 1).min(centerline.len() - 1)];
                    let mut lateral = normalize(cross(before, after));
                    if dot(lateral, lateral) == 0.0 {
                        lateral = normalize(cross(center, after));
                    }
                    let half_width = width_rad[index] * 0.5;
                    [-1_i8, 1_i8].map(|side| {
                        let signed_distance_rad = f64::from(side) * half_width;
                        let (sine, cosine) = (
                            math::sin(signed_distance_rad),
                            math::cos(signed_distance_rad),
                        );
                        FeatureStripLayoutVertex {
                            position: normalize(std::array::from_fn(|axis| {
                                cosine * center[axis] + sine * lateral[axis]
                            })),
                            side,
                            signed_distance_rad,
                        }
                    })
                })
                .collect::<Vec<_>>();
            let triangles = (0..centerline.len().saturating_sub(1))
                .flat_map(|index| {
                    let right = u32::try_from(index * 2).expect("feature strip exceeds u32");
                    [
                        [right, right + 1, right + 2],
                        [right + 1, right + 3, right + 2],
                    ]
                })
                .collect();
            let semantic_mask = match curve.feature.kind {
                FeatureKind::ChannelReach | FeatureKind::Confluence => {
                    [0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0]
                }
                FeatureKind::Shoreline => [0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0],
                FeatureKind::Ridge => [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
                FeatureKind::MaterialTransition => [0.0; 8],
            };
            FeatureStripLayout {
                feature: curve.feature,
                centerline,
                width_rad,
                vertices,
                triangles,
                semantic_mask,
                endpoints: curve.endpoints.clone(),
            }
        })
        .collect::<Vec<_>>();
    layouts.sort_by(|left, right| {
        left.feature.cmp(&right.feature).then_with(|| {
            point_cmp(&left.centerline[0], &right.centerline[0]).then_with(|| {
                point_cmp(
                    left.centerline.last().expect("strip has samples"),
                    right.centerline.last().expect("strip has samples"),
                )
            })
        })
    });
    layouts
}

fn endpoint_turn(points: &[[f64; 3]], index: usize) -> f64 {
    if index == 0 || index + 1 >= points.len() {
        return 0.0;
    }
    let incoming = normalize(cross(points[index - 1], points[index]));
    let outgoing = normalize(cross(points[index], points[index + 1]));
    angle(incoming, outgoing)
}

impl RealizedCurve {
    /// Resolve the one inherited feature this piece joins, if its downstream
    /// endpoint is a confluence. Endpoint `feature` remains the owning ID.
    pub fn downstream_feature(&self, inputs: TerrainFacetInputs<'_>) -> Option<FeatureId> {
        if self.feature.kind != FeatureKind::ChannelReach
            || self.endpoints[1].terminal != TerminalKind::Confluence
        {
            return None;
        }
        let vertex = self.feature.macro_anchor;
        let (line, _) = inputs.channels.trunk_vertex(vertex)?;
        if self.feature.ordinal == 0 {
            let mouth = *inputs.channels.run_vertices[line].last()?;
            let (owner, _) = inputs.channels.trunk_vertex(mouth)?;
            Some(trunk_id(inputs, owner))
        } else {
            let rills = rills_of(
                vertex,
                inputs.channels,
                inputs.geo,
                &CatchmentCut::Drawn(inputs.globe.rill_partition_seed()),
            );
            let rill = rills.get(self.feature.ordinal as usize - 1)?;
            Some(match rill.parent {
                Some(parent) => {
                    FeatureId::new(FeatureKind::ChannelReach, vertex, (parent + 1) as u32)
                }
                None => trunk_id(inputs, line),
            })
        }
    }

    /// Generate bank, floodplain, terrace and mouth-apron geometry from the
    /// same inherited width and slope laws as the channel network. Sampling
    /// uses the full feature so clipping does not change the transverse frame.
    pub fn cross_sections(&self, inputs: TerrainFacetInputs<'_>) -> Vec<ChannelCrossSection> {
        let (source, edges) = source_geometry(self.feature, inputs);
        self.points
            .iter()
            .map(|&point| {
                let projection =
                    nearest_segment(&source.points, point).expect("channel has segments");
                let j = projection.segment;
                let t = projection.interpolation;
                let borders: [f64; 4] =
                    std::array::from_fn(|i| edges[j][i] + t * (edges[j + 1][i] - edges[j][i]));
                let normal = cross(source.points[j], source.points[j + 1]);
                // A rill head can already lie on its parent (zero length).
                // It still carries catchment and width, but no direction of
                // its own: use the inherited trunk's transverse frame.
                let left = if dot(normal, normal) <= f64::EPSILON * f64::EPSILON {
                    let (line, index) = inputs
                        .channels
                        .trunk_vertex(self.feature.macro_anchor)
                        .expect("channel has a trunk");
                    let points = &inputs.channels.polylines[line].points;
                    normalize(cross(points[index], points[index + 1]))
                } else {
                    normalize(normal)
                };
                let offset = |distance: f64| {
                    [-1.0, 1.0].map(|sign| {
                        let (sine, cosine) = (math::sin(distance), math::cos(distance));
                        // Positive cross(a,b) is left facing downstream.
                        normalize(std::array::from_fn(|i| {
                            cosine * point[i] - sign * sine * left[i]
                        }))
                    })
                };
                let at_mouth = j + 2 == source.points.len()
                    && matches!(
                        source.endpoints[1].terminal,
                        TerminalKind::Lake | TerminalKind::Ocean
                    );
                ChannelCrossSection {
                    banks: offset(borders[0]),
                    floodplain: offset(borders[2]),
                    terrace: offset(borders[3]),
                    delta: at_mouth.then(|| offset(borders[0] + t * (borders[2] - borders[0]))),
                }
            })
            .collect()
    }
}

fn source_geometry(
    feature: FeatureId,
    inputs: TerrainFacetInputs<'_>,
) -> (RealizedCurve, Vec<[f64; 4]>) {
    assert_eq!(
        feature.kind,
        FeatureKind::ChannelReach,
        "channel geometry requires a channel feature"
    );
    let (line, _) = inputs
        .channels
        .trunk_vertex(feature.macro_anchor)
        .expect("feature has a trunk");
    if feature.ordinal == 0 {
        (
            trunk_curve(inputs, line),
            inputs.channels.band_edges[line].clone(),
        )
    } else {
        let rills = rills_of(
            feature.macro_anchor,
            inputs.channels,
            inputs.geo,
            &CatchmentCut::Drawn(inputs.globe.rill_partition_seed()),
        );
        let index = feature.ordinal as usize - 1;
        let rill = &rills[index];
        let edges = band_edges(
            rill.catchment / vertex_catchment(inputs.geo),
            local_slope(inputs.globe, inputs.geo, rill.vertex),
            crate::channel::vertex_spacing(inputs.geo, rill.vertex),
        );
        (rill_curve(inputs, rill, index), vec![edges; 2])
    }
}

/// Sample the realized bed at endpoints and three interior positions per
/// segment. Heights are metres above the terrain datum, not radial positions.
///
/// Trunks interpolate the authoritative run's decreasing vertex elevations.
/// Rills settle exactly onto their parent's bed at the inherited mouth and
/// rise upstream by the local macro slope times angular length. This realizes
/// bed geometry without asking a second graph where water should flow. No
/// settling rise is introduced; roundoff comparisons may allow 1e-8 metres.
/// type-audit: pending(wave-1: return)
pub fn bed_height_profile(curve: &RealizedCurve, terrain: &GeneratedTerrain) -> Vec<f64> {
    let inputs = TerrainFacetInputs {
        globe: terrain.globe(),
        geo: terrain.geosphere(),
        channels: terrain.channels(),
    };
    let vertex = curve.feature.macro_anchor;
    assert_eq!(
        curve.feature.kind,
        FeatureKind::ChannelReach,
        "bed profile requires a channel feature"
    );
    let (line, _) = inputs
        .channels
        .trunk_vertex(vertex)
        .expect("feature has a trunk");
    let trunk = &inputs.channels.polylines[line].points;
    let heights: Vec<_> = inputs.channels.run_vertices[line]
        .iter()
        .map(|&v| terrain.elevation_at(v).get())
        .collect();
    let (source, heights) = if curve.feature.ordinal == 0 {
        (trunk.clone(), heights)
    } else {
        let rills = rills_of(
            vertex,
            inputs.channels,
            inputs.geo,
            &CatchmentCut::Drawn(inputs.globe.rill_partition_seed()),
        );
        let index = curve.feature.ordinal as usize - 1;
        let slope = local_slope(inputs.globe, inputs.geo, vertex);
        let mut beds: Vec<[f64; 2]> = Vec::with_capacity(index + 1);
        for rill in &rills[..=index] {
            let mouth = match rill.parent {
                Some(parent) => sample_height(
                    &[rills[parent].head, rills[parent].mouth],
                    &beds[parent],
                    rill.mouth,
                ),
                None => sample_height(trunk, &heights, rill.mouth),
            };
            // atan2 is stable for short and coincident rills. acos(dot(p,p))
            // can invent a nonzero length from normalization roundoff.
            let normal = cross(rill.head, rill.mouth);
            let length = math::atan2(dot(normal, normal).sqrt(), dot(rill.head, rill.mouth));
            beds.push([mouth + slope * length, mouth]);
        }
        (
            vec![rills[index].head, rills[index].mouth],
            beds[index].to_vec(),
        )
    };
    let mut profile = Vec::new();
    for segment in curve.points.windows(2) {
        for t in [0.0, 0.25, 0.5, 0.75] {
            profile.push(sample_height(
                &source,
                &heights,
                interpolate(segment[0], segment[1], t),
            ));
        }
    }
    if let Some(&last) = curve.points.last() {
        profile.push(sample_height(&source, &heights, last));
    }
    profile
}

fn sample_height(points: &[[f64; 3]], heights: &[f64], point: [f64; 3]) -> f64 {
    // Preserve the inherited height exactly at an authored endpoint. An
    // acos-based projection loses precision near t=0/1 on short rills and
    // can otherwise create a bed step at an exactly attached mouth.
    if let Some(index) = points.iter().position(|&p| p == point) {
        return heights[index];
    }
    let projection = nearest_segment(points, point).expect("bed has segments");
    let j = projection.segment;
    heights[j] + projection.interpolation * (heights[j + 1] - heights[j])
}

fn interpolate(a: [f64; 3], b: [f64; 3], t: f64) -> [f64; 3] {
    if t == 0.0 {
        return a;
    }
    if t == 1.0 {
        return b;
    }
    normalize(std::array::from_fn(|i| a[i] + t * (b[i] - a[i])))
}

fn boundary_at(address: &FacetAddress, edge: u8, point: [f64; 3]) -> BoundaryParameter {
    let a = canonical_edge_sample(address, edge, 0.0);
    let b = canonical_edge_sample(address, edge, 1.0);
    let normal = cross(a, b);
    let before = dot(cross(a, point), normal);
    let after = dot(cross(point, b), normal);
    BoundaryParameter {
        address: address.clone(),
        edge,
        t: (before / (before + after)).clamp(0.0, 1.0),
    }
}

fn clip_curve(source: &RealizedCurve, address: &FacetAddress, output: &mut Vec<RealizedCurve>) {
    let facet = address.resolved();
    let corners = facet.corners();
    let center = facet.centroid();
    let normals: [[f64; 3]; 4] = std::array::from_fn(|i| {
        let normal = cross(corners[i], corners[(i + 1) % 4]);
        let sign = if dot(normal, center) >= 0.0 {
            1.0
        } else {
            -1.0
        };
        normal.map(|v| v * sign)
    });
    let mut piece: Option<RealizedCurve> = None;
    for (index, segment) in source.points.windows(2).enumerate() {
        let (mut enter, mut exit) = (0.0_f64, 1.0_f64);
        let (mut enter_edge, mut exit_edge) = (None, None);
        for (edge, &normal) in normals.iter().enumerate() {
            let (a, b) = (dot(normal, segment[0]), dot(normal, segment[1]));
            if a < 0.0 && b < 0.0 {
                exit = -1.0;
                break;
            }
            if a < 0.0 {
                let t = a / (a - b);
                if t > enter {
                    enter = t;
                    enter_edge = Some(edge as u8);
                }
            } else if b < 0.0 {
                let t = a / (a - b);
                if t < exit {
                    exit = t;
                    exit_edge = Some(edge as u8);
                }
            }
        }
        if enter >= exit {
            if let Some(piece) = piece.take() {
                output.push(piece);
            }
            continue;
        }
        let make_end = |t, edge: Option<u8>, side| {
            let position = interpolate(segment[0], segment[1], t);
            let mut end =
                source.endpoints[if side == EndpointSide::Upstream { 0 } else { 1 }].clone();
            // Only the ends of the complete feature inherit its terminals.
            // An interior vertex exactly on the boundary may be followed by
            // a rejected segment, or preceded by one, without a strict edge
            // crossing setting enter_edge/exit_edge.
            let is_feature_end = match side {
                EndpointSide::Upstream => index == 0 && t == 0.0,
                EndpointSide::Downstream => index + 2 == source.points.len() && t == 1.0,
            };
            if is_feature_end {
                return (position, end);
            }
            end.terminal = TerminalKind::Continuation;
            end.boundary = None;
            let boundary_edge = edge.or_else(|| {
                normals
                    .iter()
                    .position(|&normal| {
                        dot(normal, position).abs() <= f64::EPSILON * dot(normal, normal).sqrt()
                    })
                    .map(|edge| edge as u8)
            });
            let position = if let Some(boundary_edge) = boundary_edge {
                let boundary = boundary_at(address, boundary_edge, position);
                // Keep an authored vertex unchanged; only a newly clipped
                // intersection needs to be placed from its canonical token.
                let position = if edge.is_some() {
                    canonical_edge_sample(address, boundary_edge, boundary.t)
                } else {
                    position
                };
                end.boundary = Some(boundary);
                position
            } else {
                position
            };
            (position, end)
        };
        let (start, upstream) = make_end(enter, enter_edge, EndpointSide::Upstream);
        let (end, downstream) = make_end(exit, exit_edge, EndpointSide::Downstream);
        let width = |t| source.width[index] + t * (source.width[index + 1] - source.width[index]);
        let current = piece.get_or_insert_with(|| RealizedCurve {
            feature: source.feature,
            points: vec![start],
            width: vec![width(enter)],
            endpoints: [upstream, downstream.clone()],
        });
        current.points.push(end);
        current.width.push(width(exit));
        current.endpoints[1] = downstream;
        if exit_edge.is_some() {
            output.push(piece.take().expect("piece was started"));
        }
    }
    if let Some(piece) = piece {
        output.push(piece);
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    /// Cross a facet edge whose great-circle plane evaluates to exactly zero
    /// at the authored middle vertex, rather than a near-boundary crossing.
    fn exact_boundary_fixture() -> (FacetAddress, u8, [[f64; 3]; 3]) {
        let address = FacetAddress::new(
            Facet {
                face: 0,
                path: vec![0; crate::GLOBE_LEVEL as usize],
            },
            vec![],
        )
        .unwrap();
        let corners = address.resolved().corners();
        let edge = (0..4)
            .find(|&edge| {
                let boundary = canonical_edge_sample(&address, edge, 0.5);
                dot(
                    cross(corners[edge as usize], corners[(edge as usize + 1) % 4]),
                    boundary,
                ) == 0.0
            })
            .expect("fixture needs an exact-zero boundary vertex");
        let inside = address.resolved().centroid();
        let boundary = canonical_edge_sample(&address, edge, 0.5);
        let outside = normalize(std::array::from_fn(|i| 2.0 * boundary[i] - inside[i]));
        let normal = cross(corners[edge as usize], corners[(edge as usize + 1) % 4]);
        assert!(dot(normal, inside) * dot(normal, outside) < 0.0);
        (address, edge, [inside, boundary, outside])
    }

    fn source_curve(
        points: Vec<[f64; 3]>,
        upstream: TerminalKind,
        downstream: TerminalKind,
    ) -> RealizedCurve {
        let feature = FeatureId::new(FeatureKind::ChannelReach, Vertex(42), 0);
        RealizedCurve {
            feature,
            width: vec![0.001; points.len()],
            points,
            endpoints: [
                endpoint(feature, EndpointSide::Upstream, upstream),
                endpoint(feature, EndpointSide::Downstream, downstream),
            ],
        }
    }

    fn assert_boundary(end: &FeatureEndpoint, address: &FacetAddress, edge: u8, point: [f64; 3]) {
        assert_eq!(end.terminal, TerminalKind::Continuation);
        let boundary = end
            .boundary
            .as_ref()
            .expect("continuation needs a canonical token");
        assert_eq!(boundary.address, *address);
        assert_eq!(boundary.edge, edge);
        assert!((boundary.t - 0.5).abs() < 1.0e-12);
        let reconstructed = canonical_edge_sample(address, edge, boundary.t);
        assert!(
            reconstructed
                .iter()
                .zip(point)
                .all(|(a, b)| (a - b).abs() < 1.0e-12)
        );
    }

    #[test]
    fn exact_boundary_exit_is_a_continuation_before_the_real_terminal() {
        let (address, edge, points) = exact_boundary_fixture();
        for terminal in [
            TerminalKind::Lake,
            TerminalKind::Ocean,
            TerminalKind::Confluence,
        ] {
            let source = source_curve(points.to_vec(), TerminalKind::Headwater, terminal);
            let mut pieces = Vec::new();
            clip_curve(&source, &address, &mut pieces);
            assert_eq!(pieces.len(), 1);
            let piece = &pieces[0];
            assert_eq!(piece.points.len(), 2);
            assert_eq!(piece.endpoints[0], source.endpoints[0]);
            assert_boundary(
                &piece.endpoints[1],
                &address,
                edge,
                *piece.points.last().unwrap(),
            );
        }
    }

    #[test]
    fn exact_boundary_entry_is_a_continuation_after_the_real_source() {
        let (address, edge, [inside, boundary, outside]) = exact_boundary_fixture();
        for upstream in [TerminalKind::Headwater, TerminalKind::Confluence] {
            let source = source_curve(
                vec![outside, boundary, inside],
                upstream,
                TerminalKind::Lake,
            );
            let mut pieces = Vec::new();
            clip_curve(&source, &address, &mut pieces);
            assert_eq!(pieces.len(), 1);
            let piece = &pieces[0];
            assert_eq!(piece.points.len(), 2);
            assert_boundary(&piece.endpoints[0], &address, edge, piece.points[0]);
            assert_eq!(piece.endpoints[1], source.endpoints[1]);
        }
    }

    #[test]
    fn exact_boundary_genuine_terminals_and_sources_keep_their_meaning() {
        let (address, _, [inside, boundary, _]) = exact_boundary_fixture();
        for terminal in [
            TerminalKind::Lake,
            TerminalKind::Ocean,
            TerminalKind::Confluence,
        ] {
            for points in [vec![inside, boundary], vec![boundary, inside]] {
                let source = source_curve(points, TerminalKind::Headwater, terminal);
                let mut pieces = Vec::new();
                clip_curve(&source, &address, &mut pieces);
                assert_eq!(pieces.len(), 1);
                assert_eq!(pieces[0], source);
            }
        }
    }

    #[test]
    fn exact_boundary_touch_inside_a_feature_does_not_split_it() {
        let (address, _, [inside, boundary, _]) = exact_boundary_fixture();
        let source = source_curve(
            vec![inside, boundary, inside],
            TerminalKind::Headwater,
            TerminalKind::Ocean,
        );
        let mut pieces = Vec::new();
        clip_curve(&source, &address, &mut pieces);
        assert_eq!(pieces.len(), 1);
        assert_eq!(pieces[0].endpoints, source.endpoints);
        assert_eq!(pieces[0].points.len(), 3);
    }
}
