use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Facet, NearestVertexIndex, Seed, World};
use hornvale_terrain::{EndpointSide, FacetAddress, FeatureKind, TerminalKind, TerrainPins};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SurfaceBuildError, SurfacePatch, SurfaceRealizationContext,
    WorldComponents, build_world_to,
};

use hornvale_terrain::{FacetFieldSample, canonical_corner_sample, canonical_edge_sample};
use hornvale_worldgen::facet::{aggregate_children, stitch_transition};

// Positions/directions are unit-sphere values; lengths are metres. Material
// channels are f32, so their tolerance is separate from the f64 field budget.
const GEOMETRY_TOLERANCE: f64 = 1.0e-12;
const LENGTH_TOLERANCE_M: f64 = 1.0e-7;
const WEIGHT_TOLERANCE: f64 = 1.0e-6;

fn separation(a: [f64; 3], b: [f64; 3]) -> f64 {
    a.into_iter()
        .zip(b)
        .map(|(a, b)| (a - b).powi(2))
        .sum::<f64>()
        .sqrt()
}

fn assert_fields(a: &FacetFieldSample, b: &FacetFieldSample) {
    for (a, b) in [
        (a.position, b.position),
        (a.normal, b.normal),
        (a.flow_direction, b.flow_direction),
        (a.ridge_direction, b.ridge_direction),
    ] {
        assert!(
            separation(a, b) <= GEOMETRY_TOLERANCE,
            "vectors differ: {a:?} / {b:?}"
        );
    }
    for (a, b) in [
        (a.height_m, b.height_m),
        (a.shoreline_distance_m, b.shoreline_distance_m),
        (a.water_depth_m, b.water_depth_m),
        (a.channel_distance_m, b.channel_distance_m),
        (a.channel_width_m, b.channel_width_m),
    ] {
        assert!(
            (a - b).abs() <= LENGTH_TOLERANCE_M,
            "lengths differ: {a} / {b}"
        );
    }
    assert!((a.flow_strength - b.flow_strength).abs() <= GEOMETRY_TOLERANCE);
    for (a, b) in a
        .material_weights
        .into_iter()
        .zip(b.material_weights)
        .chain([
            (a.floodplain_weight, b.floodplain_weight),
            (a.bank_weight, b.bank_weight),
            (a.terrace_weight, b.terrace_weight),
            (a.delta_weight, b.delta_weight),
            (a.ridge_strength, b.ridge_strength),
        ])
    {
        assert!((f64::from(a) - f64::from(b)).abs() <= WEIGHT_TOLERANCE);
    }
    assert!(a.material_weights.iter().all(|w| (0.0..=1.0).contains(w)));
    assert!((a.material_weights.iter().sum::<f32>() - 1.0).abs() < WEIGHT_TOLERANCE as f32);
}

fn child_addresses(address: &FacetAddress) -> Vec<FacetAddress> {
    (0..4)
        .map(|digit| {
            let mut path = address.child_path.clone();
            path.push(digit);
            FacetAddress::new(address.macro_face.clone(), path).unwrap()
        })
        .collect()
}

fn shared_edge(a: &FacetAddress, b: &FacetAddress) -> Option<(usize, usize)> {
    (0..4).find_map(|ea| {
        (0..4).find_map(|eb| {
            let endpoints = [
                canonical_corner_sample(a, ea),
                canonical_corner_sample(a, (ea + 1) % 4),
            ];
            let other = [
                canonical_corner_sample(b, eb),
                canonical_corner_sample(b, (eb + 1) % 4),
            ];
            endpoints
                .iter()
                .all(|p| other.contains(p))
                .then_some((ea as usize, eb as usize))
        })
    })
}

fn macro_center_height(
    terrain: &hornvale_terrain::GeneratedTerrain,
    address: &FacetAddress,
) -> f64 {
    let nearest = NearestVertexIndex::new(terrain.geosphere());
    address
        .macro_face
        .corners()
        .into_iter()
        .map(|position| {
            let vertex = nearest.nearest_to_position(terrain.geosphere(), position);
            terrain.elevation_at(vertex).get()
        })
        .sum::<f64>()
        / 4.0
}

#[test]
fn emitted_channel_interior_follows_the_inherited_downstream_grade() {
    let source = world(42);
    #[allow(clippy::disallowed_methods)]
    let terrain = hornvale_worldgen::terrain_of(&source).unwrap();
    let context = SurfaceRealizationContext::build(&source).unwrap();
    let mut checked = 0;
    for (line, vertices) in terrain.channels().run_vertices.iter().enumerate() {
        for (j, pair) in vertices.windows(2).enumerate() {
            let heights = [
                terrain.elevation_at(pair[0]).get(),
                terrain.elevation_at(pair[1]).get(),
            ];
            if heights[0] - heights[1] < 20.0 {
                continue;
            }
            let points = &terrain.channels().polylines[line].points[j..=j + 1];
            let mut previous = f64::INFINITY;
            for t in [0.25, 0.5, 0.75] {
                let p: [f64; 3] =
                    std::array::from_fn(|i| points[0][i] * (1.0 - t) + points[1][i] * t);
                let norm = p.iter().map(|v| v * v).sum::<f64>().sqrt();
                let p = p.map(|v| v / norm);
                if terrain.channels().bank_reading(p).unwrap().line != line {
                    continue;
                }
                let fine = Facet::containing(p, 20);
                let address = FacetAddress::new(
                    Facet {
                        face: fine.face,
                        path: fine.path[..6].to_vec(),
                    },
                    fine.path[6..].to_vec(),
                )
                .unwrap();
                let patch = context.realize(&address).unwrap();
                let sample = patch
                    .samples
                    .iter()
                    .min_by(|a, b| separation(a.position, p).total_cmp(&separation(b.position, p)))
                    .unwrap();
                let expected = heights[0] + t * (heights[1] - heights[0]) - 12.0;
                assert!(
                    (sample.height_m - expected).abs() < 1.0,
                    "emitted bed missed inherited interior grade: actual={} expected={expected}",
                    sample.height_m
                );
                assert!(sample.height_m <= previous, "emitted bed rises downstream");
                previous = sample.height_m;
                checked += 1;
            }
            if checked >= 6 {
                return;
            }
        }
    }
    panic!("fixture must exercise six interior channel samples, got {checked}");
}

#[test]
fn emitted_channel_endpoints_are_incised_below_the_inherited_bed() {
    let source = world(42);
    #[allow(clippy::disallowed_methods)]
    let terrain = hornvale_worldgen::terrain_of(&source).unwrap();
    let context = SurfaceRealizationContext::build(&source).unwrap();
    let mut checked = 0;
    for (line, vertices) in terrain.channels().run_vertices.iter().enumerate() {
        let point = terrain.channels().polylines[line].points[0];
        let address = FacetAddress::new(Facet::containing(point, 6), vec![]).unwrap();
        let patch = context.realize(&address).unwrap();
        let Some(sample) = patch
            .samples
            .iter()
            .find(|s| separation(s.position, point) < GEOMETRY_TOLERANCE)
        else {
            continue;
        };
        assert!(
            sample.height_m < terrain.elevation_at(vertices[0]).get() - 1.0,
            "emitted channel head has no incision: height={} inherited={}",
            sample.height_m,
            terrain.elevation_at(vertices[0]).get()
        );
        checked += 1;
        if checked == 1 {
            break;
        }
    }
    assert_eq!(checked, 1, "fixture must exercise an emitted channel head");
}

#[test]
fn emitted_patch_centers_include_bounded_conditioned_relief() {
    let source = world(42);
    #[allow(clippy::disallowed_methods)]
    let terrain = hornvale_worldgen::terrain_of(&source).expect("terrain builds");
    let context = SurfaceRealizationContext::build(&source).expect("context builds");
    let mut active = 0;
    let mut checked = 0;

    for face in 0..6 {
        for digit in 0..4 {
            let address = address(face, digit);
            let patch = context.realize(&address).expect("patch realizes");
            let sample = patch.samples[4];
            // Hydrology has its own displacement budget. Rills now shape
            // emitted height too, so these are not ambient-relief samples.
            if sample.channel_distance_m.abs() <= sample.channel_width_m / 2.0
                || sample.bank_weight > 0.0
                || sample.floodplain_weight > 0.0
                || sample.terrace_weight > 0.0
            {
                continue;
            }
            checked += 1;
            let relief = sample.height_m - macro_center_height(&terrain, &address);
            assert!(
                relief.abs() <= 160.0,
                "facet relief exceeded its configured 160 m bound: {relief} m at {address:?}"
            );
            active += usize::from(relief.abs() > LENGTH_TOLERANCE_M);
        }
    }

    assert!(
        checked > 0,
        "fixture must exercise centers outside hydrology"
    );
    assert!(active > 0, "all dry patch centers remained macro-only");
}

#[test]
fn adjacent_patches_agree_on_fields() {
    let context = SurfaceRealizationContext::build(&world(42)).unwrap();
    // All four edges, both ordinary macro boundaries and rotated cube seams.
    for face in 0..6 {
        for digit in [0, 2] {
            let a = FacetAddress::new(
                Facet {
                    face,
                    path: vec![digit; 6],
                },
                vec![],
            )
            .unwrap();
            let patch = context.realize(&a).unwrap();
            for neighbor in a.macro_face.neighbors() {
                let b = FacetAddress::new(neighbor, vec![]).unwrap();
                let Some((ea, eb)) = shared_edge(&a, &b) else {
                    continue;
                };
                let peer = context.realize(&b).unwrap();
                assert_eq!(
                    canonical_edge_sample(&a, ea as u8, 0.5),
                    canonical_edge_sample(&b, eb as u8, 0.5)
                );
                // The derived ring carries the exact child-corner split, which
                // need not be the midpoint of a normalized chord.
                assert_eq!(patch.samples.len(), 9, "missing derived border ring");
                for i in [ea, (ea + 1) % 4, 5 + ea] {
                    let sample = &patch.samples[i];
                    let other = peer
                        .samples
                        .iter()
                        .find(|s| s.position == sample.position)
                        .unwrap();
                    assert_fields(sample, other);
                }
            }
        }
    }
}

#[test]
fn face_corner_has_one_value() {
    let context = SurfaceRealizationContext::build(&world(42)).unwrap();
    let a = address(0, 0);
    let target = canonical_corner_sample(&a, 0);
    let mut patches = vec![context.realize(&a).unwrap()];
    for neighbor in a.macro_face.neighbors() {
        if neighbor.corners().contains(&target) {
            patches.push(
                context
                    .realize(&FacetAddress::new(neighbor, vec![]).unwrap())
                    .unwrap(),
            );
        }
    }
    assert_eq!(patches.len(), 3, "three cube faces meet here");
    for patch in &patches {
        let corner = patch.samples.iter().find(|s| s.position == target).unwrap();
        assert_fields(&patches[0].samples[0], corner);
    }
}

#[test]
fn children_preserve_parent_feature_ids() {
    let source = world(42);
    // Named fixture construction (decision 0092): one retained terrain is
    // needed to select a real channel before checking refinement identities.
    #[allow(clippy::disallowed_methods)]
    let terrain = hornvale_worldgen::terrain_of(&source).unwrap();
    let context = SurfaceRealizationContext::build(&source).unwrap();
    let a = FacetAddress::new(
        Facet::containing(terrain.channels().polylines[0].points[0], 6),
        vec![],
    )
    .unwrap();
    let parent = context.realize(&a).unwrap();
    let ids: std::collections::BTreeSet<_> = parent.curves.iter().map(|c| c.feature).collect();
    assert!(!ids.is_empty());
    let children: Vec<_> = child_addresses(&a)
        .iter()
        .map(|a| context.realize(a).unwrap())
        .collect();
    let child_ids = children
        .iter()
        .flat_map(|p| p.curves.iter().map(|c| c.feature))
        .collect();
    assert_eq!(ids, child_ids);
    let parent_strip_ids: std::collections::BTreeSet<_> =
        parent.strips.iter().map(|strip| strip.feature).collect();
    let child_strip_ids = children
        .iter()
        .flat_map(|patch| patch.strips.iter().map(|strip| strip.feature))
        .collect();
    // A footprint can intersect the patch with only its bank. Such a strip
    // keeps its source ID and terminals, but has no patch-local centerline
    // curve. Refinement must preserve each set independently.
    for id in ids.difference(&parent_strip_ids).take(3) {
        let curve = parent.curves.iter().find(|c| c.feature == *id).unwrap();
        eprintln!(
            "centerline without strip: {id:?}, points={:?}, width={:?}",
            curve.points, curve.width
        );
    }
    assert!(
        parent_strip_ids.difference(&ids).next().is_some(),
        "fixture must exercise a bank-only strip"
    );
    assert!(
        parent_strip_ids == child_strip_ids,
        "strip refinement lost {:?}, added {:?}",
        parent_strip_ids
            .difference(&child_strip_ids)
            .take(3)
            .collect::<Vec<_>>(),
        child_strip_ids
            .difference(&parent_strip_ids)
            .take(3)
            .collect::<Vec<_>>()
    );
    let mut continuations = 0;
    for patch in std::iter::once(&parent).chain(&children) {
        for strip in &patch.strips {
            assert!(parent_strip_ids.contains(&strip.feature));
            assert!(strip.endpoints.iter().all(|e| e.feature == strip.feature));
            let pieces: Vec<_> = patch
                .curves
                .iter()
                .filter(|c| c.feature == strip.feature)
                .collect();
            if let (Some(first), Some(last)) = (pieces.first(), pieces.last()) {
                assert_eq!(
                    strip.endpoints,
                    [first.endpoints[0].clone(), last.endpoints[1].clone()]
                );
                continuations += strip
                    .endpoints
                    .iter()
                    .filter(|e| e.terminal == TerminalKind::Continuation && e.boundary.is_some())
                    .count();
            } else {
                // Bank-only geometry must not invent a centerline crossing.
                assert!(strip.endpoints.iter().all(|e| e.boundary.is_none()));
                assert!(
                    strip
                        .endpoints
                        .iter()
                        .all(|e| e.terminal != TerminalKind::Continuation)
                );
            }
        }
    }
    assert!(
        continuations > 0,
        "retain parent/child continuation coverage"
    );
}

/// claim: invariant(seed-42 mixed-LOD fields and topology agree across all six cube faces)
#[test]
fn unequal_lod_transition_has_no_gap() {
    let context = SurfaceRealizationContext::build(&world(42)).unwrap();
    for face in 0..6 {
        let a = FacetAddress::new(
            Facet {
                face,
                path: vec![0; 6],
            },
            vec![],
        )
        .unwrap();
        let coarse = context.realize(&a).unwrap();
        for neighbor in a.macro_face.neighbors() {
            let b = FacetAddress::new(neighbor, vec![]).unwrap();
            let Some((edge, _)) = shared_edge(&a, &b) else {
                continue;
            };
            let midpoint = coarse.samples.get(5 + edge).expect("coarse border ring");
            let mut matched = 0;
            let mut topology = None;
            for child in child_addresses(&b) {
                let fine = context.realize(&child).unwrap();
                let seam = (0..4).find(|&i| {
                    let ends = [fine.samples[i].position, fine.samples[(i + 1) % 4].position];
                    ends.contains(&midpoint.position)
                        && ends.iter().any(|p| {
                            *p == coarse.samples[edge].position
                                || *p == coarse.samples[(edge + 1) % 4].position
                        })
                });
                let Some(fine_edge) = seam else {
                    continue;
                };
                matched += 1;
                let original = (coarse.clone(), fine.clone());
                let triangles = stitch_transition(&coarse, &fine).unwrap();
                assert_eq!((&coarse, &fine), (&original.0, &original.1));
                assert_eq!(triangles.len(), 5, "replace one fan triangle with two");
                if let Some(previous) = &topology {
                    assert_eq!(previous, &triangles);
                }
                topology = Some(triangles.clone());
                // Each fine boundary segment must be an actual coarse mesh
                // edge, with identical displaced endpoints (not a T-junction).
                let ends = [fine.samples[fine_edge], fine.samples[(fine_edge + 1) % 4]];
                let indices = ends.map(|s| {
                    coarse
                        .samples
                        .iter()
                        .position(|c| c.position == s.position)
                        .unwrap() as u32
                });
                let mut uses = 0;
                for triangle in &triangles {
                    assert!(
                        triangle
                            .iter()
                            .all(|&i| (i as usize) < coarse.samples.len())
                    );
                    assert!(
                        triangle[0] != triangle[1]
                            && triangle[1] != triangle[2]
                            && triangle[0] != triangle[2]
                    );
                    for i in 0..3 {
                        let pair = [triangle[i], triangle[(i + 1) % 3]];
                        uses +=
                            usize::from(pair.contains(&indices[0]) && pair.contains(&indices[1]));
                    }
                }
                assert_eq!(uses, 1, "fine segment must occur once on coarse boundary");
                for (sample, index) in ends.iter().zip(indices) {
                    assert_fields(sample, &coarse.samples[index as usize]);
                }
                // Every original outer edge except the split survives once.
                for other in (0..4).filter(|&other| other != edge) {
                    assert!(triangles.contains(&[other as u32, ((other + 1) % 4) as u32, 4]));
                }
            }
            assert_eq!(matched, 2, "both fine halves must be covered");
        }
    }
}

#[test]
fn unequal_lod_transition_rejects_invalid_neighbors() {
    let context = SurfaceRealizationContext::build(&world(42)).unwrap();
    let a = address(0, 0);
    let coarse = context.realize(&a).unwrap();
    assert!(stitch_transition(&coarse, &coarse).is_err());
    for child in child_addresses(&a) {
        assert!(
            stitch_transition(&coarse, &context.realize(&child).unwrap()).is_err(),
            "an overlapping child is not a neighbor"
        );
    }
    let b = FacetAddress::new(a.macro_face.neighbors()[0].clone(), vec![]).unwrap();
    let fine = child_addresses(&b)
        .iter()
        .map(|a| context.realize(a).unwrap())
        .find(|p| stitch_transition(&coarse, p).is_ok())
        .unwrap();
    assert!(stitch_transition(&fine, &coarse).is_err());
    let mut stale = fine.clone();
    stale.revision.source_revision.push_str("-stale");
    assert!(matches!(
        stitch_transition(&coarse, &stale),
        Err(SurfaceBuildError::RevisionMismatch(_))
    ));
    let mut malformed = fine.clone();
    malformed.address.child_path.push(4);
    assert!(stitch_transition(&coarse, &malformed).is_err());
    let mut missing = coarse.clone();
    missing.samples.clear();
    assert!(stitch_transition(&missing, &fine).is_err());
    let remote = context
        .realize(&FacetAddress::new(address(1, 0).macro_face, vec![0]).unwrap())
        .unwrap();
    assert!(stitch_transition(&coarse, &remote).is_err());
}

#[test]
fn unequal_lod_stitching_multiple_edges_is_order_independent() {
    let context = SurfaceRealizationContext::build(&world(42)).unwrap();
    let a = FacetAddress::new(address(0, 0).macro_face, vec![3, 0]).unwrap();
    let mut resolved = a.macro_face.clone();
    resolved.path.extend_from_slice(&a.child_path);
    let coarse = context.realize(&a).unwrap();
    let mut fine = Vec::new();
    for neighbor in resolved.neighbors() {
        let macro_face = Facet {
            face: neighbor.face,
            path: neighbor.path[..6].to_vec(),
        };
        let parent = FacetAddress::new(macro_face, neighbor.path[6..].to_vec()).unwrap();
        for child in child_addresses(&parent) {
            let patch = context.realize(&child).unwrap();
            if stitch_transition(&coarse, &patch).is_ok() {
                fine.push(patch);
            }
        }
    }
    assert_eq!(fine.len(), 8, "four edges, each with two fine neighbors");
    let mut forward = coarse.clone();
    for patch in &fine {
        forward.triangles = stitch_transition(&forward, patch).unwrap();
    }
    let mut reverse = coarse.clone();
    for patch in fine.iter().rev() {
        reverse.triangles = stitch_transition(&reverse, patch).unwrap();
    }
    assert_eq!(forward.triangles.len(), 8);
    assert_eq!(forward, reverse);
    assert_eq!(forward.curves, coarse.curves);
    assert_eq!(forward.samples, coarse.samples);
}

#[test]
fn refine_then_coarsen_preserves_parent_samples() {
    let context = SurfaceRealizationContext::build(&world(42)).unwrap();
    for path in [vec![], vec![2], vec![1, 3]] {
        let a = FacetAddress::new(address(0, 0).macro_face, path).unwrap();
        let parent = context.realize(&a).unwrap();
        let children: Vec<_> = child_addresses(&a)
            .iter()
            .map(|a| context.realize(a).unwrap())
            .collect();
        let aggregate = aggregate_children(&parent, &children);
        for (a, b) in parent.samples.iter().zip(&aggregate.samples) {
            assert_fields(a, b);
        }
        assert_eq!(aggregate.samples.len(), parent.samples.len());
        assert_eq!(aggregate.curves, parent.curves);
        assert_eq!(aggregate.triangles, parent.triangles);
        assert_eq!(aggregate.revision, parent.revision);
        let mut reverse = children.clone();
        reverse.reverse();
        assert_eq!(
            canonical_patch_bytes(&aggregate),
            canonical_patch_bytes(&aggregate_children(&parent, &reverse))
        );
        // Recover from child values: cloning the supplied parent is not aggregation.
        let mut stale_samples = parent.clone();
        for sample in &mut stale_samples.samples {
            sample.height_m += 100.0;
            sample.flow_strength = -1.0;
        }
        assert_eq!(aggregate, aggregate_children(&stale_samples, &children));
        // Four different observations at the common center must contribute,
        // rather than restriction selecting one child by accident.
        let mut varied = children.clone();
        for (digit, child) in varied.iter_mut().enumerate() {
            child
                .samples
                .iter_mut()
                .find(|s| s.position == parent.samples[4].position)
                .unwrap()
                .height_m += 4.0 * digit as f64;
        }
        let averaged = aggregate_children(&parent, &varied);
        assert!(
            (averaged.samples[4].height_m - parent.samples[4].height_m - 6.0).abs()
                < LENGTH_TOLERANCE_M
        );
        varied.reverse();
        assert_eq!(averaged, aggregate_children(&parent, &varied));
    }
}

#[test]
#[should_panic(expected = "four distinct immediate children")]
fn refine_coarsen_rejects_duplicate_children() {
    let context = SurfaceRealizationContext::build(&world(42)).unwrap();
    let parent = context.realize(&address(0, 0)).unwrap();
    let child = context
        .realize(&child_addresses(&parent.address)[0])
        .unwrap();
    aggregate_children(&parent, &vec![child; 4]);
}

#[test]
#[should_panic(expected = "aggregation requires one surface revision")]
fn refine_coarsen_rejects_stale_children() {
    let context = SurfaceRealizationContext::build(&world(42)).unwrap();
    let parent = context.realize(&address(0, 0)).unwrap();
    let mut children: Vec<_> = child_addresses(&parent.address)
        .iter()
        .map(|a| context.realize(a).unwrap())
        .collect();
    children[0].revision.source_revision.push_str("-stale");
    aggregate_children(&parent, &children);
}

fn world(seed: u64) -> World {
    let components = WorldComponents::assemble().expect("canonical components assemble");
    build_world_to(
        Seed(seed),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &components,
        BuildDepth::Astronomy,
    )
    .expect("fixture world builds")
}

fn address(face: u8, digit: u8) -> FacetAddress {
    let mut path = vec![0; hornvale_terrain::GLOBE_LEVEL as usize];
    path[hornvale_terrain::GLOBE_LEVEL as usize - 1] = digit;
    FacetAddress::new(Facet { face, path }, Vec::new()).expect("fixture address is valid")
}

fn push_len(bytes: &mut Vec<u8>, len: usize) {
    bytes.extend_from_slice(&(len as u64).to_le_bytes());
}

fn push_str(bytes: &mut Vec<u8>, value: &str) {
    push_len(bytes, value.len());
    bytes.extend_from_slice(value.as_bytes());
}

fn push_address(bytes: &mut Vec<u8>, address: &FacetAddress) {
    bytes.extend_from_slice(
        &address
            .macro_face
            .pack()
            .expect("a realized patch has a valid macro face")
            .0
            .to_le_bytes(),
    );
    push_len(bytes, address.child_path.len());
    bytes.extend_from_slice(&address.child_path);
}

fn push_f64(bytes: &mut Vec<u8>, value: f64) {
    bytes.extend_from_slice(&value.to_bits().to_le_bytes());
}

fn push_f32(bytes: &mut Vec<u8>, value: f32) {
    bytes.extend_from_slice(&value.to_bits().to_le_bytes());
}

fn feature_kind(kind: FeatureKind) -> u8 {
    match kind {
        FeatureKind::ChannelReach => 0,
        FeatureKind::Confluence => 1,
        FeatureKind::Shoreline => 2,
        FeatureKind::Ridge => 3,
        FeatureKind::MaterialTransition => 4,
    }
}

fn endpoint_side(side: EndpointSide) -> u8 {
    match side {
        EndpointSide::Upstream => 0,
        EndpointSide::Downstream => 1,
    }
}

fn terminal_kind(terminal: TerminalKind) -> u8 {
    match terminal {
        TerminalKind::Headwater => 0,
        TerminalKind::Confluence => 1,
        TerminalKind::Lake => 2,
        TerminalKind::Ocean => 3,
        TerminalKind::Continuation => 4,
    }
}

/// Test-only canonical encoding: every scalar is fixed-width little-endian,
/// every sequence is length-prefixed, and enum values are mapped explicitly.
fn canonical_patch_bytes(patch: &SurfacePatch) -> Vec<u8> {
    let mut bytes = Vec::new();
    push_str(&mut bytes, &patch.revision.source_revision);
    push_str(&mut bytes, patch.revision.algorithm_version);
    bytes.extend_from_slice(&patch.revision.configuration_hash);
    push_address(&mut bytes, &patch.address);

    push_len(&mut bytes, patch.samples.len());
    for sample in &patch.samples {
        for value in sample.position {
            push_f64(&mut bytes, value);
        }
        push_f64(&mut bytes, sample.height_m);
        for value in sample.normal {
            push_f64(&mut bytes, value);
        }
        for value in sample.material_weights {
            push_f32(&mut bytes, value);
        }
        for value in [
            sample.shoreline_distance_m,
            sample.water_depth_m,
            sample.flow_strength,
            sample.channel_distance_m,
            sample.channel_width_m,
        ] {
            push_f64(&mut bytes, value);
        }
        for value in sample.flow_direction {
            push_f64(&mut bytes, value);
        }
        for value in [
            sample.floodplain_weight,
            sample.bank_weight,
            sample.terrace_weight,
            sample.delta_weight,
        ] {
            push_f32(&mut bytes, value);
        }
        for value in sample.ridge_direction {
            push_f64(&mut bytes, value);
        }
        push_f32(&mut bytes, sample.ridge_strength);
    }

    push_len(&mut bytes, patch.curves.len());
    for curve in &patch.curves {
        bytes.push(feature_kind(curve.feature.kind));
        bytes.extend_from_slice(&curve.feature.macro_anchor.0.to_le_bytes());
        bytes.extend_from_slice(&curve.feature.ordinal.to_le_bytes());
        push_len(&mut bytes, curve.points.len());
        for point in &curve.points {
            for value in point {
                push_f64(&mut bytes, *value);
            }
        }
        push_len(&mut bytes, curve.width.len());
        for &width in &curve.width {
            push_f64(&mut bytes, width);
        }
        for endpoint in &curve.endpoints {
            bytes.push(feature_kind(endpoint.feature.kind));
            bytes.extend_from_slice(&endpoint.feature.macro_anchor.0.to_le_bytes());
            bytes.extend_from_slice(&endpoint.feature.ordinal.to_le_bytes());
            bytes.push(endpoint_side(endpoint.side));
            bytes.push(terminal_kind(endpoint.terminal));
            if let Some(boundary) = &endpoint.boundary {
                bytes.push(1);
                push_address(&mut bytes, &boundary.address);
                bytes.push(boundary.edge);
                push_f64(&mut bytes, boundary.t);
            } else {
                bytes.push(0);
            }
        }
    }

    push_len(&mut bytes, patch.triangles.len());
    for triangle in &patch.triangles {
        for index in triangle {
            bytes.extend_from_slice(&index.to_le_bytes());
        }
    }
    bytes
}

#[test]
fn same_world_has_same_surface_revision() {
    let world = world(42);

    let first = SurfaceRealizationContext::build(&world).expect("first context builds");
    let second = SurfaceRealizationContext::build(&world).expect("second context builds");

    assert_eq!(first.revision, second.revision);
}

#[test]
fn patch_bytes_do_not_depend_on_request_order() {
    let world = world(42);
    let a_then_b = SurfaceRealizationContext::build(&world).expect("A-then-B context builds");
    let b_then_a = SurfaceRealizationContext::build(&world).expect("B-then-A context builds");
    let a = address(0, 0);
    let b = address(1, 3);

    let a_first = canonical_patch_bytes(&a_then_b.realize(&a).expect("A realizes first"));
    let b_second = canonical_patch_bytes(&a_then_b.realize(&b).expect("B realizes second"));
    let b_first = canonical_patch_bytes(&b_then_a.realize(&b).expect("B realizes first"));
    let a_second = canonical_patch_bytes(&b_then_a.realize(&a).expect("A realizes second"));

    assert_eq!(a_first, a_second);
    assert_eq!(b_first, b_second);
}

#[test]
fn patch_bytes_do_not_depend_on_cache_eviction() {
    let world = world(42);
    let address = address(2, 1);
    let before = {
        let context = SurfaceRealizationContext::build(&world).expect("first context builds");
        canonical_patch_bytes(
            &context
                .realize(&address)
                .expect("patch realizes before eviction"),
        )
    };

    let after = {
        let context = SurfaceRealizationContext::build(&world).expect("rebuilt context builds");
        canonical_patch_bytes(
            &context
                .realize(&address)
                .expect("patch realizes after eviction"),
        )
    };

    assert_eq!(before, after);
}

#[test]
fn revision_rejects_wrong_address_context() {
    let mut context = SurfaceRealizationContext::build(&world(42)).expect("context builds");
    let other = SurfaceRealizationContext::build(&world(43)).expect("other context builds");
    context.revision = other.revision;

    let error = context
        .realize(&address(3, 2))
        .expect_err("a revision from another macro context must be rejected");

    assert!(matches!(error, SurfaceBuildError::RevisionMismatch(_)));
}
