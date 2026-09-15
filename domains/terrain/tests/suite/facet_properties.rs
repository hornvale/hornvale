use hornvale_kernel::{Facet, Vertex};
use hornvale_terrain::{
    EndpointSide, FacetAddress, FeatureEndpoint, FeatureId, FeatureKind, FeatureStripSampling,
    RealizedCurve, TerminalKind, adaptive_feature_strips, canonical_corner_sample,
    canonical_edge_sample, feature_sample,
};

use hornvale_kernel::{Geosphere, Seed};
use hornvale_terrain::{
    GeneratedTerrain, TerrainFacetInputs, TerrainPins, WaterKind, bed_height_profile,
    channel_endpoint_kind, generate, realize_channel_curves,
};

fn terrain() -> GeneratedTerrain {
    let geo = Geosphere::new(hornvale_terrain::GLOBE_LEVEL);
    let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
    GeneratedTerrain::new(geo, outcome)
}

fn inputs(terrain: &GeneratedTerrain) -> TerrainFacetInputs<'_> {
    TerrainFacetInputs {
        globe: terrain.globe(),
        geo: terrain.geosphere(),
        channels: terrain.channels(),
    }
}

fn dot3(a: [f64; 3], b: [f64; 3]) -> f64 {
    a.into_iter().zip(b).map(|(a, b)| a * b).sum()
}

fn cross3(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}

fn contains(facet: &Facet, point: [f64; 3]) -> bool {
    let corners = facet.corners();
    (0..4).all(|i| {
        let normal = cross3(corners[i], corners[(i + 1) % 4]);
        dot3(normal, point) * dot3(normal, facet.centroid()) >= -1.0e-16
    })
}

fn address_at(point: [f64; 3]) -> FacetAddress {
    let mut facet = (0..6)
        .map(|face| Facet { face, path: vec![] })
        .find(|facet| contains(facet, point))
        .unwrap();
    for _ in 0..hornvale_terrain::GLOBE_LEVEL {
        facet = (0..4)
            .map(|child| facet.child(child).unwrap())
            .find(|facet| contains(facet, point))
            .unwrap();
    }
    FacetAddress::new(facet, vec![]).unwrap()
}

fn separation(a: [f64; 3], b: [f64; 3]) -> f64 {
    a.into_iter()
        .zip(b)
        .map(|(a, b)| (a - b).powi(2))
        .sum::<f64>()
        .sqrt()
}

#[test]
fn headwaters_have_sources_and_inherit_the_rendered_trunk() {
    let terrain = terrain();
    let net = terrain.channels();
    // A boundary-only touch need not produce a curve. Select an interior head.
    let line = net
        .polylines
        .iter()
        .position(|line| {
            let point = line.points[0];
            let corners = address_at(point).macro_face.corners();
            (0..4).all(|i| dot3(cross3(corners[i], corners[(i + 1) % 4]), point).abs() > 1.0e-10)
        })
        .unwrap();
    let point = net.polylines[line].points[0];
    let address = address_at(point);
    let curves = realize_channel_curves(inputs(&terrain), &address);
    assert!(!curves.is_empty());
    let head = curves
        .iter()
        .find(|c| c.feature.ordinal == 0 && c.feature.macro_anchor == net.run_vertices[line][0])
        .unwrap();
    assert_eq!(head.endpoints[0].terminal, TerminalKind::Headwater);
    assert_eq!(head.points[0], point);
    for curve in &curves {
        assert_eq!(curve.endpoints[0].side, EndpointSide::Upstream);
        assert_eq!(curve.endpoints[0].feature, curve.feature);
        assert_eq!(curve.points.len(), curve.width.len());
        assert!(curve.width.iter().all(|w| w.is_finite() && *w > 0.0));
        assert!(
            curve
                .points
                .iter()
                .all(|p| contains(&address.macro_face, *p))
        );
    }
}

#[test]
fn confluences_name_the_inherited_downstream_feature_and_attach() {
    let terrain = terrain();
    let net = terrain.channels();
    let (line, target) = net
        .run_vertices
        .iter()
        .enumerate()
        .find_map(|(i, run)| {
            net.trunk_vertex(*run.last().unwrap())
                .filter(|(owner, _)| *owner != i)
                .map(|owner| (i, owner))
        })
        .unwrap();
    let mouth = *net.polylines[line].points.last().unwrap();
    let curves = realize_channel_curves(inputs(&terrain), &address_at(mouth));
    let tributary = curves
        .iter()
        .find(|c| c.feature.ordinal == 0 && c.feature.macro_anchor == net.run_vertices[line][0])
        .unwrap();
    assert_eq!(tributary.endpoints[1].terminal, TerminalKind::Confluence);
    let downstream = tributary.downstream_feature(inputs(&terrain)).unwrap();
    assert_eq!(
        downstream,
        FeatureId::new(FeatureKind::ChannelReach, net.run_vertices[target.0][0], 0)
    );
    assert!(
        separation(
            *tributary.points.last().unwrap(),
            net.polylines[target.0].points[target.1]
        ) < 1.0e-10
    );
    let mouth_bed = *bed_height_profile(tributary, &terrain).last().unwrap();
    assert!(
        (mouth_bed
            - terrain
                .elevation_at(net.run_vertices[target.0][target.1])
                .get())
        .abs()
            < 1.0e-8
    );

    let rills: Vec<_> = curves
        .iter()
        .filter(|c| c.feature.ordinal > 0 && c.endpoints[1].terminal == TerminalKind::Confluence)
        .take(16)
        .collect();
    assert!(!rills.is_empty(), "fixture must exercise sub-vertex mouths");
    for rill in rills {
        let parent_id = rill.downstream_feature(inputs(&terrain)).unwrap();
        let parent = curves
            .iter()
            .find(|c| c.feature == parent_id)
            .expect("parent at the mouth must be realized");
        let (distance, _) = feature_sample(parent, *rill.points.last().unwrap());
        assert!(
            distance.abs() < 1.0e-7,
            "rill mouth misses its parent: {distance}"
        );
        // Read the parent's bed at the join, not at its downstream endpoint.
        let mut parent_at_mouth = parent.clone();
        parent_at_mouth.points = vec![*rill.points.last().unwrap(); 2];
        let parent_height = bed_height_profile(&parent_at_mouth, &terrain)[0];
        let rill_height = *bed_height_profile(rill, &terrain).last().unwrap();
        assert!(
            (parent_height - rill_height).abs() < 1.0e-8,
            "bed step at {:?}: {parent_height} vs {rill_height}",
            rill.feature
        );
    }
}

#[test]
fn terminal_basins_and_ocean_outlets_are_not_confluences() {
    let terrain = terrain();
    let net = terrain.channels();
    for (water, terminal) in [
        (WaterKind::SaltBasin, TerminalKind::Lake),
        (WaterKind::Ocean, TerminalKind::Ocean),
    ] {
        let (line, run) = net
            .run_vertices
            .iter()
            .enumerate()
            .find(|(_, run)| *terrain.globe().water_kind.get(*run.last().unwrap()) == water)
            .unwrap();
        let vertex = *run.last().unwrap();
        assert_eq!(
            channel_endpoint_kind(inputs(&terrain), vertex, true),
            terminal
        );
        let mouth = *net.polylines[line].points.last().unwrap();
        let curves = realize_channel_curves(inputs(&terrain), &address_at(mouth));
        let curve = curves
            .iter()
            .find(|c| c.feature.macro_anchor == run[0] && c.feature.ordinal == 0)
            .unwrap();
        assert_eq!(curve.endpoints[1].terminal, terminal);
        assert!(curve.downstream_feature(inputs(&terrain)).is_none());
        assert_eq!(*curve.points.last().unwrap(), mouth);
        let bed = bed_height_profile(curve, &terrain);
        assert!((bed.last().unwrap() - terrain.elevation_at(vertex).get()).abs() < 1.0e-8);
        let sections = curve.cross_sections(inputs(&terrain));
        assert!(
            sections.last().unwrap().delta.is_some(),
            "standing-water mouth needs an apron"
        );
    }
}

#[test]
fn continuation_carries_a_canonical_boundary_and_survives_refinement() {
    let terrain = terrain();
    let point = terrain.channels().polylines[0].points[0];
    let address = address_at(point);
    let curves = realize_channel_curves(inputs(&terrain), &address);
    let exits: Vec<_> = curves
        .iter()
        .flat_map(|c| c.endpoints.iter().enumerate().map(move |(i, e)| (c, i, e)))
        .filter(|(_, _, e)| e.terminal == TerminalKind::Continuation)
        .collect();
    assert!(!exits.is_empty());
    for (curve, side, endpoint) in exits.iter().take(32) {
        let boundary = endpoint
            .boundary
            .as_ref()
            .expect("unloaded continuation needs a token");
        let position = if *side == 0 {
            curve.points[0]
        } else {
            *curve.points.last().unwrap()
        };
        assert!((0.0..=1.0).contains(&boundary.t));
        assert!(
            separation(
                position,
                canonical_edge_sample(&boundary.address, boundary.edge, boundary.t)
            ) < 1.0e-10
        );
    }
    let coarse_ids: std::collections::BTreeSet<_> = curves.iter().map(|c| c.feature).collect();
    let mut child_ids = std::collections::BTreeSet::new();
    for child in 0..4 {
        let child_address = FacetAddress::new(address.macro_face.clone(), vec![child]).unwrap();
        child_ids.extend(
            realize_channel_curves(inputs(&terrain), &child_address)
                .into_iter()
                .map(|c| c.feature),
        );
    }
    assert_eq!(coarse_ids, child_ids);
}

#[test]
fn continuation_matches_the_same_feature_in_the_neighbor() {
    let terrain = terrain();
    let address = address_at(terrain.channels().polylines[0].points[0]);
    let curves = realize_channel_curves(inputs(&terrain), &address);
    let (curve, end) = curves
        .iter()
        .find_map(|curve| {
            curve
                .endpoints
                .iter()
                .find(|e| e.terminal == TerminalKind::Continuation)
                .map(|end| (curve, end))
        })
        .unwrap();
    let boundary = end.boundary.as_ref().unwrap();
    let neighbor = address
        .macro_face
        .neighbors()
        .into_iter()
        .find(|neighbor| {
            shared_edge(&address.macro_face, neighbor)
                .is_some_and(|(edge, _)| edge == boundary.edge)
        })
        .unwrap();
    let neighbor = FacetAddress::new(neighbor, vec![]).unwrap();
    let peer_curves = realize_channel_curves(inputs(&terrain), &neighbor);
    let peer = peer_curves
        .iter()
        .filter(|c| c.feature == curve.feature)
        .flat_map(|c| &c.endpoints)
        .find(|e| {
            e.side != end.side
                && e.boundary.as_ref().is_some_and(|b| {
                    separation(
                        canonical_edge_sample(&b.address, b.edge, b.t),
                        canonical_edge_sample(&boundary.address, boundary.edge, boundary.t),
                    ) < 1.0e-10
                })
        })
        .expect("same feature must continue on the other side");
    assert_eq!(peer.terminal, TerminalKind::Continuation);
    assert!((peer.boundary.as_ref().unwrap().t - boundary.t).abs() < 1.0e-10);
}

#[test]
fn bed_profile_samples_interiors_and_descends_to_the_inherited_mouth() {
    let terrain = terrain();
    let net = terrain.channels();
    let curves = realize_channel_curves(inputs(&terrain), &address_at(net.polylines[0].points[0]));
    for curve in curves
        .iter()
        .filter(|c| c.feature.ordinal == 0)
        .chain(curves.iter().filter(|c| c.feature.ordinal > 0).take(16))
    {
        let profile = bed_height_profile(curve, &terrain);
        assert!(
            profile.len() > curve.points.len(),
            "must sample segment interiors"
        );
        assert!(profile.iter().all(|h| h.is_finite()));
        assert!(
            profile.windows(2).all(|h| h[1] <= h[0] + 1.0e-8),
            "uphill bed {:?}: {profile:?}",
            curve.feature
        );
        let geometry = curve.cross_sections(inputs(&terrain));
        assert_eq!(geometry.len(), curve.points.len());
        for (section, &center) in geometry.iter().zip(&curve.points) {
            assert!(separation(section.banks[0], center) > 0.0);
            assert!(
                separation(section.floodplain[0], center) >= separation(section.banks[0], center)
            );
            assert!(
                separation(section.terrace[0], center) >= separation(section.floodplain[0], center),
                "{:?}: terrace {} floodplain {}",
                curve.feature,
                separation(section.terrace[0], center),
                separation(section.floodplain[0], center)
            );
        }
    }
}

fn macro_facet(face: u8, child: u8) -> Facet {
    Facet {
        face,
        path: vec![child; hornvale_terrain::GLOBE_LEVEL as usize],
    }
}

fn edge_corners(facet: &Facet, edge: usize) -> [[f64; 3]; 2] {
    let corners = facet.corners();
    [corners[edge], corners[(edge + 1) % 4]]
}

fn shared_edge(a: &Facet, b: &Facet) -> Option<(u8, u8)> {
    for edge_a in 0..4 {
        let a_corners = edge_corners(a, edge_a);
        for edge_b in 0..4 {
            let b_corners = edge_corners(b, edge_b);
            if a_corners.iter().all(|corner| b_corners.contains(corner)) {
                return Some((edge_a as u8, edge_b as u8));
            }
        }
    }
    None
}

#[test]
fn child_path_digits_are_bounded() {
    let macro_face = macro_facet(0, 0);

    let address = FacetAddress::new(macro_face.clone(), vec![0, 1, 2, 3]).expect("valid path");
    assert_eq!(address.child_path, vec![0, 1, 2, 3]);
    assert!(FacetAddress::new(macro_face, vec![0, 4]).is_err());
    assert!(
        FacetAddress::new(
            Facet {
                face: 0,
                path: vec![0; 5],
            },
            Vec::new(),
        )
        .is_err()
    );
    assert!(
        FacetAddress::new(
            Facet {
                face: 0,
                path: vec![0; 7],
            },
            Vec::new(),
        )
        .is_err()
    );
}

#[test]
fn adjacent_faces_share_canonical_edge_samples() {
    let a = macro_facet(0, 2);
    let b = a.neighbors()[0].clone();
    assert_ne!(a.face, b.face, "fixture must cross a cube-face seam");
    let (edge_a, edge_b) = shared_edge(&a, &b).expect("facets share an edge");
    let a = FacetAddress::new(a, Vec::new()).expect("valid address");
    let b = FacetAddress::new(b, Vec::new()).expect("valid address");

    for t in [0.0, 0.125, 0.5, 0.875, 1.0] {
        assert_eq!(
            canonical_edge_sample(&a, edge_a, t),
            canonical_edge_sample(&b, edge_b, t),
            "shared edge moved at t={t}"
        );
    }
}

#[test]
fn refined_cross_face_children_share_canonical_edge_samples() {
    let a = macro_facet(0, 2);
    let b = a.neighbors()[0].clone();
    let mut pair = None;
    'search: for child_a in 0..4u8 {
        for child_b in 0..4u8 {
            if let Some(edges) = shared_edge(
                &a.child(child_a).expect("valid child"),
                &b.child(child_b).expect("valid child"),
            ) {
                pair = Some((child_a, child_b, edges));
                break 'search;
            }
        }
    }
    let (child_a, child_b, (edge_a, edge_b)) = pair.expect("children share an edge");
    let a = FacetAddress::new(a, vec![child_a]).expect("valid refined address");
    let b = FacetAddress::new(b, vec![child_b]).expect("valid refined address");

    for t in [0.25, 0.5, 0.75] {
        assert_eq!(
            canonical_edge_sample(&a, edge_a, t),
            canonical_edge_sample(&b, edge_b, t),
            "refined shared edge moved at t={t}"
        );
    }
}

#[test]
fn face_corners_are_order_independent() {
    let facet = macro_facet(0, 0);
    let target = facet.corners()[0];
    let mut representations = vec![(facet.clone(), 0u8)];
    for neighbor in facet.neighbors().into_iter().take(4) {
        for (corner, position) in neighbor.corners().into_iter().enumerate() {
            if position == target {
                representations.push((neighbor.clone(), corner as u8));
            }
        }
    }
    assert_eq!(
        representations.len(),
        3,
        "three cube faces must meet at the fixture corner"
    );

    representations.reverse();
    for (facet, corner) in representations {
        let address = FacetAddress::new(facet, Vec::new()).expect("valid address");
        assert_eq!(canonical_corner_sample(&address, corner), target);
    }
}

#[test]
fn refinement_changes_samples_but_not_feature_id() {
    let macro_face = macro_facet(0, 1);
    let coarse = FacetAddress::new(macro_face.clone(), Vec::new()).expect("valid address");
    let refined = FacetAddress::new(macro_face, vec![3]).expect("valid address");
    let feature = FeatureId::from_address(&coarse, FeatureKind::Ridge, Vertex(42), 7);
    let same_feature_after_refinement =
        FeatureId::from_address(&refined, FeatureKind::Ridge, Vertex(42), 7);

    assert_ne!(
        canonical_corner_sample(&coarse, 0),
        canonical_corner_sample(&refined, 0)
    );
    assert_eq!(feature, same_feature_after_refinement);
}

#[test]
fn strip_sampling_adapts_to_width_and_preserves_feature_endpoints() {
    let address = FacetAddress::new(macro_facet(0, 1), Vec::new()).unwrap();
    let points = [
        canonical_corner_sample(&address, 0),
        canonical_corner_sample(&address, 2),
    ];
    let make_curve = |ordinal, width| {
        let feature = FeatureId::new(FeatureKind::ChannelReach, Vertex(42), ordinal);
        RealizedCurve {
            feature,
            points: points.to_vec(),
            width: vec![width; 2],
            endpoints: [
                FeatureEndpoint {
                    feature,
                    side: EndpointSide::Upstream,
                    boundary: None,
                    terminal: TerminalKind::Headwater,
                },
                FeatureEndpoint {
                    feature,
                    side: EndpointSide::Downstream,
                    boundary: None,
                    terminal: TerminalKind::Ocean,
                },
            ],
        }
    };
    let curves = [make_curve(1, 1.0e-8), make_curve(2, 1.0)];
    let strips = adaptive_feature_strips(
        &curves,
        &address,
        FeatureStripSampling {
            patch_divisions: 8,
            width_step_multiplier: 8.0,
            curvature_gain: 1.0,
            max_subdivisions: 32,
        },
    );
    assert!(strips[0].centerline.len() > strips[1].centerline.len());
    for (strip, curve) in strips.iter().zip(curves) {
        for triangle in &strip.triangles {
            let [a, b, c] = triangle.map(|i| strip.vertices[i as usize].position);
            let ab: [f64; 3] = std::array::from_fn(|i| b[i] - a[i]);
            let ac: [f64; 3] = std::array::from_fn(|i| c[i] - a[i]);
            let normal = [
                ab[1] * ac[2] - ab[2] * ac[1],
                ab[2] * ac[0] - ab[0] * ac[2],
                ab[0] * ac[1] - ab[1] * ac[0],
            ];
            assert!(
                normal.iter().zip(a).map(|(n, p)| n * p).sum::<f64>() > 0.0,
                "ribbon triangle must face an observer outside the sphere: {triangle:?}"
            );
        }
        assert_eq!(strip.feature, curve.feature);
        assert_eq!(strip.endpoints, curve.endpoints);
        assert_eq!(strip.vertices.len(), strip.centerline.len() * 2);
        assert!(strip.vertices.chunks_exact(2).all(|pair| {
            pair[0].side == -1
                && pair[1].side == 1
                && pair[0].signed_distance_rad < 0.0
                && pair[1].signed_distance_rad > 0.0
        }));
    }
}

#[test]
fn curve_sampling_does_not_require_a_vertex_on_the_feature() {
    let feature = FeatureId::new(FeatureKind::ChannelReach, Vertex(11), 3);
    let curve = RealizedCurve {
        feature,
        points: vec![[1.0, 0.0, 0.0], [0.0, 1.0, 0.0]],
        width: vec![2.0, 6.0],
        endpoints: [
            FeatureEndpoint {
                feature,
                side: EndpointSide::Upstream,
                boundary: None,
                terminal: TerminalKind::Headwater,
            },
            FeatureEndpoint {
                feature,
                side: EndpointSide::Downstream,
                boundary: None,
                terminal: TerminalKind::Ocean,
            },
        ],
    };
    let length = (2.04_f64).sqrt();
    let query = [1.0 / length, 1.0 / length, 0.2 / length];
    assert!(!curve.points.contains(&query));

    let (signed_distance, width) = feature_sample(&curve, query);

    assert!(signed_distance > 0.0);
    assert!(signed_distance < 0.2);
    assert!((width - 4.0).abs() < 1.0e-12, "width was {width}");
}

#[test]
fn feature_sampling_selects_nearest_segment_and_first_tie() {
    let feature = FeatureId::new(FeatureKind::ChannelReach, Vertex(11), 3);
    let curve = RealizedCurve {
        feature,
        points: vec![[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]],
        width: vec![2.0, 7.0, 19.0],
        endpoints: [
            FeatureEndpoint {
                feature,
                side: EndpointSide::Upstream,
                boundary: None,
                terminal: TerminalKind::Headwater,
            },
            FeatureEndpoint {
                feature,
                side: EndpointSide::Downstream,
                boundary: None,
                terminal: TerminalKind::Ocean,
            },
        ],
    };
    let length = 2.0_f64.sqrt();
    let near_second_segment = [0.0, 1.0 / length, 1.0 / length];
    let (distance, width) = feature_sample(&curve, near_second_segment);
    assert!(distance.abs() < 1.0e-12);
    assert!((width - 13.0).abs() < 1.0e-12, "width was {width}");

    let (_, tie_width) = feature_sample(&curve, [0.0, 1.0, 0.0]);
    assert!(
        (tie_width - 7.0).abs() < 1.0e-12,
        "tie width was {tie_width}"
    );
}

#[test]
fn feature_sampling_preserves_signed_side_of_curve() {
    let feature = FeatureId::new(FeatureKind::ChannelReach, Vertex(11), 3);
    let curve = RealizedCurve {
        feature,
        points: vec![[1.0, 0.0, 0.0], [0.0, 1.0, 0.0]],
        width: vec![2.0, 7.0],
        endpoints: [
            FeatureEndpoint {
                feature,
                side: EndpointSide::Upstream,
                boundary: None,
                terminal: TerminalKind::Headwater,
            },
            FeatureEndpoint {
                feature,
                side: EndpointSide::Downstream,
                boundary: None,
                terminal: TerminalKind::Ocean,
            },
        ],
    };
    let length = (2.04_f64).sqrt();
    let positive = feature_sample(&curve, [1.0 / length, 1.0 / length, 0.2 / length]);
    let negative = feature_sample(&curve, [1.0 / length, 1.0 / length, -0.2 / length]);
    assert!(positive.0 > 0.0);
    assert!(negative.0 < 0.0);
    assert!((positive.0.abs() - negative.0.abs()).abs() < 1.0e-12);
}

#[test]
fn one_point_curve_sampling_returns_its_width() {
    let feature = FeatureId::new(FeatureKind::ChannelReach, Vertex(11), 3);
    let curve = RealizedCurve {
        feature,
        points: vec![[1.0, 0.0, 0.0]],
        width: vec![7.0],
        endpoints: [
            FeatureEndpoint {
                feature,
                side: EndpointSide::Upstream,
                boundary: None,
                terminal: TerminalKind::Headwater,
            },
            FeatureEndpoint {
                feature,
                side: EndpointSide::Downstream,
                boundary: None,
                terminal: TerminalKind::Ocean,
            },
        ],
    };
    let (distance, width) = feature_sample(&curve, [0.0, 1.0, 0.0]);
    assert!((distance - std::f64::consts::FRAC_PI_2).abs() < 1.0e-12);
    assert_eq!(width, 7.0);
}
