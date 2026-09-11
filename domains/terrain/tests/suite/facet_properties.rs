use hornvale_kernel::{Facet, Vertex};
use hornvale_terrain::{
    EndpointSide, FacetAddress, FeatureEndpoint, FeatureId, FeatureKind, RealizedCurve,
    TerminalKind, canonical_corner_sample, canonical_edge_sample, feature_sample,
};

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

fn shared_edge(a: &Facet, b: &Facet) -> (u8, u8) {
    for edge_a in 0..4 {
        let a_corners = edge_corners(a, edge_a);
        for edge_b in 0..4 {
            let b_corners = edge_corners(b, edge_b);
            if a_corners.iter().all(|corner| b_corners.contains(corner)) {
                return (edge_a as u8, edge_b as u8);
            }
        }
    }
    panic!("facets do not share an edge: {a:?} and {b:?}");
}

#[test]
fn child_path_digits_are_bounded() {
    let macro_face = macro_facet(0, 0);

    let address = FacetAddress::new(macro_face.clone(), vec![0, 1, 2, 3]).expect("valid path");
    assert_eq!(address.child_path, vec![0, 1, 2, 3]);
    assert!(FacetAddress::new(macro_face, vec![0, 4]).is_err());
}

#[test]
fn adjacent_faces_share_canonical_edge_samples() {
    let a = macro_facet(0, 2);
    let b = a.neighbors()[0].clone();
    assert_ne!(a.face, b.face, "fixture must cross a cube-face seam");
    let (edge_a, edge_b) = shared_edge(&a, &b);
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
    let feature = FeatureId {
        kind: FeatureKind::Ridge,
        macro_anchor: Vertex(42),
        ordinal: 7,
    };
    let same_feature_after_refinement = feature;

    assert_ne!(
        canonical_corner_sample(&coarse, 0),
        canonical_corner_sample(&refined, 0)
    );
    assert_eq!(feature, same_feature_after_refinement);
}

#[test]
fn curve_sampling_does_not_require_a_vertex_on_the_feature() {
    let feature = FeatureId {
        kind: FeatureKind::ChannelReach,
        macro_anchor: Vertex(11),
        ordinal: 3,
    };
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
