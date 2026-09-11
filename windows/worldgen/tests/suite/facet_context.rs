use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Facet, Seed, World};
use hornvale_terrain::{EndpointSide, FacetAddress, FeatureKind, TerminalKind, TerrainPins};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SurfaceBuildError, SurfacePatch, SurfaceRealizationContext,
    WorldComponents, build_world_to,
};

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
