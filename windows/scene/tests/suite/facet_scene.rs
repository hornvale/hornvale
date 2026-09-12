use hornvale_kernel::{Facet, quantize};
use hornvale_scene::{SceneContext, SurfacePatchQuery, surface_patch_json, surface_patch_scene};
use hornvale_terrain::{
    FacetAddress, FacetFieldSample, FeatureId, FeatureKind, RealizedCurve, TerminalKind,
    TerrainFacetInputs, WaterKind,
};
use hornvale_worldgen::facet::stitch_transition;
use hornvale_worldgen::{SurfacePatch, SurfaceRealizationContext, seed_42_world};
use serde_json::{Value, json};

fn address_value(address: &FacetAddress) -> Value {
    json!({"face": address.macro_face.face, "macro_path": address.macro_face.path, "child_path": address.child_path})
}

fn feature_value(feature: FeatureId) -> Value {
    assert_eq!(feature.kind, FeatureKind::ChannelReach);
    json!({"kind": "channel_reach", "macro_anchor": feature.macro_anchor.0, "ordinal": feature.ordinal})
}

fn sample_value(sample: &FacetFieldSample) -> Value {
    json!({
        "position": sample.position.map(quantize), "height_m": quantize(sample.height_m),
        "normal": sample.normal.map(quantize),
        "material_weights": sample.material_weights.map(|w| quantize(f64::from(w))),
        "shoreline_distance_m": quantize(sample.shoreline_distance_m),
        "water_depth_m": quantize(sample.water_depth_m),
        "flow_direction": sample.flow_direction.map(quantize), "flow_strength": quantize(sample.flow_strength),
        "channel_distance_m": quantize(sample.channel_distance_m), "channel_width_m": quantize(sample.channel_width_m),
        "floodplain_weight": quantize(f64::from(sample.floodplain_weight)), "bank_weight": quantize(f64::from(sample.bank_weight)),
        "terrace_weight": quantize(f64::from(sample.terrace_weight)), "delta_weight": quantize(f64::from(sample.delta_weight)),
        "ridge_direction": sample.ridge_direction.map(quantize), "ridge_strength": quantize(f64::from(sample.ridge_strength))
    })
}

fn patch_value(patch: &SurfacePatch, indices: &[usize]) -> Value {
    // Channels have inherited IDs. Coast and ridge are source-owned implicit
    // fields in the current SurfacePatch contract, so reference those sample
    // fields explicitly rather than inventing curve IDs absent from the source.
    json!({
        "address": address_value(&patch.address),
        "samples": indices.iter().map(|&i| json!({"index": i, "fields": sample_value(&patch.samples[i])})).collect::<Vec<_>>(),
        "feature_references": {
            "channel_trunks": patch.curves.iter().filter(|c| c.feature.ordinal == 0).map(|c| c.feature).collect::<std::collections::BTreeSet<_>>().into_iter().map(feature_value).collect::<Vec<_>>(),
            "first_rill": patch.curves.iter().find(|c| c.feature.ordinal > 0).map(|c| feature_value(c.feature)),
            "coast": {"representation": "sample_field", "distance": "shoreline_distance_m", "depth": "water_depth_m"},
            "ridge": {"representation": "sample_field", "direction": "ridge_direction", "strength": "ridge_strength"}
        }
    })
}

fn curve_value(curve: &RealizedCurve) -> Value {
    json!({
        "feature": feature_value(curve.feature),
        "points": curve.points.iter().map(|p| p.map(quantize)).collect::<Vec<_>>(),
        "width_rad": curve.width.iter().map(|&w| quantize(w)).collect::<Vec<_>>(),
        "terminals": curve.endpoints.iter().map(|e| match e.terminal {
            TerminalKind::Headwater => "headwater", TerminalKind::Confluence => "confluence",
            TerminalKind::Lake => "lake", TerminalKind::Ocean => "ocean", TerminalKind::Continuation => "continuation"
        }).collect::<Vec<_>>()
    })
}

fn separation(a: [f64; 3], b: [f64; 3]) -> f64 {
    a.into_iter()
        .zip(b)
        .map(|(a, b)| (a - b).powi(2))
        .sum::<f64>()
        .sqrt()
}

fn live_proof() -> Value {
    // The proof observes a world, so use the committed default seed-42 ledger
    // (decision 0606). Surface fields and curves are still derived live.
    let world = seed_42_world();
    // Named fixture construction (decision 0092): retain one terrain to
    // verify actual confluence, terminal-basin and ocean-mouth authority.
    #[allow(clippy::disallowed_methods)]
    let terrain = hornvale_worldgen::terrain_of(&world).unwrap();
    let context = SurfaceRealizationContext::build(&world).unwrap();
    let net = terrain.channels();
    let inputs = TerrainFacetInputs {
        globe: terrain.globe(),
        geo: terrain.geosphere(),
        channels: net,
    };
    let at = |point| {
        FacetAddress::new(
            Facet::containing(point, hornvale_terrain::GLOBE_LEVEL),
            vec![],
        )
        .unwrap()
    };

    let (line, owner) = net
        .run_vertices
        .iter()
        .enumerate()
        .find_map(|(line, run)| {
            net.trunk_vertex(*run.last().unwrap())
                .filter(|(owner, _)| *owner != line)
                .map(|owner| (line, owner))
        })
        .unwrap();
    let mouth = *net.polylines[line].points.last().unwrap();
    let confluence = context.realize(&at(mouth)).unwrap();
    let tributary = confluence
        .curves
        .iter()
        .find(|c| c.feature.ordinal == 0 && c.feature.macro_anchor == net.run_vertices[line][0])
        .unwrap();
    let downstream = tributary.downstream_feature(inputs).unwrap();
    assert_eq!(tributary.endpoints[1].terminal, TerminalKind::Confluence);
    assert_eq!(downstream.macro_anchor, net.run_vertices[owner.0][0]);
    assert!(
        separation(
            *tributary.points.last().unwrap(),
            net.polylines[owner.0].points[owner.1]
        ) < 1.0e-12
    );

    let (line, run) = net
        .run_vertices
        .iter()
        .enumerate()
        .find(|(_, run)| terrain.water_kind_at(*run.last().unwrap()) == WaterKind::SaltBasin)
        .expect("seed 42 has a terminal basin");
    let basin_mouth = *net.polylines[line].points.last().unwrap();
    let basin = context.realize(&at(basin_mouth)).unwrap();
    let terminal = basin
        .curves
        .iter()
        .find(|c| c.feature.ordinal == 0 && c.feature.macro_anchor == run[0])
        .unwrap();
    assert_eq!(terminal.endpoints[1].terminal, TerminalKind::Lake);
    assert!(terminal.downstream_feature(inputs).is_none());
    assert!(separation(*terminal.points.last().unwrap(), basin_mouth) < 1.0e-12);
    assert!(terrain.globe().downhill.get(*run.last().unwrap()).is_none());

    // Search real ocean-mouth neighborhoods for a patch whose continuous
    // coast field straddles zero. A case label alone cannot satisfy the proof.
    let coast = net
        .run_vertices
        .iter()
        .enumerate()
        .filter(|(_, run)| terrain.water_kind_at(*run.last().unwrap()) == WaterKind::Ocean)
        .flat_map(|(line, _)| {
            let macro_face = at(*net.polylines[line].points.last().unwrap()).macro_face;
            std::iter::once(macro_face.clone()).chain(macro_face.neighbors())
        })
        .map(|facet| {
            context
                .realize(&FacetAddress::new(facet, vec![]).unwrap())
                .unwrap()
        })
        .find(|patch| {
            patch.samples.iter().any(|s| s.shoreline_distance_m > 1.0)
                && patch.samples.iter().any(|s| s.shoreline_distance_m < -1.0)
        })
        .expect("coast crossing exists");
    let dry = coast
        .samples
        .iter()
        .position(|s| s.shoreline_distance_m > 1.0)
        .unwrap();
    let wet = coast
        .samples
        .iter()
        .position(|s| s.shoreline_distance_m < -1.0)
        .unwrap();
    assert_eq!(coast.samples[dry].water_depth_m, 0.0);
    assert!(coast.samples[wet].water_depth_m > 0.0);
    assert!(coast.samples.iter().any(|s| s.ridge_strength > 0.0));

    let address = FacetAddress::new(
        Facet {
            face: 0,
            path: vec![0; 6],
        },
        vec![],
    )
    .unwrap();
    let coarse = context.realize(&address).unwrap();
    let corner = coarse.samples[0];
    let peers: Vec<_> = address
        .macro_face
        .neighbors()
        .into_iter()
        .filter(|f| f.corners().contains(&corner.position))
        .map(|f| {
            context
                .realize(&FacetAddress::new(f, vec![]).unwrap())
                .unwrap()
        })
        .collect();
    assert_eq!(peers.len(), 2);
    for peer in &peers {
        let sample = peer
            .samples
            .iter()
            .find(|s| s.position == corner.position)
            .unwrap();
        assert_eq!(*sample, corner);
    }
    let neighbor = peers[0].address.macro_face.clone();
    assert_ne!(
        neighbor.face, address.macro_face.face,
        "LOD proof crosses a cube seam"
    );
    let fine: Vec<_> = (0..4)
        .map(|digit| {
            context
                .realize(&FacetAddress::new(neighbor.clone(), vec![digit]).unwrap())
                .unwrap()
        })
        .filter(|p| stitch_transition(&coarse, p).is_ok())
        .collect();
    assert_eq!(fine.len(), 2);
    let transition = stitch_transition(&coarse, &fine[0]).unwrap();
    assert_eq!(transition, stitch_transition(&coarse, &fine[1]).unwrap());
    assert_eq!(transition.len(), 5);
    let split = (5..9)
        .find(|&i| transition.iter().flatten().any(|&j| i == j as usize))
        .unwrap();
    for patch in &fine {
        assert_eq!(
            *patch
                .samples
                .iter()
                .find(|s| s.position == coarse.samples[split].position)
                .unwrap(),
            coarse.samples[split]
        );
    }
    json!({
        "schema": "hornvale/surface-proof/v1", "seed": 42,
        "revision": {"source_revision": context.revision.source_revision, "algorithm_version": context.revision.algorithm_version,
            "configuration_hash": context.revision.configuration_hash},
        "cases": {
            "confluence": {"patch": patch_value(&confluence, &[4]), "tributary": curve_value(tributary), "downstream_feature": feature_value(downstream)},
            "terminal_basin": {"patch": patch_value(&basin, &[4]), "terminal": curve_value(terminal)},
            "coast_crossing": {"patch": patch_value(&coast, &[dry, wet])},
            "face_corner": {"patch": patch_value(&coarse, &[0]), "neighbor_addresses": peers.iter().map(|p| address_value(&p.address)).collect::<Vec<_>>()},
            "unequal_lod": {"coarse": patch_value(&coarse, &[split]), "fine_addresses": fine.iter().map(|p| address_value(&p.address)).collect::<Vec<_>>(),
                "coarse_replacement_triangles": transition}
        }
    })
}

#[test]
fn proof_fixture_contains_required_surface_fields() {
    let fixture: Value =
        serde_json::from_str(include_str!("../fixtures/surface-seed-42-proof.json")).unwrap();
    let actual = live_proof();
    assert_eq!(fixture["seed"], 42);
    for case in [
        "confluence",
        "terminal_basin",
        "coast_crossing",
        "face_corner",
        "unequal_lod",
    ] {
        assert!(fixture["cases"][case].is_object(), "missing {case}");
    }
    // Compare canonical emit values, including the complete key set: deleting
    // fields or adding presentation-only fields cannot silently pass.
    assert_eq!(fixture, actual);
}

#[test]
fn surface_document_is_canonical_and_contains_no_weather_hooks() {
    let world = seed_42_world();
    let context = SceneContext::build(&world).unwrap();
    let address = FacetAddress::new(
        Facet {
            face: 0,
            path: vec![0; 6],
        },
        vec![1],
    )
    .unwrap();
    let query = SurfacePatchQuery {
        address,
        expected_revision: context.surface_revision().clone(),
    };
    let first = surface_patch_json(&surface_patch_scene(&context, &query).unwrap());
    let second = surface_patch_json(&surface_patch_scene(&context, &query).unwrap());
    assert_eq!(first, second);
    let value: Value = serde_json::from_str(&first).unwrap();
    assert_eq!(value["schema"], "scene/surface/v1");
    assert!(value["revision"]["configuration_hash_hex"].is_string());
    assert!(value["samples"].is_array());
    assert!(value["triangles"].is_array());
    for hook in ["weather", "cloud", "precip", "roughness"] {
        assert!(!first.contains(hook), "surface document contains {hook}");
    }
}
