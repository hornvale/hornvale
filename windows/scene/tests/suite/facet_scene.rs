use hornvale_kernel::{Facet, quantize};
use hornvale_scene::{
    SceneContext, SurfacePatchQuery, surface_patch_json, surface_patch_scene,
    surface_patch_scene_with_transition,
};
use hornvale_terrain::{
    FacetAddress, FacetFieldSample, FeatureId, FeatureKind, RealizedCurve, TerminalKind,
    TerrainFacetInputs, WaterKind,
};
use hornvale_worldgen::facet::{stitch_feature_transition, stitch_transition};
use hornvale_worldgen::{SurfacePatch, SurfaceRealizationContext, seed_42_world};
use serde_json::{Value, json};
use std::collections::BTreeSet;

fn keys(value: &Value) -> BTreeSet<&str> {
    value
        .as_object()
        .unwrap()
        .keys()
        .map(String::as_str)
        .collect()
}

fn assert_field_order(document: &str, fields: &[&str]) {
    let mut previous = 0;
    for field in fields {
        let position = document
            .find(&format!("\"{field}\""))
            .unwrap_or_else(|| panic!("missing field {field}"));
        assert!(position >= previous, "field {field} is out of order");
        previous = position;
    }
}

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

/// **`windows/scene/tests/fixtures/surface-seed-42-proof.json` regenerated
/// (The Trencher repair round, 2026-09-14; again absorbing The Tidemark,
/// 2026-09-15).** This campaign absorbed 118 `origin/main` commits (main's
/// coherent-terrain work) on top of this campaign's own Task 13 (widened
/// carbonate/porosity), then absorbed The Tidemark's own worldgen changes
/// and the species metabolic-triple migration fix this merge also carries;
/// the committed seed-42 world moved each time, so the surfaces this proof
/// observes (confluence, terminal basin, coast crossing, face corner,
/// unequal-LOD) moved with it. This is a **committed generator's own
/// output** — [`live_proof`] builds this exact JSON from `seed_42_world()`,
/// and the fixture is nothing but its serialized value
/// (`assert_eq!(fixture, actual)` below, verbatim). It now honours
/// `REBASELINE=1` exactly as `hornvale_kernel::golden` does (added on the
/// merge re-pin above, matching the pattern `column_delegation.rs` and
/// `lens_purity.rs` already use), so a future world move can be accepted
/// without hand-writing the fixture. No field was added or removed
/// (confirmed: the same five `cases`, the same key set at every level);
/// every value that moved is a coordinate, distance or weight recomputed
/// from the moved terrain.
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
    if std::env::var_os("REBASELINE").is_some() {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures/surface-seed-42-proof.json");
        std::fs::write(&path, serde_json::to_string_pretty(&actual).unwrap())
            .expect("rewriting the surface proof fixture");
        println!("REBASELINE: rewrote {}", path.display());
        return;
    }
    assert_eq!(fixture, actual);
}

#[test]
fn surface_document_is_canonical_and_contains_no_weather_hooks() {
    let world = seed_42_world();
    let context = SceneContext::build(&world).unwrap();
    let independent_context = SceneContext::build(&world).unwrap();
    let macro_face = Facet {
        face: 0,
        path: vec![0; 6],
    }
    .pack()
    .unwrap()
    .0 as u32;
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
    let second = surface_patch_json(&surface_patch_scene(&independent_context, &query).unwrap());
    assert_eq!(first, second);
    let value: Value = serde_json::from_str(&first).unwrap();
    assert_field_order(
        &first,
        &[
            "schema",
            "revision",
            "address",
            "samples",
            "curves",
            "strips",
            "triangles",
        ],
    );

    assert_eq!(
        keys(&value),
        BTreeSet::from([
            "schema",
            "revision",
            "address",
            "samples",
            "curves",
            "strips",
            "triangles"
        ])
    );
    assert_eq!(value["schema"], "scene/surface/v1");
    assert_eq!(
        keys(&value["revision"]),
        BTreeSet::from([
            "source_revision",
            "algorithm_version",
            "configuration_hash_hex"
        ])
    );
    assert_eq!(
        value["revision"]["source_revision"].as_str().unwrap().len(),
        64
    );
    assert_eq!(
        value["revision"]["source_revision"],
        context.surface_revision().source_revision
    );
    assert_eq!(
        value["revision"]["algorithm_version"],
        "hornvale/surface-realization/v7"
    );
    assert_eq!(
        value["revision"]["configuration_hash_hex"]
            .as_str()
            .unwrap()
            .len(),
        64
    );
    assert_eq!(
        keys(&value["address"]),
        BTreeSet::from(["macro_face", "child_path"])
    );
    assert_eq!(value["address"]["macro_face"], macro_face);
    assert_eq!(value["address"]["child_path"], serde_json::json!([1]));
    assert_eq!(value["samples"].as_array().unwrap().len(), 9);
    assert!(!value["curves"].as_array().unwrap().is_empty());
    assert!(!value["strips"].as_array().unwrap().is_empty());
    assert!(!value["triangles"].as_array().unwrap().is_empty());
    for sample in value["samples"].as_array().unwrap() {
        assert_eq!(
            keys(sample),
            BTreeSet::from([
                "position",
                "height_m",
                "normal",
                "material_weights",
                "shoreline_distance_m",
                "water_depth_m",
                "flow_direction",
                "flow_strength",
                "channel_distance_m",
                "channel_width_m",
                "floodplain_weight",
                "bank_weight",
                "terrace_weight",
                "delta_weight",
                "ridge_direction",
                "ridge_strength"
            ])
        );
        assert_eq!(sample["position"].as_array().unwrap().len(), 3);
        assert_eq!(sample["normal"].as_array().unwrap().len(), 3);
        assert_eq!(sample["material_weights"].as_array().unwrap().len(), 8);
        assert_eq!(sample["flow_direction"].as_array().unwrap().len(), 3);
        assert_eq!(sample["ridge_direction"].as_array().unwrap().len(), 3);
    }
    for curve in value["curves"].as_array().unwrap() {
        assert_eq!(
            keys(curve),
            BTreeSet::from(["feature", "points", "width_rad", "endpoints"])
        );
        assert_eq!(
            keys(&curve["feature"]),
            BTreeSet::from(["kind", "macro_anchor", "ordinal"])
        );
        assert_eq!(curve["endpoints"].as_array().unwrap().len(), 2);
        for endpoint in curve["endpoints"].as_array().unwrap() {
            assert_eq!(
                keys(endpoint),
                BTreeSet::from(["feature", "side", "boundary", "terminal"])
            );
        }
    }
    for hook in ["weather", "cloud", "precip", "roughness"] {
        assert!(!first.contains(hook), "surface document contains {hook}");
    }
}

#[test]
fn surface_document_emits_a_strip_for_a_curve_between_terrain_samples() {
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
    let patch = surface_patch_scene(&context, &query).unwrap();
    let missed = patch
        .curves
        .iter()
        .find(|curve| {
            patch.samples.iter().all(|sample| {
                let (distance, width) = hornvale_terrain::feature_sample(curve, sample.position);
                distance.abs() > width * 0.5
            })
        })
        .expect("fixture must contain a curve whose width misses every terrain sample");

    let document: Value = serde_json::from_str(&surface_patch_json(&patch)).unwrap();
    let strips = document["strips"]
        .as_array()
        .expect("surface document must carry source-owned feature strips");
    assert!(
        strips
            .iter()
            .any(|strip| strip["feature"] == feature_value(missed.feature)),
        "between-sample feature {:?} has no render strip",
        missed.feature
    );
}

/// claim: invariant(seed-42 scene transition emits the source's canonical stitched ribbon geometry)
#[test]
fn surface_transition_emits_canonical_stitched_strips() {
    let context = SceneContext::build(&seed_42_world()).unwrap();
    let query = SurfacePatchQuery {
        address: FacetAddress::new(
            Facet {
                face: 0,
                path: vec![0; 6],
            },
            vec![],
        )
        .unwrap(),
        expected_revision: context.surface_revision().clone(),
    };
    let coarse = surface_patch_scene(&context, &query).unwrap();
    // Exercise a real cube-face seam through the public scene API. Requiring
    // changed wire geometry makes a missing assignment in that API observable.
    let neighbor = query
        .address
        .macro_face
        .neighbors()
        .into_iter()
        .find(|neighbor| neighbor.face != query.address.macro_face.face)
        .expect("fixture lies on a cube-face seam");
    let original_wire: Value = serde_json::from_str(&surface_patch_json(&coarse)).unwrap();
    let mut checked = 0;
    for digit in 0..4 {
        let fine_query = SurfacePatchQuery {
            address: FacetAddress::new(neighbor.clone(), vec![digit]).unwrap(),
            expected_revision: query.expected_revision.clone(),
        };
        let fine = surface_patch_scene(&context, &fine_query).unwrap();
        let Ok(triangles) = stitch_transition(&coarse, &fine) else {
            continue;
        };
        let mut expected = coarse.clone();
        expected.strips = stitch_feature_transition(&coarse, &fine).unwrap();
        expected.transition_triangles = triangles;
        let expected_wire: Value = serde_json::from_str(&surface_patch_json(&expected)).unwrap();
        assert!(
            expected_wire["strips"] != original_wire["strips"],
            "fixture must require visible ribbon boundary stitching"
        );
        let actual =
            surface_patch_scene_with_transition(&context, &query, Some(&fine_query.address))
                .unwrap();
        assert!(
            actual.strips == expected.strips,
            "scene must install the source's canonical boundary vertices and triangles"
        );
        let actual_wire: Value = serde_json::from_str(&surface_patch_json(&actual)).unwrap();
        assert!(
            actual_wire == expected_wire,
            "stitched geometry must reach the wire"
        );
        checked += 1;
    }
    assert_eq!(
        checked, 2,
        "both fine halves of the cube seam must be checked"
    );
}
