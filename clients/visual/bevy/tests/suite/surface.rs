use hornvale_bevy_view::{
    CameraPose, ObservationMirror,
    astronomy::{lighting, surface},
    bevy::{asset::Assets, mesh::VertexAttributeValues, pbr::StandardMaterial, prelude::*},
    camera::OrbitCamera,
    documents::{self, SurfacePatchCacheKey, SurfacePatchRevision},
    lifecycle::{self, SceneCatalog, SurfaceFeatureVisual},
};

const MACRO_FACE: u32 = 1 << 17;
#[test]
fn globe_uses_source_north_and_sea_reference_without_relief_gain() {
    let mut initial = documents::initial(include_str!("../fixtures/initial.json")).unwrap();
    initial.tiles.ocean.fill(false);
    initial
        .tiles
        .elevation_m
        .fill(initial.tiles.sea_level_m + 2000.0);
    let mesh = surface::globe_mesh(&initial.tiles, 7000.0, 1000.0);
    let Some(VertexAttributeValues::Float32x3(points)) =
        mesh.attribute(hornvale_bevy_view::bevy::mesh::Mesh::ATTRIBUTE_POSITION)
    else {
        panic!("positions")
    };
    assert!((points[0][2] - 7.002).abs() < 1e-6);
    let middle =
        (initial.tiles.height / 2 * (initial.tiles.width + 1) + initial.tiles.width / 2) as usize;
    assert!((points[middle][0] - 7.002).abs() < 1e-6);
    assert!(points[middle][1].abs() < 1e-6 && points[middle][2].abs() < 1e-6);
}
#[test]
fn point_flux_respects_distance_and_render_scale() {
    for scale in [1.0, 1000.0] {
        let intensity = f64::from(lighting::point_intensity(0.7, scale).unwrap());
        let distance = 149_597_870.7 / scale;
        let flux = intensity / (4.0 * std::f64::consts::PI * distance * distance);
        assert!((flux - 127_000.0 * 0.7).abs() < 0.01);
    }
}

fn icy_coast(sea_ice: bool) -> documents::Tiles {
    let mut t = documents::initial(include_str!("../fixtures/initial.json"))
        .unwrap()
        .tiles;
    t.snow_fraction.fill(1.0);
    t.t_mean_c.fill(-25.0);
    t.moisture.fill(0.5);
    let ice = t.biome_legend.iter().position(|b| b == "ice").unwrap();
    let ocean = t
        .biome_legend
        .iter()
        .position(|b| b == if sea_ice { "sea-ice" } else { "epipelagic" })
        .unwrap();
    for i in 0..t.elevation_m.len() {
        let water = i % (t.width as usize) < t.width as usize / 2;
        t.ocean[i] = water;
        t.biome[i] = if water { ocean } else { ice };
        t.elevation_m[i] = t.sea_level_m + if water { -1000.0 } else { 200.0 };
    }
    t
}
#[test]
fn icy_land_and_icy_ocean_do_not_gain_a_dark_reconstruction_seam() {
    let texture = surface::anchor_texture(&icy_coast(true));
    let pixels = texture.data.as_ref().unwrap();
    for x in 900..=1200 {
        assert!(pixels[(512 * 2048 + x) * 4] > 150, "dark seam at x={x}");
    }
}
#[test]
fn adjacent_icy_land_does_not_create_sea_ice_over_open_water() {
    let texture = surface::anchor_texture(&icy_coast(false));
    let pixels = texture.data.as_ref().unwrap();
    assert!(pixels[(512 * 2048 + 512) * 4] < 30);
}
#[test]
fn source_longitude_seam_and_poles_reconstruct_continuously() {
    let t = icy_coast(true);
    assert_eq!(
        surface::sample(&t, &t.elevation_m, 0.0, 0.5),
        surface::sample(&t, &t.elevation_m, 1.0, 0.5)
    );
    assert_eq!(
        surface::sample(&t, &t.elevation_m, 0.25, -1.0),
        surface::sample(&t, &t.elevation_m, 0.25, 0.0)
    );
}

#[test]
fn physical_geometry_limits_include_scaled_radius_and_relief() {
    use hornvale_bevy_view::coordinates::render_radius;
    assert!(render_radius(0.001, 0.0, 1000.0).is_ok());
    assert!(render_radius(999_920.0, 80.0, 1000.0).is_ok());
    for (radius, relief, scale) in [
        (1e300, 0.0, 1000.0),
        (7000.0, 1e300, 1000.0),
        (999_921.0, 80.0, 1000.0),
        (0.0001, 0.0, 1000.0),
        (7000.0, 0.0, 1e-300),
        (7000.0, 0.0, 1e300),
    ] {
        assert!(render_radius(radius, relief, scale).is_err());
    }
}

fn patch_json(revision: &str) -> String {
    serde_json::json!({
        "schema": "scene/surface/v1",
        "revision": {
            "source_revision": revision,
            "algorithm_version": "hornvale/surface-realization/v2",
            "configuration_hash_hex": "11".repeat(32)
        },
        "address": {"macro_face": MACRO_FACE, "child_path": []},
        "samples": [
            {"position": [1.0,0.0,0.0], "height_m": 10.0, "normal": [1.0,0.0,0.0], "material_weights": [1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], "shoreline_distance_m": 10.0, "water_depth_m": 0.0, "flow_direction": [0.0,1.0,0.0], "flow_strength": 0.0, "channel_distance_m": 10.0, "channel_width_m": 0.0, "floodplain_weight": 0.0, "bank_weight": 0.0, "terrace_weight": 0.0, "delta_weight": 0.0, "ridge_direction": [0.0,1.0,0.0], "ridge_strength": 0.0},
            {"position": [0.0,1.0,0.0], "height_m": 10.0, "normal": [0.0,1.0,0.0], "material_weights": [1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], "shoreline_distance_m": 10.0, "water_depth_m": 0.0, "flow_direction": [0.0,1.0,0.0], "flow_strength": 0.0, "channel_distance_m": 10.0, "channel_width_m": 0.0, "floodplain_weight": 0.0, "bank_weight": 0.0, "terrace_weight": 0.0, "delta_weight": 0.0, "ridge_direction": [0.0,1.0,0.0], "ridge_strength": 0.0},
            {"position": [0.0,0.0,1.0], "height_m": 10.0, "normal": [0.0,0.0,1.0], "material_weights": [1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], "shoreline_distance_m": 10.0, "water_depth_m": 0.0, "flow_direction": [0.0,1.0,0.0], "flow_strength": 0.0, "channel_distance_m": 10.0, "channel_width_m": 0.0, "floodplain_weight": 0.0, "bank_weight": 0.0, "terrace_weight": 0.0, "delta_weight": 0.0, "ridge_direction": [0.0,1.0,0.0], "ridge_strength": 0.0},
            {"position": [-1.0,0.0,0.0], "height_m": 10.0, "normal": [-1.0,0.0,0.0], "material_weights": [1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], "shoreline_distance_m": 10.0, "water_depth_m": 0.0, "flow_direction": [0.0,1.0,0.0], "flow_strength": 0.0, "channel_distance_m": 10.0, "channel_width_m": 0.0, "floodplain_weight": 0.0, "bank_weight": 0.0, "terrace_weight": 0.0, "delta_weight": 0.0, "ridge_direction": [0.0,1.0,0.0], "ridge_strength": 0.0}
        ],
        "curves": [{"feature": {"kind":"channel_reach", "macro_anchor": 1, "ordinal": 0}, "points": [[0.0,-1.0,0.0],[0.0,1.0,0.0]], "width_rad": [0.05,0.05], "endpoints": [{"feature": {"kind":"channel_reach", "macro_anchor": 1, "ordinal": 0}, "side":"upstream", "boundary": null, "terminal":"headwater"}, {"feature": {"kind":"channel_reach", "macro_anchor": 1, "ordinal": 0}, "side":"downstream", "boundary": null, "terminal":"ocean"}] }],
        "triangles": [[0,1,2],[0,2,3]]
    }).to_string()
}

fn patch_with_strip_json(revision: &str) -> String {
    let mut patch: serde_json::Value = serde_json::from_str(&patch_json(revision)).unwrap();
    patch["strips"] = serde_json::json!([{
        "feature": {"kind":"channel_reach", "macro_anchor": 1, "ordinal": 0},
        "centerline": [[0.7071067812,-0.7071067812,0.0],[0.7071067812,0.7071067812,0.0]],
        "width_rad": [0.1,0.1],
        "vertices": [
            {"position":[0.7062230818,-0.7062230818,-0.0499791693],"height_m":10.0,"normal":[0.7071067812,-0.7071067812,0.0],"side":-1,"signed_distance_rad":-0.05},
            {"position":[0.7062230818,-0.7062230818,0.0499791693],"height_m":10.0,"normal":[0.7071067812,-0.7071067812,0.0],"side":1,"signed_distance_rad":0.05},
            {"position":[0.7062230818,0.7062230818,-0.0499791693],"height_m":10.0,"normal":[0.7071067812,0.7071067812,0.0],"side":-1,"signed_distance_rad":-0.05},
            {"position":[0.7062230818,0.7062230818,0.0499791693],"height_m":10.0,"normal":[0.7071067812,0.7071067812,0.0],"side":1,"signed_distance_rad":0.05}
        ],
        "triangles": [[0,1,2],[1,3,2]],
        "semantic_mask": [0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0],
        "endpoints": [
            {"feature":{"kind":"channel_reach","macro_anchor":1,"ordinal":0},"side":"upstream","boundary":null,"terminal":"headwater"},
            {"feature":{"kind":"channel_reach","macro_anchor":1,"ordinal":0},"side":"downstream","boundary":null,"terminal":"ocean"}
        ]
    }]);
    patch.to_string()
}

fn binding_value() -> serde_json::Value {
    serde_json::json!({
        "source_id": "surface-test",
        "scope_id": "scientific:unrestricted",
        "world_sha256": "b".repeat(64),
        "source_revision": "a".repeat(40)
    })
}

fn surface_request(revision: &str, request_id: u64, child_path: &[u8]) -> String {
    serde_json::json!({
        "schema": "visual/surface-request/v1",
        "binding": binding_value(),
        "request_id": request_id,
        "generation": lifecycle::surface_patch_generation(),
        "address": {"macro_face": MACRO_FACE, "child_path": child_path},
        "expected_revision": {
            "source_revision": revision,
            "algorithm_version": "hornvale/surface-realization/v2",
            "configuration_hash_hex": "11".repeat(32)
        }
    })
    .to_string()
}

fn surface_reply(request_id: u64, patch: &str) -> String {
    serde_json::json!({
        "schema": "visual/surface-reply/v1",
        "binding": binding_value(),
        "request_id": request_id,
        "generation": lifecycle::surface_patch_generation(),
        "patch": serde_json::from_str::<serde_json::Value>(patch).unwrap()
    })
    .to_string()
}

#[test]
fn patch_document_round_trips() {
    let json = patch_json(&"a".repeat(40));
    let document = documents::surface_patch(&json).unwrap();
    let encoded = serde_json::to_string(&document).unwrap();
    let round_trip = documents::surface_patch(&encoded).unwrap();
    assert_eq!(document, round_trip);
}

#[test]
fn strip_protocol_accepts_finite_signed_geometry_and_rejects_malformed_edges() {
    let json = patch_with_strip_json(&"a".repeat(40));
    let document = documents::surface_patch(&json).unwrap();
    assert_eq!(document.strips.len(), 1);
    assert_eq!(document.strips[0].vertices[0].side, -1);
    assert!(document.strips[0].vertices[0].signed_distance_rad < 0.0);

    let mut malformed: serde_json::Value = serde_json::from_str(&json).unwrap();
    malformed["strips"][0]["vertices"][0]["side"] = serde_json::json!(0);
    assert!(documents::surface_patch(&malformed.to_string()).is_err());
    malformed["strips"][0]["vertices"][0]["side"] = serde_json::json!(-1);
    malformed["strips"][0]["vertices"][0]["signed_distance_rad"] = serde_json::json!("not-finite");
    assert!(documents::surface_patch(&malformed.to_string()).is_err());
}

#[test]
fn surface_reply_requires_complete_validated_envelope() {
    let patch = patch_json(&"a".repeat(40));
    let reply = surface_reply(7, &patch);
    let decoded = documents::surface_reply(&reply).unwrap();
    assert_eq!(decoded.request_id, 7);
    assert_eq!(decoded.patch.address.child_path, Vec::<u8>::new());

    let mut missing_binding: serde_json::Value = serde_json::from_str(&reply).unwrap();
    missing_binding.as_object_mut().unwrap().remove("binding");
    assert!(documents::surface_reply(&missing_binding.to_string()).is_err());

    let mut missing_request_id: serde_json::Value = serde_json::from_str(&reply).unwrap();
    missing_request_id
        .as_object_mut()
        .unwrap()
        .remove("request_id");
    assert!(documents::surface_reply(&missing_request_id.to_string()).is_err());
}

#[test]
fn surface_validation_rejects_unbounded_semantics_and_unknown_endpoints() {
    let mut value: serde_json::Value = serde_json::from_str(&patch_json(&"a".repeat(40))).unwrap();
    value["samples"][0]["material_weights"][0] = serde_json::json!(1.1);
    assert!(documents::surface_patch(&value.to_string()).is_err());

    let mut value: serde_json::Value = serde_json::from_str(&patch_json(&"a".repeat(40))).unwrap();
    value["curves"][0]["endpoints"][0]["side"] = serde_json::json!("sideways");
    assert!(documents::surface_patch(&value.to_string()).is_err());

    let mut value: serde_json::Value = serde_json::from_str(&patch_json(&"a".repeat(40))).unwrap();
    value["curves"][0]["endpoints"][1]["terminal"] = serde_json::json!("nowhere");
    assert!(documents::surface_patch(&value.to_string()).is_err());
}

#[test]
fn surface_validation_accepts_signed_channel_distance_but_rejects_non_finite_values() {
    let mut value: serde_json::Value = serde_json::from_str(&patch_json(&"a".repeat(40))).unwrap();
    value["samples"][0]["channel_distance_m"] = serde_json::json!(-10.0);
    let document = documents::surface_patch(&value.to_string()).unwrap();
    assert_eq!(document.vertices[0].channel_distance_m, -10.0);

    value["samples"][0]["channel_distance_m"] = serde_json::json!("not-a-number");
    assert!(documents::surface_patch(&value.to_string()).is_err());

    value["samples"][0]["channel_distance_m"] = serde_json::json!(-100_000_001.0);
    assert!(documents::surface_patch(&value.to_string()).is_err());
}

#[test]
fn signed_channel_distance_has_symmetric_channel_material_influence() {
    let mut positive: serde_json::Value =
        serde_json::from_str(&patch_json(&"a".repeat(40))).unwrap();
    positive["samples"][0]["channel_distance_m"] = serde_json::json!(10.0);
    positive["samples"][0]["channel_width_m"] = serde_json::json!(100.0);
    positive["samples"][0]["flow_strength"] = serde_json::json!(1.0);
    let mut negative = positive.clone();
    negative["samples"][0]["channel_distance_m"] = serde_json::json!(-10.0);
    let positive = documents::surface_patch(&positive.to_string()).unwrap();
    let negative = documents::surface_patch(&negative.to_string()).unwrap();
    let positive_material = surface::surface_material(&positive);
    let negative_material = surface::surface_material(&negative);
    assert_eq!(positive_material.base_color, negative_material.base_color);
}

#[test]
fn surface_mesh_preserves_source_direction_fields_as_render_attributes() {
    let document = documents::surface_patch(&patch_json(&"a".repeat(40))).unwrap();
    let mesh = surface::surface_mesh(&document, None);
    let Some(VertexAttributeValues::Float32x3(flow)) =
        mesh.attribute(surface::ATTRIBUTE_FLOW_DIRECTION)
    else {
        panic!("flow direction attribute")
    };
    let Some(VertexAttributeValues::Float32x3(ridge)) =
        mesh.attribute(surface::ATTRIBUTE_RIDGE_DIRECTION)
    else {
        panic!("ridge direction attribute")
    };
    assert_eq!(flow[0], [0.0, 1.0, 0.0]);
    assert_eq!(ridge[0], [0.0, 1.0, 0.0]);
}

#[test]
fn cache_key_includes_the_full_surface_revision() {
    let first = documents::surface_patch(&patch_json(&"a".repeat(40))).unwrap();
    let mut changed = first.clone();
    changed.revision.algorithm_version.push_str("+changed");
    assert_ne!(first.cache_key(), changed.cache_key());
}

#[test]
fn stale_patch_is_not_applied() {
    let revision = "a".repeat(40);
    lifecycle::reset_surface_patches();
    lifecycle::schedule_surface_patch(
        SurfacePatchCacheKey {
            revision: revision.clone(),
            macro_face: MACRO_FACE,
            child_path: vec![],
        },
        surface_request(&revision, 11, &[]),
    )
    .unwrap();
    let stale = documents::surface_patch(&patch_json(&"b".repeat(40))).unwrap();
    assert!(lifecycle::apply_surface_patch(&stale).is_err());
}

#[test]
fn concurrent_surface_replies_require_binding_request_and_generation_identity() {
    let revision = "a".repeat(40);
    lifecycle::reset_surface_patches();
    let a = patch_json(&revision);
    let b = patch_json(&revision);
    lifecycle::schedule_surface_patch(
        SurfacePatchCacheKey {
            revision: revision.clone(),
            macro_face: MACRO_FACE,
            child_path: vec![],
        },
        surface_request(&revision, 21, &[]),
    )
    .unwrap();
    lifecycle::schedule_surface_patch(
        SurfacePatchCacheKey {
            revision: revision.clone(),
            macro_face: MACRO_FACE,
            child_path: vec![],
        },
        surface_request(&revision, 22, &[]),
    )
    .unwrap();
    assert!(lifecycle::apply_surface_reply(&surface_reply(21, &a)).is_ok());
    assert!(lifecycle::apply_surface_reply(&surface_reply(22, &b)).is_ok());

    lifecycle::schedule_surface_patch(
        SurfacePatchCacheKey {
            revision: revision.clone(),
            macro_face: MACRO_FACE,
            child_path: vec![],
        },
        surface_request(&revision, 23, &[]),
    )
    .unwrap();
    lifecycle::reset_surface_patches();
    assert!(lifecycle::apply_surface_reply(&surface_reply(23, &a)).is_err());
}

#[test]
fn curve_mask_survives_vertex_miss() {
    let document = documents::surface_patch(&patch_json(&"a".repeat(40))).unwrap();
    let mask = surface::narrow_feature_mask(&document, &document.features[0], [0.0, 0.0, 0.0]);
    assert!(mask > 0.9);
}

#[test]
fn mixed_lod_mesh_has_no_boundary_gap() {
    let revision = "a".repeat(40);
    let mut coarse: serde_json::Value = serde_json::from_str(&patch_json(&revision)).unwrap();
    coarse["transition_triangles"] = serde_json::json!([[0, 1, 3]]);
    let mut fine: serde_json::Value = serde_json::from_str(&patch_json(&revision)).unwrap();
    fine["address"]["child_path"] = serde_json::json!([1]);
    fine["triangles"] = serde_json::json!([[0, 1, 2]]);
    let coarse = documents::surface_patch(&coarse.to_string()).unwrap();
    let fine = documents::surface_patch(&fine.to_string()).unwrap();
    let mesh = surface::surface_mesh(&coarse, Some(&fine));
    let Some(VertexAttributeValues::Float32x3(points)) =
        mesh.attribute(hornvale_bevy_view::bevy::mesh::Mesh::ATTRIBUTE_POSITION)
    else {
        panic!("positions")
    };
    assert_eq!(points.len(), coarse.vertices.len());
    assert_eq!(
        mesh.indices().unwrap().iter().collect::<Vec<_>>(),
        vec![0, 1, 3]
    );
}

#[test]
fn mixed_lod_mesh_remaps_indices_for_a_distinct_transition_layout() {
    let revision = "a".repeat(40);
    let coarse = documents::surface_patch(&patch_json(&revision)).unwrap();
    let mut fine: serde_json::Value = serde_json::from_str(&patch_json(&revision)).unwrap();
    fine["address"]["child_path"] = serde_json::json!([1]);
    fine["samples"]
        .as_array_mut()
        .unwrap()
        .push(serde_json::json!({
            "position": [0.0,-1.0,0.0], "height_m": 10.0, "normal": [0.0,-1.0,0.0],
            "material_weights": [1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
            "shoreline_distance_m": 10.0, "water_depth_m": 0.0,
            "flow_direction": [0.0,1.0,0.0], "flow_strength": 0.0,
            "channel_distance_m": 10.0, "channel_width_m": 0.0,
            "floodplain_weight": 0.0, "bank_weight": 0.0, "terrace_weight": 0.0,
            "delta_weight": 0.0, "ridge_direction": [0.0,1.0,0.0], "ridge_strength": 0.0
        }));
    fine["triangles"] = serde_json::json!([[0, 1, 4]]);
    let fine = documents::surface_patch(&fine.to_string()).unwrap();
    let mesh = surface::surface_mesh(&coarse, Some(&fine));
    let Some(VertexAttributeValues::Float32x3(points)) =
        mesh.attribute(hornvale_bevy_view::bevy::mesh::Mesh::ATTRIBUTE_POSITION)
    else {
        panic!("positions")
    };
    let indices = mesh.indices().unwrap().iter().collect::<Vec<_>>();
    assert_eq!(points.len(), 5);
    assert_eq!(indices, vec![0, 1, 4]);
    assert!(indices.iter().all(|index| *index < points.len()));
}

#[test]
fn camera_patch_selection_is_bounded_canonical_and_repeatable() {
    // Catches an unbounded/global selector or completion-order-dependent output.
    let camera = OrbitCamera::new(CameraPose {
        eye_km: [30_000.0, 0.0, 0.0],
        target_km: [0.0; 3],
        up: [0.0, 0.0, 1.0],
        vertical_fov_radians: 0.7,
        focus_distance_km: 30_000.0,
    });
    let revision = SurfacePatchRevision {
        source_revision: "a".repeat(40),
        algorithm_version: "hornvale/surface-realization/v2".into(),
        configuration_hash_hex: "11".repeat(32),
    };
    let first = lifecycle::visible_surface_patches(&camera, [0.0; 3], 7_000.0, &revision).unwrap();
    let second = lifecycle::visible_surface_patches(&camera, [0.0; 3], 7_000.0, &revision).unwrap();
    assert!(
        (5..=9).contains(&first.len()),
        "bounded one-ring: {first:?}"
    );
    assert_eq!(first, second);
    assert!(first.windows(2).all(|pair| {
        (pair[0].macro_face, &pair[0].child_path) < (pair[1].macro_face, &pair[1].child_path)
    }));
    assert!(
        first
            .iter()
            .all(|key| key.revision == revision.cache_token())
    );
}

#[test]
fn camera_patch_neighbor_ring_crosses_cube_face_seams() {
    // Catches clipping the 3x3 ring at the selected cube-face boundary.
    let camera = OrbitCamera::new(CameraPose {
        eye_km: [30_000.0, 30_000.0, 0.0],
        target_km: [0.0; 3],
        up: [0.0, 0.0, 1.0],
        vertical_fov_radians: 0.7,
        focus_distance_km: 40_000.0,
    });
    let revision = SurfacePatchRevision {
        source_revision: "a".repeat(40),
        algorithm_version: "hornvale/surface-realization/v2".into(),
        configuration_hash_hex: "11".repeat(32),
    };
    let selected =
        lifecycle::visible_surface_patches(&camera, [0.0; 3], 7_000.0, &revision).unwrap();
    assert_eq!(selected.len(), 9);
    assert!(
        selected
            .iter()
            .map(|key| key.macro_face & 0x1f)
            .collect::<std::collections::BTreeSet<_>>()
            .len()
            > 1
    );
}

#[test]
fn camera_patch_neighbor_ring_uses_canonical_cube_corner_adjacency() {
    // Catches clipping both out-of-range coordinates into a four-facet face-local corner.
    let camera = OrbitCamera::new(CameraPose {
        eye_km: [30_000.0, 30_000.0, 30_000.0],
        target_km: [0.0; 3],
        up: [0.0, 0.0, 1.0],
        vertical_fov_radians: 0.7,
        focus_distance_km: 50_000.0,
    });
    let revision = SurfacePatchRevision {
        source_revision: "a".repeat(40),
        algorithm_version: "hornvale/surface-realization/v2".into(),
        configuration_hash_hex: "11".repeat(32),
    };
    let selected =
        lifecycle::visible_surface_patches(&camera, [0.0; 3], 7_000.0, &revision).unwrap();
    let faces = selected
        .iter()
        .map(|key| key.macro_face & 0x1f)
        .collect::<std::collections::BTreeSet<_>>();
    assert_eq!(selected.len(), 8, "a cube corner has seven neighbors");
    assert_eq!(faces.len(), 3, "three cube faces meet at a corner");
}

#[test]
fn camera_selection_transforms_frame_direction_into_rotating_body_local_space() {
    // Catches selecting frame +Y instead of local +X for a quarter-turned body.
    let revision = SurfacePatchRevision {
        source_revision: "a".repeat(40),
        algorithm_version: "hornvale/surface-realization/v2".into(),
        configuration_hash_hex: "11".repeat(32),
    };
    let local_x_camera = OrbitCamera::new(CameraPose {
        eye_km: [30_000.0, 0.0, 0.0],
        target_km: [0.0; 3],
        up: [0.0, 0.0, 1.0],
        vertical_fov_radians: 0.7,
        focus_distance_km: 30_000.0,
    });
    let frame_y_camera = OrbitCamera::new(CameraPose {
        eye_km: [0.0, 30_000.0, 0.0],
        ..local_x_camera.pose.clone()
    });
    let body_to_frame = Some([[0.0, 1.0, 0.0], [-1.0, 0.0, 0.0], [0.0, 0.0, 1.0]]);
    let expected =
        lifecycle::visible_surface_patches(&local_x_camera, [0.0; 3], 7_000.0, &revision).unwrap();
    let selected = lifecycle::visible_surface_patches_for_body(
        &frame_y_camera,
        [0.0; 3],
        body_to_frame,
        7_000.0,
        &revision,
    )
    .unwrap();
    assert_eq!(selected, expected);
}

fn scene_catalog() -> (World, ObservationMirror, SceneCatalog) {
    let mut world = World::new();
    world.init_resource::<Assets<Mesh>>();
    world.init_resource::<Assets<Image>>();
    world.init_resource::<Assets<StandardMaterial>>();
    world.init_resource::<Assets<hornvale_bevy_view::bevy::light::atmosphere::ScatteringMedium>>();
    let mut mirror = ObservationMirror::new(include_str!("../fixtures/initial.json")).unwrap();
    mirror.request(0).unwrap();
    mirror
        .accept(include_str!("../fixtures/reply.json"))
        .unwrap();
    let mut catalog = SceneCatalog::default();
    catalog.populate(&mut world, &mirror).unwrap();
    (world, mirror, catalog)
}

fn catalog_request(mirror: &ObservationMirror, request_id: u64) -> String {
    let revision = &mirror.initial().binding.source_revision;
    serde_json::json!({
        "schema": "visual/surface-request/v1",
        "binding": mirror.initial().binding,
        "request_id": request_id,
        "generation": mirror.generation(),
        "address": {"macro_face": MACRO_FACE, "child_path": []},
        "expected_revision": {
            "source_revision": revision,
            "algorithm_version": "hornvale/surface-realization/v2",
            "configuration_hash_hex": "11".repeat(32)
        }
    })
    .to_string()
}

fn catalog_reply(mirror: &ObservationMirror, request_id: u64, patch: &str) -> String {
    serde_json::json!({
        "schema": "visual/surface-reply/v1",
        "binding": mirror.initial().binding,
        "request_id": request_id,
        "generation": mirror.generation(),
        "patch": serde_json::from_str::<serde_json::Value>(patch).unwrap()
    })
    .to_string()
}

#[test]
fn ready_patch_suppresses_its_fallback_region_without_hiding_uncovered_globe() {
    // Catches a global fallback hide or a patch biased behind its covered region.
    let (mut world, mirror, mut catalog) = scene_catalog();
    let revision = mirror.initial().binding.source_revision.clone();
    let key = documents::surface_patch(&patch_json(&revision))
        .unwrap()
        .cache_key();
    catalog
        .set_desired_surface_patches(&mirror, vec![key.clone()])
        .unwrap();
    catalog
        .schedule_surface_patch(key.clone(), catalog_request(&mirror, 71))
        .unwrap();
    let before_meshes = world.resource::<Assets<Mesh>>().len();
    let before_materials = world.resource::<Assets<StandardMaterial>>().len();
    let entity = catalog
        .apply_surface_reply(
            &mut world,
            &catalog_reply(&mirror, 71, &patch_json(&revision)),
        )
        .unwrap()
        .unwrap();
    let state = catalog.surface_patch_state();
    assert!(state.pending.is_empty());
    assert_eq!(state.ready, vec![key.clone()]);
    assert!(world.get_entity(entity).is_ok());
    assert_eq!(world.resource::<Assets<Mesh>>().len(), before_meshes + 1);
    assert_eq!(
        world.resource::<Assets<StandardMaterial>>().len(),
        before_materials + 1
    );
    assert!(catalog.fallback_surface_visible(&world));
    assert_eq!(world.get::<Visibility>(entity), Some(&Visibility::Visible));
    let mesh = world.get::<Mesh3d>(entity).unwrap();
    assert_eq!(
        world
            .resource::<Assets<Mesh>>()
            .get(&mesh.0)
            .unwrap()
            .indices()
            .unwrap()
            .len(),
        6,
        "suppression must be bounded to the ready patch's two triangles"
    );
    let material = world
        .get::<MeshMaterial3d<StandardMaterial>>(entity)
        .unwrap();
    assert_eq!(
        world
            .resource::<Assets<StandardMaterial>>()
            .get(&material.0)
            .unwrap()
            .depth_bias,
        1.0,
        "positive Bevy depth bias must pull only the ready patch ahead of covered fallback"
    );
}

#[test]
fn ready_patch_inserts_strip_geometry_as_an_independent_feature_entity() {
    let (mut world, mirror, mut catalog) = scene_catalog();
    let revision = mirror.initial().binding.source_revision.clone();
    let patch = patch_with_strip_json(&revision);
    let key = documents::surface_patch(&patch).unwrap().cache_key();
    catalog
        .set_desired_surface_patches(&mirror, vec![key.clone()])
        .unwrap();
    catalog
        .schedule_surface_patch(key, catalog_request(&mirror, 73))
        .unwrap();
    let before_meshes = world.resource::<Assets<Mesh>>().len();
    let before_materials = world.resource::<Assets<StandardMaterial>>().len();
    let before_entities = world.query::<&Mesh3d>().iter(&world).count();

    catalog
        .apply_surface_reply(&mut world, &catalog_reply(&mirror, 73, &patch))
        .unwrap()
        .unwrap();

    assert_eq!(world.resource::<Assets<Mesh>>().len(), before_meshes + 2);
    assert_eq!(
        world.resource::<Assets<StandardMaterial>>().len(),
        before_materials + 2
    );
    assert_eq!(
        world.query::<&Mesh3d>().iter(&world).count(),
        before_entities + 2
    );
    let identities = world
        .query::<&SurfaceFeatureVisual>()
        .iter(&world)
        .collect::<Vec<_>>();
    assert_eq!(identities.len(), 1);
    assert_eq!(identities[0].feature.kind, "channel_reach");
    assert_eq!(identities[0].generation, mirror.generation());
}

#[test]
fn stale_catalog_reply_is_rejected_before_asset_insertion() {
    // Catches generation checks performed after mutating Bevy asset storage.
    let (mut world, mut mirror, mut catalog) = scene_catalog();
    let revision = mirror.initial().binding.source_revision.clone();
    let key = documents::surface_patch(&patch_json(&revision))
        .unwrap()
        .cache_key();
    catalog
        .set_desired_surface_patches(&mirror, vec![key.clone()])
        .unwrap();
    catalog
        .schedule_surface_patch(key, catalog_request(&mirror, 72))
        .unwrap();
    let stale_reply = catalog_reply(&mirror, 72, &patch_json(&revision));
    mirror
        .reset(include_str!("../fixtures/initial.json"))
        .unwrap();
    catalog.reset(&mut world, &mirror).unwrap();
    let before = (
        world.resource::<Assets<Mesh>>().len(),
        world.resource::<Assets<StandardMaterial>>().len(),
    );
    assert!(
        catalog
            .apply_surface_reply(&mut world, &stale_reply)
            .unwrap()
            .is_none()
    );
    assert_eq!(
        before,
        (
            world.resource::<Assets<Mesh>>().len(),
            world.resource::<Assets<StandardMaterial>>().len()
        )
    );
}

#[test]
fn patch_spawn_uses_body_radius_and_source_sea_level_datum() {
    // Catches treating reference-datum elevation as height above sea level.
    let (mut world, mirror, mut catalog) = scene_catalog();
    assert_ne!(mirror.initial().tiles.sea_level_m, 0.0);
    let revision = mirror.initial().binding.source_revision.clone();
    let mut patch: serde_json::Value = serde_json::from_str(&patch_json(&revision)).unwrap();
    patch["samples"][0]["height_m"] = serde_json::json!(1000.0);
    let patch = patch.to_string();
    let key = documents::surface_patch(&patch).unwrap().cache_key();
    catalog
        .set_desired_surface_patches(&mirror, vec![key.clone()])
        .unwrap();
    catalog
        .schedule_surface_patch(key, catalog_request(&mirror, 81))
        .unwrap();
    let anchor_transform = Transform::from_translation(Vec3::new(3.0, 4.0, 5.0))
        .with_rotation(Quat::from_rotation_z(0.4));
    for mut transform in world
        .query_filtered::<&mut Transform, With<Mesh3d>>()
        .iter_mut(&mut world)
    {
        *transform = anchor_transform;
    }
    let entity = catalog
        .apply_surface_reply(&mut world, &catalog_reply(&mirror, 81, &patch))
        .unwrap()
        .unwrap();
    assert_eq!(*world.get::<Transform>(entity).unwrap(), anchor_transform);
    let mesh_handle = world.get::<Mesh3d>(entity).unwrap();
    let mesh = world
        .resource::<Assets<Mesh>>()
        .get(&mesh_handle.0)
        .unwrap();
    let Some(VertexAttributeValues::Float32x3(points)) = mesh.attribute(Mesh::ATTRIBUTE_POSITION)
    else {
        panic!("positions")
    };
    let anchor_radius_km = mirror
        .current()
        .unwrap()
        .astronomy
        .bodies
        .iter()
        .find(|body| body.id == "anchor")
        .unwrap()
        .radius_km
        .unwrap();
    let height_above_sea_km = (1000.0 - mirror.initial().tiles.sea_level_m) / 1000.0;
    let expected = ((anchor_radius_km + height_above_sea_km) / lifecycle::KM_PER_UNIT) as f32;
    assert!((Vec3::from(points[0]).length() - expected).abs() < 1e-5);
}

#[test]
fn ready_patch_overlays_visible_fallback_and_transition_retires_old_patch() {
    // Catches hiding the monolithic globe when only one bounded region is ready.
    let (mut world, mirror, mut catalog) = scene_catalog();
    let revision = mirror.initial().binding.source_revision.clone();
    let first = documents::surface_patch(&patch_json(&revision))
        .unwrap()
        .cache_key();
    catalog
        .set_desired_surface_patches(&mirror, vec![first.clone()])
        .unwrap();
    catalog
        .schedule_surface_patch(first.clone(), catalog_request(&mirror, 82))
        .unwrap();
    let first_entity = catalog
        .apply_surface_reply(
            &mut world,
            &catalog_reply(&mirror, 82, &patch_json(&revision)),
        )
        .unwrap()
        .unwrap();
    assert!(catalog.fallback_surface_visible(&world));
    assert_eq!(
        world.get::<Visibility>(first_entity),
        Some(&Visibility::Visible)
    );

    let mut second_patch: serde_json::Value = serde_json::from_str(&patch_json(&revision)).unwrap();
    second_patch["address"]["child_path"] = serde_json::json!([1]);
    let second_patch = second_patch.to_string();
    let second = documents::surface_patch(&second_patch).unwrap().cache_key();
    catalog
        .set_desired_surface_patches(&mirror, vec![second.clone()])
        .unwrap();
    assert_eq!(
        world.get::<Visibility>(first_entity),
        Some(&Visibility::Visible),
        "the old covered region remains patched until its replacement set is complete"
    );
    let mut second_request: serde_json::Value =
        serde_json::from_str(&catalog_request(&mirror, 84)).unwrap();
    second_request["address"]["child_path"] = serde_json::json!([1]);
    catalog
        .schedule_surface_patch(second.clone(), second_request.to_string())
        .unwrap();
    let second_entity = catalog
        .apply_surface_reply(&mut world, &catalog_reply(&mirror, 84, &second_patch))
        .unwrap()
        .unwrap();
    assert!(world.get_entity(first_entity).is_err());
    assert_eq!(
        world.get::<Visibility>(second_entity),
        Some(&Visibility::Visible)
    );
    assert!(catalog.fallback_surface_visible(&world));
}

#[test]
fn duplicate_schedule_preserves_the_original_pending_key() {
    // Catches BTreeMap::insert replacing the first request before returning Err.
    let (_world, mirror, mut catalog) = scene_catalog();
    let revision = mirror.initial().binding.source_revision.clone();
    let first = documents::surface_patch(&patch_json(&revision))
        .unwrap()
        .cache_key();
    let mut second = first.clone();
    second.child_path = vec![1];
    catalog
        .set_desired_surface_patches(&mirror, vec![first.clone(), second.clone()])
        .unwrap();
    catalog
        .schedule_surface_patch(first.clone(), catalog_request(&mirror, 83))
        .unwrap();
    let mut duplicate: serde_json::Value =
        serde_json::from_str(&catalog_request(&mirror, 83)).unwrap();
    duplicate["address"]["child_path"] = serde_json::json!([1]);
    assert!(
        catalog
            .schedule_surface_patch(second, duplicate.to_string())
            .is_err()
    );
    assert_eq!(catalog.surface_patch_state().pending, vec![first]);
}

#[test]
fn retired_current_generation_reply_is_an_explicit_noop() {
    // Catches reporting an expected late reply as current identity corruption.
    let (mut world, mirror, mut catalog) = scene_catalog();
    let revision = mirror.initial().binding.source_revision.clone();
    let first = documents::surface_patch(&patch_json(&revision))
        .unwrap()
        .cache_key();
    let mut second = first.clone();
    second.child_path = vec![1];
    catalog
        .set_desired_surface_patches(&mirror, vec![first.clone()])
        .unwrap();
    catalog
        .schedule_surface_patch(first, catalog_request(&mirror, 85))
        .unwrap();
    catalog
        .set_desired_surface_patches(&mirror, vec![second])
        .unwrap();
    assert!(
        catalog
            .apply_surface_reply(
                &mut world,
                &catalog_reply(&mirror, 85, &patch_json(&revision)),
            )
            .unwrap()
            .is_none()
    );
}

#[test]
fn malformed_current_generation_identity_is_not_swallowed_as_late() {
    // Catches live treating every Binding error as an expected retired reply.
    let (mut world, mirror, mut catalog) = scene_catalog();
    let revision = mirror.initial().binding.source_revision.clone();
    let key = documents::surface_patch(&patch_json(&revision))
        .unwrap()
        .cache_key();
    catalog
        .set_desired_surface_patches(&mirror, vec![key.clone()])
        .unwrap();
    catalog
        .schedule_surface_patch(key, catalog_request(&mirror, 86))
        .unwrap();
    let mut corrupt: serde_json::Value =
        serde_json::from_str(&catalog_reply(&mirror, 86, &patch_json(&revision))).unwrap();
    corrupt["binding"]["source_id"] = serde_json::json!("wrong-current-source");
    assert!(
        catalog
            .apply_surface_reply(&mut world, &corrupt.to_string())
            .is_err()
    );
}
