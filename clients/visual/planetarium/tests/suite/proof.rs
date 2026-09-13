use hornvale_bevy_view::{
    CameraPose, camera::OrbitCamera, documents::SurfacePatchRevision,
    lifecycle::visible_surface_patches,
};
use planetarium::review::{
    CapturedFrames, SurfaceProofMetrics, compare_surface_review, run_rendered_surface_proof,
    run_surface_proof,
};
use std::collections::BTreeSet;

#[test]
fn proof_region_contains_required_cases() {
    let metrics = run_surface_proof(42).unwrap();
    assert_eq!(metrics.seed, 42);
    assert_eq!(metrics.refinement_levels, vec![0, 1]);
    assert_eq!(metrics.patch_count, 13);
}

#[test]
fn proof_records_all_three_cost_dimensions() {
    let metrics = run_surface_proof(42).unwrap();
    assert_eq!(metrics.generation_latency_ms.len(), metrics.patch_count);
    assert_eq!(metrics.peak_memory_bytes.len(), metrics.patch_count);
    assert_eq!(metrics.frame_time_ms.len(), metrics.patch_count);
    assert!(metrics.generation_latency_ms.iter().all(|value| *value > 0));
    assert!(metrics.peak_memory_bytes.iter().all(|value| *value > 0));
    assert!(metrics.frame_time_ms.iter().all(|value| *value > 0.0));
}

#[test]
fn before_after_review_checks_required_features() {
    let before = CapturedFrames::default();
    let after = CapturedFrames {
        required_features: BTreeSet::from([
            "confluence".into(),
            "terminal_basin".into(),
            "coast_crossing".into(),
            "face_corner".into(),
            "unequal_lod".into(),
        ]),
        seams_coherent: true,
        rivers_reach_declared_ends: true,
        coast_is_continuous: true,
        mountain_direction_reads: true,
        biome_transitions_are_blended: true,
    };
    let review = compare_surface_review(&before, &after);
    assert!(review.required_features_visible);
    assert!(review.seams_coherent);
    assert!(review.rivers_reach_declared_ends);
    assert!(review.coast_is_continuous);
    assert!(review.mountain_direction_reads);
    assert!(review.biome_transitions_are_blended);
}

#[test]
fn before_capture_must_differ_from_after_capture() {
    let after = CapturedFrames {
        required_features: BTreeSet::from([
            "confluence".into(),
            "terminal_basin".into(),
            "coast_crossing".into(),
            "face_corner".into(),
            "unequal_lod".into(),
        ]),
        seams_coherent: true,
        rivers_reach_declared_ends: true,
        coast_is_continuous: true,
        mountain_direction_reads: true,
        biome_transitions_are_blended: true,
    };
    let review = compare_surface_review(&after, &after);
    assert!(!review.required_features_visible);
    assert!(!review.seams_coherent);
    assert!(!review.rivers_reach_declared_ends);
    assert!(!review.coast_is_continuous);
    assert!(!review.mountain_direction_reads);
    assert!(!review.biome_transitions_are_blended);
}

#[test]
fn rendered_proof_reads_distinct_frames_and_patch_readiness() {
    let proof = run_rendered_surface_proof(42).unwrap();
    assert_ne!(proof.before.png_sha256, proof.after.png_sha256, "{proof:?}");
    assert!(proof.after.patch_entities > 0);
    assert!(proof.after.narrow_feature_entities > 0);
    assert!(
        proof.after.narrow_feature_pixels > 0,
        "feature geometry did not change PNG pixels: {proof:?}"
    );
    assert!(!proof.after.fallback_visible);
    assert_eq!(proof.before.camera_sha256, proof.after.camera_sha256);
    assert!(!proof.after.source_revision.is_empty());
    assert_eq!(proof.before.source_revision, proof.after.source_revision);
    assert!(!proof.source_generation_ms.is_empty());
    assert!(proof.mesh_material_application_ms > 0.0);
    assert!(proof.first_visible_frame_ms > 0.0);
    assert!(proof.steady_state_frame_ms > 0.0);
    assert!(proof.peak_rss_bytes > 0);
}

#[test]
fn capture_provenance_names_source_owned_ground() {
    let capture = include_str!("../../src/capture.rs");
    assert!(capture.contains("source-owned coherent ground"));
    assert!(capture.contains("dynamic weather remains deferred"));
}

#[test]
fn visible_patch_addresses_are_stable_capture_metadata() {
    // Catches a selector whose capture identity varies with request completion.
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
    let addresses = visible_surface_patches(&camera, [0.0; 3], 7_000.0, &revision).unwrap();
    let first = serde_json::to_vec(&addresses).unwrap();
    let second = serde_json::to_vec(
        &visible_surface_patches(&camera, [0.0; 3], 7_000.0, &revision).unwrap(),
    )
    .unwrap();
    assert_eq!(first, second);
    assert!(!first.is_empty());
}

#[allow(dead_code)]
fn _api_shape_is_public(metrics: SurfaceProofMetrics) {
    let _ = metrics.seed;
}
