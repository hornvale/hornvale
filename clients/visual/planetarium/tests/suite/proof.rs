use planetarium::review::{
    CapturedFrames, SurfaceProofMetrics, compare_surface_review, run_surface_proof,
};
use std::collections::BTreeSet;

#[test]
fn proof_region_contains_required_cases() {
    let metrics = run_surface_proof(42).unwrap();
    assert_eq!(metrics.seed, 42);
    assert_eq!(metrics.refinement_levels, vec![0, 1]);
    assert!(metrics.patch_count >= 5);
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
fn capture_provenance_names_source_owned_ground() {
    let capture = include_str!("../../src/capture.rs");
    assert!(capture.contains("source-owned coherent ground"));
    assert!(capture.contains("dynamic weather remains deferred"));
}

#[allow(dead_code)]
fn _api_shape_is_public(metrics: SurfaceProofMetrics) {
    let _ = metrics.seed;
}
