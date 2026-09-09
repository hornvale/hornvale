//! Observation manifests refuse ambiguous claims at their file boundary.

use hornvale::observations::{
    Approval, CapabilityState, EpisodeManifest, ObservationStatus, TimeWindow, VisualGrammar,
    read_manifest, validate_manifest,
};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};

static NEXT_TEMP_FILE: AtomicU64 = AtomicU64::new(0);

fn valid_manifest() -> EpisodeManifest {
    EpisodeManifest {
        id: "HV-001".to_string(),
        title: "Where the high country gathers water".to_string(),
        object: "geography".to_string(),
        scale: "world".to_string(),
        primary_axis: "spatial arrangement".to_string(),
        phenomenon: "drainage basins".to_string(),
        visual_grammar: VisualGrammar::Spatial,
        observation_sentence: "Ridges divide the world's drainage basins.".to_string(),
        world_revision: "test-revision".to_string(),
        seed: 42,
        time_window: Some(TimeWindow {
            start_day: 0.0,
            end_day: 1.0,
        }),
        frame_count: 120,
        frame_rate: 30.0,
        source_commands: vec!["hornvale map --world world.json --field elevation".to_string()],
        evidence_status: ObservationStatus::Draft,
        approval: None,
        capability_state: CapabilityState::Existing,
        comparison_reference: None,
        caption_draft: vec!["The ridges are doing more than looking dramatic.".to_string()],
    }
}

fn temp_manifest(tag: &str, json: &str) -> PathBuf {
    let serial = NEXT_TEMP_FILE.fetch_add(1, Ordering::Relaxed);
    let path = std::env::temp_dir().join(format!(
        "hornvale-observation-{tag}-{}-{serial}.json",
        std::process::id()
    ));
    std::fs::write(&path, json).expect("write temporary manifest");
    path
}

fn manifest_json(overrides: &[(&str, serde_json::Value)]) -> String {
    let mut value = serde_json::json!({
        "id": "HV-001",
        "title": "Where the high country gathers water",
        "object": "geography",
        "scale": "world",
        "primary_axis": "spatial arrangement",
        "phenomenon": "drainage basins",
        "visual_grammar": "spatial",
        "observation_sentence": "Ridges divide the world's drainage basins.",
        "world_revision": "test-revision",
        "seed": 42,
        "time_window": { "start_day": 0.0, "end_day": 1.0 },
        "frame_count": 120,
        "frame_rate": 30.0,
        "source_commands": ["hornvale map --world world.json --field elevation"],
        "evidence_status": "draft",
        "approval": null,
        "capability_state": "existing",
        "comparison_reference": null,
        "caption_draft": ["The ridges are doing more than looking dramatic."]
    });
    let object = value.as_object_mut().expect("manifest object");
    for (key, replacement) in overrides {
        if replacement.is_null() && *key != "approval" && *key != "comparison_reference" {
            object.remove(*key);
        } else {
            object.insert((*key).to_string(), replacement.clone());
        }
    }
    serde_json::to_string_pretty(&value).expect("serialize test manifest")
}

fn read_error(tag: &str, overrides: &[(&str, serde_json::Value)]) -> String {
    let path = temp_manifest(tag, &manifest_json(overrides));
    let error = read_manifest(&path).expect_err("manifest must be refused");
    std::fs::remove_file(path).expect("remove temporary manifest");
    error.to_string()
}

#[test]
fn observations_valid_spatial_manifest_is_accepted() {
    assert_eq!(validate_manifest(&valid_manifest()), Ok(()));
}

#[test]
fn observations_missing_object_or_scale_is_refused() {
    for field in ["object", "scale"] {
        let error = read_error(field, &[(field, serde_json::Value::Null)]);
        assert!(error.contains(field), "{field} error was: {error}");
    }
}

#[test]
fn observations_zero_frame_count_is_refused() {
    let error = read_error("zero-frames", &[("frame_count", serde_json::json!(0))]);
    assert!(error.contains("frame_count"), "error was: {error}");
}

#[test]
fn observations_non_finite_frame_rate_and_time_are_refused() {
    let mut manifest = valid_manifest();
    manifest.frame_rate = f64::NAN;
    assert!(
        validate_manifest(&manifest)
            .expect_err("NaN frame rate must fail")
            .to_string()
            .contains("frame_rate")
    );

    manifest.frame_rate = 30.0;
    manifest.time_window = Some(TimeWindow {
        start_day: f64::NEG_INFINITY,
        end_day: 1.0,
    });
    assert!(
        validate_manifest(&manifest)
            .expect_err("infinite time must fail")
            .to_string()
            .contains("time_window.start_day")
    );
}

#[test]
fn observations_empty_source_command_list_is_refused() {
    let error = read_error(
        "empty-commands",
        &[("source_commands", serde_json::json!([]))],
    );
    assert!(error.contains("source_commands"), "error was: {error}");
}

#[test]
fn observations_unknown_visual_grammar_is_refused() {
    let error = read_error(
        "unknown-grammar",
        &[("visual_grammar", serde_json::json!("panorama"))],
    );
    assert!(error.contains("visual_grammar"), "error was: {error}");
}

#[test]
fn observations_published_status_without_approval_is_refused() {
    let error = read_error(
        "published-unapproved",
        &[("evidence_status", serde_json::json!("published"))],
    );
    assert!(error.contains("approval"), "error was: {error}");
}

#[test]
fn observations_distinct_social_units_cannot_be_substituted() {
    let error = read_error(
        "unit-substitution",
        &[
            ("object", serde_json::json!("population")),
            ("scale", serde_json::json!("settlement")),
        ],
    );
    assert!(error.contains("object"), "error was: {error}");
    assert!(error.contains("scale"), "error was: {error}");
}

#[test]
fn observations_comparison_metadata_does_not_replace_public_claim_fields() {
    let path = temp_manifest(
        "comparison",
        &manifest_json(&[(
            "comparison_reference",
            serde_json::json!({
                "source": "internal catalogue",
                "note": "research only"
            }),
        )]),
    );
    let manifest = read_manifest(&path).expect("comparison metadata is valid internally");
    std::fs::remove_file(path).expect("remove temporary manifest");
    assert_eq!(manifest.title, "Where the high country gathers water");
    assert_eq!(
        manifest.observation_sentence,
        "Ridges divide the world's drainage basins."
    );
    let comparison = manifest
        .comparison_reference
        .expect("comparison metadata remains attached internally");
    assert_eq!(comparison["source"], "internal catalogue");
    assert_eq!(comparison["note"], "research only");
}

#[test]
fn observations_validate_cli_reports_identity_and_shape() {
    let path = temp_manifest("cli-valid", &manifest_json(&[]));
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["observations", "validate", "--manifest"])
        .arg(&path)
        .output()
        .expect("run observations validate");
    std::fs::remove_file(path).expect("remove temporary manifest");
    assert!(out.status.success(), "command failed: {out:?}");
    assert_eq!(
        String::from_utf8(out.stdout).expect("utf-8 stdout"),
        "validated observation HV-001: object=geography scale=world axis=spatial arrangement frames=120\n"
    );
}

#[test]
fn observations_validate_cli_names_path_and_field_on_failure() {
    let path = temp_manifest(
        "cli-invalid",
        &manifest_json(&[("frame_count", serde_json::json!(0))]),
    );
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["observations", "validate", "--manifest"])
        .arg(&path)
        .output()
        .expect("run observations validate");
    std::fs::remove_file(&path).expect("remove temporary manifest");
    assert!(!out.status.success(), "invalid manifest was accepted");
    let stderr = String::from_utf8(out.stderr).expect("utf-8 stderr");
    assert!(
        stderr.contains(path.to_str().expect("utf-8 path")),
        "{stderr}"
    );
    assert!(stderr.contains("frame_count"), "{stderr}");
}

#[test]
fn observations_approved_manifest_carries_editorial_record() {
    let mut manifest = valid_manifest();
    manifest.evidence_status = ObservationStatus::Approved;
    manifest.approval = Some(Approval {
        reviewer: "Nathan".to_string(),
        approved_at: "2026-09-09T12:00:00Z".to_string(),
    });
    assert_eq!(validate_manifest(&manifest), Ok(()));
}

#[test]
fn observations_fixture_is_a_valid_internal_record() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root");
    let manifest = read_manifest(&root.join("observations/episodes/HV-001.json"))
        .expect("committed fixture validates");
    assert_eq!(manifest.id, "HV-001");
}
