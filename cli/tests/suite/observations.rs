//! Observation manifests refuse ambiguous claims at their file boundary.

use hornvale::observations::{
    Approval, CapabilityState, EpisodeManifest, EvidenceStatus, ObservationStatus, TimeWindow,
    VisualGrammar, read_manifest, validate_manifest,
};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};

static NEXT_TEMP_FILE: AtomicU64 = AtomicU64::new(0);

fn temp_output_dir(tag: &str) -> PathBuf {
    let serial = NEXT_TEMP_FILE.fetch_add(1, Ordering::Relaxed);
    std::env::temp_dir().join(format!(
        "hornvale-observation-{tag}-{}-{serial}",
        std::process::id()
    ))
}

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
        evidence_status: EvidenceStatus::Draft,
        capability_state: CapabilityState::Existing,
        controlled_inputs: serde_json::Map::from_iter([(
            "seed".to_string(),
            serde_json::json!(42),
        )]),
        comparison_reference: None,
        lead_time: None,
        source_data: vec!["hornvale map output".to_string()],
        render_output: None,
        caption_draft: vec!["The ridges are doing more than looking dramatic.".to_string()],
        editorial_status: ObservationStatus::Draft,
        approval: None,
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
        "controlled_inputs": { "seed": 42 },
        "lead_time": null,
        "source_data": ["hornvale underworld stdout"],
        "render_output": null,
        "editorial_status": "draft",
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
fn observations_unknown_manifest_field_is_refused() {
    let json = manifest_json(&[("surprise", serde_json::json!(true))]);
    let path = temp_manifest("unknown-field", &json);
    let error = read_manifest(&path)
        .expect_err("unknown fields must not cross the manifest boundary")
        .to_string();
    std::fs::remove_file(path).expect("remove temporary manifest");
    assert!(error.contains("surprise"), "error was: {error}");
}

#[test]
fn observations_duplicate_manifest_key_is_refused() {
    let json = manifest_json(&[]);
    let needle = "  \"title\": \"Where the high country gathers water\",";
    assert!(json.contains(needle), "title line exists before mutation");
    let duplicated = json.replacen(needle, &format!("{needle}\n{needle}"), 1);
    let path = temp_manifest("duplicate-key", &duplicated);
    let error = read_manifest(&path)
        .expect_err("duplicate keys must not cross the manifest boundary")
        .to_string();
    std::fs::remove_file(path).expect("remove temporary manifest");
    assert!(error.contains("duplicate"), "error was: {error}");
    assert!(error.contains("title"), "error was: {error}");
}

#[test]
fn observations_duplicate_nested_metadata_key_is_refused() {
    let json = manifest_json(&[]);
    let needle = "    \"seed\": 42";
    assert!(
        json.contains(needle),
        "controlled-input seed exists before mutation"
    );
    let duplicated = json.replacen(needle, &format!("{needle},\n{needle}"), 1);
    let path = temp_manifest("duplicate-nested-key", &duplicated);
    let error = read_manifest(&path)
        .expect_err("nested duplicate keys must not cross the manifest boundary")
        .to_string();
    std::fs::remove_file(path).expect("remove temporary manifest");
    assert!(error.contains("duplicate key `seed`"), "error was: {error}");
}

#[test]
fn observations_published_status_without_approval_is_refused() {
    let error = read_error(
        "published-unapproved",
        &[("editorial_status", serde_json::json!("published"))],
    );
    assert!(error.contains("approval"), "error was: {error}");
}

#[test]
fn observations_evidence_and_editorial_status_are_independent() {
    let path = temp_manifest(
        "independent-statuses",
        &manifest_json(&[("evidence_status", serde_json::json!("reviewed"))]),
    );
    let manifest =
        read_manifest(&path).expect("reviewed evidence does not imply editorial approval metadata");
    std::fs::remove_file(path).expect("remove temporary manifest");
    assert_eq!(manifest.evidence_status, EvidenceStatus::Reviewed);
    assert_eq!(manifest.editorial_status, ObservationStatus::Draft);
}

#[test]
fn observations_evidence_status_rejects_editorial_only_values() {
    for status in ["approved", "published"] {
        let error = read_error(
            "editorial-evidence-status",
            &[
                ("evidence_status", serde_json::json!(status)),
                ("editorial_status", serde_json::json!("approved")),
                (
                    "approval",
                    serde_json::json!({
                        "reviewer": "Nathan",
                        "approved_at": "2026-09-09T12:00:00Z"
                    }),
                ),
            ],
        );
        assert!(
            error.contains("evidence_status"),
            "status {status} produced: {error}"
        );
    }
}

#[test]
fn observations_approved_editorial_status_requires_approval() {
    let error = read_error(
        "approved-without-record",
        &[("editorial_status", serde_json::json!("approved"))],
    );
    assert!(error.contains("approval"), "error was: {error}");
}

#[test]
fn observations_draft_editorial_status_forbids_approval() {
    let error = read_error(
        "draft-with-approval",
        &[(
            "approval",
            serde_json::json!({
                "reviewer": "Nathan",
                "approved_at": "2026-09-09T12:00:00Z"
            }),
        )],
    );
    assert!(error.contains("approval"), "error was: {error}");
}

#[test]
fn observations_approval_reviewer_must_be_nathan() {
    let error = read_error(
        "wrong-reviewer",
        &[
            ("editorial_status", serde_json::json!("approved")),
            (
                "approval",
                serde_json::json!({
                    "reviewer": "Editor",
                    "approved_at": "2026-09-09T12:00:00Z"
                }),
            ),
        ],
    );
    assert!(error.contains("approval.reviewer"), "error was: {error}");
}

#[test]
fn observations_approval_timestamp_is_strict_utc_second_precision() {
    for timestamp in [
        "2026-9-9T12:00:00Z",
        "2026-09-09T12:00:00-04:00",
        "2026-09-09T12:00:00.000Z",
        "2026-02-30T12:00:00Z",
        "2026-09-09T25:00:00Z",
    ] {
        let error = read_error(
            "bad-timestamp",
            &[
                ("editorial_status", serde_json::json!("approved")),
                (
                    "approval",
                    serde_json::json!({
                        "reviewer": "Nathan",
                        "approved_at": timestamp
                    }),
                ),
            ],
        );
        assert!(
            error.contains("approval.approved_at"),
            "timestamp {timestamp} produced: {error}"
        );
    }
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
fn observations_validate_cli_rejects_trailing_arguments() {
    let path = temp_manifest("cli-extra", &manifest_json(&[]));
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["observations", "validate", "--manifest"])
        .arg(&path)
        .arg("unexpected")
        .output()
        .expect("run observations validate");
    std::fs::remove_file(path).expect("remove temporary manifest");
    assert!(!out.status.success(), "unexpected argument was accepted");
    let stderr = String::from_utf8(out.stderr).expect("utf-8 stderr");
    assert!(stderr.contains("unexpected"), "{stderr}");
}

#[test]
fn observations_approved_manifest_carries_editorial_record() {
    let mut manifest = valid_manifest();
    manifest.editorial_status = ObservationStatus::Approved;
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

#[test]
fn observations_fixture_source_command_runs_from_repository_root() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root");
    let manifest = read_manifest(&root.join("observations/episodes/HV-001.json"))
        .expect("committed fixture validates");
    assert_eq!(manifest.source_commands.len(), 1);
    let command = manifest.source_commands[0]
        .strip_prefix("cargo run -p hornvale -- ")
        .expect("fixture command uses the repository-root cargo form");
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(command.split_whitespace())
        .current_dir(root)
        .output()
        .expect("execute fixture source command through the current binary");
    assert!(out.status.success(), "fixture command failed: {out:?}");
}

#[test]
fn observations_export_is_contiguous_identified_and_byte_deterministic() {
    let manifest = temp_manifest(
        "export",
        &manifest_json(&[
            ("frame_count", serde_json::json!(3)),
            (
                "source_commands",
                serde_json::json!(["cargo run -p hornvale -- underworld --seed 42"]),
            ),
        ]),
    );
    let first = temp_output_dir("export-first");
    let second = temp_output_dir("export-second");

    for output_dir in [&first, &second] {
        let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
            .args(["observations", "export", "--manifest"])
            .arg(&manifest)
            .arg("--out")
            .arg(output_dir)
            .output()
            .expect("run observations export");
        assert!(out.status.success(), "command failed: {out:?}");
    }

    let expected_names = ["frame-000.json", "frame-001.json", "frame-002.json"];
    for (index, name) in expected_names.iter().enumerate() {
        let bytes = std::fs::read(first.join(name)).expect("read first exported packet");
        let packet: serde_json::Value =
            serde_json::from_slice(&bytes).expect("packet is valid JSON");
        assert_eq!(packet["frame_index"], index);
        assert_eq!(packet["episode_id"], "HV-001");
        assert_eq!(packet["world_seed"], "42");
        assert_eq!(packet["world_revision"], "test-revision");
        assert!(
            packet["source_digest"]
                .as_str()
                .is_some_and(|digest| !digest.is_empty()),
            "packet must bind itself to authoritative source bytes: {packet}"
        );
        assert_eq!(
            bytes,
            std::fs::read(second.join(name)).expect("read second exported packet"),
            "repeated export changed {name}"
        );
    }
    assert_eq!(
        std::fs::read_dir(&first)
            .expect("read first output directory")
            .count(),
        expected_names.len(),
        "export must emit exactly frame_count packets"
    );

    std::fs::remove_file(manifest).expect("remove temporary manifest");
    std::fs::remove_dir_all(first).expect("remove first export");
    std::fs::remove_dir_all(second).expect("remove second export");
}

#[test]
fn observations_export_missing_manifest_does_not_create_output_directory() {
    let manifest = temp_output_dir("missing-manifest").join("absent.json");
    let output_dir = temp_output_dir("missing-manifest-output");
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["observations", "export", "--manifest"])
        .arg(&manifest)
        .arg("--out")
        .arg(&output_dir)
        .output()
        .expect("run observations export with missing manifest");

    assert!(!out.status.success(), "missing manifest was accepted");
    assert!(
        !output_dir.exists(),
        "output directory was created before the manifest was read"
    );
}

#[test]
fn observations_export_refuses_needs_simulation_extension() {
    let manifest = temp_manifest(
        "simulation-extension",
        &manifest_json(&[
            ("frame_count", serde_json::json!(1)),
            (
                "source_commands",
                serde_json::json!(["cargo run -p hornvale -- underworld --seed 42"]),
            ),
            (
                "capability_state",
                serde_json::json!("needs_simulation_extension"),
            ),
        ]),
    );
    let output_dir = temp_output_dir("simulation-extension-output");
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["observations", "export", "--manifest"])
        .arg(&manifest)
        .arg("--out")
        .arg(&output_dir)
        .output()
        .expect("run observations export for unsupported capability state");

    assert!(!out.status.success(), "simulation extension was downgraded");
    let stderr = String::from_utf8(out.stderr).expect("utf-8 stderr");
    assert!(stderr.contains("needs_simulation_extension"), "{stderr}");
    assert!(!output_dir.exists(), "refused export created output");

    std::fs::remove_file(manifest).expect("remove temporary manifest");
}

#[test]
fn observations_export_refuses_every_non_existing_capability_state_by_name() {
    for (tag, state) in [
        ("observation-surface", "needs_observation_surface"),
        ("renderer", "needs_renderer"),
        ("simulation-extension", "needs_simulation_extension"),
    ] {
        let manifest = temp_manifest(
            tag,
            &manifest_json(&[
                ("frame_count", serde_json::json!(1)),
                (
                    "source_commands",
                    serde_json::json!(["cargo run -p hornvale -- underworld --seed 42"]),
                ),
                ("capability_state", serde_json::json!(state)),
            ]),
        );
        let output_dir = temp_output_dir(tag);
        let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
            .args(["observations", "export", "--manifest"])
            .arg(&manifest)
            .arg("--out")
            .arg(&output_dir)
            .output()
            .expect("run observations export for unsupported capability state");

        assert!(!out.status.success(), "{state} was accepted");
        let stderr = String::from_utf8(out.stderr).expect("utf-8 stderr");
        assert!(stderr.contains(state), "{state} was not named: {stderr}");
        assert!(!output_dir.exists(), "{state} created output");
        std::fs::remove_file(manifest).expect("remove temporary manifest");
    }
}

#[test]
fn observations_export_reconciles_owned_frames_without_touching_unrelated_files() {
    let manifest = temp_manifest(
        "stale-frames",
        &manifest_json(&[
            ("frame_count", serde_json::json!(3)),
            (
                "source_commands",
                serde_json::json!(["cargo run -p hornvale -- underworld --seed 42"]),
            ),
        ]),
    );
    let output_dir = temp_output_dir("stale-frames-output");
    let first = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["observations", "export", "--manifest"])
        .arg(&manifest)
        .arg("--out")
        .arg(&output_dir)
        .output()
        .expect("run initial observations export");
    assert!(first.status.success(), "initial export failed: {first:?}");
    std::fs::write(output_dir.join("notes.txt"), "unrelated").expect("write unrelated file");

    let manifest_one = temp_manifest(
        "stale-frames-one",
        &manifest_json(&[
            ("frame_count", serde_json::json!(1)),
            (
                "source_commands",
                serde_json::json!(["cargo run -p hornvale -- underworld --seed 42"]),
            ),
        ]),
    );
    let second = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["observations", "export", "--manifest"])
        .arg(&manifest_one)
        .arg("--out")
        .arg(&output_dir)
        .output()
        .expect("rerun observations export with fewer frames");
    assert!(second.status.success(), "rerun export failed: {second:?}");
    assert!(output_dir.join("frame-000.json").is_file());
    assert!(!output_dir.join("frame-001.json").exists());
    assert!(!output_dir.join("frame-002.json").exists());
    assert_eq!(
        std::fs::read_to_string(output_dir.join("notes.txt")).expect("read unrelated file"),
        "unrelated"
    );

    std::fs::remove_file(manifest).expect("remove first temporary manifest");
    std::fs::remove_file(manifest_one).expect("remove second temporary manifest");
    std::fs::remove_dir_all(output_dir).expect("remove stale frame export");
}

#[test]
fn observations_hv_001_fixture_is_producer_backed_and_contains_no_client_classification() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root");
    let manifest_path = root.join("observations/episodes/HV-001.json");
    let expected_path = root.join("observations/fixtures/HV-001/expected-frame-000.json");
    let output_dir = temp_output_dir("fixture-export");

    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["observations", "export", "--manifest"])
        .arg(&manifest_path)
        .arg("--out")
        .arg(&output_dir)
        .output()
        .expect("export committed HV-001 manifest");
    assert!(out.status.success(), "fixture export failed: {out:?}");

    let expected = std::fs::read(&expected_path).expect("read committed frame fixture");
    let actual = std::fs::read(output_dir.join("frame-000.json"))
        .expect("read freshly exported first frame");
    assert_eq!(actual, expected, "committed fixture drifted from producer");

    let packet: serde_json::Value =
        serde_json::from_slice(&expected).expect("fixture is valid JSON");
    let label_keys: Vec<_> = packet["labels"]
        .as_object()
        .expect("labels object")
        .keys()
        .map(String::as_str)
        .collect();
    assert_eq!(
        label_keys,
        ["object", "observation_sentence", "primary_axis", "scale"],
        "renderer labels must remain authored manifest fields"
    );
    let spatial_keys: Vec<_> = packet["spatial"]
        .as_object()
        .expect("spatial observation object")
        .keys()
        .map(String::as_str)
        .collect();
    assert_eq!(
        spatial_keys,
        ["readout", "source"],
        "spatial evidence must remain the producer readout, without client classification"
    );

    std::fs::remove_dir_all(output_dir).expect("remove fixture export");
}
