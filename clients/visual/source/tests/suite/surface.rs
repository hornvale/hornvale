use hornvale_kernel::{Facet, World};
use hornvale_visual_source::{Source, SourceError};
use hornvale_worldgen::SurfaceRealizationContext;
use serde_json::{Value, json};
use std::path::PathBuf;

const REV: &str = "4e06e33492a82e245aec899559b8cdf33ac7fdcd";

fn fixture() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../cli/tests/fixtures/world-seed-42.json")
}

fn surface_revision() -> Value {
    let world = World::from_json(&std::fs::read_to_string(fixture()).unwrap()).unwrap();
    let revision = SurfaceRealizationContext::build(&world).unwrap().revision;
    json!({
        "source_revision": revision.source_revision,
        "algorithm_version": revision.algorithm_version,
        "configuration_hash_hex": revision.configuration_hash.iter().map(|byte| format!("{byte:02x}")).collect::<String>()
    })
}

fn request(binding: &Value, expected_revision: &Value, request_id: u64) -> String {
    let macro_face = Facet {
        face: 0,
        path: vec![0; 6],
    }
    .pack()
    .unwrap()
    .0 as u32;
    json!({
        "schema": "visual/surface-request/v1",
        "binding": binding,
        "request_id": request_id,
        "address": {"macro_face": macro_face, "child_path": [1]},
        "expected_revision": expected_revision
    })
    .to_string()
}

fn source_and_binding() -> (Source, Value) {
    let mut source = Source::open(&fixture(), REV, "surface-test").unwrap();
    let initial: Value = serde_json::from_str(&source.initial_document(16).unwrap()).unwrap();
    (source, initial["binding"].clone())
}

#[test]
fn surface_document_is_stable() {
    let (mut source, binding) = source_and_binding();
    let revision = surface_revision();
    let request = request(&binding, &revision, 17);
    let first = source.observe_surface(&request).unwrap();
    let second = source.observe_surface(&request).unwrap();
    assert_eq!(first, second);
    assert!(first.contains("\"scene/surface/v1\""));
}

#[test]
fn surface_rejects_wrong_binding() {
    let (mut source, mut binding) = source_and_binding();
    binding["scope_id"] = json!("wrong-scope");
    let error = source
        .observe_surface(&request(&binding, &surface_revision(), 18))
        .unwrap_err();
    assert!(matches!(error, SourceError::InvalidRequest(message) if message.contains("binding")));
}

#[test]
fn surface_rejects_stale_revision() {
    let (mut source, binding) = source_and_binding();
    let mut revision = surface_revision();
    revision["source_revision"] = json!("0".repeat(64));
    let error = source
        .observe_surface(&request(&binding, &revision, 19))
        .unwrap_err();
    assert!(matches!(error, SourceError::InvalidRequest(message) if message.contains("revision")));
}

#[test]
fn surface_preserves_request_id() {
    let (mut source, binding) = source_and_binding();
    let reply: Value = serde_json::from_str(
        &source
            .observe_surface(&request(&binding, &surface_revision(), 4_294_967_297))
            .unwrap(),
    )
    .unwrap();
    assert_eq!(reply["request_id"], 4_294_967_297u64);
    assert_eq!(reply["schema"], "visual/surface-reply/v1");
}

#[test]
fn surface_contains_no_weather_hooks() {
    let (mut source, binding) = source_and_binding();
    let document = source
        .observe_surface(&request(&binding, &surface_revision(), 20))
        .unwrap();
    for hook in ["weather", "cloud", "precip", "roughness"] {
        assert!(!document.contains(hook), "surface transport contains {hook}");
    }
}
