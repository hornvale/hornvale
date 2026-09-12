use hornvale_kernel::Facet;
use hornvale_visual_source::{Source, SourceError};
use serde::Deserialize;
use serde_json::{Value, json, value::RawValue};
use std::collections::BTreeSet;
use std::path::PathBuf;

const REV: &str = "4e06e33492a82e245aec899559b8cdf33ac7fdcd";

fn fixture() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../cli/tests/fixtures/world-seed-42.json")
}

fn request(
    binding: &Value,
    expected_revision: &Value,
    request_id: u64,
    child_path: &[u8],
) -> String {
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
        "generation": 0,
        "address": {"macro_face": macro_face, "child_path": child_path},
        "expected_revision": expected_revision
    })
    .to_string()
}

fn request_with_transition(
    binding: &Value,
    expected_revision: &Value,
    request_id: u64,
    address: &Facet,
    child_path: &[u8],
    transition: &Facet,
    transition_child_path: &[u8],
) -> String {
    let macro_face = address.pack().unwrap().0 as u32;
    let transition_macro_face = transition.pack().unwrap().0 as u32;
    json!({
        "schema": "visual/surface-request/v1",
        "binding": binding,
        "request_id": request_id,
        "generation": 0,
        "address": {"macro_face": macro_face, "child_path": child_path},
        "transition_address": {"macro_face": transition_macro_face, "child_path": transition_child_path},
        "expected_revision": expected_revision
    })
    .to_string()
}

fn source_and_initial() -> (Source, Value) {
    let mut source = Source::open(&fixture(), REV, "surface-test").unwrap();
    let initial: Value = serde_json::from_str(&source.initial_document(16).unwrap()).unwrap();
    (source, initial)
}

fn keys(value: &Value) -> BTreeSet<&str> {
    value
        .as_object()
        .unwrap()
        .keys()
        .map(String::as_str)
        .collect()
}

#[derive(Deserialize)]
struct SurfaceReply<'a> {
    #[serde(borrow)]
    patch: &'a RawValue,
}

fn patch_bytes(reply: &str) -> String {
    serde_json::from_str::<SurfaceReply<'_>>(reply)
        .unwrap()
        .patch
        .get()
        .to_owned()
}

#[test]
fn initial_bootstraps_surface_revision_matching_binding() {
    let (mut source, initial) = source_and_initial();
    let binding = &initial["binding"];
    let revision = &initial["surface_revision"];

    assert_eq!(
        keys(&initial),
        BTreeSet::from([
            "schema",
            "binding",
            "surface_revision",
            "system",
            "moons",
            "tiles",
            "ticks_per_std_day"
        ])
    );
    assert_eq!(initial["schema"], "visual/initial/v1");
    assert_eq!(revision["source_revision"], binding["source_revision"]);
    assert_eq!(
        revision["algorithm_version"],
        "hornvale/surface-realization/v6"
    );
    assert_eq!(
        revision["configuration_hash_hex"].as_str().unwrap().len(),
        64
    );

    let reply: Value = serde_json::from_str(
        &source
            .observe_surface(&request(binding, revision, 17, &[1]))
            .unwrap(),
    )
    .unwrap();
    assert_eq!(reply["binding"], *binding);
    assert_eq!(reply["patch"]["revision"], *revision);
}

#[test]
fn surface_rejects_wrong_binding() {
    let (mut source, initial) = source_and_initial();
    let mut binding = initial["binding"].clone();
    binding["scope_id"] = json!("wrong-scope");
    let error = source
        .observe_surface(&request(&binding, &initial["surface_revision"], 18, &[1]))
        .unwrap_err();
    assert!(matches!(error, SourceError::InvalidRequest(message) if message.contains("binding")));
}

#[test]
fn surface_rejects_stale_revision() {
    let (mut source, initial) = source_and_initial();
    let binding = &initial["binding"];
    let mut revision = initial["surface_revision"].clone();
    revision["source_revision"] = json!("0".repeat(64));
    let error = source
        .observe_surface(&request(binding, &revision, 19, &[1]))
        .unwrap_err();
    assert!(matches!(error, SourceError::InvalidRequest(message) if message.contains("revision")));
}

#[test]
fn surface_preserves_request_id() {
    let (mut source, initial) = source_and_initial();
    let reply: Value = serde_json::from_str(
        &source
            .observe_surface(&request(
                &initial["binding"],
                &initial["surface_revision"],
                4_294_967_297,
                &[1],
            ))
            .unwrap(),
    )
    .unwrap();
    assert_eq!(reply["request_id"], 4_294_967_297u64);
    assert_eq!(reply["schema"], "visual/surface-reply/v1");
}

#[test]
fn surface_contains_no_weather_hooks() {
    let (mut source, initial) = source_and_initial();
    let document = source
        .observe_surface(&request(
            &initial["binding"],
            &initial["surface_revision"],
            20,
            &[1],
        ))
        .unwrap();
    for hook in ["weather", "cloud", "precip", "roughness"] {
        assert!(
            !document.contains(hook),
            "surface transport contains {hook}"
        );
    }
}

#[test]
fn independent_sources_and_request_orders_return_identical_patch_bytes() {
    let (mut first, first_initial) = source_and_initial();
    let (mut second, second_initial) = source_and_initial();
    let first_binding = &first_initial["binding"];
    let second_binding = &second_initial["binding"];
    let first_revision = &first_initial["surface_revision"];
    let second_revision = &second_initial["surface_revision"];
    let first_a = request(first_binding, first_revision, 101, &[1]);
    let first_b = request(first_binding, first_revision, 102, &[2]);
    let second_a = request(second_binding, second_revision, 101, &[1]);
    let second_b = request(second_binding, second_revision, 102, &[2]);

    let first_b_reply = patch_bytes(&first.observe_surface(&first_b).unwrap());
    let first_a_reply = patch_bytes(&first.observe_surface(&first_a).unwrap());
    let second_a_reply = patch_bytes(&second.observe_surface(&second_a).unwrap());
    let second_b_reply = patch_bytes(&second.observe_surface(&second_b).unwrap());

    assert_eq!(first_a_reply, second_a_reply);
    assert_eq!(first_b_reply, second_b_reply);
}

#[test]
fn source_reply_carries_stable_feature_strips() {
    let (mut source, initial) = source_and_initial();
    let request = request(&initial["binding"], &initial["surface_revision"], 103, &[1]);
    let first = source.observe_surface(&request).unwrap();
    let second = source.observe_surface(&request).unwrap();
    assert_eq!(patch_bytes(&first), patch_bytes(&second));
    let reply: Value = serde_json::from_str(&first).unwrap();
    let strips = reply["patch"]["strips"].as_array().unwrap();
    assert!(!strips.is_empty());
    assert!(strips.iter().all(|strip| {
        strip["feature"] == strip["endpoints"][0]["feature"]
            && strip["feature"] == strip["endpoints"][1]["feature"]
    }));
}

#[test]
fn surface_reply_carries_source_owned_transition_triangles() {
    let (mut source, initial) = source_and_initial();
    let coarse = Facet {
        face: 0,
        path: vec![0; 6],
    };
    let mut request_id = 1000;
    let mut transition = None;
    'faces: for neighbor in coarse.neighbors() {
        for digit in 0..4 {
            let reply = source.observe_surface(&request_with_transition(
                &initial["binding"],
                &initial["surface_revision"],
                request_id,
                &coarse,
                &[],
                &neighbor,
                &[digit],
            ));
            request_id += 1;
            if let Ok(reply) = reply {
                let value: Value = serde_json::from_str(&reply).unwrap();
                if !value["patch"]["transition_triangles"]
                    .as_array()
                    .is_some_and(Vec::is_empty)
                {
                    transition = Some(value);
                    break 'faces;
                }
            }
        }
    }
    let reply = transition.expect("source must carry a real mixed-LOD transition");
    assert_eq!(reply["schema"], "visual/surface-reply/v1");
    assert!(
        !reply["patch"]["transition_triangles"]
            .as_array()
            .unwrap()
            .is_empty()
    );
}
