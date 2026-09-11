use hornvale_visual_source::Source;
use serde_json::{Value, json};
use std::path::PathBuf;
const REV: &str = "4e06e33492a82e245aec899559b8cdf33ac7fdcd";
fn fixture() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../cli/tests/fixtures/world-seed-42.json")
}
fn request(binding: &Value, ticks: i64) -> String {
    json!({"schema":"visual/request/v1","binding":binding,"request_id":7,"ticks":ticks}).to_string()
}
#[test]
fn source_native_and_cli_bytes_agree_under_out_of_order_queries() {
    let path = fixture();
    let mut source = Source::open(&path, REV, "test-source").unwrap();
    let initial: Value = serde_json::from_str(&source.initial_document(16).unwrap()).unwrap();
    let mut world =
        hornvale_kernel::World::from_json(&std::fs::read_to_string(&path).unwrap()).unwrap();
    hornvale_worldgen::register_all(&mut world.registry).unwrap();
    let ctx = hornvale_scene::AstronomyContext::build(&world).unwrap();
    let mut replies = Vec::new();
    #[derive(serde::Deserialize)]
    struct Reply {
        astronomy: Box<serde_json::value::RawValue>,
    }
    for ticks in [0, 100000, -1, 0] {
        let reply = source
            .observe(&request(&initial["binding"], ticks))
            .unwrap();
        let parsed: Reply = serde_json::from_str(&reply).unwrap();
        let native = hornvale_scene::astronomy_at_json(
            &hornvale_scene::astronomy_at_scene_in(
                &ctx,
                hornvale_kernel::WorldTime::from_ticks(ticks),
            )
            .unwrap(),
        );
        assert_eq!(parsed.astronomy.get(), native);
        // Root CLI must have been built by the native CLI scoped test command.
        let output = std::process::Command::new(
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../target/debug/hornvale"),
        )
        .args([
            "scene",
            "astronomy-at",
            "--world",
            path.to_str().unwrap(),
            "--ticks",
            &ticks.to_string(),
        ])
        .output()
        .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        assert_eq!(String::from_utf8(output.stdout).unwrap().trim_end(), native);
        replies.push(reply);
    }
    assert_eq!(replies[0], replies[3]);
}
#[test]
fn validates_revision_and_every_binding_component_and_query_types() {
    for revision in [
        "main",
        "123abc",
        "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz",
        "",
    ] {
        assert!(Source::open(&fixture(), revision, "test").is_err());
    }
    assert!(Source::open(&fixture(), REV, "").is_err());
    let mut source = Source::open(&fixture(), REV, "test").unwrap();
    let initial: Value = serde_json::from_str(&source.initial_document(16).unwrap()).unwrap();
    for key in ["source_id", "scope_id", "world_sha256", "source_revision"] {
        let mut binding = initial["binding"].clone();
        binding[key] = json!("wrong");
        assert!(source.observe(&request(&binding, 0)).is_err(), "{key}");
    }
    for bad in [
        "{}",
        "not json",
        r#"{"schema":"visual/request/v1","ticks":1.5}"#,
    ] {
        assert!(source.observe(bad).is_err());
    }
    for (key, value) in [
        ("ticks", json!(1.5)),
        ("ticks", json!(9223372036854775808u64)),
        ("request_id", json!(-1)),
        ("schema", json!("visual/request/v2")),
    ] {
        let mut doc: Value = serde_json::from_str(&request(&initial["binding"], 0)).unwrap();
        doc[key] = value;
        assert!(source.observe(&doc.to_string()).is_err(), "{key}");
    }
    assert!(
        matches!(source.observe(&request(&initial["binding"],i64::MIN)),Err(hornvale_visual_source::SourceError::Observation(message)) if message.contains("luminosity"))
    );
    assert!(source.initial_document(15).is_err());
}
#[test]
fn same_seed_different_pins_has_different_hash_and_exact_file_binding() {
    use hornvale_astronomy::{SkyPins, StellarTopology};
    use sha2::{Digest, Sha256};
    let mut bindings = Vec::new();
    for topology in [
        StellarTopology::Single,
        StellarTopology::WideBinary,
        StellarTopology::CloseBinary,
    ] {
        let world = hornvale_worldgen::build_world_to(
            hornvale_kernel::Seed(42),
            &SkyPins {
                topology: Some(topology),
                ..Default::default()
            },
            &Default::default(),
            &Default::default(),
            &hornvale_worldgen::components::WorldComponents::assemble().unwrap(),
            hornvale_worldgen::BuildDepth::Astronomy,
        )
        .unwrap();
        let bytes = world.to_json();
        let path = std::env::temp_dir().join(format!(
            "planetarium-source-{}-{topology:?}.json",
            std::process::id()
        ));
        std::fs::write(&path, &bytes).unwrap();
        let mut source = Source::open(&path, REV, "test").unwrap();
        std::fs::write(&path, "changed after open").unwrap();
        let initial: Value = serde_json::from_str(&source.initial_document(16).unwrap()).unwrap();
        assert_eq!(
            initial["binding"]["world_sha256"],
            format!("{:x}", Sha256::digest(bytes.as_bytes()))
        );
        source.observe(&request(&initial["binding"], 0)).unwrap();
        bindings.push(initial["binding"].clone());
        std::fs::remove_file(path).unwrap();
    }
    assert_ne!(bindings[0], bindings[1]);
    assert_ne!(bindings[1], bindings[2]);
}
