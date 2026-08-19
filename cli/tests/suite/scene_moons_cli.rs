//! The `hornvale scene moons` subcommand emits scene/moons/v1 JSON.
use std::process::Command;

fn run(args: &[&str]) -> (String, String, bool) {
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(args)
        .output()
        .expect("run hornvale");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.success(),
    )
}

#[test]
fn scene_moons_emits_the_schema() {
    // Build a seed-42 world (default sky is `generated`, which has moons).
    let dir = std::env::temp_dir();
    let world = dir.join("hv-scene-moons-test.json");
    let (_o, e, ok) = run(&["new", "--seed", "42", "--out", world.to_str().unwrap()]);
    assert!(ok, "new failed: {e}");
    let w = world.to_str().unwrap();
    let (out, err, ok) = run(&["scene", "moons", "--world", w]);
    assert!(ok, "scene moons failed: {err}");
    assert!(
        out.contains("\"schema\":\"scene/moons/v1\""),
        "missing schema tag: {out}"
    );
    assert!(
        out.contains("\"surface_class\""),
        "missing surface_class: {out}"
    );
}
