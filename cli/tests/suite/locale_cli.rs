//! The `hornvale locale` subcommand renders a room deterministically.
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
fn locale_json_is_deterministic_for_a_coordinate() {
    // Build a seed-42 world to a temp path.
    let dir = std::env::temp_dir();
    let world = dir.join("hv-locale-test.json");
    let (_o, e, ok) = run(&["new", "--seed", "42", "--out", world.to_str().unwrap()]);
    assert!(ok, "new failed: {e}");
    let w = world.to_str().unwrap();
    let (a, ea, ok_a) = run(&["locale", "--world", w, "--at", "35.0,-80.0", "--json"]);
    assert!(ok_a, "locale failed: {ea}");
    let (b, _eb, _) = run(&["locale", "--world", w, "--at", "35.0,-80.0", "--json"]);
    assert_eq!(a, b, "same coordinate → identical json");
    assert!(a.contains("\"schema\""), "json carries the schema tag");
    assert!(a.contains("locale/room/v2"));
}

#[test]
fn locale_requires_a_target() {
    let dir = std::env::temp_dir();
    let world = dir.join("hv-locale-test2.json");
    let _ = run(&["new", "--seed", "42", "--out", world.to_str().unwrap()]);
    let (_o, _e, ok) = run(&["locale", "--world", world.to_str().unwrap()]);
    assert!(!ok, "missing --at/--room must fail");
}
