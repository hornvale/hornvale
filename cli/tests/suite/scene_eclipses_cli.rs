//! The `hornvale scene eclipses` subcommand exposes the v3 observer query.
use std::process::{Command, Output};
use std::sync::OnceLock;

fn run(args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(args)
        .output()
        .expect("run hornvale")
}

fn world_path() -> &'static str {
    static WORLD: OnceLock<String> = OnceLock::new();
    WORLD.get_or_init(|| {
        let path = std::env::temp_dir().join(format!(
            "hv-scene-eclipses-cli-test-{}.json",
            std::process::id()
        ));
        let output = run(&[
            "new",
            "--seed",
            "42",
            "--out",
            path.to_str().expect("temp path is UTF-8"),
        ]);
        assert!(
            output.status.success(),
            "new failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        path.into_os_string()
            .into_string()
            .expect("temp path is UTF-8")
    })
}

fn eclipses(extra: &[&str]) -> Output {
    let mut args = vec![
        "scene",
        "eclipses",
        "--world",
        world_path(),
        "--from",
        "0",
        "--until",
        "2000",
    ];
    args.extend_from_slice(extra);
    run(&args)
}

fn json(output: &Output) -> serde_json::Value {
    assert!(
        output.status.success(),
        "scene eclipses failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout).expect("scene eclipses emits JSON")
}

#[test]
fn scene_eclipses_without_observer_omits_observer_results() {
    let document = json(&eclipses(&[]));

    assert_eq!(document["schema"], "scene/eclipses/v3");
    assert!(document.get("observer").is_none());
    let events = document["events"].as_array().expect("events array");
    assert!(!events.is_empty());
    assert!(events.iter().all(|event| event.get("observer").is_none()));
}

#[test]
fn scene_eclipses_observer_is_normalized_and_keeps_unseen_results() {
    let document = json(&eclipses(&["--latitude", "0", "--longitude", "540"]));

    assert_eq!(document["observer"]["latitude_deg"], 0.0);
    assert_eq!(document["observer"]["longitude_deg"], -180.0);
    let events = document["events"].as_array().expect("events array");
    assert!(events.iter().all(|event| event["observer"].is_object()));
    assert!(
        events
            .iter()
            .any(|event| event["observer"]["visibility"] == "unseen"),
        "a supplied observer's unseen result must remain distinct from omission"
    );
}

#[test]
fn scene_eclipses_rejects_incomplete_observer_pairs() {
    for args in [
        &["--latitude"][..],
        &["--longitude"][..],
        &["--latitude", "0"][..],
        &["--longitude", "0"][..],
    ] {
        let output = eclipses(args);
        let error = String::from_utf8_lossy(&output.stderr);
        assert!(
            !output.status.success(),
            "incomplete pair was accepted: {args:?}"
        );
        assert!(
            error.contains("--latitude and --longitude must be supplied together"),
            "unexpected error for {args:?}: {error}"
        );
    }
}

#[test]
fn scene_eclipses_rejects_invalid_observer_coordinates() {
    for (flag, value, message) in [
        ("--latitude", "90.000001", "observer latitude"),
        ("--latitude", "-90.000001", "observer latitude"),
        ("--latitude", "north", "--latitude must be a number"),
        ("--latitude", "NaN", "observer latitude"),
        ("--latitude", "inf", "observer latitude"),
        ("--latitude", "-inf", "observer latitude"),
        ("--longitude", "east", "--longitude must be a number"),
        ("--longitude", "NaN", "observer longitude"),
        ("--longitude", "inf", "observer longitude"),
        ("--longitude", "-inf", "observer longitude"),
    ] {
        let mut observer = ["--latitude", "0", "--longitude", "0"];
        let offset = if flag == "--latitude" { 1 } else { 3 };
        observer[offset] = value;
        let output = eclipses(&observer);
        let error = String::from_utf8_lossy(&output.stderr);
        assert!(
            !output.status.success(),
            "invalid {flag}={value} was accepted"
        );
        assert!(
            error.contains(message),
            "unexpected error for {flag}={value}: {error}"
        );
    }
}

#[test]
fn help_names_the_v3_observer_query() {
    let output = run(&["help"]);
    let help = String::from_utf8_lossy(&output.stdout);

    assert!(output.status.success());
    assert!(
        help.contains("scene/eclipses/v3"),
        "stale schema help: {help}"
    );
    assert!(help.contains("--latitude LAT --longitude LON"));
}
