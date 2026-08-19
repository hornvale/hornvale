//! `hornvale scene surrounds --lens colour` emits a `sight` block whose
//! `sun_altitude_deg` must be the altitude the light was ACTUALLY built
//! from — not an unrelated placeholder. F1 (The Beholding's final-fix
//! wave): the `--lens colour` arm once declared a hardcoded `0.0` while
//! lighting the scene with unattenuated `daylight(&star)`, so the document
//! claimed a horizon sun that never entered the computation at all.
//!
//! This test re-derives the expected altitude independently — through
//! `hornvale_astronomy::Calendar::solar_altitude_at`, a different call site
//! than the CLI's own `hornvale_vessel::eyes::daylight_at` — from
//! quantities the JSON document itself discloses (`observer.latitude`, the
//! day requested, and the world's own calendar), and checks the declared
//! `sight.sun_altitude_deg` against that independent computation. A
//! hardcoded `0.0` reddens this the moment the real altitude (day 0.32, a
//! seed-42 world) is anything else, which it is: nowhere near the horizon.

use hornvale_astronomy::StdDays;
use hornvale_kernel::World;
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

/// A day chosen (spec §9's H4 amendment) so seed 42's flagship latitude
/// puts the sun genuinely above the horizon rather than at the
/// `at_elevation` MAX_AIRMASS clamp — day 0.27 lands at -13.442°, below
/// the horizon, which would confirm the clamp rather than a real altitude
/// read.
const DAY: f64 = 0.32;

#[test]
fn colour_lens_declares_the_altitude_the_light_was_actually_built_from() {
    let dir = std::env::temp_dir();
    let world_path = dir.join("hv-scene-surrounds-colour-test.json");
    let (_o, e, ok) = run(&["new", "--seed", "42", "--out", world_path.to_str().unwrap()]);
    assert!(ok, "new failed: {e}");
    let w = world_path.to_str().unwrap();

    let (out, err, ok) = run(&[
        "scene",
        "surrounds",
        "--world",
        w,
        "--render",
        "json",
        "--lens",
        "colour",
        "--day",
        &DAY.to_string(),
    ]);
    assert!(ok, "scene surrounds --lens colour failed: {err}");

    let doc: serde_json::Value = serde_json::from_str(&out).expect("valid JSON");
    let sight = doc
        .get("sight")
        .expect("a coloured document declares sight");
    let declared_altitude = sight
        .get("sun_altitude_deg")
        .and_then(serde_json::Value::as_f64)
        .expect("sight.sun_altitude_deg is a number");
    let latitude = doc
        .get("observer")
        .and_then(|o| o.get("latitude"))
        .and_then(serde_json::Value::as_f64)
        .expect("observer.latitude is a number");

    // Independently re-derive the altitude a real sun would have at this
    // world's calendar, this latitude, this day — a SEPARATE call site
    // from the one the CLI itself used, so this cannot pass by the two
    // sharing one (possibly wrong) computation.
    let world = World::load(&world_path).expect("world.json reloads");
    let calendar = hornvale_worldgen::sky_of(&world)
        .expect("seed 42's default sky is generated")
        .calendar()
        .cloned()
        .expect("a generated sky always has a calendar");
    let expected_altitude = calendar
        .solar_altitude_at(
            StdDays::new(DAY).expect("day 0.32 is a valid StdDays"),
            latitude,
        )
        .expect("a non-locked world always has a solar altitude");

    // Anti-vacuity: the bug this test exists to catch declared a hardcoded
    // 0.0. If the real altitude also happened to be ~0.0 this test could
    // not tell a lie from the truth, so pin that it is not.
    assert!(
        expected_altitude.abs() > 1.0,
        "day {DAY} at latitude {latitude} must put the sun well away from 0.0° \
         for this test to discriminate a hardcoded 0.0 from a computed altitude; \
         got {expected_altitude}"
    );

    assert!(
        (declared_altitude - expected_altitude).abs() < 1e-3,
        "sight.sun_altitude_deg ({declared_altitude}) does not match the \
         altitude a real sun would have at this world's calendar, latitude \
         {latitude}, day {DAY} ({expected_altitude}) — the sight block is \
         lying about the light that lit these colours"
    );

    let _ = std::fs::remove_file(&world_path);
}
