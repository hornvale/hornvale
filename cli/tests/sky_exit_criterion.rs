//! Campaign 2b exit criterion: rotation flips faith, moons flip calendars.
//! Generated sky tests: prove rotation and moons parameters affect the right
//! sections, that reload is byte-deterministic, that graded pins survive
//! across seeds, that scout is deterministic, and that refusals are recorded.

use std::path::PathBuf;
use std::process::Command;

fn bin() -> Command {
    Command::new(env!("CARGO_BIN_EXE_hornvale"))
}

fn temp_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("hornvale-2b-{tag}-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn make_world(dir: &std::path::Path, seed: u64) -> PathBuf {
    make_world_with(dir, seed, &[])
}

fn make_world_with(dir: &std::path::Path, seed: u64, extra_args: &[&str]) -> PathBuf {
    let path = dir.join(format!("world-{seed}.json"));
    let out = bin()
        .args([
            "new",
            "--seed",
            &seed.to_string(),
            "--out",
            path.to_str().unwrap(),
        ])
        .args(extra_args)
        .output()
        .unwrap();
    assert!(out.status.success(), "new failed: {:?}", out);
    path
}

/// Like `make_world_with`, but writes to a `tag`-suffixed filename so two
/// worlds sharing a seed (e.g. the same seed under different rotation pins)
/// don't clobber each other's file when both are read back later.
fn make_world_tagged(dir: &std::path::Path, seed: u64, tag: &str, extra_args: &[&str]) -> PathBuf {
    let path = dir.join(format!("world-{seed}-{tag}.json"));
    let out = bin()
        .args([
            "new",
            "--seed",
            &seed.to_string(),
            "--out",
            path.to_str().unwrap(),
        ])
        .args(extra_args)
        .output()
        .unwrap();
    assert!(out.status.success(), "new failed: {:?}", out);
    path
}

fn almanac_of(path: &std::path::Path) -> String {
    let out = bin()
        .args(["almanac", "--world", path.to_str().unwrap()])
        .output()
        .unwrap();
    assert!(out.status.success());
    String::from_utf8(out.stdout).unwrap()
}

/// Extract the "## The Gods" section from an almanac.
/// Returns the content from "## The Gods" to the next "---" or end of file.
fn extract_gods_section(almanac: &str) -> String {
    if let Some(start) = almanac.find("## The Gods") {
        let section = &almanac[start..];
        if let Some(end) = section.find("---") {
            section[..end].to_string()
        } else {
            section.to_string()
        }
    } else {
        String::new()
    }
}

/// Extract the "## The Calendar" section from an almanac.
/// Returns the content from "## The Calendar" to the next "---" or end of file.
fn extract_calendar_section(almanac: &str) -> String {
    if let Some(start) = almanac.find("## The Calendar") {
        let section = &almanac[start..];
        if let Some(end) = section.find("---") {
            section[..end].to_string()
        } else {
            section.to_string()
        }
    } else {
        String::new()
    }
}

#[test]
fn rotation_flip_flips_the_religion() {
    let dir = temp_dir("rotation");
    let seed = 42u64;

    // Generate with --rotation normal. Tagged filenames (not the bare
    // seed-keyed `make_world_with` path): both worlds share seed 42, and the
    // sentiment facts are read back from disk below, so a shared filename
    // would let the second `new` silently clobber the first world's file.
    let normal_path = make_world_tagged(
        &dir,
        seed,
        "normal",
        &["--sky", "generated", "--rotation", "normal"],
    );
    let normal_almanac = almanac_of(&normal_path);
    let normal_gods = extract_gods_section(&normal_almanac);

    // Generate with --rotation locked
    let locked_path = make_world_tagged(
        &dir,
        seed,
        "locked",
        &["--sky", "generated", "--rotation", "locked"],
    );
    let locked_almanac = almanac_of(&locked_path);
    let locked_gods = extract_gods_section(&locked_almanac);

    // Gods sections must differ
    assert_ne!(
        normal_gods, locked_gods,
        "Gods sections must differ between rotation modes"
    );

    // Religion emits structured sentiment facts, not inline prose (The
    // Tongues, spec §6) — the almanac's rendered Gods section no longer
    // carries "never"/"every"/"returns" template words to assert on, so
    // this reads the committed `sentiment` fact straight from the saved
    // world instead: locked skies commit an eternal sentiment, spinning
    // skies a cyclic one.
    let has_sentiment = |path: &std::path::Path, tag: &str| {
        let json = std::fs::read_to_string(path).expect("world.json readable");
        let world = hornvale_kernel::World::from_json(&json).expect("world.json parses");
        world
            .ledger
            .find("sentiment")
            .any(|f| matches!(&f.object, hornvale_kernel::Value::Text(t) if t == tag))
    };
    assert!(
        has_sentiment(&locked_path, "eternal"),
        "locked rotation must commit an eternal sentiment fact"
    );
    assert!(
        has_sentiment(&normal_path, "cyclic"),
        "normal rotation must commit a cyclic sentiment fact"
    );

    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn moons_flip_flips_the_calendar_and_grows_the_pantheon_without_displacing_the_head() {
    let dir = temp_dir("moons");
    let seed = 42u64;

    // Generate with --moons 0
    let zero_path = make_world_with(
        &dir,
        seed,
        &["--sky", "generated", "--rotation", "normal", "--moons", "0"],
    );
    let zero_almanac = almanac_of(&zero_path);
    let zero_gods = extract_gods_section(&zero_almanac);
    let zero_calendar = extract_calendar_section(&zero_almanac);

    // Generate with --moons 3
    let three_path = make_world_with(
        &dir,
        seed,
        &["--sky", "generated", "--rotation", "normal", "--moons", "3"],
    );
    let three_almanac = almanac_of(&three_path);
    let three_gods = extract_gods_section(&three_almanac);
    let three_calendar = extract_calendar_section(&three_almanac);

    // The pantheon is salience-ordered with the sun always first; each moon
    // is itself a salient celestial-body phenomenon, so more moons seat more
    // deities. The head deity (the sun) must stay put regardless — "coarse
    // constrains fine": moons never displace it.
    let head_belief = |gods: &str| gods.split("\n\n").nth(1).unwrap_or("").to_string();
    assert_eq!(
        head_belief(&zero_gods),
        head_belief(&three_gods),
        "the head deity is unchanged by moon count"
    );
    assert_ne!(
        zero_gods, three_gods,
        "more moons seat more deities in the pantheon"
    );

    // Calendar sections must differ
    assert_ne!(
        zero_calendar, three_calendar,
        "Calendar sections must differ based on moons count"
    );

    // The 3-moon almanac must contain "third moon"
    assert!(
        three_calendar.contains("third moon"),
        "3-moon almanac must contain 'third moon'"
    );

    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn worlds_survive_reload_byte_identically() {
    let dir = temp_dir("reload");
    let world = make_world(&dir, 42);

    // Generate almanac twice
    let almanac1 = almanac_of(&world);
    let almanac2 = almanac_of(&world);

    // Must be byte-identical
    assert_eq!(
        almanac1, almanac2,
        "Almanac must be byte-identical on reload"
    );

    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); runs in make gate-full / cloud nightly"]
fn graded_pins_never_fail_above_min() {
    let dir = temp_dir("pins");

    for seed in 1..=20 {
        // Test with graded pin --moons 0+3 (min 0, never hard-fails)
        let moons = "0+3";
        let out = bin()
            .args([
                "new",
                "--seed",
                &seed.to_string(),
                "--moons",
                moons,
                "--out",
                dir.join(format!("world-{seed}-graded.json"))
                    .to_str()
                    .unwrap(),
            ])
            .output()
            .unwrap();
        assert!(
            out.status.success(),
            "new --seed {} --moons {} failed: {:?}",
            seed,
            moons,
            out
        );
    }

    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn scout_is_deterministic_and_finds_three_moon_worlds() {
    let dir = temp_dir("scout");

    // Run scout twice with same parameters
    let run_scout = || {
        let out = bin()
            .args(["scout", "--moons", "3", "--limit", "2"])
            .output()
            .unwrap();
        assert!(out.status.success(), "scout failed: {:?}", out);
        String::from_utf8(out.stdout).unwrap()
    };

    let scout1 = run_scout();
    let scout2 = run_scout();

    // Identical stdout
    assert_eq!(
        scout1, scout2,
        "scout must be deterministic with same parameters"
    );

    // Contains at least one line starting with "seed "
    assert!(
        scout1.lines().any(|line| line.starts_with("seed ")),
        "scout output must contain at least one line starting with 'seed '"
    );

    // Contains a final "scanned" line
    assert!(
        scout1.contains("scanned"),
        "scout output must contain 'scanned' line"
    );

    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn refusals_are_recorded_in_the_world() {
    let dir = temp_dir("refusals");
    let world = make_world_with(&dir, 23, &["--sky", "generated"]);
    let almanac = almanac_of(&world);

    // Almanac must contain "was sought"
    assert!(
        almanac.contains("was sought"),
        "Almanac must contain 'was sought' under Notes from genesis"
    );

    std::fs::remove_dir_all(&dir).unwrap();
}
