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

/// The distinct phenomenon kinds the world's deities derive from, read from
/// the **committed ledger** rather than from rendered prose. Reading the
/// ledger is the point: a deity's `derived-from-phenomenon` is a fact the sky
/// put there, where a rendered name is a string several unrelated systems get
/// a vote on (see the moons test below).
fn deity_source_kinds(path: &std::path::Path) -> std::collections::BTreeSet<String> {
    let json = std::fs::read_to_string(path).expect("world.json readable");
    let world = hornvale_kernel::World::from_json(&json).expect("world.json parses");
    world
        .ledger
        .find("derived-from-phenomenon")
        .filter_map(|f| match &f.object {
            hornvale_kernel::Value::Text(t) => Some(t.clone()),
            _ => None,
        })
        .collect()
}

#[test]
fn moons_flip_the_calendar_and_seat_gods_no_moonless_world_can_hold() {
    // **Restated by The Contour, and the restatement is a finding.** What
    // stood here asserted that the moons pin left "the head" alone, and it
    // never tested that. Its helper read `gods.split("\n\n").nth(1)`, but
    // paragraph 0 of the Gods section is the heading and paragraph 1 is the
    // SETTLEMENT LEAD LINE (`windows/almanac/src/lib.rs`); deity blocks start
    // at paragraph 2. A helper named `head_belief` therefore compared a
    // settlement NAME STRING, never a deity.
    //
    // That mattered, because the claim it was standing in for is false. The
    // sky-debut plan asked for `moons_flip_flips_the_calendar_not_the_faith`
    // — Gods sections EQUAL. Gods sections are not equal, so the claim was
    // later softened to "the head is not displaced"; but measured at seed 42
    // the head DEITY is displaced, an eclipse deity leading at `--moons 3`
    // against a wandering-star deity at `--moons 0`, and the two pantheons
    // share no member at all (2 deities against 14). Both the original claim
    // and its softening are false, and were false long before The Contour —
    // the test passed only because the paragraph it compared happened to
    // match.
    //
    // What finally reddened it was not a leak into head selection. The head
    // is NOT displaced: the same entity heads the section in both arms, same
    // cell, same people, same population, same `name-gloss`. Only its
    // rendered name moved, one letter, because toponymic wear (The Wearing)
    // keys a morpheme to its own culture's corpus size and the moons pin
    // moves that corpus — moons reach tides, tides reach climate, climate
    // reaches history, and history places settlements. That non-pin-isolation
    // is documented in `windows/worldgen/src/lib.rs`; the old assertion's
    // stability was a coincidence of corpus sizes that the `history/bake/v2`
    // epoch expired.
    //
    // So this asserts the claim that is actually PHYSICS, at the altitude
    // where the divergence method wants it — vary one pin, show the
    // downstream culture differs LEGIBLY — and reads it from the ledger, not
    // from prose:
    //
    //   * a moonless world can seat no eclipse god and no tide god, because
    //     it has neither phenomenon to observe; and
    //   * moons only ADD source kinds, never remove one — which is the
    //     salvageable half of "coarse constrains fine", stated over the
    //     KINDS of god a sky affords rather than over a rendered line.
    //
    // Both are robust to exactly what broke the old form: settlement churn,
    // naming drift, corpus wear, and any history change. Neither can be
    // satisfied by comparing the wrong paragraph.
    let dir = temp_dir("moons");
    let seed = 42u64;

    // TAGGED filenames, not `make_world_with`: both arms share a seed, so the
    // untagged helper writes both worlds to `world-42.json` and the second
    // `new` silently clobbers the first. That was invisible while this test
    // read only almanacs (captured as Strings before the overwrite); it is
    // not invisible now that the arms are read back from their ledgers.
    let zero_path = make_world_tagged(
        &dir,
        seed,
        "moons0",
        &["--sky", "generated", "--rotation", "normal", "--moons", "0"],
    );
    let zero_almanac = almanac_of(&zero_path);
    let zero_gods = extract_gods_section(&zero_almanac);
    let zero_calendar = extract_calendar_section(&zero_almanac);

    let three_path = make_world_tagged(
        &dir,
        seed,
        "moons3",
        &["--sky", "generated", "--rotation", "normal", "--moons", "3"],
    );
    let three_almanac = almanac_of(&three_path);
    let three_gods = extract_gods_section(&three_almanac);
    let three_calendar = extract_calendar_section(&three_almanac);

    // Each moon is itself a salient celestial-body phenomenon, and moons
    // additionally afford eclipses and tides, so more moons seat more deities.
    assert_ne!(
        zero_gods, three_gods,
        "more moons seat more deities in the pantheon"
    );

    // The sky reaches the faith, read from the ledger. A moonless world has
    // no eclipse and no tide to observe, so it can seat no god derived from
    // either; a three-moon world seats both.
    let zero_kinds = deity_source_kinds(&zero_path);
    let three_kinds = deity_source_kinds(&three_path);
    for absent in ["eclipse", "tide"] {
        assert!(
            !zero_kinds.contains(absent),
            "a moonless world cannot seat a god derived from {absent}: {zero_kinds:?}"
        );
        assert!(
            three_kinds.contains(absent),
            "a three-moon world must seat a god derived from {absent}: {three_kinds:?}"
        );
    }
    // Moons only ADD source kinds — the salvageable half of "coarse
    // constrains fine", stated over the KINDS of god a sky affords.
    assert!(
        zero_kinds.is_subset(&three_kinds),
        "moons must not retire a source kind a moonless sky already had: \
         {zero_kinds:?} is not a subset of {three_kinds:?}"
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
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
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
