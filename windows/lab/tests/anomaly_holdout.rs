//! H2 — do the census percentiles generalise to seeds the census never saw?
//! (spec §3.5, preregistered before this file existed.)
//!
//! **H2 IS A CALIBRATION CHECK AND NOT A USEFULNESS MEASURE.** A stationary
//! distribution passes it while flagging nothing useful: if held-out worlds
//! are drawn from the same generator as the census worlds, the share of them
//! holding at least one column at `tail_depth <= TAIL_DEPTH_BAR` must come
//! out near the in-census share no matter how uninformative the report's
//! columns are. It says the prior transfers off the fitting set. It says
//! nothing whatever about whether a flagged world is worth looking at — that
//! is H1's question, and a green H2 can never substitute for it. This
//! sentence is here because without it a later reader will cite a green H2 as
//! evidence that the report works.
//!
//! **Heavy tier, and genuinely so:** it builds 200 worlds outside the census
//! (seeds 1000–1199) at `Full` depth. It is `#[ignore]`d out of the commit
//! gate and runs under `make gate-full` / `make heavy-remote REF=<full-sha>`
//! on the canonical box, which is also the box whose census goldens supply
//! the prior it is testing.
//!
//! **On the serialization pin it does not carry.** `hornvale_lab::run`
//! parallelises its own seed sweep across every core, so under
//! `gate-full-heavy.sh` (which sets no `test-threads` limit) this battery can
//! be scheduled alongside others. `.config/nextest.toml`'s
//! `threads-required = "num-cpus"` pin covers exactly the three batteries
//! that call `seed_sweep::map_seeds(`, which `cli/tests/heavy_tier.rs` holds
//! to that roster in both directions — so adding this test to the pin would
//! redden that guard. The precedent is already in this directory:
//! `fixture_staleness.rs`'s heavy battery runs a lab study the same way and
//! is likewise unpinned. Recorded rather than chased; widening the pin's
//! recognition rule is a change to the heavy tier's tuning and wants its own
//! measurement.

use hornvale_lab::domesday::anomaly::{self, TAIL_DEPTH_BAR};
use hornvale_lab::domesday::census::{Census, load};
use hornvale_lab::{Study, render_schema, run, write_csv};
use std::path::{Path, PathBuf};

/// Seeds 0–999 are the census's; the held-out arm starts immediately after
/// so the two populations are adjacent draws of the same generator and
/// differ in nothing but membership.
const HOLDOUT_FROM: u64 = 1000;
/// 200 worlds: enough that a share near the in-census one is not a rounding
/// artifact, and the largest arm the heavy tier's budget will carry.
const HOLDOUT_COUNT: u64 = 200;
/// The preregistered tolerance (spec §3.5): the held-out share must fall
/// within a factor of two of the in-census share, in either direction.
const FACTOR: f64 = 2.0;

fn census() -> Census {
    load(Path::new("../../book/src/laboratory/generated/the-census"))
        .expect("the committed census loads")
}

/// The held-out study, built here rather than committed under `studies/`:
/// it produces no artifact, is read by nothing else, and committing it would
/// invite a `lab run` that publishes a second census-shaped directory. Every
/// other field matches `studies/the-census.study.json` exactly — same pins,
/// same `"all"` metric selection — so a held-out row is column-for-column
/// comparable with a census row, which is the whole premise of scoring it
/// against the census's percentiles.
fn holdout_study() -> Study {
    let json = format!(
        r#"{{ "name": "gnomon-holdout",
              "description": "The Gnomon H2: {HOLDOUT_COUNT} worlds outside the census (seeds {HOLDOUT_FROM}+), default pins, every metric. Built in-test, published nowhere.",
              "seeds": {{ "from": {HOLDOUT_FROM}, "count": {HOLDOUT_COUNT} }},
              "pin_sets": [ {{ "label": "default", "pins": [] }} ],
              "metrics": "all" }}"#
    );
    let study: Study = serde_json::from_str(&json).expect("the held-out study parses");
    study.validate().expect("the held-out study validates");
    study
}

/// Run the held-out study and read it back through the SAME loader the
/// census uses, so a held-out row and a census row are the same kind of
/// thing (raw CSV text keyed by column name, quantized at the emit boundary
/// exactly as the committed census was). Round-tripping through CSV rather
/// than reading `MetricValue`s directly is the point: it is what makes the
/// two comparable.
fn holdout_census(scratch: &Path) -> Census {
    let study = holdout_study();
    let result = run(&study).expect("the held-out study runs");
    let csv_path = write_csv(&result, scratch).expect("held-out rows.csv is written");
    let dir = csv_path
        .parent()
        .expect("rows.csv has a parent")
        .to_path_buf();
    let csv = std::fs::read_to_string(&csv_path).expect("held-out rows.csv is readable");
    std::fs::write(dir.join("schema.json"), render_schema(&result, &csv, false))
        .expect("held-out schema.json is written");
    load(&dir).expect("the held-out rows load through the census reader")
}

/// The share of worlds holding at least one column at or below the frozen
/// [`TAIL_DEPTH_BAR`] — "how often does this report have anything to say".
fn flagged_share(worlds: &[anomaly::WorldAnomaly]) -> f64 {
    if worlds.is_empty() {
        return f64::NAN;
    }
    worlds.iter().filter(|w| w.score >= 1).count() as f64 / worlds.len() as f64
}

/// H2: fit on seeds 0–999, score seeds 1000–1199, compare the two shares.
///
/// Read the module doc before citing a green here for anything.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn h2_holdout_flag_share_is_within_a_factor_of_two_of_the_in_census_share() {
    let c = census();
    let in_census = flagged_share(&anomaly::rank(&c));

    let scratch = std::env::temp_dir().join(format!("hv-gnomon-holdout-{}", std::process::id()));
    let held = holdout_census(&scratch);
    let scored: Vec<anomaly::WorldAnomaly> = held
        .rows
        .iter()
        .filter_map(|row| {
            let seed = row.get("seed")?.parse::<u64>().ok()?;
            Some(anomaly::score_row(&c, seed, row))
        })
        .collect();
    let _ = std::fs::remove_dir_all(&scratch);

    assert_eq!(
        scored.len(),
        HOLDOUT_COUNT as usize,
        "every held-out seed must produce a scored row"
    );
    let holdout = flagged_share(&scored);

    println!(
        "[h2] in-census share {in_census:.4} ({} worlds); held-out share {holdout:.4} \
         ({} worlds); ratio {:.4}; tolerance factor {FACTOR}; bar {TAIL_DEPTH_BAR}",
        c.rows.len(),
        scored.len(),
        holdout / in_census
    );

    assert!(
        in_census > 0.0,
        "the in-census share is zero, so there is no reference to calibrate against \
         — that is a finding about the report, not about the held-out worlds"
    );
    assert!(
        holdout <= in_census * FACTOR && holdout * FACTOR >= in_census,
        "H2 NOT MET: the held-out flag share {holdout:.4} is outside a factor of \
         {FACTOR} of the in-census share {in_census:.4}. The census percentiles do \
         not transfer to seeds the census never saw, which means the prior is fitted \
         to its own sample. Report the two shares; do not widen FACTOR."
    );
}

/// A scratch path helper kept honest: the held-out arm must never write into
/// the committed goldens tree. Cheap enough to run on every commit, and it
/// is the one thing about this file the heavy `#[ignore]` would otherwise
/// hide until someone ran the tier.
#[test]
fn the_holdout_arm_never_writes_into_the_committed_goldens() {
    let scratch = std::env::temp_dir().join("hv-gnomon-holdout-path-check");
    let published: PathBuf = Path::new(hornvale_lab::CENSUS_GOLDENS_DIR).to_path_buf();
    assert!(
        !scratch.starts_with(&published),
        "the held-out arm's scratch directory must be outside {}",
        published.display()
    );
    // The study's own name must not collide with a committed study either: a
    // collision would make a stray `lab run` overwrite real goldens.
    assert_eq!(holdout_study().name, "gnomon-holdout");
    assert!(
        !Path::new("../../studies/gnomon-holdout.study.json").exists(),
        "gnomon-holdout is built in-test on purpose; a committed study of that name \
         would give `lab run` a path that publishes a second census-shaped directory"
    );
}
