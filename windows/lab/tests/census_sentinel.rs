//! The census sentinel (The Sexton, Task 7).
//!
//! Runs the full metric roster over the census's first three seeds and compares
//! against the committed `rows.csv` — so census drift reddens at the commit
//! that caused it rather than five hours into a campaign close.
//!
//! THE GOLDEN IS THE CENSUS'S OWN COMMITTED ROWS, not a separate fixture. A
//! second copy of the same values would be a second source of truth and a new
//! drift surface; reading the census's prefix makes this automatically correct
//! the moment a census lands.
//!
//! SECOND JOB, AND IT IS THE LARGER ONE. Decision 0090 audited cross-host
//! reproducibility once, over 40 worlds, and found it clean. This makes that
//! audit CONTINUOUS, over metrics that did not exist when it ran. A metric that
//! is host-divergent reddens here — which is precisely the failure decision
//! 0079 feared and could not detect.
//!
//! DIRECTION THIS CHECK ENFORCES: the first three census rows, recomputed here,
//! equal the committed ones. It says nothing about the other 997.
//!
//! DELIBERATELY CALLS `run` RATHER THAN THE CLI PATH. `cmd_lab_run` takes the
//! census claim (decision 0081) and enforces the canonical host (0079); this
//! test does neither, on purpose. A three-world run must not claim the box, and
//! the host guard governs WRITES — this only reads. Do not "fix" the missing
//! guard: it is what makes a Mac-side sentinel possible at all.

use std::path::Path;

/// Metric names allowed to disagree with the committed census, each with a
/// reason. A reasonless entry is a parse error — the same ratchet
/// `tropes check` and seam-guard use, because a waiver nobody has to justify
/// becomes a place to hide a real divergence.
fn waivers() -> Vec<(String, String)> {
    include_str!("fixtures/sentinel-waivers.txt")
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(|l| {
            let (name, reason) = l.split_once(':').unwrap_or_else(|| {
                panic!(
                    "sentinel-waivers.txt: '{l}' has no ':' — every waiver must \
                     carry a reason, because a reasonless waiver is where a real \
                     cross-host divergence would hide"
                )
            });
            assert!(
                !reason.trim().is_empty(),
                "sentinel-waivers.txt: '{name}' has an empty reason"
            );
            (name.trim().to_string(), reason.trim().to_string())
        })
        .collect()
}

#[test]
fn the_first_three_census_worlds_match_the_committed_rows() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("windows/lab always has a grandparent");

    let study = hornvale_lab::load_study(&root.join("studies/the-census.study.json"))
        .expect("the census study must load");

    // Narrow the study to its first three seeds. Everything else — the pin set
    // and the `"all"` metric roster — is inherited unchanged, so this measures
    // exactly what the census measures.
    let mut sentinel = study.clone();
    sentinel.seeds.count = 3;

    let live = hornvale_lab::run(&sentinel).expect("the sentinel study must run");

    let csv = std::fs::read_to_string(
        root.join(hornvale_lab::CENSUS_GOLDENS_DIR)
            .join("the-census/rows.csv"),
    )
    .expect("the committed census rows must exist");
    let committed = hornvale_lab::load_rows(&sentinel, &csv)
        .expect("the committed rows must parse against the census study");

    let waived: Vec<String> = waivers().into_iter().map(|(n, _)| n).collect();
    let mut moved: Vec<String> = Vec::new();

    // `load_rows` does NOT validate row count against the study — it parses
    // every record, so `committed` holds all 1000 census rows while `live`
    // holds 3. `zip` therefore compares the first three, which is what we
    // want. But it would just as happily compare MISALIGNED pairs if the
    // census CSV's leading rows were ever not seeds 0..2, and pass or fail
    // meaninglessly. Assert the alignment rather than relying on it.
    assert!(
        committed.rows.len() >= live.rows.len(),
        "the committed census has fewer rows ({}) than the sentinel ran ({})",
        committed.rows.len(),
        live.rows.len()
    );

    for (live_row, want_row) in live.rows.iter().zip(committed.rows.iter()) {
        let got = hornvale_lab::canonical_row(live_row);
        assert_eq!(
            got.seed, want_row.seed,
            "sentinel/committed row misalignment: the sentinel measured seed {} \
             where the committed census row is seed {}. The comparison below \
             would be meaningless — check that the census still starts at \
             seed 0 and that the sentinel's `seeds.from` matches it.",
            got.seed, want_row.seed
        );
        for ((name, g), w) in live
            .metric_names
            .iter()
            .zip(got.values.iter())
            .zip(want_row.values.iter())
        {
            if g != w && !waived.iter().any(|x| x == name) {
                moved.push(format!(
                    "seed {} · {name}: live {g:?} vs committed {w:?}",
                    got.seed
                ));
            }
        }
    }

    assert!(
        moved.is_empty(),
        "the census sentinel disagrees with the committed rows.csv. Either this \
         change moved the census (refresh it on lefford and commit the goldens), \
         or a metric is host-divergent (add it to \
         windows/lab/tests/fixtures/sentinel-waivers.txt WITH A REASON, and open \
         a follow-up — decision 0079's failure is exactly this, undetected):\n  {}",
        moved.join("\n  ")
    );
}
