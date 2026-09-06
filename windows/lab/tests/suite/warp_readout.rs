//! The Warp, Task 7: the preregistered readout (spec §7, decision 0016).
//!
//! Seeds 13, 7, 1 and 100 — The Prospect's five less the calibration seed —
//! built here for the first time. Seed 42 is printed beside them and gates
//! nothing. Every bar below was frozen in spec §7 before Task 6 set a
//! constant; a bar that fails is reported as the campaign's headline, and
//! **no constant and no bar moves after this file has run**.
//!
//! **The bars are spec §7's, as amended before any readout seed was built,
//! and this file is the authority's mirror, not its author:**
//!
//! - **H1** — found fraction spring ≥ 0.60 and overhang ≥ 0.60; thicket
//!   within [0.30, 0.55] (the non-regression band; its recipe is unchanged).
//! - **H2** — best-class lift for spring and for overhang ≥ 2 × the
//!   erratic's on the same seed. The between-kind ordering clause
//!   ("spring's channel net ≥ overhang's") was **withdrawn** from the gate
//!   on 2026-09-05, before any readout seed was built: mutual information in
//!   bits scales with `H(Y)`, so an ordering between two kinds can be
//!   satisfied by making the lower kind rarer. It is printed below, raw and
//!   normalised by each kind's own `H(Y)`, and gates nothing.
//! - **H3** — learner gain > 0 for spring, overhang and thicket; the
//!   erratic's learner gain ≤ 0.001 bits/facet, **one-sided** (a held-out
//!   table over ~469 classes of noise must LOSE to the base rate; the
//!   control is that it never gains). The oracle ratio clause was withdrawn
//!   from the gate; `warp-oracle-gain-*` is printed, never read as a bar.
//! - **H4** — erratic channel MI net ≤ 0.008 bits (one-sided), and every
//!   kind's false-sign net within ±0.002 bits. Both are four standard
//!   deviations of the estimator's own permutation null, measured on seed 42.
//! - **H5** — no kind's max class rate exceeds 0.75.
//!
//! **Reported, never gated:** the channel-net ordering (raw and normalised),
//! learner / (channel net) per kind, `warp-oracle-gain-*`, and spec §6.3's
//! consequence — each kind's existence density and encounter rate, the
//! frequency that "fell out" of the rate/floor recipe rather than being set.
//!
//! **`Absent` is a failure line, not a panic.** `warp-found-fraction-erratic`
//! is `Absent` by design (the erratic has no cause to be found at, so the
//! fraction is undefined) and is never gated. Any OTHER `Absent` on a gated
//! metric — a learner or null reading can go undefined on a seed with a
//! degenerate fit half — is recorded as `seed N: <metric> Absent` so the
//! whole table prints before the verdict, rather than aborting the run at the
//! first seed that produced one.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_lab::{BuiltView, ClimateView, MetricValue, registry};

/// The four seeds spec §7 froze as the readout, built here for the first
/// time and never used to set a constant.
const READOUT_SEEDS: [u64; 4] = [13, 7, 1, 100];

/// Task 6's calibration seed. Printed beside the four; gates nothing.
const CALIBRATION_SEED: u64 = 42;

/// The four `hornvale_worldgen::WeftKind::ALL` suffixes, in slot order.
const KINDS: [&str; 4] = ["spring", "overhang", "thicket", "erratic"];

/// The metric's reading, or `NaN` when it is `Absent`. The two stay
/// distinguishable: the table prints `Absent` rather than fabricating a
/// zero, and [`gate`] turns a `NaN` on a gated input into its own failure
/// line naming the metric.
fn number(built: &BuiltView, name: &str) -> f64 {
    match registry()
        .into_iter()
        .find(|m| m.name == name)
        .unwrap_or_else(|| panic!("{name} registered"))
        .extract
        .apply(built)
    {
        MetricValue::Number(n) => n,
        MetricValue::Absent => f64::NAN,
        other => panic!("{name}: {other:?}"),
    }
}

/// Every §7 quantity for one sign kind on one seed.
struct Row {
    /// The kind's metric suffix, so a failure line can name the metric.
    kind: &'static str,
    /// `warp-found-fraction-<kind>` — H1. `Absent` for the erratic by design.
    found: f64,
    /// `warp-channel-mi-<kind>` — the raw channel reading.
    mi: f64,
    /// `warp-channel-null-<kind>` — the five-shift permutation null.
    null: f64,
    /// The reading: `mi − null`. H4 gates the erratic's.
    net: f64,
    /// `warp-best-lift-<kind>` — H2.
    lift: f64,
    /// `warp-learner-gain-<kind>` — H3.
    learner: f64,
    /// `warp-oracle-gain-<kind>` — reported, never gated (§7 H3 amendment).
    oracle: f64,
    /// `warp-false-sign-net-<kind>` — H4's address-noise control.
    false_net: f64,
    /// `warp-max-class-rate-<kind>` — H5.
    max_rate: f64,
    /// `weft-existence-density-<kind>` — §6.3's consequence, reported.
    density: f64,
    /// `weft-encounter-rate-<kind>` — §6.3's consequence, reported.
    encounter: f64,
}

impl Row {
    /// Name one of this kind's `warp-*` metrics and pair it with its value,
    /// for [`gate`]'s `Absent` reporting.
    fn input(&self, family: &str, value: f64) -> (String, f64) {
        (format!("warp-{family}-{}", self.kind), value)
    }
}

/// Read every §7 quantity for one kind off a built world.
fn row(built: &BuiltView, kind: &'static str) -> Row {
    let g = |family: &str| number(built, &format!("warp-{family}-{kind}"));
    let mi = g("channel-mi");
    let null = g("channel-null");
    Row {
        kind,
        found: g("found-fraction"),
        mi,
        null,
        net: mi - null,
        lift: g("best-lift"),
        learner: g("learner-gain"),
        oracle: g("oracle-gain"),
        false_net: g("false-sign-net"),
        max_rate: g("max-class-rate"),
        density: number(built, &format!("weft-existence-density-{kind}")),
        encounter: number(built, &format!("weft-encounter-rate-{kind}")),
    }
}

/// One table entry: the number, or the word `Absent`. Named `entry` to
/// match `warp_calibration.rs`'s printer beside it.
fn entry(v: f64) -> String {
    if v.is_nan() {
        "  Absent".to_string()
    } else {
        format!("{v:8.5}")
    }
}

/// The binary entropy of a base rate, in bits, for the normalised ordering
/// report. `hornvale_kernel::math::log2`, never `f64::log2` — the platform
/// libm diverges in the last ULP and the workspace bans it. Undefined at
/// `p == 0` and `p == 1`, where it returns `NaN` and the report says so.
fn binary_entropy(p: f64) -> f64 {
    if !(p > 0.0 && p < 1.0) {
        return f64::NAN;
    }
    -(p * hornvale_kernel::math::log2(p) + (1.0 - p) * hornvale_kernel::math::log2(1.0 - p))
}

/// Apply one preregistered bar.
///
/// `inputs` are the metrics the bar reads, by name. An `Absent` among them
/// is recorded as its own failure line and the bar itself is not adjudicated
/// — a comparison against `NaN` is false, so without this an undefined
/// reading would be reported as a violated bar with a `NaN` in it.
fn gate(failures: &mut Vec<String>, seed: u64, inputs: &[(String, f64)], ok: bool, what: String) {
    let mut absent = false;
    for (name, value) in inputs {
        if value.is_nan() {
            failures.push(format!("seed {seed}: {name} Absent"));
            absent = true;
        }
    }
    if !absent && !ok {
        failures.push(format!("seed {seed}: {what}"));
    }
}

/// claim: readout(preregistered, 0016) — The Warp's H1–H5 over four frozen readout seeds (four Climate-rung builds and grid sweeps, ~6 s release); the table is recorded in the campaign ledger
#[test]
#[ignore = "claim: readout(preregistered, 0016) — The Warp's H1–H5 over four frozen readout seeds (four Climate-rung builds and grid sweeps, ~6 s release); the table is recorded in the campaign ledger"]
fn the_preregistered_readout_holds_on_every_readout_seed() {
    let mut failures: Vec<String> = Vec::new();

    println!(
        "=== The Warp, Task 7 — the preregistered readout (spec section 7, decision 0016) ==="
    );
    println!(
        "readout seeds {READOUT_SEEDS:?}; seed {CALIBRATION_SEED} is printed and gates nothing"
    );

    for seed in READOUT_SEEDS.iter().copied().chain([CALIBRATION_SEED]) {
        let built = BuiltView::Climate(
            ClimateView::build(Seed(seed), &SkyPins::default())
                .unwrap_or_else(|e| panic!("seed {seed} builds: {e:?}")),
        );
        let rows: Vec<Row> = KINDS.iter().map(|k| row(&built, k)).collect();

        let banner = if seed == CALIBRATION_SEED {
            "  (calibration seed — reported, gates nothing)"
        } else {
            ""
        };
        println!("\n--- seed {seed}{banner} ---");
        println!(
            "{:<10} {:>8} {:>8} {:>8} {:>8} {:>7} {:>8} {:>8} {:>8} {:>9} {:>8} {:>9}",
            "kind",
            "found",
            "ch-mi",
            "ch-null",
            "ch-net",
            "lift",
            "maxrate",
            "learner",
            "oracle",
            "false-net",
            "density",
            "encounter",
        );
        for r in &rows {
            println!(
                "{:<10} {} {} {} {} {:>7} {} {} {} {:>9} {} {:>9}",
                r.kind,
                entry(r.found),
                entry(r.mi),
                entry(r.null),
                entry(r.net),
                if r.lift.is_nan() {
                    " Absent".to_string()
                } else {
                    format!("{:7.3}", r.lift)
                },
                entry(r.max_rate),
                entry(r.learner),
                entry(r.oracle),
                if r.false_net.is_nan() {
                    "  Absent".to_string()
                } else {
                    format!("{:9.5}", r.false_net)
                },
                entry(r.density),
                if r.encounter.is_nan() {
                    "   Absent".to_string()
                } else {
                    format!("{:9.5}", r.encounter)
                },
            );
        }

        // REPORTED, NEVER GATED — spec §7's H2 and H3 amendments, and §6.3's
        // consequence. These print for every seed including the calibration
        // seed, and no line below feeds `failures`.
        println!("  reported, never gated:");
        for r in &rows {
            let h = binary_entropy(r.density);
            if h.is_nan() {
                println!(
                    "    {:<9} net {:>9.5}   H(Y) undefined (density {:.5}) — normalised figure skipped",
                    r.kind, r.net, r.density
                );
            } else {
                println!(
                    "    {:<9} net {:>9.5}   H(Y) {:>8.5}   net/H(Y) {:>8.5}",
                    r.kind,
                    r.net,
                    h,
                    r.net / h
                );
            }
        }
        let (sp, ov, th, er) = (&rows[0], &rows[1], &rows[2], &rows[3]);
        let (h_sp, h_ov) = (binary_entropy(sp.density), binary_entropy(ov.density));
        println!(
            "    ordering  spring net >= overhang net: {} (raw: {:.5} vs {:.5})",
            sp.net >= ov.net,
            sp.net,
            ov.net
        );
        if h_sp.is_nan() || h_ov.is_nan() {
            println!("    ordering  normalised: H(Y) undefined for one kind — skipped");
        } else {
            println!(
                "    ordering  spring net/H(Y) >= overhang net/H(Y): {} ({:.5} vs {:.5})",
                sp.net / h_sp >= ov.net / h_ov,
                sp.net / h_sp,
                ov.net / h_ov
            );
        }
        for r in &rows {
            println!(
                "    {:<9} learner/(channel net) {:>9.5}   oracle {:>9.5}",
                r.kind,
                r.learner / r.net,
                r.oracle
            );
        }
        println!("    section 6.3 consequence (frequency, not set by anything):");
        for r in &rows {
            println!(
                "    {:<9} existence density {:>9.5}   encounter rate {:>9.5}",
                r.kind, r.density, r.encounter
            );
        }

        if seed == CALIBRATION_SEED {
            continue;
        }

        // ------------------------------------------------------------------
        // The preregistered bars. Spec §7 is the authority; nothing here is
        // adjusted by a reading.
        // ------------------------------------------------------------------

        // H1 — found rather than extruded. The erratic's found fraction is
        // `Absent` by design and is not a bar.
        for r in [sp, ov] {
            gate(
                &mut failures,
                seed,
                &[r.input("found-fraction", r.found)],
                r.found >= 0.60,
                format!("H1 {} found {:.4} < 0.60", r.kind, r.found),
            );
        }
        gate(
            &mut failures,
            seed,
            &[th.input("found-fraction", th.found)],
            (0.30..=0.55).contains(&th.found),
            format!("H1 thicket found {:.4} outside [0.30, 0.55]", th.found),
        );

        // H2 — the walker can tell. The erratic's lift is the null lift, from
        // cardinality alone, so the bar is relative to it on the same seed.
        for r in [sp, ov] {
            gate(
                &mut failures,
                seed,
                &[r.input("best-lift", r.lift), er.input("best-lift", er.lift)],
                r.lift >= 2.0 * er.lift,
                format!(
                    "H2 {} lift {:.3} < 2 x erratic {:.3} = {:.3}",
                    r.kind,
                    r.lift,
                    er.lift,
                    2.0 * er.lift
                ),
            );
        }

        // H3 — it can be learned. One-sided on the erratic: a held-out table
        // over noise must never GAIN; how much it loses is set by cardinality.
        for r in [sp, ov, th] {
            gate(
                &mut failures,
                seed,
                &[r.input("learner-gain", r.learner)],
                r.learner > 0.0,
                format!("H3 {} learner gain {:.5} <= 0", r.kind, r.learner),
            );
        }
        gate(
            &mut failures,
            seed,
            &[er.input("learner-gain", er.learner)],
            er.learner <= 0.001,
            format!("H3 erratic learner gain {:.5} > 0.001", er.learner),
        );

        // H4 — the instrument credits nothing to noise. Both bars are four
        // standard deviations of the estimator's own null, fixed from the
        // seed-42 measurement rather than recomputed per seed.
        gate(
            &mut failures,
            seed,
            &[
                er.input("channel-mi", er.mi),
                er.input("channel-null", er.null),
            ],
            er.net <= 0.008,
            format!("H4 erratic channel net {:.5} > 0.008", er.net),
        );
        for r in &rows {
            gate(
                &mut failures,
                seed,
                &[r.input("false-sign-net", r.false_net)],
                r.false_net.abs() <= 0.002,
                format!(
                    "H4 {} false-sign net {:.5} outside +/-0.002",
                    r.kind, r.false_net
                ),
            );
        }

        // H5 — nothing is told.
        for r in &rows {
            gate(
                &mut failures,
                seed,
                &[r.input("max-class-rate", r.max_rate)],
                r.max_rate <= 0.75,
                format!("H5 {} max class rate {:.5} > 0.75", r.kind, r.max_rate),
            );
        }
    }

    // One metric absent on one seed can fail several bars; report it once.
    let mut seen = std::collections::BTreeSet::new();
    failures.retain(|f| seen.insert(f.clone()));

    println!("\n--- verdict ---");
    if failures.is_empty() {
        println!("every preregistered bar met on every readout seed");
    } else {
        println!("{} preregistered bar(s) not met:", failures.len());
        for f in &failures {
            println!("  {f}");
        }
    }

    assert!(
        failures.is_empty(),
        "preregistered bars not met (a FINDING, not a bug — record it, do not retune):\n  {}",
        failures.join("\n  ")
    );
}
