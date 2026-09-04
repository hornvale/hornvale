//! The Weft, Task 9: the preregistered readout over spec §7's H1 (density,
//! two numbers), H2 (coherence, paired with an anti-vacuity companion in The
//! Ford's shape) and H3 (legibility — allowed to fail, and its preregistered
//! claim is the ORDERING `spring > thicket > overhang > erratic`, erratic
//! near zero). Population is land-eligible facets only (spec §7's
//! amendment). Decision 0016: a falsified prediction is reported as the
//! result, never rescued by retuning a constant after the fact.
//!
//! # The readout (seed 42, this tree, `--release`)
//!
//! **H1 — density, two numbers, per kind and combined.** Existence density
//! is god's-eye, over a vertex-centred subsample — one representative facet
//! per geosphere vertex (`n_land = 11,218` of `n = 40,962`, at
//! `hornvale_terrain::GLOBE_LEVEL = 6`), a 1-in-9,830 sample of the ~4e8
//! walk-depth facets on the grid, not literally "the whole grid" (fix round
//! 1, M-5) — the same resolution spec §7's own gate-component diagnostic
//! reads at; encounter rate is walked, over 78 land-eligible 60-step walks (4,680
//! steps total, `STRIDE = 137` — the identical sample
//! `weft_prevalence.rs::land_eligible_walks` draws, and the walk count (78)
//! reproduces that file's own measured figure independently).
//!
//! | kind | existence density | (numerator) | encounter rate | (numerator) |
//! | --- | ---: | ---: | ---: | ---: |
//! | spring | 0.035924 | 403 | 0.026709 | 125 |
//! | overhang | 0.075147 | 843 | 0.058761 | 275 |
//! | thicket | 0.135229 | 1,517 | 0.197436 | 924 |
//! | erratic | 0.038153 | 428 | 0.049786 | 233 |
//! | any (union) | 0.256285 | 2,875 | 0.307692 | 1,440 |
//!
//! The four existence-density numerators (403 / 843 / 1,517 / 428) and the
//! union (2,875, 25.63%) match this task's own cross-check figures exactly.
//!
//! **H2 — a construction-validation, paired with an anti-vacuity companion,
//! not a discovery that the surface is "coherent" (fix round 1, I-1 —
//! corrected here because an earlier draft of this doc oversold it).**
//! `occurs` thresholds a position-continuous `prevalence` field, so a
//! positive lag-1-style reading is near-guaranteed by construction; this
//! statistic is a regression guard against address-hashed speckle, and the
//! discriminating power actually lives in `weft_prevalence.rs`'s
//! real-vs-address-hashed-mutant table (real 0.998/0.982/0.99994/0.868
//! against mutant 0.209/0.089/0.890/-0.025), which this test's own module
//! doc already pointed at. Moran's I here is computed over the SAME walk
//! pool's within-walk chain adjacency (see `weft_morans_i`'s own doc in
//! `metrics.rs` for the full power argument for why geosphere-vertex
//! adjacency — tried first — cannot discriminate a sound construction from
//! an address-hashed one at that lag, and the discarded readings, published
//! in full rather than deleted). The anti-vacuity companion (occurs-count)
//! is the SAME walked population's raw hit count — which is therefore
//! numerically identical to H1's own encounter-rate numerator above; that
//! is not a bug, it is the same "how much substance underlies this"
//! question H1 already answers, reused rather than recomputed.
//!
//! | kind | Moran's I | occurs-count (companion) |
//! | --- | ---: | ---: |
//! | spring | 0.900576 | 125 |
//! | overhang | 0.813206 | 275 |
//! | thicket | 0.962205 | 924 |
//! | erratic | 0.587038 | 233 |
//!
//! **H3 — legibility (mutual information, bits), allowed to fail.**
//!
//! | kind | MI (bits) |
//! | --- | ---: |
//! | thicket | 0.038604 |
//! | spring | 0.007812 |
//! | overhang | 0.002497 |
//! | erratic | 0.000000 |
//!
//! # Reading the numbers
//!
//! **H1 holds, cleanly.** The union existence density (0.2563, "one in about
//! 3.9 land facets") is ~21,500x the placed baseline The Prospect measured
//! (`~1.19e-5`, "one site per ~84,200 land facets") — more than four orders
//! of magnitude, clearing H1's ">= 3 orders of magnitude" claim with room to
//! spare. Existence density and encounter rate agree closely per kind on
//! this world (encounter rate reads somewhat higher for thicket/erratic and
//! somewhat lower for spring/overhang than existence density, never by more
//! than ~30% relative), so the two questions this task's brief distinguishes
//! ("is it there" vs. "does a walker meet it") give similar but NOT
//! identical answers here, which is itself the expected shape for two
//! genuinely different samples of the same underlying process — not
//! evidence either number is redundant.
//!
//! **H2's construction-validation passes for all four kinds — no
//! address-hashed defect detected, which is what this statistic can
//! actually show (see the H2 paragraph above).** Every kind reads clearly
//! positive Moran's I (0.59-0.96) over a substantial occurs-count
//! (125-924) — not the near-zero reading an address-hashed regression
//! would produce, and not a numerical artifact of a handful of adjacent
//! hits. Erratic is the WEAKEST of the four (0.587, against 0.81-0.96 for
//! the other three) but is not near zero — its own design brief (Task 7)
//! requires it stay at the free-noise end for H3's legibility test
//! specifically, and a short (5-facet) correlation length still produces
//! real facet-to-facet texture over a single walked step, just less of it
//! than the longer-correlation-length kinds. This matches
//! `weft_prevalence.rs`'s own independent measurement of erratic's real
//! (not address-hashed) lag-1 prevalence autocorrelation, 0.868 — high in
//! absolute terms, and still the lowest of that file's own four real
//! readings too.
//!
//! **H3's preregistered ORDERING IS FALSIFIED, and is reported as the
//! result per decision 0016 — no constant here was retuned after seeing
//! it.** Preregistered: `spring > thicket > overhang > erratic`. Measured:
//! **thicket (0.0386) > spring (0.0078) > overhang (0.0025) > erratic
//! (0.0000)**. Thicket, not spring, is the most legible kind by this
//! estimator, though the shape spec §7 asked about is real: erratic reads
//! EXACTLY zero (not merely "near" it — a genuine `0.0` at 6-decimal
//! resolution, consistent with a macro-state that is a true constant, per
//! `WeftKind::macro_state`'s own doc: mutual
//! information between any variable and a constant is algebraically zero),
//! and spring still outscores overhang, so two of the three pairwise
//! relations the ordering makes survive; the one that does not is spring
//! vs. thicket. A plausible reading is that thicket's TWO-FIELD macro-state
//! recipe (temperature × moisture, `THICKET_CONTEXTUALITY = 0.85`, the
//! SAME contextuality as spring) simply has more usable structure for a
//! 4-bin discretization to resolve than spring's own two-field recipe
//! (carbonate × drainage) does at seed 42's particular land distribution —
//! this is a hypothesis about THIS estimator and THIS world, not a
//! re-derivation, and it is recorded as a hypothesis rather than asserted.
//!
//! # Cost
//!
//! `#[ignore]`d rather than left in the commit or stage gate: this test
//! builds one `Full`-depth-adjacent (`Climate` rung) seed-42 world and reads
//! all 22 Weft metrics off it, which (via `ClimateView::weft_grid`'s and
//! `weft_walks`'s caching) pays for one whole-grid sweep and one cheaper walk
//! sweep. Task 11's post-fix lefford profile attributes about 0.75
//! CPU-s/world to the grid pool in a 150-world all-metrics run — cheap in
//! isolation, but this file's job is to RECORD a preregistered reading once,
//! not to re-run it on every commit.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_lab::{BuiltView, ClimateView, MetricValue, registry};

/// The four kinds this task's spec §7 names, in the fixed order H1/H2/H3's
/// tables above use — matching `hornvale_worldgen::WeftKind::ALL`'s own
/// order (spring, overhang, thicket, erratic), which every
/// `weft-*-<kind>` metric in `metrics.rs`'s registry was built from.
const KINDS: [&str; 4] = ["spring", "overhang", "thicket", "erratic"];

/// Read one registered metric's `Number` value off `built`, panicking with
/// the metric's own name on anything else — every Weft metric this file
/// reads is documented `Absent` only on a world with no land at all, which
/// seed 42 is not.
fn number(built: &BuiltView, name: &str) -> f64 {
    let metric = registry()
        .into_iter()
        .find(|m| m.name == name)
        .unwrap_or_else(|| panic!("metric {name} is registered"));
    match metric.extract.apply(built) {
        MetricValue::Number(n) => n,
        other => panic!("metric {name} did not read a Number: {other:?}"),
    }
}

/// claim: readout(preregistered, 0016) — H1/H2/H3's seed-42 measurement,
/// recorded once (builds one Climate-rung world and reads all 22 registered
/// Weft metrics off it; see this file's own module doc for the current cost
/// accounting and the full readout table)
#[test]
#[ignore = "claim: readout(preregistered, 0016) — builds one seed-42 Climate-rung world and reads all 22 weft-* metrics off it (~524ms release-mode grid sweep, measured); see this file's module doc for the full table"]
fn the_preregistered_readout_is_measured_and_recorded() {
    let view = ClimateView::build(Seed(42), &SkyPins::default()).expect("seed 42 builds");
    let built = BuiltView::Climate(view);

    println!("=== H1: density (two numbers) ===");
    println!("kind       existence-density  encounter-rate");
    let mut existence = [0.0; 4];
    let mut encounter = [0.0; 4];
    for (i, kind) in KINDS.iter().enumerate() {
        existence[i] = number(&built, &format!("weft-existence-density-{kind}"));
        encounter[i] = number(&built, &format!("weft-encounter-rate-{kind}"));
        println!("{kind:<10} {:>17.6} {:>15.6}", existence[i], encounter[i]);
    }
    let existence_any = number(&built, "weft-existence-density-any");
    let encounter_any = number(&built, "weft-encounter-rate-any");
    println!("{:<10} {existence_any:>17.6} {encounter_any:>15.6}", "any");

    // Every existence-density reading is a real fraction: `[0, 1]`, and the
    // per-kind readings can never exceed the union (a kind's own occurrence
    // is a subset of "any kind occurred").
    for &d in existence.iter().chain(std::iter::once(&existence_any)) {
        assert!(
            (0.0..=1.0).contains(&d),
            "existence density {d} is not a fraction"
        );
    }
    for &d in existence.iter() {
        assert!(
            d <= existence_any + 1e-12,
            "a per-kind existence density ({d}) exceeded the union ({existence_any})"
        );
    }
    for &e in encounter.iter().chain(std::iter::once(&encounter_any)) {
        assert!(
            (0.0..=1.0).contains(&e),
            "encounter rate {e} is not a fraction"
        );
    }

    // H1's claim: the union existence density clears the placed baseline
    // The Prospect measured (`~1.19e-5`) by at least three orders of
    // magnitude.
    const PLACED_BASELINE: f64 = 1.19e-5;
    assert!(
        existence_any >= PLACED_BASELINE * 1_000.0,
        "H1: union existence density {existence_any} does not clear 1000x the placed \
         baseline {PLACED_BASELINE} — this is H1's headline claim"
    );

    println!("\n=== H2: coherence + anti-vacuity companion ===");
    println!("kind        Moran's I  occurs-count");
    let mut morans = [0.0; 4];
    let mut occurs_count = [0.0; 4];
    for (i, kind) in KINDS.iter().enumerate() {
        morans[i] = number(&built, &format!("weft-coherence-morans-i-{kind}"));
        occurs_count[i] = number(&built, &format!("weft-coherence-occurs-count-{kind}"));
        println!("{kind:<10} {:>10.6} {:>13.0}", morans[i], occurs_count[i]);
    }

    // The anti-vacuity companion: every kind's occurs-count is well above a
    // degenerate few-points regime, so none of the Moran's I readings above
    // are numerically suspect on that account.
    for (kind, &count) in KINDS.iter().zip(occurs_count.iter()) {
        assert!(
            count >= 20.0,
            "H2 anti-vacuity: {kind}'s occurs-count ({count}) is small enough that its \
             Moran's I reading may be a numerical artifact of a handful of adjacent hits, \
             not a genuine spatial process"
        );
    }
    // H2's construction-validation, all four kinds: every kind reads clear
    // of the near-zero band an address-hashed defect would produce — this
    // is a regression guard, not independent evidence of "coherence" (see
    // this file's own module doc, H2 section, fix round 1 I-1). Erratic is
    // NOT H2's negative control — that is H3's job (macro-state
    // legibility) — and erratic's short (5-facet) correlation length still
    // produces real facet-to-facet texture, just less of it than the other
    // three (`weft_prevalence.rs`'s own real-vs-mutant table already shows
    // this: erratic's real lag-1 prevalence autocorrelation is 0.868, far
    // above its address-hashed mutant's -0.025, even though it is the
    // weakest of the four kinds there too). So erratic is asserted to be
    // the WEAKEST reading, not a near-zero one.
    for (kind, &i_stat) in KINDS.iter().zip(morans.iter()) {
        assert!(
            i_stat > 0.2,
            "H2: {kind}'s Moran's I ({i_stat}) reads inside the near-zero band an \
             address-hashed defect would produce — the construction-validation this \
             statistic performs has failed"
        );
    }
    let erratic_is_weakest = morans[3] == morans.iter().cloned().fold(f64::INFINITY, f64::min);
    assert!(
        erratic_is_weakest,
        "erratic's Moran's I ({}) is not the weakest of the four — its short (5-facet) \
         correlation length predicts it should cluster least, even though H2 is not the \
         axis erratic is built to score near zero on (that is H3): {morans:?}",
        morans[3]
    );

    println!("\n=== H3: legibility (mutual information, bits) — allowed to fail ===");
    let mi: [f64; 4] = std::array::from_fn(|i| {
        let v = number(&built, &format!("weft-legibility-mi-{}", KINDS[i]));
        println!("{:<10} {v:.6}", KINDS[i]);
        v
    });
    let [mi_spring, mi_overhang, mi_thicket, mi_erratic] = mi;

    // Every MI reading is non-negative (a property of the estimator, not a
    // prediction) and finite.
    for (kind, &v) in KINDS.iter().zip(mi.iter()) {
        assert!(
            v.is_finite() && v >= -1e-9,
            "{kind}'s mutual information ({v}) is not a valid non-negative bit count"
        );
    }

    // The erratic near-zero requirement — checked and asserted FIRST,
    // separately from the four-way ordering below, and it is NOT
    // vacuous: it is the one part of H3's prediction that survives.
    assert!(
        mi_erratic < 0.01,
        "H3: erratic's mutual information ({mi_erratic}) is not near zero — spec §7: \
         \"if it does not, the instrument is measuring something other than legibility and \
         that is the finding\""
    );

    // THE PREREGISTERED CLAIM (spec §7): the ORDERING spring > thicket >
    // overhang > erratic. Measured on this tree: this does NOT hold —
    // thicket outscores spring. Per decision 0016 this is reported as the
    // result, not fixed by retuning a world constant, and the test does
    // NOT fail on it (a falsified prediction is a finding, not a bug —
    // `site_density.rs`'s own H3 readout asserts only that its reading is a
    // real number for the identical reason). The assertion below pins the
    // ACTUAL measured ordering as a witness, so a future change that moves
    // it is visible rather than silently absorbed.
    let ordering_holds =
        mi_spring > mi_thicket && mi_thicket > mi_overhang && mi_overhang > mi_erratic;
    println!(
        "\nH3 preregistered ordering (spring > thicket > overhang > erratic) holds: \
         {ordering_holds} — measured spring={mi_spring:.6} thicket={mi_thicket:.6} \
         overhang={mi_overhang:.6} erratic={mi_erratic:.6}"
    );
    if !ordering_holds {
        println!(
            "H3's preregistered ordering is FALSIFIED on this tree (decision 0016: reported \
             as the result, not rescued). See this file's module doc for the reading."
        );
    }
    // The witness: erratic is the smallest of the four (holds under both
    // the preregistered and the measured ordering) and spring beats
    // overhang (also holds under both) — the two ordering relations the
    // measured result shares with the prediction, asserted so a FUTURE
    // change that breaks even these is visible. The one relation that does
    // NOT hold (spring vs. thicket) is deliberately not asserted here: a
    // hard assertion on a relation already known false would be
    // permanently red, which is the "disable a red instead of reporting
    // it" shape decision 0016 forbids in the other direction.
    assert!(
        mi_erratic < mi_overhang && mi_erratic < mi_spring && mi_erratic < mi_thicket,
        "erratic is no longer the smallest of the four legibility readings — a change \
         worth investigating even independent of the ordering's own falsification: \
         spring={mi_spring:.6} thicket={mi_thicket:.6} overhang={mi_overhang:.6} \
         erratic={mi_erratic:.6}"
    );
    assert!(
        mi_spring > mi_overhang,
        "spring no longer outscores overhang — spring={mi_spring:.6} overhang={mi_overhang:.6}"
    );
}
