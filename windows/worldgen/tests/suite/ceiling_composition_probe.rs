//! THE CEILING, M2: does averaging the seven per-source subterranean energy
//! yields into one scalar (`subterranean_energy`'s own `MeanOfSeven` rule)
//! destroy world-to-world variation that a composition-preserving rule would
//! keep? This file builds the scaffolding: a `CombinationRule` abstraction
//! over `EnergySource::yield_at`'s seven readings at one chamber, and Q1's
//! `separation` statistic (spec-frozen, reproduced verbatim in
//! `subterranean_energy_probe.rs`'s `between_worlds_separation_and_within_world_width`
//! doc comment) computed generically over whichever rule is supplied.
//!
//! **Task 1's only deliverable is a trustworthy positive control**: does
//! this file's independent re-implementation of Q1's formula, run with the
//! SHIPPED `MeanOfSeven` rule, reproduce the `0.145249` that file already
//! published? If it does not, nothing built on top of this file in later
//! tasks is interpretable, so the positive control is written and run
//! before anything else.
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen
//! batteries. `world_at`, `Q6_SEEDS`, `UNDERGROUND_RUNGS`, `median`, `pct`
//! and `iqr` below are copied from `subterranean_energy_probe.rs` rather
//! than imported — test modules do not share private helpers across files —
//! and are unchanged from that file's own definitions.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Band, Geosphere, Seed, VertexMap};
use hornvale_terrain::delve::rung_evaluation_depth_m;
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::energy::{EnergySource, dominant_source};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, Substrate, WorldComponents, build_world_to_with_artifacts,
    climate_of, substrate_field, subterranean_substrate_field_per_rung,
};

/// THE SOURCES, Task 6: the twelve seeds spec §6 preregisters for the
/// between-worlds `separation` statistic. Copied verbatim from
/// `subterranean_energy_probe.rs`'s `Q6_SEEDS` (spec's own listed order,
/// `S = {1, 7, 42, 99, 123, 256, 512, 777, 1024, 1234, 4096, 9001}`).
const Q6_SEEDS: [u64; 12] = [1, 7, 42, 99, 123, 256, 512, 777, 1024, 1234, 4096, 9001];

/// The five underground rungs `Band::all()` carries below `Surface`. Copied
/// verbatim from `subterranean_energy_probe.rs`'s constant of the same name.
const UNDERGROUND_RUNGS: [Band; 5] = [
    Band::Undercroft,
    Band::Shallows,
    Band::Deeps,
    Band::Underdeep,
    Band::Nadir,
];

/// Build `seed_value` to `BuildDepth::Terrain` and return its terrain and
/// surface substrate field. Copied verbatim from
/// `subterranean_energy_probe.rs`'s `world_at`, which mirrors
/// `underworld_conditions_probe::terrain_and_surface`'s own world-building
/// idiom.
fn world_at(seed_value: u64, wc: &WorldComponents) -> (GeneratedTerrain, VertexMap<Substrate>) {
    let seed = Seed(seed_value);
    let artifacts = build_world_to_with_artifacts(
        seed,
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        BuildDepth::Terrain,
    )
    .expect("probe seed builds");
    let world = artifacts.world;
    let terrain = artifacts
        .terrain
        .expect("terrain is Some at BuildDepth::Terrain");
    let climate = climate_of(&world).expect("climate reconstructs");
    let geo = terrain.geosphere();

    let surface = substrate_field(
        geo,
        &terrain,
        &climate,
        climate.obliquity_deg(),
        climate.insolation(),
        &climate.regime(),
    );
    (terrain, surface)
}

/// The median of a slice, sorted in place. Copied verbatim from
/// `subterranean_energy_probe.rs`'s `median` — empty is `NaN`, not a panic
/// (see that file's doc comment for why).
fn median(v: &mut [f64]) -> f64 {
    if v.is_empty() {
        return f64::NAN;
    }
    v.sort_by(f64::total_cmp);
    v[v.len() / 2]
}

/// Nearest-rank percentile of an ascending-sorted slice. Copied verbatim
/// from `subterranean_energy_probe.rs`'s `pct`.
fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    let i = (((sorted.len() - 1) as f64) * q).round() as usize;
    sorted[i]
}

/// `p75 - p25` of an ascending-sorted slice. Copied verbatim from
/// `subterranean_energy_probe.rs`'s `iqr`.
fn iqr(sorted: &[f64]) -> f64 {
    pct(sorted, 0.75) - pct(sorted, 0.25)
}

/// How the seven per-source yields at one chamber are combined into the one
/// scalar `separation` is computed over. The SHIPPED rule is the mean; `Max`
/// is M2's DIAGNOSTIC — a genuinely non-averaging rule, useful precisely
/// because if dilution-by-averaging were what flattens the signal, `Max`
/// should separate worlds at least as well as `Mean` does. **Neither is
/// proposed as a replacement.**
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CombinationRule {
    /// The mean of all seven yields — `subterranean_energy`'s rule at the
    /// time this file was written, and **no longer the shipped rule**: The
    /// Trencher (Task 4, 2026-09-11) replaced it with a per-metabolite sum
    /// (`energy::chemical_supply`). This file re-implements the mean locally
    /// rather than calling the shipped function, so M2's positive control
    /// still reproduces its published `0.145249` — but the number now
    /// describes a retired rule, not the world. Read M2's conclusions as a
    /// record of why the mean was replaced, not as a description of today's
    /// field.
    MeanOfSeven,
    /// The largest single yield.
    ///
    /// **Not** "the composition-preserving extreme" — an earlier draft of
    /// this doc comment called it that, and Task 2's review found it false.
    /// `mean` gives every one of the seven sources derivative `1/7`: none
    /// discarded, only diluted. `max` gives the winner derivative `1` and
    /// the other six **exactly `0`**, so it discards the MOST composition,
    /// not the least. What it actually is: a genuinely **non-averaging**
    /// rule, which is what makes it a useful diagnostic — if dilution-by-
    /// averaging were flattening the signal, `max` should have separated at
    /// least as well as `mean` did. It did not (see
    /// [`max_of_seven_separates_worlds_the_mean_does_not`]'s measured
    /// result).
    ///
    /// Constructed by Task 2's diagnostic,
    /// [`max_of_seven_separates_worlds_the_mean_does_not`] — Task 1's own
    /// test ran [`CombinationRule::MeanOfSeven`] alone.
    MaxOfSeven,
}

impl CombinationRule {
    /// Combine seven (or fewer, at this module's own test boundary) per-
    /// source yields into the one scalar this rule stands for.
    fn combine(self, yields: &[f64]) -> f64 {
        match self {
            CombinationRule::MeanOfSeven => yields.iter().sum::<f64>() / yields.len() as f64,
            CombinationRule::MaxOfSeven => yields.iter().copied().fold(f64::MIN, f64::max),
        }
    }
}

/// Every cave-bearing vertex's rung-level yield at `seed_value`, combined by
/// `rule`, pooled into one sample — the per-seed input [`separation`]
/// reduces to `m_s`/`IQR(E_s)`.
///
/// **Mirrors `subterranean_energy_probe.rs`'s own loop** (its
/// `between_worlds_separation_and_within_world_width`), same
/// `rung_evaluation_depth_m` guard, same
/// `subterranean_substrate_field_per_rung` moisture, same per-vertex
/// `drainage` — with one substitution: that file reads
/// `subterranean_energy_field_per_rung`'s already-composed (fixed
/// mean-of-seven) entry, while this recomputes the seven per-source
/// [`EnergySource::yield_at`] readings independently and combines them with
/// `rule`, so a rule other than [`CombinationRule::MeanOfSeven`] can be
/// measured at all.
fn pooled_sample(wc: &WorldComponents, seed_value: u64, rule: CombinationRule) -> Vec<f64> {
    let (terrain, surface) = world_at(seed_value, wc);
    let geo: &Geosphere = terrain.geosphere();
    let moisture_field = subterranean_substrate_field_per_rung(geo, &terrain, &surface);

    let mut seed_pooled: Vec<f64> = Vec::new();
    for vertex in geo.vertices() {
        let Some(cave) = terrain.cave_at(vertex) else {
            continue;
        };
        let material = terrain.material_at(vertex);
        let gradient = terrain.geothermal_gradient_at(vertex);
        let drainage = terrain.drainage_at(vertex);
        for &rung in &UNDERGROUND_RUNGS {
            let idx = rung as usize;
            let Some(depth_m) = rung_evaluation_depth_m(rung, gradient, cave.depth_reach_m) else {
                continue;
            };
            let Some(sub) = moisture_field.get(vertex)[idx] else {
                continue;
            };
            let yields: Vec<f64> = EnergySource::ALL
                .iter()
                .map(|source| source.yield_at(&material, gradient, depth_m, sub.moisture, drainage))
                .collect();
            seed_pooled.push(rule.combine(&yields));
        }
    }
    seed_pooled
}

/// Q1's formula, verbatim from `subterranean_energy_probe.rs`, with ONLY the
/// combination rule substituted:
///
/// ```text
/// separation = IQR({ median(E_s) }) / median({ IQR(E_s) })
/// ```
fn separation(wc: &WorldComponents, rule: CombinationRule) -> f64 {
    let mut medians = Vec::with_capacity(Q6_SEEDS.len());
    let mut iqrs = Vec::with_capacity(Q6_SEEDS.len());
    for &seed in &Q6_SEEDS {
        let mut sample = pooled_sample(wc, seed, rule);
        sample.sort_by(f64::total_cmp);
        medians.push(median(&mut sample.clone()));
        iqrs.push(iqr(&sample));
    }
    medians.sort_by(f64::total_cmp);
    iqrs.sort_by(f64::total_cmp);
    iqr(&medians) / median(&mut iqrs.clone())
}

/// THE CEILING, M2 positive control: does this file's re-implementation of
/// Q1's `separation` reproduce the number `subterranean_energy_probe.rs`
/// published for the SHIPPED combination rule?
///
/// `0.145249` was measured 2026-08-26 and re-measured at this campaign's base
/// `26003913d`; it reproduced exactly. A mismatch here means this file is
/// measuring something else, and every M2 conclusion drawn from it would be
/// uninterpretable — so this test asserts and the rest of M2 depends on it.
///
/// claim: readout(off-gate, run by hand; the control for M2)
#[test]
#[ignore = "probe: M2's positive control; run by hand (The Ceiling, Stage 1)"]
fn mean_of_seven_reproduces_the_published_separation() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let sep = separation(&wc, CombinationRule::MeanOfSeven);
    println!("separation(mean-of-seven) = {sep:.6}");
    assert!(
        (sep - 0.145_249).abs() < 5e-7,
        "positive control FAILED: separation(mean-of-seven) = {sep:.6}, \
         expected 0.145249 as published by subterranean_energy_probe.rs. \
         This file is measuring something else; do not interpret M2."
    );
}

/// THE CEILING, M2: is the magnitude compression caused by the ROCK or by
/// `subterranean_energy`'s mean-of-seven?
///
/// The metaplan attributes it to the rock ("roughly three near-constant
/// categorical states") and instructs rung 3 to design against that. A mean of
/// seven gated terms compresses by construction. Nothing had separated the two
/// causes.
///
/// PREREGISTERED (spec §4): `separation(MaxOfSeven) >= 0.25` — the bar Q1 set
/// and the shipped rule failed at 0.145249.
///
/// BOTH POLES SHIP. Clearing it means the combination rule is a major cause
/// and composition is more available than the metaplan's inherited diagnosis
/// implies. Failing it means the rock is the cause and that diagnosis stands
/// unqualified. Neither is a failure; record whichever was measured.
///
/// **PREREGISTRATION NOT MET — measured 2026-09-11, `Q6_SEEDS` (n=12),
/// `BuildDepth::Terrain`.** `separation(mean-of-seven) = 0.145249` (control,
/// reproduces the published number exactly) and
/// `separation(max-of-seven) = 0.040745` — not only short of the 0.25 bar but
/// **lower than the mean it was meant to bound**, `ratio max/mean = 0.2805`.
/// The composition-preserving extreme separates worlds LESS than the
/// averaging rule does, not more. This falsifies the diagnostic's own premise
/// ("the rule that discards the least composition ... bounds what the mean is
/// costing") along with the prediction: swapping the combination rule is not
/// merely insufficient here, it moves the statistic the wrong way. **The rock
/// is the cause, not the combination rule; the metaplan's inherited diagnosis
/// stands unqualified.** No `EnergySource` was retuned and this assertion now
/// pins the measured null — a future drift here is a fresh finding, not a bar
/// to loosen.
///
/// claim: readout(off-gate, prints both rules' separation before any verdict)
#[test]
#[ignore = "probe: M2, rock vs mean; run by hand (The Ceiling, Stage 1)"]
fn max_of_seven_separates_worlds_the_mean_does_not() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let mean = separation(&wc, CombinationRule::MeanOfSeven);
    let max = separation(&wc, CombinationRule::MaxOfSeven);

    // Report BEFORE asserting: the pair is the finding, the bar is its floor.
    println!("separation(mean-of-seven) = {mean:.6}   [shipped rule, control]");
    println!("separation(max-of-seven)  = {max:.6}   [diagnostic]");
    println!("ratio max/mean            = {:.4}", max / mean);

    assert!(
        (mean - 0.145_249).abs() < 5e-7,
        "control drifted inside M2: mean-of-seven = {mean:.6}, expected 0.145249"
    );
    // PREREGISTERED PREDICTION FALSIFIED 2026-09-11 (spec §4, decision 0016):
    // separation(max-of-seven) did not clear 0.25 — it measured 0.040745,
    // below even the mean-of-seven control (see this test's doc comment for
    // the exact measured value and date). This assertion pins the DIRECTION
    // of the measured null, not the exact value: the finding is that
    // max-of-seven still falls short of the bar, not that this particular
    // number has not moved. An exact pin would red on any unrelated
    // upstream change and read as "M2 broke" rather than as a fresh
    // measurement to record.
    assert!(
        max < 0.25,
        "MEASURED FINDING CHANGED from the 2026-09-11 reading recorded in \
         this test's doc comment: separation(max-of-seven) was {max:.6} (< \
         0.25, falsifying the diagnostic's prediction) and has since crossed \
         0.25. Record a fresh measurement with today's date — do NOT retune \
         any EnergySource to force a particular outcome."
    );
}

/// [`dominant_source`]'s verdict, tallied across every cave-bearing vertex
/// of `seed_value`'s world at rung `rung`, normalized to sum to `1.0` —
/// indexed by position in [`EnergySource::ALL`] (Task 3, M1).
///
/// **Calls the SHIPPED `dominant_source` directly rather than re-deriving
/// which source wins** — M1 measures what that function reports, not a
/// parallel computation of it. Mirrors `pooled_sample`'s own per-vertex
/// construction: same `rung_evaluation_depth_m` guard, same
/// `subterranean_substrate_field_per_rung` moisture, same per-vertex
/// `drainage`.
///
/// A rung with no eligible chamber at this seed returns all-zero (never
/// `NaN`). **This is not a safe input to [`argmax_index`].** An all-zero
/// seven-entry array is a seven-way tie, and `argmax_index` resolves a tie
/// to the LATER index — `EnergySource::ALL[6]`, `DetritalImport` — so
/// feeding it an all-zero histogram would silently cast a vote for
/// `DetritalImport` dominance with no data behind it at all. Every entry is
/// non-negative and, when `total > 0`, sums to exactly `1.0`; a caller must
/// treat `total == 0` (every entry `0.0`) as "no vote" and exclude that
/// `(seed, rung)` pair from any argmax-based computation rather than mapping
/// it to any index — `composition_separates_worlds_at_some_rung` does this
/// and counts exclusions per rung.
fn normalized_dominant_histogram(wc: &WorldComponents, seed_value: u64, rung: Band) -> [f64; 7] {
    let (terrain, surface) = world_at(seed_value, wc);
    let geo: &Geosphere = terrain.geosphere();
    let moisture_field = subterranean_substrate_field_per_rung(geo, &terrain, &surface);
    let idx = rung as usize;

    let mut counts = [0.0_f64; 7];
    let mut total = 0.0_f64;
    for vertex in geo.vertices() {
        let Some(cave) = terrain.cave_at(vertex) else {
            continue;
        };
        let material = terrain.material_at(vertex);
        let gradient = terrain.geothermal_gradient_at(vertex);
        let drainage = terrain.drainage_at(vertex);
        let Some(depth_m) = rung_evaluation_depth_m(rung, gradient, cave.depth_reach_m) else {
            continue;
        };
        let Some(sub) = moisture_field.get(vertex)[idx] else {
            continue;
        };
        let dominant = dominant_source(&material, gradient, depth_m, sub.moisture, drainage);
        let pos = EnergySource::ALL
            .iter()
            .position(|s| *s == dominant)
            .expect("dominant_source always returns a member of EnergySource::ALL");
        counts[pos] += 1.0;
        total += 1.0;
    }
    if total > 0.0 {
        for c in counts.iter_mut() {
            *c /= total;
        }
    }
    counts
}

/// The index of `h`'s maximum entry.
///
/// Resolves an exact tie by the LATER index, matching
/// [`dominant_source`]'s own documented `Iterator::max_by` behaviour ("if
/// several elements are equally maximum, the last element is returned") —
/// so a tie surfacing in the histogram is broken the same direction a tie
/// in the underlying per-vertex yields would be.
fn argmax_index(h: &[f64]) -> usize {
    h.iter()
        .enumerate()
        .max_by(|(_, a), (_, b)| a.total_cmp(b))
        .map(|(i, _)| i)
        .expect("h is non-empty")
}

/// Total variation distance between two equal-length normalized histograms:
/// `0.5 * Σ|p_i − q_i|` (Task 3, M1's within-rung pairwise distances).
fn total_variation(p: &[f64], q: &[f64]) -> f64 {
    assert_eq!(
        p.len(),
        q.len(),
        "total_variation requires equal-length histograms"
    );
    0.5 * p
        .iter()
        .zip(q.iter())
        .map(|(a, b)| (a - b).abs())
        .sum::<f64>()
}

/// THE CEILING, M1: holding the rung fixed, do worlds disagree about which
/// energy source dominates?
///
/// ```text
/// h(s,r) = normalized dominant-source histogram for seed s at rung r
/// a(s,r) = argmax(h(s,r))
/// M1(r)  = |{ a(s,r) : s in S }|        -- distinct modal sources ACROSS WORLDS
/// M1     = max over r of M1(r)
/// ```
///
/// PREREGISTERED (spec §4): `M1 >= 2`. Two is what decision 0966's quadrants
/// require — the allocation axis must take more than one value across worlds.
/// `M1 == 1` is the falsifier and supersedes 0966.
///
/// **The statistic is PER-RUNG and that is the whole of its validity.**
/// Composition is driven hard by depth, which every world shares, so
/// pooling the rungs would return the falsifier for a methodological
/// reason rather than a substantive one — see this file's module doc and
/// the campaign ledger.
///
/// **PREREGISTRATION MET — measured 2026-09-11, `Q6_SEEDS` (n=12),
/// `BuildDepth::Terrain`.** `M1 = 3`, clearing the `>= 2` bar. Per-rung
/// distinct-argmax counts: Undercroft 2, Shallows 2, Deeps 3, Underdeep 3,
/// Nadir 2 — every rung clears `>= 2` on its own, not only the maximum over
/// rungs. Median within-rung pairwise TV distance ranges from 0.1424
/// (Undercroft) to 0.2436 (Underdeep), well above sampling noise, so the
/// argmax disagreement is not an artifact of near-tied histograms.
/// Composition separates worlds at every measured rung: decision 0966's
/// allocation axis DOES take more than one value across worlds, so its
/// quadrants are reachable. Per spec §3.3's branch table this is the **≥ 3
/// at some rung** row — the row the spec itself flags as the surprising
/// one (row 2, `M1 == 2`, was the expected result; no prior measurement
/// supported richness). Stage 2 authors a consumer whose niche favours a
/// **named dominant source**; the successor inherits a rich allocation
/// axis, and 0966 stands as written.
///
/// **Fix round 1 (Task 3 review), re-measured 2026-09-11.** Two corrections,
/// neither changing `M1`: (1) a zero-total `(seed, rung)` pair — no
/// cave-bearing chamber matched that rung — is now EXCLUDED from the
/// argmax/distinct-count and TV/margin computations, rather than silently
/// feeding an all-zero histogram to `argmax_index`, which would have
/// resolved the seven-way tie to index `6` (`DetritalImport`) and cast a
/// vote with no data behind it. **It did not fire: excluded pairs = 0 at
/// every one of the five rungs**, so `M1 = 3` stands unchanged. (2) each
/// `(seed, rung)`'s top1-vs-top2 margin and each rung's minimum margin
/// across the twelve seeds are now printed — diagnostic only, asserted on
/// nothing. **Measured minimum margins: Undercroft 0.0315, Shallows 0.0341,
/// Deeps 0.0216, Underdeep 0.0009, Nadir 0.0036.** Undercroft and Shallows
/// are comfortably separated; Deeps is modest; **Underdeep and Nadir are
/// near-zero** — both minima land on seed 1 (Underdeep: Methanogenesis
/// 0.29659 vs IronReduction 0.29570; Nadir: IronReduction 0.30018 vs
/// Methanogenesis 0.29659), essentially a coin-flip at that one seed. This
/// does not change either rung's `M1(r)` — Underdeep's distinct set is
/// `{1, 3, 4}` and Nadir's is `{1, 3}` from the other eleven seeds regardless
/// — but it means `M1(r) = 3` at Underdeep is one noise-sensitive seed away
/// from `M1(r) = 2` there, worth carrying into Stage 2's own scrutiny of the
/// surprising row.
///
/// claim: readout(off-gate, prints all sixty histograms, each seed's top1/
/// top2/margin, the per-rung pairwise TV distances, the per-rung minimum
/// margin, and per-rung excluded-pair counts, all before any verdict)
#[test]
#[ignore = "probe: M1, per-rung composition separation; run by hand (The Ceiling, Stage 1)"]
fn composition_separates_worlds_at_some_rung() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let mut per_rung_distinct = Vec::new();
    let mut total_excluded = 0usize;

    for (ri, &rung) in UNDERGROUND_RUNGS.iter().enumerate() {
        let mut argmaxes = Vec::new();
        let mut hists = Vec::new();
        let mut margins = Vec::new();
        let mut excluded = 0usize;
        for &seed in &Q6_SEEDS {
            let h = normalized_dominant_histogram(&wc, seed, rung);
            println!("{rung:?} seed {seed}: h = {h:?}");

            // "No chamber means no vote" (Task 3 fix round 1, Finding 1): an
            // all-zero histogram is a seven-way tie that argmax_index would
            // silently resolve to index 6 (DetritalImport, the later-index
            // tie-break) with no data behind it. Excluded entirely from the
            // argmax/distinct-count and TV/margin computations below, rather
            // than mapped to any index.
            if h.iter().all(|&x| x == 0.0) {
                excluded += 1;
                println!(
                    "{rung:?} seed {seed}: EXCLUDED -- zero cave-bearing chambers matched this rung, no vote cast"
                );
                continue;
            }

            // Top1-vs-top2 margin (Task 3 fix round 1, Finding 2): diagnostic
            // only, asserted on nothing. Makes a coin-flip argmax visible at
            // the per-(seed, rung) level, which the median TV distance alone
            // cannot.
            let mut sorted = h;
            sorted.sort_by(|a, b| b.total_cmp(a));
            let (top1, top2) = (sorted[0], sorted[1]);
            let margin = top1 - top2;
            println!(
                "{rung:?} seed {seed}: top1 = {top1:.4}, top2 = {top2:.4}, margin = {margin:.4}"
            );
            margins.push(margin);

            argmaxes.push(argmax_index(&h));
            hists.push(h);
        }
        let mut distinct: Vec<usize> = argmaxes.clone();
        distinct.sort_unstable();
        distinct.dedup();

        // The TV distances make a noise-driven argmax visible rather than
        // hidden behind the count.
        let mut tvs = Vec::new();
        for i in 0..hists.len() {
            for j in (i + 1)..hists.len() {
                tvs.push(total_variation(&hists[i], &hists[j]));
            }
        }
        tvs.sort_by(f64::total_cmp);
        let min_margin = margins
            .iter()
            .copied()
            .min_by(f64::total_cmp)
            .unwrap_or(f64::NAN);
        println!(
            "{rung:?}: M1(r) = {} distinct argmaxes {:?}, median pairwise TV = {:.4}, \
             minimum top1-top2 margin = {:.4}, excluded pairs = {excluded}",
            distinct.len(),
            distinct,
            median(&mut tvs.clone()),
            min_margin
        );
        if excluded > 0 {
            println!(
                "{rung:?}: {excluded} of {} (seed, rung) pairs EXCLUDED (zero cave-bearing \
                 chambers) -- this is itself a finding, not merely a diagnostic",
                Q6_SEEDS.len()
            );
        }
        total_excluded += excluded;
        per_rung_distinct.push((ri, distinct.len()));
    }

    if total_excluded > 0 {
        println!(
            "TOTAL EXCLUDED (seed, rung) PAIRS ACROSS ALL RUNGS = {total_excluded} \
             (expected 0; see per-rung EXCLUDED lines above)"
        );
    }

    let m1 = per_rung_distinct.iter().map(|(_, n)| *n).max().unwrap_or(0);
    println!("M1 = {m1}");
    assert!(
        m1 >= 2,
        "PREREGISTRATION NOT MET (spec §4): M1 = {m1}. Every world shares one \
         modal source at every rung, so the allocation axis is constant across \
         worlds and decision 0966's quadrants are unreachable. This is the \
         NULL and it is the headline: record it, supersede 0966 with a record \
         choosing between C.3's original two, and take branch-table row 3 or 4. \
         DO NOT retune a source to spread the histogram."
    );
}
