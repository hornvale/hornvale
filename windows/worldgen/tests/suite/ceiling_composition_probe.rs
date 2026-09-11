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
use hornvale_worldgen::energy::EnergySource;
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
/// is M2's DIAGNOSTIC — the rule that discards the least composition, so it
/// bounds what the mean is costing. **Neither is proposed as a replacement.**
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CombinationRule {
    /// `subterranean_energy`'s own rule: the mean of all seven yields.
    MeanOfSeven,
    /// The composition-preserving extreme: the largest single yield.
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
    // below even the mean-of-seven control. This assertion now pins the
    // MEASURED null rather than the originally hoped-for threshold; if it
    // ever fails, the field has moved again and needs a fresh measurement
    // recorded in this test's doc comment, never a source retuned to force
    // either outcome.
    assert!(
        (max - 0.040_745).abs() < 5e-7,
        "MEASURED VALUE CHANGED from the 2026-09-11 reading recorded in this \
         test's doc comment: separation(max-of-seven) was {max:.6}, expected \
         0.040745. Record a fresh measurement with today's date — do NOT \
         retune any EnergySource to force a particular outcome."
    );
}
