//! H1: stone fabrics derived from real bedrock are distinguishable.
//!
//! MEASURED ON REAL TERRAIN, NOT FIXTURES. Every reflectance in this file
//! comes from a world this battery builds — `GeneratedTerrain::material_at`
//! and `rock_at` at the cell a *generated* settlement actually condensed on.
//! No `MaterialBuffer` is authored anywhere in it. The Beholding's
//! 28-of-255 on authored fixtures collapsed to 2-of-255 on real terrain, and
//! this claim is exactly the one that failure mode would fake.
//!
//! **Why this file lives under `windows/vessel/tests/` and not
//! `windows/worldgen/tests/`**, where the plan put it: `hornvale-vessel`
//! depends on `hornvale-worldgen`, so worldgen cannot see `fabric` at all.
//! Vessel's own integration tier already builds live worlds through
//! worldgen (`session_snapshot.rs`, `possession_moves.rs`), so it is the
//! shallowest crate that can hold both halves of the measurement.
//!
//! **Which cell a settlement is on.** A settlement carries its own
//! `hornvale_settlement::CELL_ID` fact, and that is the identical cell the
//! composition root read `climate.biome_at` at when it committed the
//! settlement's biome (`settlement_descriptor_facts`' caller). So fabric and
//! biome cannot disagree here — they are not two calculations that happen to
//! match, they are one fact read twice. (A *room* is the other case, and
//! resolves through `hornvale_locale`'s `dominant_corner`; `fabric.rs`'s
//! module doc records both rules.)
//!
//! **Build depth is `Settlements`**, the shallowest rung that places any:
//! this battery reads terrain, climate and the settlement facts and nothing
//! above them, so a `Full` build would pay for culture, religion, species
//! and deep time it never looks at (`windows/worldgen/CLAUDE.md`'s
//! build-depth-ladder rule).
//!
//! Test fixture (decision 0092): calls `build_world_to_with_artifacts`
//! directly and reuses the terrain and climate the build already produced,
//! rather than re-sculpting them — the same idiom
//! `windows/worldgen/tests/deep_realm_substrate.rs` uses.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::color::{Illuminant, Observer, Reflectance, blackbody, standard_observer};
use hornvale_kernel::{CellId, Seed, Value};
use hornvale_terrain::TerrainPins;
use hornvale_vessel::fabric::{self, Fabric, FabricContext};
use hornvale_vessel::lens::{self, Lens};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};

/// The seeds H1 sweeps. Eight worlds, not one: a single world's settlements
/// are one draw of one plate layout, and one world is an anecdote — seed 42
/// alone has given four wrong readings in this project's history.
const SEEDS: [u64; 8] = [1, 7, 42, 99, 256, 1024, 4096, 9001];

/// The light H1 reads stone under: a 5800 K blackbody.
///
/// **Deliberately not spectrally flat.** A flat illuminant is the exact
/// shape of a guard that cannot fail — The Beholding shipped a flat probe
/// that cancelled the effect it was measuring — and flatness here would
/// mean every band of a reflectance contributes only through the observer's
/// own curve, erasing the illuminant's part of the three-way product.
///
/// 5800 K specifically because it is the row the spec's §2 table already
/// publishes for daylight, so H1's numbers sit on the same axis as the
/// campaign's other measurements instead of on a private one. It is also
/// the honest reference for a *material* claim: H1 asks whether two stones
/// differ, and answering that under a hearth would confound the material
/// with the flame.
fn reference_light() -> Illuminant {
    blackbody(5800.0)
}

/// A reflectance rendered to screen bytes under [`reference_light`], through
/// the human-calibrated standard observer.
fn srgb_under_reference_light(observer: &Observer, refl: &Reflectance) -> [u8; 3] {
    observer
        .to_srgb(&observer.sense(refl, &reference_light()))
        .expect("the standard observer declares a projection")
}

/// The ground under every generated settlement of one seed's world, read at
/// the settlement's own committed cell.
///
/// Builds the world; never authors a placement, a buffer or a rock class.
/// **No land-only filter**: settlements can be marine (founded on land that
/// later drowned), and `rock_at` answers for any cell, so filtering here
/// would silently drop real settlements.
fn settlement_ground(seed: u64) -> Vec<FabricContext> {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let artifacts = build_world_to_with_artifacts(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Settlements,
    )
    .unwrap_or_else(|e| panic!("seed {seed} failed to build: {e:?}"));
    let terrain = artifacts
        .terrain
        .as_ref()
        .expect("BuildDepth::Settlements produces terrain");
    let climate = artifacts
        .climate
        .as_ref()
        .expect("BuildDepth::Settlements produces climate");
    let world = &artifacts.world;

    let settlements = hornvale_settlement::all_settlements(world);
    assert!(
        !settlements.is_empty(),
        "seed {seed} condensed no settlements: H1 cannot be measured on an \
         empty world, and an empty sweep would pass a max-spread assertion \
         vacuously"
    );
    settlements
        .iter()
        .map(|s| {
            let cell = match world.ledger.value_of(s.id, hornvale_settlement::CELL_ID) {
                Some(Value::Number(n)) => CellId(*n as u32),
                _ => panic!("settlement {} has no cell-id fact", s.id.0),
            };
            FabricContext::at(terrain, climate, cell)
        })
        .collect()
}

/// Every sampled settlement's stone wall, as screen bytes. The one sweep
/// both H1 assertions read, so they cannot drift apart about what was
/// measured.
fn all_settlement_stone_triples() -> Vec<[u8; 3]> {
    let observer = standard_observer();
    let mut out = Vec::new();
    for seed in SEEDS {
        for ctx in settlement_ground(seed) {
            let refl = fabric::reflectance_of(Fabric::Stone, &ctx);
            out.push(srgb_under_reference_light(&observer, &refl));
        }
    }
    out
}

/// The widest single channel spans the sample, in `u8` steps: for each of
/// the three output slots, `max - min`, then the largest of the three.
fn max_channel_spread(triples: &[[u8; 3]]) -> u16 {
    let mut widest = 0u16;
    for slot in 0..3 {
        let mut lo = u8::MAX;
        let mut hi = u8::MIN;
        for t in triples {
            lo = lo.min(t[slot]);
            hi = hi.max(t[slot]);
        }
        widest = widest.max(u16::from(hi) - u16::from(lo));
    }
    widest
}

/// For every unordered pair, the largest per-channel difference, in `u8`
/// steps. `f64`-valued because the caller wants percentiles of it, not
/// because the quantity is continuous — these are whole screen steps, the
/// unit H1's "visibly different" is a claim about.
fn pairwise_max_channel_diffs(triples: &[[u8; 3]]) -> Vec<f64> {
    let mut out = Vec::with_capacity(triples.len() * triples.len() / 2);
    for (i, a) in triples.iter().enumerate() {
        for b in &triples[i + 1..] {
            let d = (0..3)
                .map(|s| u16::from(a[s]).abs_diff(u16::from(b[s])))
                .max()
                .expect("three slots");
            out.push(f64::from(d));
        }
    }
    out
}

/// H1 — two settlements on different bedrock produce stone walls differing
/// by more than one `u8` step in at least one channel.
///
/// Measured on REAL TERRAIN across the eight seeds in [`SEEDS`], on every
/// settlement each world generated — never an authored `MaterialBuffer`.
///
/// FIRES WHEN: bedrock variation across settlements is too small to survive
/// the fabric transform — the spec §11 risk 1 outcome, in which the campaign
/// ships walls that all look alike. **A falsified H1 is a finding, not a
/// failure**: it means fabric needs a second axis, and it must be reported
/// rather than rescued by widening the threshold.
///
/// claim: readout(preregistered, 0016) — H1 is a preregistered measured
/// distribution (spec §3, §6), not a per-seed property: the assertion is on an
/// AGGREGATE over the swept population (max channel spread across every
/// settlement of every seed), so no single seed can satisfy or violate it.
#[test]
fn h1_stone_fabrics_differ_across_settlements() {
    let triples = all_settlement_stone_triples();
    let spread = max_channel_spread(&triples);
    eprintln!(
        "H1: {} settlements over {} seeds, max channel spread {spread} u8 steps",
        triples.len(),
        SEEDS.len()
    );
    assert!(
        spread > 1,
        "H1 FALSIFIED: {} sampled settlements span only {spread} u8 steps \
         in every channel — derived stone cannot vary. Report this; do not \
         widen the threshold.",
        triples.len()
    );
}

/// The distribution, not the extremum. A single pair of outlier settlements
/// can satisfy H1 while every ordinary pair is identical, and a max-only
/// reading cannot tell those apart.
///
/// Same substrate as the sibling assertion: real terrain, eight seeds, every
/// generated settlement.
///
/// FIRES WHEN: the median pairwise difference collapses even though the
/// extremes are far apart.
///
/// The **linear** spread is printed beside the quantized one and asserted on
/// nothing. That is deliberate: a `u8` rounding step silently absorbing a
/// regression is a shipped failure mode in this project, so the unrounded
/// number is on the record for a reader to compare against — but authoring
/// a second threshold over it would be inventing a claim the spec never
/// preregistered.
///
/// claim: readout(preregistered, 0016) — literally a distribution: p10, median
/// and max over 1,131,760 pairs. A median floor cannot see a heavy tail, which
/// is why this reports percentiles rather than one number.
#[test]
fn h1_reports_the_whole_distribution_not_just_the_extremes() {
    let observer = standard_observer();
    let mut triples = Vec::new();
    let mut linear_lo = [f64::INFINITY; 3];
    let mut linear_hi = [f64::NEG_INFINITY; 3];
    for seed in SEEDS {
        for ctx in settlement_ground(seed) {
            let refl = fabric::reflectance_of(Fabric::Stone, &ctx);
            let signal = observer.sense(&refl, &reference_light());
            for (slot, channel) in [2usize, 1, 0].iter().enumerate() {
                let v = signal.get()[*channel];
                linear_lo[slot] = linear_lo[slot].min(v);
                linear_hi[slot] = linear_hi[slot].max(v);
            }
            triples.push(srgb_under_reference_light(&observer, &refl));
        }
    }

    let mut diffs = pairwise_max_channel_diffs(&triples);
    assert!(
        !diffs.is_empty(),
        "fewer than two settlements were sampled: a percentile of an empty \
         distribution would assert nothing"
    );
    diffs.sort_by(|a, b| a.total_cmp(b));
    let median = diffs[diffs.len() / 2];
    let p10 = diffs[diffs.len() / 10];
    eprintln!(
        "H1 distribution over {} settlements ({} pairs): p10 {p10}, median {median}, max {}",
        triples.len(),
        diffs.len(),
        diffs[diffs.len() - 1]
    );
    eprintln!(
        "H1 pre-quantization channel spans (linear signal, R/G/B): {:.6} {:.6} {:.6}",
        linear_hi[0] - linear_lo[0],
        linear_hi[1] - linear_lo[1],
        linear_hi[2] - linear_lo[2]
    );
    assert!(
        median > 0.0,
        "H1 median pairwise difference is zero: the typical pair of \
         settlements is IDENTICAL even if the extremes differ"
    );
}

/// One pair of settlements the model kept a single `u8` step apart, before and
/// after the lens: `(a, b, lensed_a, lensed_b)`. Reported so a reader sees the
/// actual arithmetic rather than only the verdict.
type WorkedPair = ([u8; 3], [u8; 3], [u8; 3], [u8; 3]);

/// **The lens must not undo what H1 measured** (The Lantern, Task 8, spec §7).
///
/// H1's own distribution is what makes this dangerous: the median pair differs
/// by 41 `u8` steps, but **p10 = 1** — a tenth of settlement pairs differ by a
/// single step, because settlements cluster on shared rock classes. A lens that
/// compresses dynamic range erases that whole decile, and the median goes on
/// looking fine while it happens. So the tail is checked, not the middle:
/// **every** pair the model can only just tell apart must still be told apart
/// after the lens.
///
/// Measured on the same real sweep as the two H1 assertions above — the same
/// 1505 settlements, the same eight seeds, the same derived bedrock — because a
/// synthetic pair one step apart would answer a different question. The
/// sibling `lantern_lens.rs` proves the same property exhaustively over the
/// input range; this one proves it on the population that actually exists.
///
/// FIRES WHEN: the lens compresses anywhere H1's real stone lives.
///
/// claim: invariant(forall-seed) — universally quantified over the pinned
/// eight-seed set: EVERY pair one `u8` step apart must stay apart. One
/// counterexample falsifies it, so a violation names the pair.
#[test]
fn the_lens_preserves_every_pair_h1_can_barely_tell_apart() {
    let triples = all_settlement_stone_triples();
    let mut barely_apart = 0usize;
    let mut worked_example: Option<WorkedPair> = None;
    for (i, a) in triples.iter().enumerate() {
        for b in &triples[i + 1..] {
            let before = (0..3)
                .map(|s| u16::from(a[s]).abs_diff(u16::from(b[s])))
                .max()
                .expect("three slots");
            if before != 1 {
                continue;
            }
            barely_apart += 1;
            let la = lens::apply(&Lens::default(), *a);
            let lb = lens::apply(&Lens::default(), *b);
            let after = (0..3)
                .map(|s| u16::from(la[s]).abs_diff(u16::from(lb[s])))
                .max()
                .expect("three slots");
            worked_example.get_or_insert((*a, *b, la, lb));
            assert!(
                after >= 1,
                "the lens collapsed {a:?} and {b:?} — two settlements the model \
                 kept one u8 step apart — onto {la:?} and {lb:?}. That is a real \
                 distinction destroyed, not a rounding artifact; report it rather \
                 than shipping it."
            );
        }
    }
    assert!(
        barely_apart > 0,
        "no pair of the {} sampled settlements is exactly one u8 step apart, so \
         this guard checked nothing — H1's p10 decile has moved and the claim \
         needs re-measuring, not this assertion relaxing",
        triples.len()
    );
    let (a, b, la, lb) = worked_example.expect("a worked example exists once a pair does");
    eprintln!(
        "lens vs H1's p10: {barely_apart} pairs sit one u8 step apart; \
         e.g. {a:?} vs {b:?} -> {la:?} vs {lb:?}"
    );
}
