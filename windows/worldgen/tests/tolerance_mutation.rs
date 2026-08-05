//! The Tolerance, Task 5: **the mutation proof** — and the campaign's named
//! zero-dispersion baseline.
//!
//! A green test proves the code ran. Only the mutation proves the axis is
//! *visible*: setting a people's dispersion to zero must collapse its
//! between-settlement variance to zero. If it does not, the dispersion
//! parameter is not being read, and every preregistered result of this
//! campaign is an artifact of something else.
//!
//! ## What is measured, and why it is NOT the raid outcome (task ruling 1)
//!
//! The measured quantity is the **gate input**: the `threat_response` a
//! settlement's community actually draws at `Bake::open`, i.e. exactly the
//! expression `Bake::drawn_disposition` evaluates —
//! `drawn_threat_response(world seed, site, occupation_draw_key(founded),
//! authored location, authored spread)` — and, derived from it, whether the
//! gate opens (`>= RAID_DISPOSITION_MIN`).
//!
//! It is deliberately **not** the realized raid rate.
//! `Bake::takes_the_initiative` is only one conjunct of the raid decision; the
//! other is `Bake::can_fight`, which reads population and strength and
//! **already varies per settlement**, independently of anything this campaign
//! added. `windows/worldgen/tests/tolerance_baseline.rs` measured precisely
//! that: victim-side raid-outcome variance of 0.029–0.22 *before any dispersion
//! existed*. So zeroing dispersion cannot collapse raid-outcome variance to
//! zero even for a perfectly correct implementation, and a mutation proof
//! written against that quantity would fail on correct code — the two ways out
//! (weakening the assertion, or retuning) are both forbidden here.
//!
//! Against the gate input the claim is **exact**: at spread 0 the perturbation
//! is `(location + unit·√3·0.0).clamp(0, 1) = location` for every authored
//! location in `[0, 1]`, so every settlement of a people receives bit-for-bit
//! the same value. The zeroed arm's variance is therefore expected to be
//! exactly 0, not merely small, and
//! [`every_zeroed_draw_is_the_authored_location`] below asserts the stronger
//! per-settlement equality that produces it.
//!
//! ## The matched pair, and why this file is also the BASELINE (task ruling 2)
//!
//! `origin/main` was absorbed into this branch at merge `a025e55a`, bringing
//! The Keeping — which changed the placement gate and moved world identity.
//! Task 1's pre-dispersion numbers (`tolerance_baseline.rs`) were measured on
//! *pre-merge* physics, so they are no longer a clean "before" for this
//! campaign's readout.
//!
//! **The zero-dispersion arm below replaces them as the comparison point.** It
//! is a matched pair — the same settlements, the same worlds, one run, one
//! physics — which is strictly better evidence than a before/after spanning a
//! merge. Task 6 should compare against the per-people zero-dispersion
//! baseline this file prints. Task 1's numbers stand as the *pre-merge record*
//! and are neither deleted nor re-pointed.
//!
//! Both arms are evaluated over one population, collected once, precisely so
//! the pair is matched. That is why [`between_settlement_variance`] takes the
//! population as an argument rather than rebuilding worlds per call.
//!
//! ## The population
//!
//! Every occupation record of the six settling peoples over seeds
//! [`SEEDS`] (`1..=30`), built through
//! `hornvale_worldgen::history_for` at `BuildDepth::Terrain` — the same entry
//! point, depth, seed range and roster `tolerance_baseline.rs` uses, so the two
//! files measure the same population. **All** records, not only the settlements
//! alive at `now`: the gate reads a community's drawn disposition from the
//! moment it opens, so a ruin was gated on its own draw exactly as a standing
//! settlement was.
//!
//! Roughly 3–15% of occupation records share a `(site, founded-year)` draw key
//! with another record (measured in
//! `tolerance_draw.rs::the_draw_key_is_reachable_and_its_uniqueness_has_the_measured_shape`);
//! those records share a drawn value. That is a disclosed property of the key,
//! not of this measurement — it can only *reduce* the real arm's variance, so
//! it cannot manufacture the contrast this file reports.
//!
//! ## What this file proves about the live pipeline, and what it does not
//!
//! The variance comparison is a statement about the derivation the gate calls,
//! evaluated on the keys real worlds actually produced. The remaining link —
//! that the *shipped bake* gates on a drawn value rather than on its people's
//! authored constant — is checked separately and live, by
//! [`the_shipped_bake_gates_on_a_drawn_value_not_the_authored_constant`]'s
//! reading of raid initiators: `human` and `goblin` are authored at
//! `threat_response` 0.5, below the 0.6 gate, so **before this campaign
//! neither could ever take the initiative**. Observing their settlements
//! initiating raids on live worlds is only possible if the gate reads
//! something that varies per settlement.
//!
//! What that observation excludes is exactly the pre-Tolerance behaviour (a
//! gate reading the per-people authored constant). It does not, on its own,
//! distinguish a config that never received human's disposition at all —
//! `Bake::takes_the_initiative` fails *open* on `None`. That case is covered
//! where it is reachable: by `history_bake.rs`'s in-crate tests of
//! `BakeConfig::disposition`/`disposition_spread`, and by
//! `tolerance_draw.rs`'s two-sided agreement pin. `bake_history_from` fills the
//! two maps from adjacent lines over one `peoples` list, so a roster reaching
//! one and not the other is not a state this crate can be in.
//!
//! Ignored: builds 30 worlds. Reason token `heavy:` puts it in the heavy tier
//! (`cli/tests/heavy_tier.rs`), not the commit gate.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_history::record::{CauseOfEnd, Ended};
use hornvale_kernel::{CellId, KindId, Seed};
use hornvale_species::Dispersion;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::disposition::{drawn_threat_response, occupation_draw_key};
use hornvale_worldgen::{SettlementPins, SkyChoice, WorldComponents, history_for};
use std::collections::BTreeMap;

/// Seeds `1..=30` — the range `tolerance_baseline.rs` pools over, matched here
/// so Task 6 compares populations rather than seed ranges.
const SEEDS: std::ops::RangeInclusive<u64> = 1..=30;

/// The six settling peoples (post-Generalist roster), mirroring
/// `tolerance_baseline.rs`'s constant of the same name.
const PEOPLES_WITH_HUMAN: [&str; 6] =
    ["bugbear", "gnoll", "goblin", "hobgoblin", "human", "kobold"];

/// The raid-initiative threshold the gate compares a drawn disposition
/// against, mirroring `hornvale_worldgen::history_bake`'s private
/// `RAID_DISPOSITION_MIN` (the same restatement
/// `windows/lab/tests/disposition_calibration.rs` and
/// `tolerance_baseline.rs` make — the constant is crate-private, and this is
/// the established precedent for reading it from outside).
const RAID_DISPOSITION_MIN: f64 = 0.6;

/// The floor the authored-dispersion arm must clear, **calibrated against the
/// measurement it guards** rather than picked a priori.
///
/// Measured on 2026-08-04 over [`SEEDS`] (`1..=30`), pooled across all **2_305
/// `human` occupation records** those thirty worlds produced:
///
/// - authored dispersion (σ = 0.35): between-settlement variance
///   **0.113_103_002_1** (mean 0.507_153, gate open on 42.34% of settlements)
/// - zeroed dispersion: between-settlement variance **0.000_000_000_0**
///   (exactly 0.0; every settlement drew the authored 0.5, gate open on 0.00%)
///
/// `0.01` sits an order of magnitude below the measured authored value and
/// unboundedly above the measured zeroed one, so it separates the two arms
/// without pinning either. It is a *floor on a live measurement*, not a
/// golden: a real arm that fell to it would mean the authored spread had
/// stopped reaching the draw, which is the failure this file exists to catch.
///
/// **It is calibrated for `human` specifically and is not a roster-wide
/// floor.** Dispersion is authored per people, and the same run measured
/// hobgoblin (σ = 0.10) at variance 0.010_235 — above this floor by 2%, not by
/// an order of magnitude. A witness other than human needs its own floor,
/// measured the same way.
const VARIANCE_FLOOR: f64 = 0.01;

/// One settlement-occupation, reduced to the draw key its community was gated
/// on: the world it belongs to, its people, and the `(site, founded-year)`
/// pair `Bake::drawn_disposition` keys on.
struct Settlement {
    /// The world seed — the same `Seed` the bake derives its streams from.
    seed: u64,
    /// The people occupying the site.
    people: KindId,
    /// The Geosphere cell the occupation sits on.
    site: CellId,
    /// The founding year, reduced through `occupation_draw_key`.
    founded_key: i64,
}

/// One arm's reading for one people.
struct Reading {
    /// Settlements measured.
    n: usize,
    /// Mean drawn gate input.
    mean: f64,
    /// Population variance of the drawn gate input across settlements.
    variance: f64,
    /// Share of settlements whose draw opens the gate
    /// (`>= RAID_DISPOSITION_MIN`).
    gate_open: f64,
}

/// The authored per-people inputs, resolved from **the same two sources the
/// composition root resolves them from** (`lib.rs::bake_history_from`): the
/// location off `WorldComponents::psyche`, the spread off
/// `hornvale_species::dispersion_registry()`. Returned as bare ratios, which
/// is also how the bake receives them.
fn authored(wc: &WorldComponents) -> (BTreeMap<KindId, f64>, BTreeMap<KindId, f64>) {
    let dispersion = hornvale_species::dispersion_registry();
    let mut locations = BTreeMap::new();
    let mut spreads = BTreeMap::new();
    for (kind, psyche) in wc.psyche.iter() {
        if !PEOPLES_WITH_HUMAN.contains(&kind.0) {
            continue;
        }
        locations.insert(*kind, psyche.threat_response);
        let spread = dispersion
            .get(kind)
            .unwrap_or_else(|| panic!("{}: no authored dispersion reached this test", kind.0))
            .mind;
        spreads.insert(*kind, spread);
    }
    (locations, spreads)
}

/// Build every seed in [`SEEDS`] and return one [`Settlement`] per occupation
/// record of the six settling peoples, plus the number of raids each people
/// **initiated** (resolved victim-side: a raid closes its victim's record with
/// `CauseOfEnd::Fled` + `Ended::By(raider_id)`, and `Bake::open` mints a fresh
/// id per record, so `raider_id` names exactly one record — the raider's).
fn population(wc: &WorldComponents) -> (Vec<Settlement>, BTreeMap<KindId, u64>) {
    let mut rows = Vec::new();
    let mut initiated: BTreeMap<KindId, u64> = BTreeMap::new();
    for seed in SEEDS {
        let history = history_for(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            wc,
        )
        .unwrap_or_else(|e| panic!("seed {seed} failed to build: {e:?}"));

        let by_community: BTreeMap<_, KindId> = history
            .records
            .iter()
            .map(|rec| (rec.community, rec.core.people))
            .collect();

        for rec in &history.records {
            if matches!(rec.core.cause, Some(CauseOfEnd::Fled))
                && let Ended::By(raider) = rec.ended_by
            {
                let people = by_community.get(&raider).copied().unwrap_or_else(|| {
                    panic!("seed {seed}: raider {raider:?} names no occupation record")
                });
                *initiated.entry(people).or_default() += 1;
            }
            if !PEOPLES_WITH_HUMAN.contains(&rec.core.people.0) {
                continue;
            }
            rows.push(Settlement {
                seed,
                people: rec.core.people,
                site: rec.core.site,
                founded_key: occupation_draw_key(rec.core.founded),
            });
        }
    }
    (rows, initiated)
}

/// Every drawn gate input for one people, under one dispersion regime.
///
/// `spread_override` is the mutation seam: `None` reads the people's authored
/// `Dispersion::mind` (the shipped regime — the same value
/// `bake_history_from` hands the bake through `BakeConfig::disposition_spread`),
/// `Some(d)` substitutes `d.mind` for it. The authored registry is never
/// mutated.
fn drawn_values(
    pop: &[Settlement],
    people: KindId,
    spread_override: Option<Dispersion>,
    locations: &BTreeMap<KindId, f64>,
    spreads: &BTreeMap<KindId, f64>,
) -> Vec<f64> {
    let location = locations[&people];
    let spread = match spread_override {
        Some(d) => d.mind,
        None => spreads[&people],
    };
    pop.iter()
        .filter(|s| s.people == people)
        .map(|s| drawn_threat_response(Seed(s.seed), s.site, s.founded_key, location, spread))
        .collect()
}

/// The between-settlement reading for one people under one dispersion regime
/// — the mutation proof's measured quantity (see the module doc: the gate
/// INPUT, never the raid outcome).
fn between_settlement_variance(
    pop: &[Settlement],
    people: KindId,
    spread_override: Option<Dispersion>,
    locations: &BTreeMap<KindId, f64>,
    spreads: &BTreeMap<KindId, f64>,
) -> Reading {
    let vals = drawn_values(pop, people, spread_override, locations, spreads);
    assert!(!vals.is_empty(), "{}: no settlements sampled", people.0);
    let n = vals.len();
    let mean = vals.iter().sum::<f64>() / n as f64;
    let variance = vals.iter().map(|v| (v - mean) * (v - mean)).sum::<f64>() / n as f64;
    let open = vals.iter().filter(|v| **v >= RAID_DISPOSITION_MIN).count();
    Reading {
        n,
        mean,
        variance,
        gate_open: open as f64 / n as f64,
    }
}

/// A dispersion of exactly zero on every axis — a people that is a point, not
/// a distribution. This is the model's pre-Tolerance behaviour expressed as a
/// parameter value.
const ZERO_DISPERSION: Dispersion = Dispersion {
    mind: 0.0,
    society: 0.0,
    perception: 0.0,
};

/// **The mutation proof.** Setting a people's dispersion to zero MUST collapse
/// its between-settlement variance to zero. If it does not, the dispersion
/// parameter is not being read and every H2 result is an artifact of something
/// else.
///
/// `human` is the witness: the widest authored dispersion on the roster
/// (σ = 0.35), and an authored location (0.5) below the 0.6 gate — so at
/// spread 0 it raids at exactly 0% and at authored spread at a fraction. That
/// is the largest contrast the shipped roster offers.
///
/// This test also prints the campaign's **named zero-dispersion baseline** for
/// all six settling peoples (task ruling 2) — the arm Task 6 compares against.
/// H1/H2/H3 are reported here, never asserted: the assertions below are a
/// *parameter-is-read* proof, not a preregistered prediction.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn zero_dispersion_collapses_between_settlement_variance() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let (locations, spreads) = authored(&wc);
    assert_eq!(
        locations.len(),
        6,
        "all six settling peoples must carry an authored threat_response; got {:?}",
        locations.keys().collect::<Vec<_>>()
    );
    let (pop, initiated) = population(&wc);
    assert!(!pop.is_empty(), "no settlements sampled");

    let human = KindId("human");
    let real = between_settlement_variance(&pop, human, None, &locations, &spreads);
    let zeroed =
        between_settlement_variance(&pop, human, Some(ZERO_DISPERSION), &locations, &spreads);

    println!(
        "MUTATION (human, n = {} settlements over seeds {:?}): authored sigma = {:.4} -> \
         variance {:.10} (mean {:.6}, gate-open {:.4}); zeroed -> variance {:.10} \
         (mean {:.6}, gate-open {:.4})",
        real.n,
        SEEDS,
        spreads[&human],
        real.variance,
        real.mean,
        real.gate_open,
        zeroed.variance,
        zeroed.mean,
        zeroed.gate_open
    );

    assert!(
        real.variance > VARIANCE_FLOOR,
        "authored dispersion produces no variance: {} (floor {VARIANCE_FLOOR}) — the \
         authored spread is not reaching the draw",
        real.variance
    );
    assert!(
        zeroed.variance < 1e-12,
        "zeroing dispersion did NOT collapse variance ({}) — the parameter is not being \
         read, so H2 proves nothing",
        zeroed.variance
    );

    // THE NAMED ZERO-DISPERSION BASELINE (task ruling 2). Reported for every
    // settling people, alongside the authored arm it is the control for.
    println!("--- zero-dispersion baseline (matched pair, seeds {SEEDS:?}) ---");
    for name in PEOPLES_WITH_HUMAN {
        let people = KindId(name);
        let real = between_settlement_variance(&pop, people, None, &locations, &spreads);
        let zeroed =
            between_settlement_variance(&pop, people, Some(ZERO_DISPERSION), &locations, &spreads);
        println!(
            "{name}: n = {}, authored location = {:.4}, sigma = {:.4} | AUTHORED mean \
             {:.6} var {:.10} gate-open {:.6} | ZEROED mean {:.6} var {:.10} gate-open {:.6}",
            real.n,
            locations[&people],
            spreads[&people],
            real.mean,
            real.variance,
            real.gate_open,
            zeroed.mean,
            zeroed.variance,
            zeroed.gate_open
        );
        assert!(
            zeroed.variance < 1e-12,
            "{name}: zeroing dispersion left variance {} standing",
            zeroed.variance
        );
    }

    // The live-pipeline half (see the module doc). Reported here because the
    // population it reads is already in hand; asserted in its own test below.
    println!("--- raids INITIATED, by the initiator's people (live bake) ---");
    for (people, count) in &initiated {
        println!("{}: {count}", people.0);
    }
}

/// Every settlement of a zero-dispersion people draws its people's authored
/// location **exactly** — the per-settlement equality that makes the mutation
/// proof's collapsed variance an exact zero rather than a small number.
///
/// Cheap: no world is built. It sweeps synthetic keys across the default bake
/// grid, which is enough — spread 0 annihilates the drawn offset before the
/// key can matter.
#[test]
fn every_zeroed_draw_is_the_authored_location() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let (locations, _) = authored(&wc);
    for (people, &location) in &locations {
        for year in [0i64, 25, 725, 1975, 2000] {
            for cell in [0u32, 7, 4242, 65_535] {
                assert_eq!(
                    drawn_threat_response(Seed(42), CellId(cell), year, location, 0.0),
                    location,
                    "{}: spread 0 moved the draw off the authored location at cell \
                     {cell}, year {year}",
                    people.0
                );
            }
        }
    }
}

/// **The live half of the mutation proof.** `human` and `goblin` are authored
/// at `threat_response` 0.5, below `RAID_DISPOSITION_MIN` — so a gate reading a
/// people's authored constant (the pre-Tolerance gate) could never let either
/// take the initiative. Their settlements initiating raids on live worlds is
/// therefore only possible if the shipped bake gates on a value that varies per
/// settlement.
///
/// Measured 2026-08-04 over seeds `1..=6`: goblin initiated 64 raids and human
/// 70, against 1_280 for the whole roster. The assertion asks only for a
/// non-zero count from each, so it carries roughly two orders of magnitude of
/// headroom — it is a wiring check, not a rate, and it is not a hypothesis
/// about how often either people raids.
///
/// A separate, cheaper test than the mutation proof above (six worlds, not
/// thirty) so that the wiring question can be answered without the full
/// battery; it is heavy-tier all the same.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn the_shipped_bake_gates_on_a_drawn_value_not_the_authored_constant() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let (locations, _) = authored(&wc);
    let mut initiated: BTreeMap<KindId, u64> = BTreeMap::new();
    for seed in 1u64..=6 {
        let history = history_for(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
        )
        .unwrap_or_else(|e| panic!("seed {seed} failed to build: {e:?}"));
        let by_community: BTreeMap<_, KindId> = history
            .records
            .iter()
            .map(|rec| (rec.community, rec.core.people))
            .collect();
        for rec in &history.records {
            if matches!(rec.core.cause, Some(CauseOfEnd::Fled))
                && let Ended::By(raider) = rec.ended_by
            {
                let people = by_community.get(&raider).copied().unwrap_or_else(|| {
                    panic!("seed {seed}: raider {raider:?} names no occupation record")
                });
                *initiated.entry(people).or_default() += 1;
            }
        }
    }
    let total: u64 = initiated.values().sum();
    for (people, count) in &initiated {
        println!("{}: initiated {count} raids", people.0);
    }
    assert!(total > 0, "no raids were initiated at all over seeds 1..=6");
    for name in ["human", "goblin"] {
        let people = KindId(name);
        assert!(
            locations[&people] < RAID_DISPOSITION_MIN,
            "{name} is no longer authored below the gate ({} >= {RAID_DISPOSITION_MIN}); \
             re-pick this test's witness, it proves nothing about a people that would \
             raid anyway",
            locations[&people]
        );
        assert!(
            initiated.get(&people).copied().unwrap_or(0) > 0,
            "{name} (authored {:.2}, below the {RAID_DISPOSITION_MIN} gate) initiated no \
             raids in {total} — the shipped bake is still gating on the authored \
             per-people constant, so the per-settlement draw is not reaching it",
            locations[&people]
        );
    }
}
