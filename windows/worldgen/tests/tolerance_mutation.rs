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
//! ## THE NAMED ZERO-DISPERSION BASELINE — committed, not merely printed
//!
//! Measured 2026-08-04 over seeds `1..=30`, every occupation record of each
//! settling people. The AUTHORED arm is the shipped regime; the ZEROED arm is
//! the baseline Task 6 compares against. Both are re-emitted by
//! [`zero_dispersion_collapses_between_settlement_variance`]'s `println!`,
//! which is how this table is re-measured — but the table lives here, in
//! committed text, because a comparison point a reader on main cannot see
//! without dispatching a heavy run to lefford is not a baseline.
//!
//! ```text
//!   people        n   location   sigma  | AUTHORED mean       var  gate-open | ZEROED mean       var  gate-open
//!   ---------  ----  ---------  ------  | ------------- --------- ---------- | ----------- --------- ----------
//!   bugbear    4163       0.80    0.20  |      0.778766  0.032630   0.771079  |    0.800000  0.000000   1.000000
//!   gnoll      4057       0.85    0.22  |      0.819846  0.031197   0.833374  |    0.850000  0.000000   1.000000
//!   goblin     1945       0.50    0.25  |      0.490512  0.060181   0.360411  |    0.500000  0.000000   0.000000
//!   hobgoblin  4065       0.70    0.10  |      0.700371  0.010235   0.784994  |    0.700000  0.000000   1.000000
//!   human      2305       0.50    0.35  |      0.507153  0.113103   0.423427  |    0.500000  0.000000   0.000000
//!   kobold     3461       0.80    0.12  |      0.800680  0.014493   0.982953  |    0.800000  0.000000   1.000000
//! ```
//!
//! In every zeroed arm each settlement drew its people's authored location bit
//! for bit — which is the whole content of the mutation proof, read across the
//! roster instead of on the witness alone. The *variance* of those identical
//! values is exactly `0.0` for human and goblin, whose authored 0.5 is exactly
//! representable, and is a summation residue of order 1e-27 for the peoples
//! authored at 0.7/0.8/0.85. That residue is a property of computing a mean in
//! IEEE arithmetic, not of the model, which is why the assertions below pin the
//! per-settlement values exactly and the variance to a stated bound.
//!
//! Two things the table says that the human row alone does not:
//!
//! - **The gate-open share is the axis's visible consequence.** Zeroed, a
//!   people is entirely inside or entirely outside the gate (0.0 or 1.0);
//!   authored, every people sits strictly between. That is the difference
//!   between a roster of six kinds and a roster of thousands of settlements.
//! - **The authored means are displaced toward the interior on exactly the
//!   axes the clamp table predicts.** `disposition.rs`'s disclosed table
//!   predicts a `threat_response` shift for bugbear (−0.0155) and gnoll
//!   (−0.0350) and none for the rest; measured here, bugbear came in at −0.021
//!   and gnoll at −0.030, while hobgoblin (+0.0004), kobold (+0.0007) and
//!   human (+0.007, its two bounds clamping symmetrically) sit at draw noise.
//!   That is the clamping bias measured on the live population rather than
//!   derived. **Reported, not asserted** — H1 is not this file's to adjudicate.
//!
//! ## What this file proves about the live pipeline, and what it does not
//!
//! The variance comparison is a statement about the derivation the gate calls,
//! evaluated on the keys real worlds actually produced. Two further links are
//! needed before that is a statement about the shipped pipeline, and each has
//! its own test:
//!
//! 1. **The bake gates on a drawn value, not on its people's authored
//!    constant** —
//!    [`the_shipped_bake_gates_on_a_drawn_value_not_the_authored_constant`],
//!    live and in the commit gate. `human` and `goblin` are authored at
//!    `threat_response` 0.5, below the 0.6 gate, so **before this campaign
//!    neither could ever take the initiative**. Observing their settlements
//!    initiating raids on live worlds is only possible if the gate reads
//!    something that varies per settlement.
//! 2. **The composition root hands the bake the AUTHORED spread** —
//!    `hornvale_worldgen`'s in-crate
//!    `the_composition_root_hands_the_bake_the_authored_dispersion`, at the
//!    `disposition_maps` seam in `src/lib.rs`. No integration test can reach
//!    this: `Community.disposition` is private and is never committed, so the
//!    value the bake gated on is unobservable from outside the crate. Task 5's
//!    review demonstrated the hole by handing every people a fabricated spread
//!    of 0.15 — all three tests in *this* file stayed green while the mutation
//!    proof reported a σ of 0.35 it had read from the registry. Nothing here
//!    closes that; the in-crate test does.
//!
//! What link 1 excludes on its own is exactly the pre-Tolerance behaviour (a
//! gate reading the per-people authored constant). It does not distinguish a
//! config that never received human's disposition at all —
//! `Bake::takes_the_initiative` fails *open* on `None`. That residue is closed
//! here rather than deferred: [`authored`]'s `locations.len() == 6` proves all
//! six settling peoples carry an authored `threat_response` in `wc.psyche`, and
//! a non-empty human population proves human is in the bake's `peoples` list —
//! and `disposition_maps` is a `filter_map` over exactly those two, so
//! `cfg.disposition` containing human is forced, not assumed.
//!
//! The two maps are coextensive on the shipped roster, but **not** because they
//! are built from one candidate list: they read different registries
//! (`wc.psyche` and `hornvale_species::dispersion_registry()`), so one
//! `peoples` list can yield two different key sets. What makes them coextensive
//! is Task 2's `every_kind_with_a_mind_carries_a_dispersion`, and the in-crate
//! seam test asserts it directly rather than inheriting it.
//!
//! (In-crate tests of the bake's own consumption of these maps live in
//! `windows/worldgen/src/history_bake.rs`'s test module — *not* in
//! `windows/worldgen/tests/history_bake.rs`, which has none.)
//!
//! Ignored: the mutation proof builds 30 worlds. Reason token `heavy:` puts it
//! in the heavy tier (`cli/tests/heavy_tier.rs`), not the commit gate. The
//! wiring test above is deliberately NOT ignored — see its own doc.

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
/// **THE DELVERS (C2c, 2026-08-07): RENAMED, NOT WIDENED.** The roster is
/// eleven peoples now, so the old name `PEOPLES_WITH_HUMAN` read as
/// "the peoples, plus human" — i.e. as the whole roster — and is a lie by
/// omission at arity 6. It is renamed to say what it actually is: the
/// population this campaign's readout was PREREGISTERED over, frozen at the
/// six peoples that existed when The Generalist measured. Widening it to
/// eleven would silently change what every assertion below measured, which is
/// the exact failure a frozen population must never suffer.
const PEOPLES_AS_OF_THE_GENERALIST: [&str; 6] =
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
/// floor** — hence the name. Dispersion is authored per people, and the same
/// run measured hobgoblin (σ = 0.10) at variance 0.010_235: above this floor by
/// 2%, not by an order of magnitude. A witness other than human needs its own
/// floor, measured the same way.
const HUMAN_VARIANCE_FLOOR: f64 = 0.01;

/// The matching **ceiling** on the authored arm (standing repo lesson: a floor
/// without a ceiling only catches half the failures). The drawn gate input is
/// clamped to `[0, 1]`, so its variance cannot exceed 0.25 whatever σ is
/// authored — an inflated or mis-scaled spread would sail straight past a
/// floor of 0.01 and look like a healthy signal. `0.20` is above the measured
/// 0.113 with room for the roster to move and below the 0.25 a two-point
/// distribution on the bounds would reach.
const HUMAN_VARIANCE_CEILING: f64 = 0.20;

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
        if !PEOPLES_AS_OF_THE_GENERALIST.contains(&kind.0) {
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
            if !PEOPLES_AS_OF_THE_GENERALIST.contains(&rec.core.people.0) {
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
/// claim: rate(forall-seed, variance in (HUMAN_VARIANCE_FLOOR,
/// HUMAN_VARIANCE_CEILING)) — over SEEDS, off-gate (heavy:)
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
        real.variance > HUMAN_VARIANCE_FLOOR,
        "authored dispersion produces no variance: {} (floor {HUMAN_VARIANCE_FLOOR}) — the \
         authored spread is not reaching the draw",
        real.variance
    );
    assert!(
        real.variance < HUMAN_VARIANCE_CEILING,
        "authored dispersion produces variance {} (ceiling {HUMAN_VARIANCE_CEILING}) — a \
         drawn gate input on [0, 1] cannot honestly spread this far around the authored \
         0.5, so the spread reaching the draw is not the authored one",
        real.variance
    );
    // Exactly zero, not merely small: at spread 0 the offset is
    // `unit * √3 * 0.0 == 0.0`, `location + 0.0 == location`, and `clamp` fixes
    // every authored location in [0, 1] — so every settlement holds the same
    // value bit for bit. Human's authored 0.5 is a power of two, so summing
    // 2305 copies of it and dividing is exact too, and every deviation from the
    // mean is an exact 0.0. **That last step is human's alone** — see the
    // roster loop below, where a people authored at 0.8 leaves a ~1e-27
    // summation residue that says nothing about the model.
    assert_eq!(
        zeroed.variance, 0.0,
        "zeroing dispersion did NOT collapse variance to exactly zero — the parameter is \
         not being read, so H2 proves nothing"
    );

    // THE NAMED ZERO-DISPERSION BASELINE (task ruling 2). Reported for every
    // settling people, alongside the authored arm it is the control for.
    println!("--- zero-dispersion baseline (matched pair, seeds {SEEDS:?}) ---");
    for name in PEOPLES_AS_OF_THE_GENERALIST {
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
        // The exact claim, asserted where it is exactly true: every settlement
        // of a zero-dispersion people holds its people's authored location, bit
        // for bit.
        //
        // The VARIANCE of those identical values is a different matter, and
        // asserting it equals 0.0 across the roster is a claim about IEEE
        // summation rather than about the model: `mean = Σv / n` is exact only
        // when the location is exactly representable. Human's 0.5 is; bugbear's
        // 0.8 is not, and 4163 copies of it leave a residue of 2.5e-27 in the
        // variance. That is why the exact-zero form above is human's alone, and
        // why this loop pins the values exactly and the variance to a stated
        // bound — the strong claim where it is true, not the strong-looking one
        // everywhere.
        let zeroed_vals = drawn_values(&pop, people, Some(ZERO_DISPERSION), &locations, &spreads);
        let location = locations[&people];
        assert!(
            zeroed_vals.iter().all(|v| *v == location),
            "{name}: a zero-dispersion settlement drew something other than the authored \
             {location} — the collapse is not to a point"
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
/// **Deliberately NOT `#[ignore]`d, though it builds worlds.** Task 5's review
/// established that this is the *only* test anywhere that names an unwired
/// `disposition_spread`: under that mutation the crate suite does redden, but
/// uselessly — five golden/fixture tests fail with messages about marsh roots
/// and pre-campaign lexicons, collateral from shifted histories that says
/// nothing about disposition and is pure noise during a campaign that
/// legitimately moves those goldens. The in-crate
/// `two_settlements_of_one_people_can_differ_in_raiding` is blind to it too: it
/// hand-builds a `Bake` with an explicit spread map and never exercises
/// `bake_history_from`.
///
/// It costs six worlds and ~6–8 s, which is well inside the commit-gate budget,
/// and `cli/tests/heavy_tier.rs` requires no live-worldgen test to be ignored —
/// it only constrains the reason string of those that are. A diagnosis that
/// only arrives when someone dispatches `make gate-full` to lefford is not
/// guarding the campaign's acceptance criterion.
/// claim: reachability(seed: 1..=6) — existence claim: raids initiated by
/// below-gate peoples over the pooled sweep
#[test]
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
