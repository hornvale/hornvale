//! The Tolerance, Task 1: what raiding looks like BEFORE a species has an
//! interior. Every settlement of a people shares one disposition today, so
//! the between-settlement variance measured here should be ~0 by
//! construction — that zero is the baseline H2 must move.
//!
//! ## Two quantities, kept separate (task ruling, 2026-08-04)
//!
//! The raid gate, `Bake::takes_the_initiative`
//! (`windows/worldgen/src/history_bake.rs`), is a per-PEOPLE predicate: it
//! reads `BakeConfig.disposition[people]` (filled from that people's
//! authored `MindVector.threat_response`) against the private
//! `RAID_DISPOSITION_MIN = 0.6`. `BakeCensus.raided` is a per-WORLD counter.
//! Neither is per-settlement, so resolving that gap is this file's job.
//! Following `windows/worldgen/tests/generalist_baseline.rs`'s precedent of
//! keeping fit and competitive share visually and structurally separate,
//! this file reports two distinct per-settlement quantities and never
//! conflates them:
//!
//! 1. **The gate input** — the value `takes_the_initiative` actually reads,
//!    per settlement: its people's authored `threat_response`, assigned
//!    uniformly to every settlement of that people (today there is no other
//!    source). Its between-settlement variance is **exactly 0 by
//!    construction** — measured here rather than merely asserted from the
//!    source, because a measured zero and an argued zero are different
//!    evidence, and this zero is the number Task 2's dispersion draw must
//!    move.
//! 2. **The gate outcome** — a per-settlement raid observable, described
//!    below. This one is NOT expected to be zero-variance; it is a property
//!    of the raid mechanic's outcomes, not of the (currently uniform) input.
//!
//! ## What the gate-outcome proxy is, and what it does not capture
//!
//! `hornvale_worldgen::History::records` (`Vec<BakeOccupation>`, read via
//! [`history_for`] — the same entry point
//! `windows/lab/tests/disposition_calibration.rs` uses) is the only
//! per-settlement view of the bake reachable from outside the crate: each
//! `BakeOccupation` is one span of one people occupying one site, i.e. one
//! settlement-occupation. (`hornvale_settlement::all_settlements`, named in
//! the task brief's sketch of the interface, was checked and rejected: its
//! `VillageInfo` carries only an id/name/population, no people and no raid
//! history, so it cannot answer a per-people question at all.)
//!
//! Reading `Bake::maybe_raid`'s body (`history_bake.rs`) shows that **every**
//! `BakeCensus::raided` increment (both the direct-raid path and the
//! cascade/roll-downhill eviction path — the two are the only non-test call
//! sites) closes its VICTIM's occupation record with
//! `CauseOfEnd::Fled` + `Ended::By(raider_id)`, and does so exactly once per
//! increment. That pairing is therefore an exact, checkable per-record proxy
//! for "was this settlement raided" — the DEFENSE side of the mechanic — and
//! `measure_one` below asserts the pairing holds (per-record count vs.
//! `census().raided`) as a self-consistency check, not a hypothesis.
//!
//! The OFFENSE side — "did this settlement's people take the initiative" —
//! is NOT reachable the same way: a winning raider's OLD record closes as
//! `CauseOfEnd::Migrated`/`Ended::Nature` (indistinguishable from an ordinary
//! orderly migration) and its NEW record opens with
//! `Founding::From(raider_id)` (indistinguishable from an ordinary
//! daughter-founding or a vassal's relocation). No field on a `BakeOccupation`
//! marks "this record was created by winning a raid." So the per-settlement
//! outcome reported here is **"was raided" (victim), not "raided" (initiator)**
//! — a real, per-settlement observable, but the opposite side of the gate
//! from what `takes_the_initiative` itself decides. Task 6 needs this
//! asymmetry: a people's own disposition does not directly predict how often
//! ITS settlements are raided (that depends on its neighbours' dispositions
//! and the local value gradient), so a flat or noisy per-people "raided"
//! rate here is not evidence against H2 the way a flat "gate input" rate is
//! evidence for the pre-dispersion premise.
//!
//! World-level `raided`/`fled` tallies ([`census`]) are reported alongside
//! for context (Task 6's comparison point at the world scale), but they are
//! not a substitute for the per-settlement numbers above.
//!
//! Six settling peoples (The Generalist folded human in): bugbear, gnoll,
//! goblin, hobgoblin, human, kobold — `PEOPLES_WITH_HUMAN` mirrors
//! `generalist_baseline.rs`'s constant of the same name.
//!
//! World-building idiom reused verbatim from `generalist_baseline.rs`/
//! `disposition_calibration.rs`: [`history_for`] builds only to
//! `BuildDepth::Terrain` (decision 0092's sanctioned fixture posture) —
//! nothing here reads settlements, language, or any stage past the bake.
//!
//! Ignored: builds 30 worlds. Reason token `heavy:` puts it in the heavy
//! tier (`cli/tests/heavy_tier.rs`), not the commit gate.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_history::record::{CauseOfEnd, Ended};
use hornvale_kernel::{KindId, Seed};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BakeCensus, SettlementPins, SkyChoice, WorldComponents, census, history_for,
};
use std::collections::BTreeMap;

/// Seeds `1..=30`, the range every reading below is pooled over — the same
/// range `generalist_baseline.rs` and `disposition_calibration.rs` (there
/// `1..=60`, halved here to keep this a fresh, independent battery) sample.
const SEEDS: std::ops::RangeInclusive<u64> = 1..=30;

/// The six settling peoples (post-Generalist roster) — mirrors
/// `generalist_baseline.rs`'s `PEOPLES_WITH_HUMAN` verbatim, so Task 1's
/// population here matches the campaign's current roster rather than the
/// pre-human five.
const PEOPLES_WITH_HUMAN: [&str; 6] =
    ["bugbear", "gnoll", "goblin", "hobgoblin", "human", "kobold"];

/// The raid-initiative threshold on authored `threat_response`, mirroring
/// `hornvale_worldgen::history_bake`'s private `RAID_DISPOSITION_MIN`.
/// Restated here (not imported — it is private) purely for the context line
/// `measure_one`'s caller prints alongside the gate-input numbers; nothing in
/// this file's guard assertions depends on it.
const RAID_DISPOSITION_MIN: f64 = 0.6;

/// One settlement's reading: which people it belongs to, the gate's own
/// input for that people (`threat_response`, uniform across all of a
/// people's settlements today), and whether this settlement was raided (the
/// victim-side proxy described in the module doc; `1.0`/`0.0` so it can share
/// [`mean`]/[`variance`] with the gate-input column).
struct SettlementRow {
    /// The settlement's people (one of [`PEOPLES_WITH_HUMAN`]).
    people: &'static str,
    /// The gate's own input for `people`: authored `threat_response`.
    gate_input: f64,
    /// `1.0` if this settlement's occupation record closed as
    /// `CauseOfEnd::Fled` + `Ended::By(_)` (raided), else `0.0`.
    raided_victim: f64,
}

/// Build `seed` to `BuildDepth::Terrain` (via [`history_for`]) and return one
/// [`SettlementRow`] per occupation record belonging to one of the six
/// settling peoples, plus that seed's world-level [`BakeCensus`] (for the
/// context totals the caller prints). Every occupation record in the shipped
/// roster belongs to one of the six settling peoples (module doc), so the
/// per-record raid-victim count is asserted equal to `census().raided` here
/// — a self-consistency check on the proxy itself, not a hypothesis.
fn measure_one(
    seed: Seed,
    wc: &WorldComponents,
    threat_response: &BTreeMap<KindId, f64>,
) -> (Vec<SettlementRow>, BakeCensus) {
    let history = history_for(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
    )
    .unwrap_or_else(|e| panic!("{seed:?} failed to build: {e:?}"));

    let mut rows = Vec::new();
    let mut victims_counted: u64 = 0;
    for rec in &history.records {
        let people = rec.core.people;
        if !PEOPLES_WITH_HUMAN.contains(&people.0) {
            continue;
        }
        let gate_input = *threat_response.get(&people).unwrap_or_else(|| {
            panic!(
                "{}: no authored threat_response reached this test",
                people.0
            )
        });
        let raided = matches!(rec.core.cause, Some(CauseOfEnd::Fled))
            && matches!(rec.ended_by, Ended::By(_));
        if raided {
            victims_counted += 1;
        }
        rows.push(SettlementRow {
            people: people.0,
            gate_input,
            raided_victim: if raided { 1.0 } else { 0.0 },
        });
    }

    let tally = census(&history);
    assert_eq!(
        victims_counted, tally.raided,
        "{seed:?}: per-record Fled+Ended::By count {victims_counted} != census().raided \
         {} — the raid-victim proxy no longer matches `Bake::maybe_raid`'s own bookkeeping",
        tally.raided
    );

    (rows, tally)
}

/// Mean of a non-empty sample.
fn mean(vals: &[f64]) -> f64 {
    vals.iter().sum::<f64>() / vals.len() as f64
}

/// Population variance of a non-empty sample: the mean squared deviation
/// from [`mean`] — a descriptive statistic over exactly the settlements
/// sampled, not an estimate of a larger population.
fn variance(vals: &[f64]) -> f64 {
    let m = mean(vals);
    vals.iter().map(|v| (v - m) * (v - m)).sum::<f64>() / vals.len() as f64
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn report_pre_dispersion_raid_rates() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");

    // The gate's own input, per people - `Bake::takes_the_initiative`'s
    // exact read, looked up once here (not per-record) from the shipped
    // psyche registry.
    let mut threat_response: BTreeMap<KindId, f64> = BTreeMap::new();
    for (kind, psyche) in wc.psyche.iter() {
        if PEOPLES_WITH_HUMAN.contains(&kind.0) {
            threat_response.insert(*kind, psyche.threat_response);
        }
    }
    assert_eq!(
        threat_response.len(),
        6,
        "all six settling peoples must carry an authored threat_response; got {:?}",
        threat_response.keys().collect::<Vec<_>>()
    );

    let mut rows: Vec<f64> = Vec::new();
    let mut per_people_gate_input: BTreeMap<&'static str, Vec<f64>> = BTreeMap::new();
    let mut per_people_raided: BTreeMap<&'static str, Vec<f64>> = BTreeMap::new();
    let mut world_raided: u64 = 0;
    let mut world_fled: u64 = 0;
    let mut worlds_sampled: u64 = 0;

    for seed in SEEDS {
        let (settlement_rows, tally) = measure_one(Seed(seed), &wc, &threat_response);
        world_raided += tally.raided;
        world_fled += tally.fled;
        worlds_sampled += 1;
        for row in settlement_rows {
            rows.push(row.raided_victim);
            per_people_gate_input
                .entry(row.people)
                .or_default()
                .push(row.gate_input);
            per_people_raided
                .entry(row.people)
                .or_default()
                .push(row.raided_victim);
        }
    }

    // Guard assertions (task brief Step 2) - a harness that measures nothing
    // must not look like one that works.
    assert!(!rows.is_empty(), "no settlements sampled");
    assert!(rows.iter().all(|r| r.is_finite()), "non-finite rate");
    assert_eq!(
        per_people_raided.len(),
        6,
        "all six peoples measured; got {:?}",
        per_people_raided.keys().collect::<Vec<_>>()
    );

    println!(
        "world-level (census, summed over {worlds_sampled} worlds): raided = {world_raided}, fled = {world_fled}"
    );

    for name in PEOPLES_WITH_HUMAN {
        let gate_vals = &per_people_gate_input[name];
        let raid_vals = &per_people_raided[name];
        let n = raid_vals.len();
        let gate_mean = mean(gate_vals);
        let gate_var = variance(gate_vals);
        let raid_mean = mean(raid_vals);
        let raid_var = variance(raid_vals);
        let eligible = gate_mean >= RAID_DISPOSITION_MIN;
        println!(
            "{name}: n = {n} settlements, threat_response = {gate_mean:.6} \
             (>= {RAID_DISPOSITION_MIN} = {eligible}), gate-input between-settlement \
             variance = {gate_var:.10}"
        );
        println!(
            "{name}: raided-as-victim rate = {raid_mean:.6}, between-settlement \
             variance = {raid_var:.10} (n = {n})"
        );
    }
}
