//! The Tolerance, Task 1: what raiding looks like BEFORE a species has an
//! interior. Every settlement of a people shares one disposition today, so
//! the between-settlement variance measured here should be ~0 by
//! construction — that zero is the baseline H2 must move.
//!
//! **Read "today" as "at Task 1" (2026-08-04).** Task 4 replaced the raid
//! gate's input with a per-settlement draw, so the *gate* no longer reads a
//! per-people constant. This file is unaffected and deliberately not updated
//! to follow it: column 1 below measures the **authored** `threat_response`
//! off the psyche registry, which is exactly the pre-dispersion quantity this
//! baseline exists to freeze, and its exact-zero variance is the number Task 6
//! compares against. Do not re-point it at the drawn value — that would
//! overwrite the baseline with the readout.
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
//! goblin, hobgoblin, human, kobold — `PEOPLES_AS_OF_THE_GENERALIST` mirrors
//! `generalist_baseline.rs`'s constant of the same name.
//!
//! World-building idiom reused verbatim from `generalist_baseline.rs`/
//! `disposition_calibration.rs`: [`history_for`] builds only to
//! `BuildDepth::Terrain` (decision 0092's sanctioned fixture posture) —
//! nothing here reads settlements, language, or any stage past the bake.
//!
//! # Task 6 lives here too: the preregistered readout
//!
//! [`report_the_preregistered_readout`] below is the campaign's H1/H2/H3
//! readout. It shares this file's seed range, roster, build depth and
//! victim-side proxy — the whole point of putting it here rather than in a
//! third file — but it is a **separate test with a separate population
//! collection**, because it reads three things Task 1's function does not:
//! the per-settlement **drawn** disposition, the **gate-open** share that
//! drawn value produces, and the **initiator** side of raiding (Task 5's
//! technique, reused rather than reinvented).
//!
//! Task 1's `report_pre_dispersion_raid_rates` is deliberately untouched by
//! it. See the warning above: that test freezes the *authored* constant and
//! its exact-zero variance, which is the pre-dispersion premise, not the
//! readout.
//!
//! Ignored: builds 30 worlds. Reason token `heavy:` puts it in the heavy
//! tier (`cli/tests/heavy_tier.rs`), not the commit gate.

use hornvale_astronomy::SkyPins;
use hornvale_history::record::{CauseOfEnd, Ended};
use hornvale_kernel::{KindId, Seed};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::disposition::{drawn_threat_response, occupation_draw_key};
use hornvale_worldgen::{
    BakeCensus, SettlementPins, SkyChoice, WorldComponents, census, history_for,
};
use std::collections::{BTreeMap, BTreeSet};

/// Seeds `1..=30`, the range every reading below is pooled over — the same
/// range `generalist_baseline.rs` and `disposition_calibration.rs` (there
/// `1..=60`, halved here to keep this a fresh, independent battery) sample.
const SEEDS: std::ops::RangeInclusive<u64> = 1..=30;

/// The six settling peoples (post-Generalist roster) — mirrors
/// `generalist_baseline.rs`'s `PEOPLES_AS_OF_THE_GENERALIST` verbatim, so Task 1's
/// population here matches the campaign's current roster rather than the
/// pre-human five.
///
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
    /// The settlement's people (one of [`PEOPLES_AS_OF_THE_GENERALIST`]).
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
        if !PEOPLES_AS_OF_THE_GENERALIST.contains(&people.0) {
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

/// claim: readout(off-gate, heavy:) — pre-dispersion raid rates by people
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn report_pre_dispersion_raid_rates() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");

    // The gate's own input, per people - `Bake::takes_the_initiative`'s
    // exact read, looked up once here (not per-record) from the shipped
    // psyche registry.
    let mut threat_response: BTreeMap<KindId, f64> = BTreeMap::new();
    for (kind, psyche) in wc.psyche.iter() {
        if PEOPLES_AS_OF_THE_GENERALIST.contains(&kind.0) {
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

    for name in PEOPLES_AS_OF_THE_GENERALIST {
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

// ---------------------------------------------------------------------------
// Task 6 — the preregistered readout. Everything below REPORTS; the only
// assertions are guards (non-empty, finite, six peoples), because encoding a
// preregistered prediction as a build failure creates pressure to retune until
// the suite goes green.
// ---------------------------------------------------------------------------

/// The authored per-people inputs, resolved from **the same two sources the
/// composition root resolves them from** (`lib.rs::bake_history_from`): the
/// location off `WorldComponents::psyche`, the spread off
/// `hornvale_species::dispersion_registry()`. Mirrors `tolerance_mutation.rs`'s
/// `authored` (same body, different name — this file already has an
/// `authored`-shaped concept in Task 1's `threat_response` map, so the longer
/// name keeps the two readable side by side) so the readout and the mutation
/// proof cannot read different authored values.
fn authored_inputs(wc: &WorldComponents) -> (BTreeMap<KindId, f64>, BTreeMap<KindId, f64>) {
    let dispersion = hornvale_species::dispersion_registry();
    let mut locations = BTreeMap::new();
    let mut spreads = BTreeMap::new();
    for (kind, psyche) in wc.psyche.iter() {
        if !PEOPLES_AS_OF_THE_GENERALIST.contains(&kind.0) {
            continue;
        }
        locations.insert(*kind, psyche.threat_response);
        spreads.insert(
            *kind,
            dispersion
                .get(kind)
                .unwrap_or_else(|| panic!("{}: no authored dispersion reached this test", kind.0))
                .mind,
        );
    }
    (locations, spreads)
}

/// One settlement-occupation as the readout reads it: the drawn gate input
/// under both dispersion regimes, and three per-settlement outcomes.
///
/// The zeroed columns are **measured, not argued** — they are the same
/// `drawn_threat_response` call with `spread = 0.0`, evaluated on the same
/// settlement in the same run, which is what makes this a matched pair rather
/// than a before/after spanning the `a025e55a` merge (task ruling 2).
struct ReadoutRow {
    /// The settlement's people (one of [`PEOPLES_AS_OF_THE_GENERALIST`]).
    people: &'static str,
    /// The gate input this settlement's community actually drew, under the
    /// authored dispersion.
    drawn: f64,
    /// `1.0` if the authored-dispersion draw opens the raid gate.
    gate_open: f64,
    /// The gate input the same settlement would have drawn at dispersion 0 —
    /// its people's authored location, the pre-Tolerance regime.
    drawn_zeroed: f64,
    /// `1.0` if the zero-dispersion draw opens the raid gate. Constant within
    /// a people by construction; that constancy is the baseline.
    gate_open_zeroed: f64,
    /// `1.0` if this settlement **initiated** at least one raid on a live
    /// world (Task 5's technique: a raid closes its victim's record with
    /// `CauseOfEnd::Fled` + `Ended::By(raider)`, and the bake mints one id per
    /// record, so `raider` names exactly one settlement — the aggressor's).
    initiated: f64,
    /// How many raids this settlement initiated (a count, not a flag).
    raids_initiated: f64,
    /// `1.0` if this settlement was itself raided — the VICTIM side, Task 1's
    /// proxy, carried forward so both sides are reported side by side.
    raided_victim: f64,
}

/// What the thirty probe worlds actually contained — reported so every rate
/// below names the population it is over.
struct Coverage {
    /// Worlds built.
    worlds: u64,
    /// Occupation records across all worlds, all kinds.
    records_total: u64,
    /// Occupation records belonging to the six settling peoples.
    records_roster: u64,
    /// Records still alive at the end of the bake.
    alive_total: u64,
    /// Seeds whose world produced no occupation records at all.
    seeds_with_no_records: u64,
    /// Seeds whose world ended with no settlement alive.
    seeds_with_no_alive: u64,
    /// `census().raided` summed over every world — the world-level counter the
    /// initiator-side reconstruction is cross-checked against.
    census_raided: u64,
    /// Occupation records sharing a `(site, founded-year)` draw key with an
    /// earlier record in the same world, summed over worlds. Measured **on
    /// this readout's own population** rather than inherited: `tolerance_draw.rs`
    /// reports 3–15% from seeds 1 and 42, and this readout pools thirty seeds.
    records_sharing_key: u64,
    /// The same count restricted to records still alive at the end of the bake
    /// — expected 0, since `Bake.node_index` holds one alive community per cell.
    alive_records_sharing_key: u64,
}

/// Build every seed in [`SEEDS`] and reduce each occupation record of the six
/// settling peoples to a [`ReadoutRow`], alongside the [`Coverage`] of the
/// probe.
fn readout_population(
    wc: &WorldComponents,
    locations: &BTreeMap<KindId, f64>,
    spreads: &BTreeMap<KindId, f64>,
) -> (Vec<ReadoutRow>, Coverage) {
    let mut rows = Vec::new();
    let mut cov = Coverage {
        worlds: 0,
        records_total: 0,
        records_roster: 0,
        alive_total: 0,
        seeds_with_no_records: 0,
        seeds_with_no_alive: 0,
        census_raided: 0,
        records_sharing_key: 0,
        alive_records_sharing_key: 0,
    };

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

        cov.worlds += 1;
        cov.records_total += history.records.len() as u64;
        if history.records.is_empty() {
            cov.seeds_with_no_records += 1;
        }
        let alive = history.records.iter().filter(|r| r.core.is_alive()).count() as u64;
        cov.alive_total += alive;
        if alive == 0 {
            cov.seeds_with_no_alive += 1;
        }
        cov.census_raided += census(&history).raided;

        // The draw-key collision rate, measured on THIS population (see
        // `Coverage::records_sharing_key`). Grouped exactly as
        // `tolerance_draw.rs` groups it: `(site, occupation_draw_key(founded))`,
        // over all occupation records, with the alive-only restriction reported
        // separately.
        let mut key_groups: BTreeMap<(u32, i64), (u64, u64)> = BTreeMap::new();
        for rec in &history.records {
            let entry = key_groups
                .entry((rec.core.site.0, occupation_draw_key(rec.core.founded)))
                .or_default();
            entry.0 += 1;
            if rec.core.is_alive() {
                entry.1 += 1;
            }
        }
        for (total, alive_in_group) in key_groups.values() {
            cov.records_sharing_key += total - 1;
            cov.alive_records_sharing_key += alive_in_group.saturating_sub(1);
        }

        // The initiator side (Task 5's technique). One pass over the victims
        // names every aggressor by its own record's handle.
        let mut raids_by_initiator = BTreeMap::new();
        for rec in &history.records {
            if matches!(rec.core.cause, Some(CauseOfEnd::Fled))
                && let Ended::By(raider) = rec.ended_by
            {
                *raids_by_initiator.entry(raider).or_insert(0u64) += 1;
            }
        }

        for rec in &history.records {
            let people = rec.core.people;
            if !PEOPLES_AS_OF_THE_GENERALIST.contains(&people.0) {
                continue;
            }
            cov.records_roster += 1;
            let location = locations[&people];
            let spread = spreads[&people];
            let key = occupation_draw_key(rec.core.founded);
            let drawn = drawn_threat_response(Seed(seed), rec.core.site, key, location, spread);
            let drawn_zeroed = drawn_threat_response(Seed(seed), rec.core.site, key, location, 0.0);
            let raids = raids_by_initiator
                .get(&rec.community)
                .copied()
                .unwrap_or(0u64);
            let raided = matches!(rec.core.cause, Some(CauseOfEnd::Fled))
                && matches!(rec.ended_by, Ended::By(_));
            rows.push(ReadoutRow {
                people: people.0,
                drawn,
                gate_open: f64::from(u8::from(drawn >= RAID_DISPOSITION_MIN)),
                drawn_zeroed,
                gate_open_zeroed: f64::from(u8::from(drawn_zeroed >= RAID_DISPOSITION_MIN)),
                initiated: f64::from(u8::from(raids > 0)),
                raids_initiated: raids as f64,
                raided_victim: f64::from(u8::from(raided)),
            });
        }
    }

    (rows, cov)
}

/// Discordant pairs between authored dispersion and a measured per-people
/// statistic, out of the 15 unordered pairs six peoples make: pairs where the
/// people with the **larger** authored σ carries the **smaller** measured
/// value. `0` means the ordering tracks the authored dispersion exactly; `15`
/// means it is exactly inverted. Ties in either coordinate are not counted.
///
/// This is the readable form of H2's ordering claim, reported rather than
/// asserted. Entries are `(people, sigma, statistic)`.
fn discordant_pairs(stats: &[(&'static str, f64, f64)]) -> usize {
    let mut n = 0;
    for (i, a) in stats.iter().enumerate() {
        for b in &stats[i + 1..] {
            let sigma_order = a.1.total_cmp(&b.1);
            let stat_order = a.2.total_cmp(&b.2);
            if sigma_order != std::cmp::Ordering::Equal
                && stat_order != std::cmp::Ordering::Equal
                && sigma_order != stat_order
            {
                n += 1;
            }
        }
    }
    n
}

/// **The preregistered readout (Task 6).** H1, H2 and H3 are reported and
/// never asserted — the assertions in this test are guards only (a non-empty
/// population, finite values, the expected roster), so that no measured
/// outcome can turn a frozen prediction into a build failure someone is
/// tempted to retune away.
///
/// The comparison point is the **zero-dispersion arm**, re-measured here on
/// the same settlements in the same run, not Task 1's numbers: `origin/main`
/// was absorbed at merge `a025e55a` (The Keeping), which changed the placement
/// gate and moved world identity, so Task 1's figures are the *pre-merge
/// record* rather than a clean "before". `tolerance_mutation.rs`'s module doc
/// carries the committed form of this baseline.
///
/// Two readings of H1 are printed, because the prediction is ambiguous between
/// them and the ambiguity is the finding — see the readout's own commentary
/// block at the end of the output.
///
/// # The gate-open column is a RE-DERIVATION, not an observation
///
/// `Community.disposition` is private and is never committed, so the value the
/// bake actually gated on is unobservable from outside the crate. The
/// `gate-open` column therefore **redoes the bake's procedure** — the same
/// `drawn_threat_response(seed, site, occupation_draw_key(founded), location,
/// spread)` call, on the keys real worlds produced — rather than reading a
/// recorded outcome. That is the right way round (redo the procedure, never
/// restate the definition), and it is checked from two directions: at spread 0
/// it reduces to the authored location exactly
/// (`tolerance_mutation.rs::every_zeroed_draw_is_the_authored_location`), and
/// the numbers it produces reproduce `tolerance_mutation.rs`'s **committed**
/// table to every printed digit — two independently written harnesses over the
/// same population agreeing to ten digits. The *live* columns
/// (`initiator rate`, `victim rate`) are observations proper, and the
/// `census().raided` cross-check above is asserted, so the re-derived column and
/// the observed ones can be read against each other.
///
/// # The population is ALL occupation records, and that is deliberate
///
/// Not only the settlements alive at `now`: the gate reads a community's drawn
/// disposition from the moment it opens, so a ruin was gated on its own draw
/// exactly as a standing settlement was, and excluding ruins would drop most of
/// the raiding this campaign is about. `disposition.rs`'s accepted-collision
/// reasoning was corrected alongside this task to say so — its earlier reason 3
/// asserted the instrument measured only alive settlements, which stopped being
/// true at Task 5 and is not true here. The collision rate is measured on this
/// exact population above rather than inherited from `tolerance_draw.rs`'s
/// two-seed figure, so the effective *n* behind every "that is noise" claim
/// below is a stated number rather than an assumption.
/// claim: readout(off-gate, heavy:, preregistered) — own name states the
/// shape; guards only, nothing below asserts a hypothesis
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn report_the_preregistered_readout() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let (locations, spreads) = authored_inputs(&wc);
    assert_eq!(
        locations.len(),
        6,
        "all six settling peoples must carry an authored threat_response; got {:?}",
        locations.keys().collect::<Vec<_>>()
    );

    let (rows, cov) = readout_population(&wc, &locations, &spreads);

    // Guards ONLY. A harness that measures nothing must not look like one that
    // works; nothing below this point asserts a hypothesis.
    assert!(!rows.is_empty(), "no settlements sampled");
    assert!(
        rows.iter().all(|r| r.drawn.is_finite()
            && r.drawn_zeroed.is_finite()
            && r.gate_open.is_finite()
            && r.gate_open_zeroed.is_finite()
            && r.initiated.is_finite()
            && r.raids_initiated.is_finite()
            && r.raided_victim.is_finite()),
        "non-finite value in the readout population"
    );
    let sampled: BTreeSet<&'static str> = rows.iter().map(|r| r.people).collect();
    assert_eq!(
        sampled.len(),
        6,
        "all six peoples measured; got {sampled:?}"
    );
    // GUARD, not a hypothesis: the initiator-side reconstruction must account
    // for every raid the bake's own counter recorded, exactly once. This is the
    // same self-consistency discipline `measure_one` applies to the VICTIM-side
    // proxy one function above, moved to the offense side — without it the
    // cross-check exists only as a human comparing two lines of one log, and a
    // regression in the `rec.community` lookup would print a wrong number
    // silently.
    let raids_reconstructed: f64 = rows.iter().map(|r| r.raids_initiated).sum();
    assert_eq!(
        raids_reconstructed as u64, cov.census_raided,
        "initiator-side reconstruction attributed {raids_reconstructed} raids but \
         census().raided summed to {} over the same worlds — `Ended::By(raider)` no \
         longer names exactly one occupation record, so every initiator rate below \
         is wrong",
        cov.census_raided
    );

    // Per-people columns, collected once.
    let mut drawn: BTreeMap<&'static str, Vec<f64>> = BTreeMap::new();
    let mut drawn_zeroed: BTreeMap<&'static str, Vec<f64>> = BTreeMap::new();
    let mut gate: BTreeMap<&'static str, Vec<f64>> = BTreeMap::new();
    let mut gate_zeroed: BTreeMap<&'static str, Vec<f64>> = BTreeMap::new();
    let mut initiated: BTreeMap<&'static str, Vec<f64>> = BTreeMap::new();
    let mut raids: BTreeMap<&'static str, Vec<f64>> = BTreeMap::new();
    let mut victim: BTreeMap<&'static str, Vec<f64>> = BTreeMap::new();
    for r in &rows {
        drawn.entry(r.people).or_default().push(r.drawn);
        drawn_zeroed
            .entry(r.people)
            .or_default()
            .push(r.drawn_zeroed);
        gate.entry(r.people).or_default().push(r.gate_open);
        gate_zeroed
            .entry(r.people)
            .or_default()
            .push(r.gate_open_zeroed);
        initiated.entry(r.people).or_default().push(r.initiated);
        raids.entry(r.people).or_default().push(r.raids_initiated);
        victim.entry(r.people).or_default().push(r.raided_victim);
    }

    // Peoples ordered by authored dispersion, widest first — the order H2's
    // prediction is stated in.
    let mut by_sigma: Vec<(&'static str, f64)> = PEOPLES_AS_OF_THE_GENERALIST
        .iter()
        .map(|n| (*n, spreads[&KindId(n)]))
        .collect();
    by_sigma.sort_by(|a, b| b.1.total_cmp(&a.1).then(a.0.cmp(b.0)));

    println!("=== THE TOLERANCE, Task 6: the preregistered readout ===");
    println!(
        "population: seeds {SEEDS:?} ({} worlds), {} occupation records total, \
         {} of the six settling peoples, {} alive at the end of the bake",
        cov.worlds, cov.records_total, cov.records_roster, cov.alive_total
    );
    println!(
        "empty probes: {} of {} seeds produced NO occupation records; {} of {} ended \
         with NO settlement alive",
        cov.seeds_with_no_records, cov.worlds, cov.seeds_with_no_alive, cov.worlds
    );
    println!(
        "gate: RAID_DISPOSITION_MIN = {RAID_DISPOSITION_MIN}; the baseline arm is \
         dispersion 0 measured on these same occupation records (matched pair, one run)"
    );
    println!(
        "draw-key collisions on THIS population: {} of {} records ({:.4}%) share a \
         (site, founded-year) key with an earlier record; {} of the {} ALIVE records do",
        cov.records_sharing_key,
        cov.records_total,
        100.0 * cov.records_sharing_key as f64 / cov.records_total as f64,
        cov.alive_records_sharing_key,
        cov.alive_total
    );
    println!(
        "initiator reconstruction cross-check: {raids_reconstructed} raids attributed \
         = {} census().raided (asserted above, as a guard)",
        cov.census_raided
    );

    // ---- H1 -------------------------------------------------------------
    println!();
    println!(
        "--- H1 (reported, not asserted): does each people's MEAN behaviour survive \
         the draw? Two readings. ---"
    );
    println!(
        "H1(a) the mean of the DRAWN DISPOSITION vs the authored location. \
         The clamp table in windows/worldgen/src/disposition.rs predicts the shifts."
    );
    println!(
        "  {:<10} {:>5} {:>8} {:>6} {:>11} {:>11} {:>10}",
        "people", "n", "location", "sigma", "zeroed mean", "drawn mean", "shift"
    );
    for (name, sigma) in &by_sigma {
        let d = &drawn[name];
        let z = &drawn_zeroed[name];
        println!(
            "  {:<10} {:>5} {:>8.4} {:>6.4} {:>11.6} {:>11.6} {:>+10.6}",
            name,
            d.len(),
            locations[&KindId(name)],
            sigma,
            mean(z),
            mean(d),
            mean(d) - mean(z)
        );
    }
    println!(
        "H1(b) the mean of the GATED OUTCOME (share of settlements whose draw opens \
         the gate) vs the same share at dispersion 0."
    );
    println!(
        "  {:<10} {:>5} {:>13} {:>13} {:>10}",
        "people", "n", "zeroed gate", "drawn gate", "shift"
    );
    for (name, _) in &by_sigma {
        let g = &gate[name];
        let gz = &gate_zeroed[name];
        println!(
            "  {:<10} {:>5} {:>13.6} {:>13.6} {:>+10.6}",
            name,
            g.len(),
            mean(gz),
            mean(g),
            mean(g) - mean(gz)
        );
    }

    // ---- H2 -------------------------------------------------------------
    println!();
    println!(
        "--- H2 (reported, not asserted): is between-settlement variance high for \
         high-dispersion peoples and near-zero for low-dispersion ones? ---"
    );
    println!(
        "  {:<10} {:>6} {:>11} {:>11} {:>11} {:>11} {:>11}",
        "people", "sigma", "var drawn", "var zeroed", "var gate", "var initiat", "var victim"
    );
    for (name, sigma) in &by_sigma {
        println!(
            "  {:<10} {:>6.4} {:>11.8} {:>11.8} {:>11.8} {:>11.8} {:>11.8}",
            name,
            sigma,
            variance(&drawn[name]),
            variance(&drawn_zeroed[name]),
            variance(&gate[name]),
            variance(&initiated[name]),
            variance(&victim[name]),
        );
    }
    for (label, col) in [
        ("drawn disposition", &drawn),
        ("gate-open (thresholded)", &gate),
        ("initiated a raid (live)", &initiated),
        ("was raided (live, victim)", &victim),
    ] {
        let stats: Vec<(&'static str, f64, f64)> = by_sigma
            .iter()
            .map(|(n, s)| (*n, *s, variance(&col[n])))
            .collect();
        println!(
            "  ordering vs authored sigma, {label}: {} of 15 pairs discordant",
            discordant_pairs(&stats)
        );
    }
    // What those four rows can and cannot say. THREE of them are binary
    // per-settlement indicators (gate-open, initiated, was-raided), so their
    // between-settlement variance is identically `p(1-p)` — a function of the
    // column's OWN mean, not an independent second moment. Printed rather than
    // argued, so a reader can check it: `p(1-p)` against the measured variance.
    println!(
        "  the three binary columns have variance pinned to their own mean \
         (var = p(1-p), and every rate here is below 0.5, where p(1-p) is monotone \
         in p) — so their 'discordant pairs' counts measure RATE ordering, not \
         spread. Only the drawn-disposition row can test H2's ordering claim:"
    );
    for (label, col) in [
        ("gate-open", &gate),
        ("initiated", &initiated),
        ("was raided", &victim),
    ] {
        let residual: f64 = PEOPLES_AS_OF_THE_GENERALIST
            .iter()
            .map(|n| {
                let p = mean(&col[n]);
                (variance(&col[n]) - p * (1.0 - p)).abs()
            })
            .fold(0.0, f64::max);
        println!("    {label}: max |var - p(1-p)| over the six peoples = {residual:.3e}");
    }

    // ---- H3 -------------------------------------------------------------
    println!();
    println!(
        "--- H3 (reported, not asserted): does human raid at a rate strictly between \
         goblin's and hobgoblin's, rather than at 0 or 1? ---"
    );
    println!(
        "  {:<10} {:>5} {:>11} {:>13} {:>13} {:>13}",
        "people", "n", "gate-open", "initiator rate", "raids/settl", "victim rate"
    );
    for (name, _) in &by_sigma {
        let g = &gate[name];
        println!(
            "  {:<10} {:>5} {:>11.6} {:>13.6} {:>13.6} {:>13.6}",
            name,
            g.len(),
            mean(g),
            mean(&initiated[name]),
            mean(&raids[name]),
            mean(&victim[name]),
        );
    }
    println!(
        "  roster totals: {raids_reconstructed} raids initiated by the six settling \
         peoples across {} occupation records ({} of them alive at the end of the \
         bake) over seeds {SEEDS:?}",
        rows.len(),
        cov.alive_total
    );
    for stat in ["gate-open", "initiator rate"] {
        let col = if stat == "gate-open" {
            &gate
        } else {
            &initiated
        };
        let (g, h, hb) = (
            mean(&col["goblin"]),
            mean(&col["human"]),
            mean(&col["hobgoblin"]),
        );
        let between = (g < h && h < hb) || (hb < h && h < g);
        println!(
            "  H3 on {stat}: goblin {g:.6}, human {h:.6}, hobgoblin {hb:.6} — human \
             strictly between = {between}; human strictly inside (0, 1) = {}",
            h > 0.0 && h < 1.0
        );
    }
}
