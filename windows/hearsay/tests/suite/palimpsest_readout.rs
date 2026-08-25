//! The Palimpsest's preregistered readout (spec §6), as a HEAVY BATTERY.
//!
//! Not registered lab metrics, and deliberately so: nine studies declare
//! `"metrics": "all"` with no opt-out, so a metric added here would run on
//! every world of `the-census` (~2000) forever, redden every census-reading
//! calibration test until fixtures were refreshed on the canonical host, and
//! restage fixtures no drift check covers. Campaign 2's own readout
//! (`retelling_readout_seed42.rs`) is a heavy battery for the same reason;
//! this follows it. What it costs: the readout is not tracked over time in
//! the census. Accepted — a campaign readout answers a frozen hypothesis
//! once.
//!
//! **Four quantities, per accumulation rule, over a seed panel:**
//!
//! - `rho_generation_precision` — Spearman rho across held claims between the
//!   holder's people's generation length and the retained precision rung.
//!   **H1** (§6.2) predicts NEGATIVE: a longer generation means the same
//!   elapsed time is fewer generations, so precision survives.
//! - `distinct_rungs` — how many distinct retained rungs carry at least one
//!   held claim. **H2** (§6.3): three or more under at least one rule
//!   confirms; a collapse to two falsifies.
//! - `saturated_fraction` — the share of held claims sitting at the coarsest
//!   rung of their own people's ladder. **The null detector.** §3.7 measured
//!   that transmission chains span 8–13 generations while the longest natural
//!   duration in this world is ~2.2 generations, so saturation is a
//!   PREDICTED, live outcome. If it happens, it is reported, not tuned away.
//! - `rho_antichain_variants` — Spearman rho between an ending's
//!   maximum-antichain width and its distinct variant count. **H3** (§6.5), a
//!   direction test only: campaign 2's 0.662 is not a threshold here (§6.1).
//!
//! **No rule is nominated.** Spec §6.4: substrate for all three was measured
//! before the rule was chosen, so picking a favourite after the readout would
//! be selection on data already in hand. All three are reported side by side.
//!
//! **HOW TO READ THE WIDTHS, AND A QUESTION THIS READOUT RAISES BUT DOES NOT
//! ANSWER.** The accumulator in `derive::variants_about_accumulating` seeds
//! its width from `ladder.span(Precision::FINEST)` — a length in STD DAYS,
//! ~1.0 — and then adds `amplitude::gen_span`, which is a DIMENSIONLESS COUNT
//! OF GENERATIONS. `accumulate::precision_at` then compares that width against
//! rung spans, which are std days. So under `Additive` and `Quadrature` the
//! accumulated width is numerically `1 + (generations accumulated)` and is
//! read against a day-scale ladder, which is why those two rules land almost
//! entirely on the day and first-moon rungs and NEVER saturate here, while
//! `Multiplicative` — whose growth is a product of `(1 + span)` factors and so
//! is scale-free — climbs the whole ladder and does saturate.
//!
//! That shape is what the plan's own Task 5 sample code specified and what
//! Task 5 committed; it is reported here as a measured property of the model
//! under test, not repaired. Spec §3.7 predicted TOTAL saturation under any
//! accumulating rule (chains span 8–13 generations against a longest natural
//! duration of ~2.2), and additive/quadrature do not show it. Whether that is
//! the model surviving its prediction or the units disagreeing is a review
//! question for the campaign, and answering it by editing the accumulator
//! after seeing this readout is precisely the move the project forbids.
//!
//! The heavy battery ASSERTS ONLY SUBSTRATE CONTROLS — that the panel built,
//! that held claims exist, and that no claim reports a rung its own ladder
//! does not have. The hypotheses are REPORTED. This file must never be edited
//! to rescue a prediction.

use crate::common;

use common::{eid, ledger_with, put, put_on};
use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::accumulate::Accumulation;
use hornvale_hearsay::contact::{ContactGraph, contact_of};
use hornvale_hearsay::derive::{variants_about_accumulating, witnesses_of};
use hornvale_hearsay::divergence::maximum_antichain;
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_hearsay::ladder::{PeopleLadders, PrecisionLadder};
use hornvale_hearsay::lineage::{Lineage, lineage_of};
use hornvale_hearsay::spearman;
use hornvale_hearsay::transmission::{Transmission, Walk};
use hornvale_kernel::Precision;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use std::collections::{BTreeMap, BTreeSet};

/// The seed panel: the first 40 seeds of the census panel (`the-census` runs
/// seeds 0–999), so the readout is a strict subset of the census population
/// the spec named rather than an unrelated sample.
///
/// **Chosen by measurement, per the plan's decision rule** — seeds 0–4 were
/// run first and cost **12.40 s of test time** (21.1 s wall including the
/// incremental build), i.e. ~2.5 s/seed with world construction dominating.
/// That is far under the 2-minute bar the plan set for selecting 40, so 40 it
/// is. The panel size and its measured cost are printed by the battery itself,
/// so the numbers live in the artifact and not only in a report.
const PANEL: [u64; 40] = [
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
];

/// The predicate every claim in this readout is about. Only an ending has
/// parties beyond its subject (campaign 2 spec §6.1), so it is the only
/// predicate a transmission tree can be built over.
const PREDICATE: &str = hornvale_history::OCC_ENDED;

/// One accumulation rule's readout over one world.
#[derive(Clone, Debug)]
struct Readout {
    /// Held claims summed over every ending in the world.
    held: usize,
    /// H1: Spearman rho over held claims, generation length vs retained rung.
    rho_generation_precision: Option<f64>,
    /// H1, supplementary: the same rho over INHERITORS only (`hops > 0`).
    /// Witnesses are pinned at the finest rung by construction, so they add a
    /// large block of ties that dilutes the correlation without bearing on
    /// the mechanism. Reported beside the primary, never in place of it.
    rho_generation_precision_inheritors: Option<f64>,
    /// H2: how many distinct rung INDICES carry at least one held claim.
    distinct_rungs: usize,
    /// H2, supplementary: the distinct rung LABELS. A rung index means a
    /// different span on a long-lived people's ladder than a short-lived
    /// one's, so the labels say which reckonings are actually in play.
    rung_labels: BTreeSet<String>,
    /// The null detector's numerator: held claims at the coarsest rung of
    /// their own people's ladder.
    saturated: usize,
    /// Endings with at least three holders (§6's population rule).
    qualifying_events: usize,
    /// H3: Spearman rho between antichain width and distinct variant count.
    rho_antichain_variants: Option<f64>,
    /// How many held claims sit on each rung index.
    rung_histogram: BTreeMap<u8, usize>,
}

impl Readout {
    /// The share of held claims at the coarsest rung, or `NAN` with no claims.
    fn saturated_fraction(&self) -> f64 {
        if self.held == 0 {
            f64::NAN
        } else {
            self.saturated as f64 / self.held as f64
        }
    }
}

/// Everything one world contributes to the readout, assembled once and shared
/// by all three rules so the world is walked for its people table once.
struct WorldRead {
    /// The founding tree.
    lineage: Lineage,
    /// The raid seam. Built alongside the lineage so `measure` can hand both
    /// to a [`Walk`] under `Transmission::AS_SHIPPED`, which never consults
    /// it, without building it once per rule.
    contact: ContactGraph,
    /// Per-people generation and lifespan, in std days.
    durations: PeopleDurations,
    /// One ladder per people.
    ladders: PeopleLadders,
    /// Each people's generation length, in years — H1's x-axis.
    generation_years: BTreeMap<String, f64>,
    /// Which people each occupation belongs to.
    people_of: BTreeMap<EntityId, String>,
    /// Every ending in the world, ascending by subject.
    events: Vec<EntityId>,
}

/// Compute one rule's four quantities over one world.
///
/// The single code path both the heavy battery and the hand-built control
/// below run, so a control that says "these vary" says it about the code the
/// panel actually uses.
fn measure(ledger: &Ledger, read: &WorldRead, rule: Accumulation) -> Readout {
    let mut generations: Vec<f64> = Vec::new();
    let mut rungs: Vec<f64> = Vec::new();
    let mut generations_inherited: Vec<f64> = Vec::new();
    let mut rungs_inherited: Vec<f64> = Vec::new();
    let mut rung_labels: BTreeSet<String> = BTreeSet::new();
    let mut rung_histogram: BTreeMap<u8, usize> = BTreeMap::new();
    let mut widths: Vec<f64> = Vec::new();
    let mut counts: Vec<f64> = Vec::new();
    let mut held = 0usize;
    let mut saturated = 0usize;

    let walk = Walk {
        ledger,
        lineage: &read.lineage,
        contact: &read.contact,
        policy: Transmission::AS_SHIPPED,
    };
    for subject in &read.events {
        let variants = variants_about_accumulating(
            &walk,
            &read.ladders,
            &read.durations,
            rule,
            *subject,
            PREDICATE,
        );

        // H3's population: an ending with at least three holders (§6's rule,
        // the same threshold `variant_count` applies to campaign 2's walk).
        if variants.len() >= 3 {
            let mut seen: BTreeSet<(Precision, String)> = BTreeSet::new();
            for variant in &variants {
                seen.insert((variant.precision, format!("{:?}", variant.object)));
            }
            counts.push(seen.len() as f64);
            let witnesses = witnesses_of(ledger, &read.lineage, *subject, PREDICATE);
            widths.push(maximum_antichain(&read.lineage, &witnesses).len() as f64);
        }

        for variant in &variants {
            held += 1;
            let rung = variant.precision.rung();
            *rung_histogram.entry(rung).or_default() += 1;

            // The holder's OWN people's ladder decides both the label and
            // what "coarsest" means. Fission never crosses a people boundary
            // (campaign 2 §3.1), so this is also the ladder the walk used.
            let people = read.people_of.get(&variant.holder);
            let ladder = read.ladders.for_people(people.map_or("", |p| p.as_str()));
            if let Some(label) = ladder.label(variant.precision) {
                rung_labels.insert(label.to_string());
            }
            if !ladder.is_empty() && rung as usize == ladder.len() - 1 {
                saturated += 1;
            }

            if let Some(generation) = people.and_then(|p| read.generation_years.get(p)) {
                generations.push(*generation);
                rungs.push(f64::from(rung));
                if variant.hops > 0 {
                    generations_inherited.push(*generation);
                    rungs_inherited.push(f64::from(rung));
                }
            }
        }
    }

    Readout {
        held,
        rho_generation_precision: spearman(&generations, &rungs),
        rho_generation_precision_inheritors: spearman(&generations_inherited, &rungs_inherited),
        distinct_rungs: rung_histogram.len(),
        rung_labels,
        saturated,
        qualifying_events: counts.len(),
        rho_antichain_variants: spearman(&widths, &counts),
        rung_histogram,
    }
}

/// Assemble one world's people table, ladders and events.
///
/// `None` when the world offers no year rung to convert allometric years into
/// standard days — the readout reports such a seed as skipped rather than
/// guessing a year length.
fn read_world(
    ledger: &Ledger,
    components: &hornvale_worldgen::WorldComponents,
) -> Option<WorldRead> {
    let lineage = lineage_of(ledger);
    let contact = contact_of(ledger);
    let astronomical = PrecisionLadder::of(ledger);
    let year_days = astronomical
        .labels()
        .iter()
        .position(|label| *label == "year")
        .and_then(|i| astronomical.span(Precision(i as u8)))
        .map(|span| span.get())?;

    let mut people_of: BTreeMap<EntityId, String> = BTreeMap::new();
    for occ in lineage.all() {
        if let Some(Value::Text(people)) = ledger.value_of(occ, hornvale_history::OCC_PEOPLE) {
            people_of.insert(occ, people.clone());
        }
    }

    let mut durations = PeopleDurations::default();
    let mut generation_years: BTreeMap<String, f64> = BTreeMap::new();
    let named: BTreeSet<&String> = people_of.values().collect();
    for people in named {
        let Some(bio) = components.biosphere.get_by_label(people) else {
            continue;
        };
        let life = hornvale_species::life_history(bio.mass, bio.thermal_strategy, bio.schedule);
        let to_days = |years: hornvale_kernel::Years| StdDays::new(years.get() * year_days).ok();
        let generation = life.generation_length.and_then(to_days);
        let lifespan = life.lifespan.and_then(to_days);
        durations.insert(people, generation, lifespan);
        if let Some(years) = life.generation_length {
            generation_years.insert(people.clone(), years.get());
        }
    }

    let ladders = PeopleLadders::of(ledger, &durations);
    let mut events: Vec<EntityId> = ledger.find(PREDICATE).map(|fact| fact.subject).collect();
    events.sort();
    events.dedup();

    Some(WorldRead {
        lineage,
        contact,
        durations,
        ladders,
        generation_years,
        people_of,
        events,
    })
}

/// Print one summary line for a column of per-seed values, so a null reads as
/// a null instead of an absence.
fn summarise(name: &str, values: &[Option<f64>]) {
    let mut defined: Vec<f64> = values.iter().filter_map(|v| *v).collect();
    let undefined = values.len() - defined.len();
    if defined.is_empty() {
        println!("    {name:<28} NO VALUE on any of {} seeds", values.len());
        return;
    }
    defined.sort_by(f64::total_cmp);
    let negative = defined.iter().filter(|v| **v < 0.0).count();
    let positive = defined.iter().filter(|v| **v > 0.0).count();
    let zero = defined.len() - negative - positive;
    println!(
        "    {name:<28} n={:<3} undefined={undefined:<3} min={:+.4} median={:+.4} max={:+.4}  \
         sign: {negative} neg / {zero} zero / {positive} pos",
        defined.len(),
        defined[0],
        defined[defined.len() / 2],
        defined[defined.len() - 1],
    );
}

/// Print one summary line for a column of per-seed counts.
fn summarise_counts(name: &str, values: &[f64]) {
    if values.is_empty() {
        println!("    {name:<28} NO VALUE");
        return;
    }
    let mut sorted = values.to_vec();
    sorted.sort_by(f64::total_cmp);
    let mean = sorted.iter().sum::<f64>() / sorted.len() as f64;
    println!(
        "    {name:<28} n={:<3} min={:.4} median={:.4} mean={:.4} max={:.4}",
        sorted.len(),
        sorted[0],
        sorted[sorted.len() / 2],
        mean,
        sorted[sorted.len() - 1],
    );
}

/// THE READOUT. Spec §6, over a fixed seed panel, under all three
/// accumulation rules, with no rule nominated.
///
/// claim: readout(off-gate, heavy:) — the preregistered §6 readout over
/// `PANEL`; reports H1, H2 and H3 against their decision tables and asserts
/// only substrate controls. A falsified prediction is a finding here, and
/// saturation is one §3.7 predicted in advance.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn the_palimpsest_readout_over_a_seed_panel() {
    let components = hornvale_worldgen::WorldComponents::assemble().expect("components assemble");

    // Per rule, per seed.
    let mut per_rule: Vec<Vec<(u64, Readout)>> = vec![Vec::new(); Accumulation::ALL.len()];
    let mut skipped: Vec<u64> = Vec::new();
    let mut ladder_shapes: BTreeMap<String, usize> = BTreeMap::new();

    for seed in PANEL {
        let world = hornvale_worldgen::build_world(
            hornvale_kernel::Seed(seed),
            &hornvale_astronomy::SkyPins::default(),
            hornvale_worldgen::SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
        )
        .expect("panel seed builds");
        let ledger = &world.ledger;
        let Some(read) = read_world(ledger, &components) else {
            skipped.push(seed);
            continue;
        };

        for people in read.durations.peoples() {
            let ladder = read.ladders.for_people(people);
            *ladder_shapes
                .entry(format!("{:?}", ladder.labels()))
                .or_default() += 1;
        }

        for (index, rule) in Accumulation::ALL.iter().enumerate() {
            per_rule[index].push((seed, measure(ledger, &read, *rule)));
        }
    }

    println!("\n================ THE PALIMPSEST READOUT ================");
    println!(
        "panel                : {} seeds (census seeds 0-39)",
        PANEL.len()
    );
    println!("seeds skipped (no year rung): {skipped:?}");
    println!("predicate            : {PREDICATE}");
    println!("rules                : additive, quadrature, multiplicative (NO PRIMARY, spec §6.4)");
    println!(
        "panel chosen by       : a measured 5-seed pilot (seeds 0-4) costing 12.40 s of test \
         time, ~2.5 s/seed"
    );
    println!(
        "width unit caveat    : the accumulator seeds width in STD DAYS and increments it by a \
         DIMENSIONLESS generation count — see this file's module doc before reading the rung \
         columns"
    );
    println!("\nLADDER SHAPES SEEN (people-ladder -> how many peoples across the panel):");
    for (shape, count) in &ladder_shapes {
        println!("  {count:>5}x {shape}");
    }

    for (index, rule) in Accumulation::ALL.iter().enumerate() {
        let rows = &per_rule[index];
        println!("\n=== RULE: {} ===", rule.label());
        println!(
            "  {:<6} {:>8} {:>9} {:>7} {:>10} {:>7} {:>11}  rung histogram",
            "seed", "held", "H1 rho", "H2 rgs", "saturated", "events", "H3 rho"
        );
        for (seed, readout) in rows {
            let h1 = readout
                .rho_generation_precision
                .map_or("     n/a".to_string(), |v| format!("{v:+.5}"));
            let h3 = readout
                .rho_antichain_variants
                .map_or("        n/a".to_string(), |v| format!("{v:+.5}"));
            println!(
                "  {seed:<6} {:>8} {h1:>9} {:>7} {:>10.4} {:>7} {h3:>11}  {:?}",
                readout.held,
                readout.distinct_rungs,
                readout.saturated_fraction(),
                readout.qualifying_events,
                readout.rung_histogram,
            );
        }

        println!("  --- panel summary, rule {} ---", rule.label());
        summarise(
            "H1 rho(gen, rung)",
            &rows
                .iter()
                .map(|(_, r)| r.rho_generation_precision)
                .collect::<Vec<_>>(),
        );
        summarise(
            "H1 rho, inheritors only",
            &rows
                .iter()
                .map(|(_, r)| r.rho_generation_precision_inheritors)
                .collect::<Vec<_>>(),
        );
        summarise_counts(
            "H2 distinct rungs",
            &rows
                .iter()
                .map(|(_, r)| r.distinct_rungs as f64)
                .collect::<Vec<_>>(),
        );
        summarise_counts(
            "saturated fraction",
            &rows
                .iter()
                .map(|(_, r)| r.saturated_fraction())
                .collect::<Vec<_>>(),
        );
        summarise(
            "H3 rho(width, variants)",
            &rows
                .iter()
                .map(|(_, r)| r.rho_antichain_variants)
                .collect::<Vec<_>>(),
        );

        let mut labels: BTreeSet<&str> = BTreeSet::new();
        let mut pooled: BTreeMap<u8, usize> = BTreeMap::new();
        for (_, readout) in rows {
            for label in &readout.rung_labels {
                labels.insert(label.as_str());
            }
            for (rung, count) in &readout.rung_histogram {
                *pooled.entry(*rung).or_default() += count;
            }
        }
        println!("    rungs reached (labels)       {labels:?}");
        println!("    rungs reached (pooled index) {pooled:?}");
    }

    // ---- SUBSTRATE CONTROLS ONLY. No hypothesis is asserted. ----
    assert!(
        skipped.len() < PANEL.len(),
        "control: every panel seed was skipped for want of a year rung"
    );
    for (index, rule) in Accumulation::ALL.iter().enumerate() {
        let total: usize = per_rule[index].iter().map(|(_, r)| r.held).sum();
        assert!(
            total > 0,
            "control: rule {} produced no held claims at all",
            rule.label()
        );
    }
}

// ===========================================================================
// THE NON-VACUITY CONTROL. Cheap, hand-built, and NOT ignored.
//
// The plan's decision rule says a metric constant across the whole panel is
// evidence of a vacuous computation before it is evidence of a finding. That
// distinction is only usable if something independently proves the four
// quantities CAN move, so this fixture makes each of them move through the
// same `measure` the panel runs.
// ===========================================================================

/// The control's peoples: label, generation length, lifespan — all in std
/// days, chosen so their ladders interleave with the astronomical rungs the
/// fixture commits (day 1, moon 41.7, year 372.4).
///
/// `brief` exists to make SATURATION reachable: with a two-day generation the
/// accumulated width runs past the year rung, which is its ladder's coarsest.
/// `slow` exists to make it avoidable: with a four-hundred-day generation
/// nothing leaves the day rung. Without both, `saturated_fraction` would be a
/// constant 0 or 1 in the control and could not detect a stuck computation.
const CONTROL_PEOPLES: [(&str, f64, f64); 4] = [
    ("brief", 2.0, 6.0),
    ("swift", 20.0, 60.0),
    ("mid", 100.0, 300.0),
    ("slow", 400.0, 1200.0),
];

/// Where each control people's lineage is rooted.
fn control_base(index: usize) -> u64 {
    10 * (index as u64) + 1
}

/// One people's branched lineage, as `(occupation, founding day)` offsets
/// from its base.
///
/// ```text
/// R(+0, founded 0, ENDS on day 100)
///  |-- A(+1, founded 100)  <- refounded the day R ended: a WITNESS
///  |    `-- C(+3, founded 300, ENDS on day 2000)
///  |         `-- D(+4, founded 700)
///  |              `-- E(+5, founded 1500)
///  `-- B(+2, founded 100)  <- a second survivor, incomparable to A
///       `-- F(+6, founded 900)
/// ```
///
/// Two endings with DIFFERENT maximum-antichain widths — R's witnesses are
/// `{R, A, B}` whose maximum antichain is `{A, B}` (width 2), while C's are
/// `{C}` alone (width 1) — which is what keeps H3's x-axis from being
/// constant. `E` is reachable from both `R` and `A`, so the multi-path merge
/// runs too.
const CONTROL_FOUNDINGS: [(u64, f64); 7] = [
    (0, 0.0),
    (1, 100.0),
    (2, 100.0),
    (3, 300.0),
    (4, 700.0),
    (5, 1500.0),
    (6, 900.0),
];

/// The parent offset of each occupation in [`CONTROL_FOUNDINGS`], `None` for
/// the root.
const CONTROL_PARENTS: [(u64, Option<u64>); 7] = [
    (0, None),
    (1, Some(0)),
    (2, Some(0)),
    (3, Some(1)),
    (4, Some(3)),
    (5, Some(4)),
    (6, Some(2)),
];

/// The control world: four peoples, each with the branched lineage above, on
/// one shared sky (day 1, one moon 41.7, year 372.4 — the same trio
/// `tests/common`'s fixtures commit).
fn control_world() -> (Ledger, WorldRead) {
    let mut edges: Vec<(u64, Option<u64>)> = Vec::new();
    for index in 0..CONTROL_PEOPLES.len() {
        let base = control_base(index);
        for (child, parent) in CONTROL_PARENTS {
            edges.push((base + child, parent.map(|p| base + p)));
        }
    }
    let mut led = ledger_with(&edges);

    let mut durations = PeopleDurations::default();
    let mut generation_years: BTreeMap<String, f64> = BTreeMap::new();
    let mut people_of: BTreeMap<EntityId, String> = BTreeMap::new();

    for (index, (people, generation, lifespan)) in CONTROL_PEOPLES.iter().enumerate() {
        let base = control_base(index);
        for (offset, day) in CONTROL_FOUNDINGS {
            put(
                &mut led,
                base + offset,
                hornvale_history::OCC_FOUNDED,
                Value::Number(day),
            );
            put(
                &mut led,
                base + offset,
                hornvale_history::OCC_PEOPLE,
                Value::Text((*people).to_string()),
            );
            people_of.insert(eid(base + offset), (*people).to_string());
        }
        // R ends the day A and B were refounded; C ends much later, alone.
        put(
            &mut led,
            base,
            hornvale_history::OCC_ENDED,
            Value::Number(100.0),
        );
        put(
            &mut led,
            base + 3,
            hornvale_history::OCC_ENDED,
            Value::Number(2000.0),
        );
        durations.insert(
            people,
            StdDays::new(*generation).ok(),
            StdDays::new(*lifespan).ok(),
        );
        generation_years.insert((*people).to_string(), *generation);
    }

    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::DAY_LENGTH_STD,
        Value::Number(1.0),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::MOON_PERIOD_STD,
        Value::Number(41.7),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::YEAR_LENGTH_STD,
        Value::Number(372.4),
    );

    let lineage = lineage_of(&led);
    let contact = contact_of(&led);
    let ladders = PeopleLadders::of(&led, &durations);
    let mut events: Vec<EntityId> = led.find(PREDICATE).map(|fact| fact.subject).collect();
    events.sort();
    events.dedup();

    let read = WorldRead {
        lineage,
        contact,
        durations,
        ladders,
        generation_years,
        people_of,
        events,
    };
    (led, read)
}

#[test]
fn the_control_world_moves_every_quantity_the_readout_reports() {
    let (led, read) = control_world();
    assert_eq!(read.events.len(), 8, "four peoples x two endings");

    for rule in Accumulation::ALL {
        let readout = measure(&led, &read, rule);
        assert!(
            readout.held > 0,
            "{}: the control produced no held claims",
            rule.label()
        );
        assert!(
            readout.distinct_rungs >= 3,
            "{}: distinct_rungs stuck at {} — the rung computation cannot spread",
            rule.label(),
            readout.distinct_rungs
        );
        let fraction = readout.saturated_fraction();
        assert!(
            fraction > 0.0 && fraction < 1.0,
            "{}: saturated_fraction is {fraction}, not strictly between 0 and 1 — \
             the null detector cannot tell saturation from its absence",
            rule.label()
        );
        assert!(
            readout.rho_generation_precision.is_some(),
            "{}: H1's rho is undefined on a fixture whose generation lengths and \
             rungs both vary — the correlation cannot be computed at all",
            rule.label()
        );
        assert!(
            readout.rho_antichain_variants.is_some(),
            "{}: H3's rho is undefined on a fixture with two antichain widths — \
             the correlation cannot be computed at all",
            rule.label()
        );
    }
}

/// H1's DIRECTION is reachable on a fixture built to have it: a shorter
/// generation must retain a coarser rung, so the correlation is negative.
///
/// This asserts nothing about the real world. It asserts that a negative rho
/// is reachable through `measure`, so a non-negative panel result is a finding
/// about worlds rather than a sign error in this file.
#[test]
fn a_shorter_generation_retains_a_coarser_rung_in_the_control() {
    let (led, read) = control_world();
    for rule in Accumulation::ALL {
        let rho = measure(&led, &read, rule)
            .rho_generation_precision
            .expect("defined on the control");
        assert!(
            rho < 0.0,
            "{}: expected a negative rho on a fixture built for one, got {rho}",
            rule.label()
        );
    }
}
