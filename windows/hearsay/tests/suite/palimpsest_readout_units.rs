//! THE POST-HOC, UNIT-CORRECTED READOUT. **NOT PREREGISTERED.**
//!
//! Read this paragraph before any number below it. The preregistered result of
//! this campaign is `windows/hearsay/tests/palimpsest_readout.rs` (spec §6),
//! frozen before the code that could move it. **This file is not that.** It was
//! written AFTER unblinding, in response to a defect the readout surfaced, and
//! it exists only so that the defect's consequence is measured rather than
//! left as an inference. Every number it prints is exploratory. Nothing here
//! replaces, amends, or reinterprets a preregistered number, and a reader who
//! finds this output in isolation must treat it as what it says it is.
//!
//! **The defect (spec §6.6, the erratum).** The frozen model adds two
//! different quantities:
//!
//! ```text
//! derive.rs:294    width seeded from ladder.span(FINEST)      -> STD DAYS (~0.88)
//! amplitude.rs:53  gen_span returns (fh - ft).abs() / g       -> GENERATIONS
//! accumulate.rs:77 precision_at compares span.get() <= width  -> STD DAYS
//! ```
//!
//! Spec §5.1 puts the amplitude in generations, §5.2 puts the rungs in days,
//! §5.3 compares them, and no section specifies a conversion. **The defect is
//! in the spec; the implementation is faithful to it.** So `Additive` and
//! `Quadrature` never tested §3.7's saturation prediction — they measured a
//! dimensionless count against a day-scale ladder — while `Multiplicative` is
//! scale-free and therefore unaffected.
//!
//! **What this file changes, and it is exactly one thing.** The amplitude is
//! converted to std days before it is accumulated, by multiplying
//! [`hornvale_hearsay::amplitude::gen_span`]'s generation count by the
//! teller's people's generation length in std days — the quantity
//! [`hornvale_hearsay::durations::PeopleDurations::get`] already returns. That
//! is [`amplitude_days`] below, and it is the whole delta.
//!
//! Everything else is deliberately identical to the frozen readout: the same
//! 40-seed census panel, the same predicate, the same three accumulation rules,
//! the same four reported quantities in the same order, the same
//! `divergence::maximum_antichain` for H3's x-axis. The two readouts therefore
//! compare line for line.
//!
//! **Why the walk is re-derived here instead of fixed in `src/`.** Repairing
//! `variants_about_accumulating`, `gen_span`, `Accumulation::step` or
//! `precision_at` after seeing a readout would silently replace a
//! preregistered measurement with a post-hoc one — the move this project
//! forbids, and the reason the defect was reported rather than quietly
//! corrected. Task 6's battery must keep measuring exactly what it measured,
//! so the corrected walk lives in this test file and nowhere else. The frozen
//! model is untouched; campaign 4 gets the conversion with its own
//! preregistration, written by someone who has not seen these numbers.
//!
//! **The scale check runs first, and it can stop the run.** The recurring
//! failure of this campaign is a plausible-looking number that measures
//! nothing, so before the panel this battery prints, on one seed, the median
//! single-step contribution in days, every people's rung spans in days, and
//! how many steps a median path needs to pass each rung. Two decision rules
//! govern the reading, and they were fixed before the run:
//!
//! - a median step already past the coarsest rung is **the saturation §3.7
//!   predicts** — a ladder built only from durations the world contains cannot
//!   hold a chain that outlives every one of them. That is a result. It is
//!   reported, never tuned away. (Spec §3.6's day-scale figures are not a
//!   comparator for it: those are path-weighted, this column is one unweighted
//!   entry per lineage edge. See [`single_step_days`].)
//! - a median step under the FINEST rung means the conversion is inverted.
//!   That is a broken instrument, not a finding, so it is the one thing this
//!   file asserts about scale — the run stops there rather than printing a
//!   panel nobody could interpret.
//!
//! **MULTIPLICATIVE IS NOT UNIT-CORRECTED HERE, AND ITS EXPLORATORY ROW IS NOT
//! A COUNTERPART TO THE FROZEN ONE.** This is a correction to spec §6.6, found
//! in review after the first run. `Accumulation::Multiplicative` is
//! `width * (1 + span)`; with the frozen dimensionless `span` that is
//! days × dimensionless = **days, already coherent**. Substituting `span_days`
//! uniformly — which is what this battery does, deliberately, so that exactly
//! one thing changes across all three rules — turns it into
//! `width * (1 + span_days)`, i.e. a duration added to a pure number. That is
//! a THIRD, differently-incoherent model, not a repair: growth runs ~2.7e4 per
//! step and ~1e66 over fifteen, unrelated to any rung on any ladder. The tell
//! was in the first run's numbers and was missed — §6.6 calls multiplicative
//! the rule the defect does not touch, yet it moved the most (H2 median
//! 5.0 → 4.0, saturation 0.13 → 0.79). The computation is left exactly as it
//! is and the number stands; only its LABEL was wrong. Read the multiplicative
//! rows as a third model, never as "multiplicative, corrected".
//!
//! As in the frozen battery, the hypotheses themselves are REPORTED and the
//! assertions are substrate controls only. Two of those controls are NOT
//! `#[ignore]`d and run in every gate, because the 77-line local walk below is
//! a hand transcription of the project's most subtle function and the panel
//! that exercises it is heavy-tier: [`the_local_walk_matches_the_frozen_one_when_a_generation_is_one_day`]
//! pins the transcription, and [`the_conversion_actually_moves_the_walk_when_a_generation_is_not_one_day`]
//! pins the conversion being applied at all. Neither builds a world.

use crate::common;

use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::accumulate::{Accumulation, precision_at};
use hornvale_hearsay::amplitude::gen_span;
use hornvale_hearsay::contact::contact_of;
use hornvale_hearsay::derive::{variants_about_accumulating, witnesses_of};
use hornvale_hearsay::divergence::maximum_antichain;
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_hearsay::ladder::{PeopleLadders, PrecisionLadder};
use hornvale_hearsay::lineage::{Lineage, lineage_of};
use hornvale_hearsay::spearman;
use hornvale_hearsay::transmission::{Transmission, Walk};
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use hornvale_kernel::provenance::Provenance;
use hornvale_kernel::{Claim, Precision};
use std::collections::{BTreeMap, BTreeSet};

/// The same panel the frozen readout uses: the first 40 seeds of the census
/// panel, so the two are strictly comparable and both are a subset of the
/// population the spec named.
const PANEL: [u64; 40] = [
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
];

/// The same predicate: only an ending has parties beyond its subject.
const PREDICATE: &str = hornvale_history::OCC_ENDED;

// ===========================================================================
// THE ONE DELTA: the amplitude, in std days.
// ===========================================================================

/// The retelling `teller -> hearer` as a span in **std days** rather than in
/// the teller's generations.
///
/// The frozen model accumulates [`gen_span`], which is
/// `|founded(hearer) - founded(teller)| / generation_length(people(teller))` —
/// a dimensionless generation count — into a width seeded in std days and read
/// against a day-scale ladder. Multiplying that count back by the same
/// generation length restores the day scale the ladder is written in, and it
/// is the only change this battery makes to the model.
///
/// `gen_span` is called rather than reimplemented, so the two readouts share
/// every totality rule it carries: a missing people, a missing founding day or
/// a non-positive generation length all yield 0.0 here exactly as they do
/// there, and `Accumulation::step` ignores a non-positive span under every
/// rule.
fn amplitude_days(
    led: &Ledger,
    durations: &PeopleDurations,
    teller: EntityId,
    hearer: EntityId,
) -> f64 {
    let generations = gen_span(led, durations, teller, hearer);
    if generations <= 0.0 {
        return 0.0;
    }
    let Some(Value::Text(people)) = led.value_of(teller, hornvale_history::OCC_PEOPLE) else {
        return 0.0;
    };
    let (Some(generation), _) = durations.get(people) else {
        return 0.0;
    };
    generations * generation.get()
}

/// The unit-corrected sibling of
/// `hornvale_hearsay::derive::variants_about_accumulating`.
///
/// A line-for-line re-derivation of that function with [`amplitude_days`]
/// substituted for `gen_span` at the single call site where the amplitude
/// enters the accumulator. The multi-path tie-break, the witness seeding, the
/// per-people ladder and the emit-time rung resolution are all unchanged, so
/// any difference between this readout and the frozen one is attributable to
/// the units and to nothing else.
fn variants_about_accumulating_in_days(
    ledger: &Ledger,
    lineage: &Lineage,
    ladders: &PeopleLadders,
    durations: &PeopleDurations,
    rule: Accumulation,
    subject: EntityId,
    predicate: &str,
) -> Vec<Claim> {
    let Some(object) = ledger.value_of(subject, predicate) else {
        return Vec::new();
    };
    let base = Claim {
        holder: subject,
        subject,
        predicate: predicate.to_string(),
        object: object.clone(),
        grade: Provenance::Witnessed,
        hops: 0,
        precision: Precision::FINEST,
    };
    let witnesses = witnesses_of(ledger, lineage, subject, predicate);
    let mut best: BTreeMap<EntityId, ((u64, u32, EntityId), Claim)> = BTreeMap::new();

    for w in &witnesses {
        let mut c = base.clone();
        c.holder = *w;
        best.insert(*w, ((0, 0, *w), c));
    }

    for w in &witnesses {
        for d in lineage.descendants_of(*w) {
            if witnesses.contains(&d) {
                continue;
            }
            let ancestry = lineage.ancestry(d);
            let Some(pos) = ancestry.iter().position(|a| a == w) else {
                continue;
            };
            let mut path: Vec<EntityId> = ancestry[..=pos].to_vec();
            path.reverse();

            let people = match ledger.value_of(*w, hornvale_history::OCC_PEOPLE) {
                Some(Value::Text(p)) => p.clone(),
                _ => String::new(),
            };
            let ladder = ladders.for_people(&people);
            let mut width = ladder
                .span(Precision::FINEST)
                .map(|days| days.get())
                .unwrap_or(0.0);

            let mut c = base.clone();
            c.holder = *w;
            for pair in path.windows(2) {
                let (teller, hearer) = (pair[0], pair[1]);
                width = rule.step(width, amplitude_days(ledger, durations, teller, hearer));
                let precision = precision_at(ladder, width);
                let object = match &c.object {
                    Value::Number(day) => Value::Number(ladder.apply(precision, *day)),
                    other => other.clone(),
                };
                c = c.retold_by_lossy(hearer, precision, object);
            }

            let key = (width.to_bits(), c.hops, *w);
            match best.get(&d) {
                Some((best_key, _)) if *best_key <= key => {}
                _ => {
                    best.insert(d, (key, c));
                }
            }
        }
    }

    best.into_values().map(|(_, c)| c).collect()
}

// ===========================================================================
// Everything below mirrors the frozen battery's reporting, so the two outputs
// line up column for column.
// ===========================================================================

/// One accumulation rule's readout over one world.
#[derive(Clone, Debug)]
struct Readout {
    /// Held claims summed over every ending in the world.
    held: usize,
    /// H1: Spearman rho over held claims, generation length vs retained rung.
    rho_generation_precision: Option<f64>,
    /// H1, supplementary: the same rho over INHERITORS only (`hops > 0`).
    rho_generation_precision_inheritors: Option<f64>,
    /// H2: how many distinct rung INDICES carry at least one held claim.
    distinct_rungs: usize,
    /// H2, supplementary: the distinct rung LABELS actually in play.
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

/// Everything one world contributes, assembled once and shared by all three
/// rules.
struct WorldRead {
    /// The founding tree.
    lineage: Lineage,
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
    /// The astronomical-only ladder, used by the scale check for a
    /// people-independent finest rung.
    astronomical: PrecisionLadder,
}

/// Compute one rule's four quantities over one world, through the
/// unit-corrected walk.
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

    for subject in &read.events {
        let variants = variants_about_accumulating_in_days(
            ledger,
            &read.lineage,
            &read.ladders,
            &read.durations,
            rule,
            *subject,
            PREDICATE,
        );

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
/// standard days — such a seed is reported as skipped rather than guessed at.
fn read_world(
    ledger: &Ledger,
    components: &hornvale_worldgen::WorldComponents,
) -> Option<WorldRead> {
    let lineage = lineage_of(ledger);
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
        durations,
        ladders,
        generation_years,
        people_of,
        events,
        astronomical,
    })
}

/// The value at `fraction` through a sorted, non-empty column, by nearest
/// index — never interpolated, so every printed figure is an observed one.
fn percentile(sorted: &[f64], fraction: f64) -> f64 {
    let index = ((sorted.len() - 1) as f64 * fraction).round() as usize;
    sorted[index]
}

/// Every single-step contribution in this world, in std days: **one entry per
/// parent -> child founding edge of the lineage, unweighted**.
///
/// **This is NOT the population the walk steps over, and the difference is
/// large enough to matter.** The walk's `path.windows(2)` visits an edge once
/// per (event, witness, descendant) path that crosses it, so a step near the
/// root of a deep line is counted many times and an edge on no
/// witness-to-descendant path is not counted at all — a path-weighted
/// distribution. This function counts each edge exactly once whether the walk
/// traverses it or not, including edges whose founding facts are missing (they
/// enter as 0.0). Seed 0 has 655 edges here; the path-weighted population on a
/// comparable world runs into the tens of thousands.
///
/// Both are distributions of the same per-edge quantity and neither is wrong,
/// but they are not interchangeable and their percentiles must not be compared
/// across the two. What this one is FOR is the scale question — is the
/// converted amplitude on the order of a rung, or orders of magnitude off it —
/// and for that an unweighted per-edge median is the honest instrument,
/// because it is not dominated by whichever lines happen to be deepest.
fn single_step_days(ledger: &Ledger, read: &WorldRead) -> Vec<f64> {
    let mut out: Vec<f64> = Vec::new();
    for occ in read.lineage.all() {
        if let Some(parent) = read.lineage.parent(occ) {
            out.push(amplitude_days(ledger, &read.durations, parent, occ));
        }
    }
    out.sort_by(f64::total_cmp);
    out
}

/// Under `Additive`, how many steps of size `step` a width seeded at `start`
/// needs before it reaches `target`. Zero means the seed width is already
/// there; the additive rule is used because it is the slowest-climbing of the
/// three for a positive step, so this is an upper bound across rules.
fn steps_to_pass(start: f64, step: f64, target: f64) -> f64 {
    if step <= 0.0 {
        return f64::INFINITY;
    }
    ((target - start) / step).ceil().max(0.0)
}

/// Print one summary line for a column of per-seed values.
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

/// Build one panel world at `seed`.
fn build(seed: u64) -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("panel seed builds")
}

/// The scale check, on one seed, printed before the panel and asserted before
/// the panel is interpreted.
///
/// Returns the median single-step contribution in std days.
fn print_scale_check(seed: u64, components: &hornvale_worldgen::WorldComponents) -> f64 {
    let world = build(seed);
    let ledger = &world.ledger;
    let read = read_world(ledger, components).expect("the scale-check seed offers a year rung");

    let steps = single_step_days(ledger, &read);
    assert!(
        !steps.is_empty(),
        "control: the scale-check seed has no founding edges to measure a step over"
    );
    let zeros = steps.iter().filter(|v| **v <= 0.0).count();
    let median = percentile(&steps, 0.50);

    println!("\n---- SCALE CHECK (seed {seed}), BEFORE the panel ----");
    println!(
        "  single-step amplitude, in STD DAYS (one per parent->child founding edge, n={}, \
         {zeros} of them zero):",
        steps.len()
    );
    println!(
        "    p10={:.3}  p50={:.3}  p75={:.3}  p90={:.3}  p99={:.3}  max={:.3}",
        percentile(&steps, 0.10),
        median,
        percentile(&steps, 0.75),
        percentile(&steps, 0.90),
        percentile(&steps, 0.99),
        steps[steps.len() - 1],
    );

    let finest = read
        .astronomical
        .span(Precision::FINEST)
        .map(|span| span.get())
        .expect("a world with a year rung has a finest rung");
    println!(
        "  astronomical rungs (days): {:?}",
        read.astronomical
            .labels()
            .iter()
            .enumerate()
            .map(|(i, label)| {
                let span = read
                    .astronomical
                    .span(Precision(i as u8))
                    .map_or(f64::NAN, |days| days.get());
                format!("{label}={span:.3}")
            })
            .collect::<Vec<_>>()
    );
    println!(
        "  per-people ladders, and how many MEDIAN steps (p50={median:.3} d, additive, seeded at \
         the finest rung) it takes to pass each rung:"
    );
    for people in read.durations.peoples() {
        let ladder = read.ladders.for_people(people);
        let start = ladder
            .span(Precision::FINEST)
            .map(|span| span.get())
            .unwrap_or(0.0);
        let vertices: Vec<String> = ladder
            .labels()
            .iter()
            .enumerate()
            .map(|(i, label)| {
                let span = ladder
                    .span(Precision(i as u8))
                    .map_or(f64::NAN, |days| days.get());
                format!(
                    "{label}={span:.1}d/{}steps",
                    steps_to_pass(start, median, span)
                )
            })
            .collect();
        println!("    {people:<14} {}", vertices.join("  "));
    }

    let coarsest_spans: Vec<f64> = read
        .durations
        .peoples()
        .iter()
        .filter_map(|people| {
            let ladder = read.ladders.for_people(people);
            if ladder.is_empty() {
                None
            } else {
                ladder
                    .span(Precision((ladder.len() - 1) as u8))
                    .map(|span| span.get())
            }
        })
        .collect();
    let past_coarsest = coarsest_spans
        .iter()
        .filter(|span| median >= **span)
        .count();
    println!(
        "  READING: the median single step ({median:.3} d) is past the COARSEST rung of {} of {} \
         people-ladders, and past the finest rung ({finest:.3} d) by {:.1}x.",
        past_coarsest,
        coarsest_spans.len(),
        median / finest,
    );
    println!(
        "  DECISION RULE (fixed before the run): median past the coarsest rung is the saturation \
         §3.7 predicts and is REPORTED, not tuned; median under the finest rung means an \
         inverted conversion and stops the run."
    );
    println!(
        "  POPULATION CAVEAT: this column is ONE ENTRY PER LINEAGE EDGE, UNWEIGHTED, including \
         edges no walk traverses. Spec §3.6's day-scale figures are PATH-WEIGHTED (one entry per \
         step of every witness-to-descendant path). Same per-edge quantity, different \
         distributions — do not compare percentiles across the two."
    );

    assert!(
        median >= finest,
        "control: the median single step is {median} d, under the finest rung's {finest} d — \
         the generations-to-days conversion is inverted, and the panel below would measure \
         nothing. Stop and report."
    );
    median
}

/// THE UNIT-CORRECTED EXPLORATORY READOUT. Post-hoc, not preregistered; the
/// preregistered readout is `palimpsest_readout.rs` (spec §6) and this never
/// replaces it.
///
/// claim: readout(off-gate, heavy:) — the post-hoc §6.7 readout over `PANEL`,
/// identical to the frozen §6 battery except that the amplitude is converted
/// to std days before accumulating. Reports H1, H2 and H3; asserts only
/// substrate controls and the one scale control that would catch an inverted
/// conversion.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn the_palimpsest_unit_corrected_exploratory_readout() {
    let components = hornvale_worldgen::WorldComponents::assemble().expect("components assemble");

    println!("\n======== THE PALIMPSEST READOUT — POST-HOC, UNIT-CORRECTED ========");
    println!("STATUS               : EXPLORATORY. **NOT PREREGISTERED.** Run after unblinding.");
    println!(
        "THE PREREGISTERED ONE: windows/hearsay/tests/palimpsest_readout.rs (spec §6). Its \
         numbers stand as measured; nothing here amends them."
    );
    println!(
        "WHY THIS EXISTS      : spec §6.6's erratum — the frozen model adds a DIMENSIONLESS \
         generation count to a STD-DAYS width and reads it against a STD-DAYS ladder, so \
         additive and quadrature never tested §3.7's saturation prediction."
    );
    println!(
        "THE ONE DELTA        : the amplitude is multiplied by the teller's people's generation \
         length in std days before accumulating. Panel, predicate, rules, quantities and \
         antichain code are identical to the frozen battery."
    );
    println!(
        "MULTIPLICATIVE CAVEAT: the multiplicative rows below are NOT a unit-corrected \
         counterpart to the frozen ones. `width * (1 + span)` with a DIMENSIONLESS span was \
         ALREADY coherent (days x dimensionless = days); substituting span_days makes it \
         `width * (1 + span_days)`, a duration added to a pure number — a THIRD, differently \
         incoherent model. Its numbers stand; its label was wrong (correcting spec §6.6)."
    );
    println!(
        "H2 CAVEAT            : under additive and quadrature the occupied rung set is exactly \
         {{finest}} u {{the top three rungs}} on all 40 seeds, so distinct_rungs is a constant 4 \
         carrying NO per-seed information. '4 >= 3' is NOT evidence for §6.3 — H2's evidence \
         lives in the frozen column."
    );

    let median_step = print_scale_check(PANEL[0], &components);

    let mut per_rule: Vec<Vec<(u64, Readout)>> = vec![Vec::new(); Accumulation::ALL.len()];
    let mut skipped: Vec<u64> = Vec::new();
    let mut ladder_shapes: BTreeMap<String, usize> = BTreeMap::new();

    for seed in PANEL {
        let world = build(seed);
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

    println!("\n---- PANEL ----");
    println!(
        "panel                : {} seeds (census seeds 0-39), the same panel the frozen battery \
         runs",
        PANEL.len()
    );
    println!("seeds skipped (no year rung): {skipped:?}");
    println!("predicate            : {PREDICATE}");
    println!("rules                : additive, quadrature, multiplicative (NO PRIMARY, spec §6.4)");
    println!(
        "scale-check median step (seed {}): {median_step:.3} std days",
        PANEL[0]
    );
    println!("\nLADDER SHAPES SEEN (people-ladder -> how many peoples across the panel):");
    for (shape, count) in &ladder_shapes {
        println!("  {count:>5}x {shape}");
    }

    for (index, rule) in Accumulation::ALL.iter().enumerate() {
        let rows = &per_rule[index];
        println!(
            "\n=== RULE: {} ({}, exploratory) ===",
            rule.label(),
            match rule {
                Accumulation::Multiplicative =>
                    "NOT unit-corrected — a THIRD model; see the multiplicative caveat above",
                _ => "unit-corrected",
            }
        );
        if *rule == Accumulation::Multiplicative {
            println!(
                "  CAVEAT: frozen multiplicative was already dimensionally coherent \
                 (days x dimensionless). Substituting span_days INTRODUCES an incoherence \
                 rather than removing one, so these rows are not a corrected counterpart to \
                 the frozen multiplicative rows — they are a third model."
            );
        } else {
            println!(
                "  CAVEAT: distinct_rungs is a constant 4 on every seed — the occupied set is \
                 exactly {{finest}} u {{the top three rungs}} — so it carries no per-seed \
                 information and is not evidence for §6.3."
            );
        }
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

        println!(
            "  --- panel summary, rule {} (EXPLORATORY) ---",
            rule.label()
        );
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

    println!(
        "\nREMINDER: every number above is EXPLORATORY and post-hoc. The preregistered result of \
         this campaign is in palimpsest_readout.rs."
    );

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
// THE TRANSCRIPTION CONTROLS. Cheap, hand-built, and NOT `#[ignore]`d — these
// are the only things in this file any gate ever executes.
//
// `variants_about_accumulating_in_days` is 77 hand-copied lines reproducing
// `derive::variants_about_accumulating`, the most subtle walk in the project:
// witness seeding, the ancestry slice, and a three-key multi-path tie-break.
// A slip in any of them changes WHICH claim a holder keeps without changing
// how many, so it would surface only as a plausible number in a report nobody
// can re-derive. The panel that would exercise it is heavy-tier and runs
// nowhere by default.
//
// The two controls below pin the two things that can independently be wrong,
// and NEITHER SUBSUMES THE OTHER:
//
//   1. the transcription — is the local walk the frozen walk?
//   2. the conversion — is it applied at all?
//
// The first is exact by construction. When every people's generation length
// is 1.0 std day, `gen_span` divides by 1.0 and `amplitude_days` multiplies
// by 1.0, so the two amplitudes are identically equal and the two walks must
// return claim-for-claim identical vectors. **But that also means the first
// control cannot see a FORGOTTEN conversion** — deleting the `* generation`
// factor is a no-op at g = 1.0, and the equivalence would still hold. That is
// what the second control is for: at g != 1.0 the two walks must DIFFER, and
// they differ in `precision`, so dropping the factor turns the second control
// red immediately. Round 1 review proposed the first alone as covering both;
// it does not, and the pair is the honest version.
// ===========================================================================

/// A ledger whose peoples all have generation length `generation` std days,
/// paired with the ladders and durations both walks read.
///
/// `common::chain_with_foundings` is a six-deep single line (lossy steps
/// accumulate over five retellings) and
/// `common::chain_with_a_survivor_shortcut` is the branched fixture whose
/// descendant `3` is reachable by two genuinely different routes — the one
/// shape that exercises the multi-path tie-break at all. Both commit
/// `occ-people = "human"` on every occupation, so one duration entry covers
/// each.
fn fixture(ledger: Ledger, generation: f64) -> (Ledger, Lineage, PeopleLadders, PeopleDurations) {
    let mut durations = PeopleDurations::default();
    durations.insert(
        "human",
        StdDays::new(generation).ok(),
        StdDays::new(generation * 3.0).ok(),
    );
    let lineage = lineage_of(&ledger);
    let ladders = PeopleLadders::of(&ledger, &durations);
    (ledger, lineage, ladders, durations)
}

/// The subject both fixtures put an `occ-ended` on.
fn ended_subject() -> EntityId {
    EntityId::new(1).expect("nonzero")
}

/// Run both walks over one fixture and hand back their results in order
/// (frozen, local).
fn both_walks(ledger: Ledger, generation: f64, rule: Accumulation) -> (Vec<Claim>, Vec<Claim>) {
    let (led, lineage, ladders, durations) = fixture(ledger, generation);
    let graph = contact_of(&led);
    let walk = Walk {
        ledger: &led,
        lineage: &lineage,
        contact: &graph,
        policy: Transmission::AS_SHIPPED,
    };
    let frozen = variants_about_accumulating(
        &walk,
        &ladders,
        &durations,
        rule,
        ended_subject(),
        PREDICATE,
    );
    let local = variants_about_accumulating_in_days(
        &led,
        &lineage,
        &ladders,
        &durations,
        rule,
        ended_subject(),
        PREDICATE,
    );
    (frozen, local)
}

/// CONTROL 1 — the transcription. At a generation length of exactly one std
/// day the conversion is the identity, so the local walk must reproduce the
/// frozen walk claim for claim: holder, precision, hops, grade and object all
/// equal, in the same order.
///
/// Vector equality rather than length equality is the point. The multi-path
/// tie-break decides WHICH of two tellings a holder keeps, so a slip there
/// changes a claim's precision and hops while leaving the count untouched —
/// exactly the failure a length check cannot see.
#[test]
fn the_local_walk_matches_the_frozen_one_when_a_generation_is_one_day() {
    for rule in Accumulation::ALL {
        for (name, ledger) in [
            ("deep chain", common::chain_with_foundings()),
            (
                "survivor shortcut",
                common::chain_with_a_survivor_shortcut(),
            ),
        ] {
            let (frozen, local) = both_walks(ledger, 1.0, rule);

            // Substrate: a walk that returned nothing, or that never retold
            // anything, would satisfy equality vacuously.
            assert!(
                frozen.len() >= 3,
                "{name}/{}: the fixture produced {} claims — too few to pin a walk",
                rule.label(),
                frozen.len()
            );
            assert!(
                frozen.iter().any(|c| c.hops > 0),
                "{name}/{}: no claim was ever retold, so no step was ever accumulated",
                rule.label()
            );

            assert_eq!(
                local,
                frozen,
                "{name}/{}: the local walk diverged from `variants_about_accumulating` on a \
                 fixture where the conversion is the identity (generation = 1.0 std day, so \
                 `gen_span` divides by 1 and `amplitude_days` multiplies by 1). The \
                 transcription is wrong, not the units.",
                rule.label()
            );
        }
    }
}

/// CONTROL 2 — the conversion is applied. At a generation length of 50 std
/// days the two amplitudes differ by a factor of 50, so the two walks must
/// reach different rungs. Deleting `* generation.get()` from
/// [`amplitude_days`] makes this test red; control 1 alone would stay green.
#[test]
fn the_conversion_actually_moves_the_walk_when_a_generation_is_not_one_day() {
    for rule in Accumulation::ALL {
        let (frozen, local) = both_walks(common::chain_with_foundings(), 50.0, rule);
        assert_eq!(
            frozen.len(),
            local.len(),
            "{}: the two walks disagree on how many holders exist, which is a transcription \
             fault rather than a units one",
            rule.label()
        );
        assert!(
            frozen
                .iter()
                .zip(&local)
                .any(|(a, b)| a.precision != b.precision),
            "{}: the corrected walk landed on exactly the frozen walk's rungs with a 50-day \
             generation, where the amplitudes differ 50-fold. The conversion is not being \
             applied — check that `amplitude_days` still multiplies by the generation length.",
            rule.label()
        );
    }
}
