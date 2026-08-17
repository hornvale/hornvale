//! Substrate probe (The Palimpsest, Myth campaign 3): which teller<->event
//! relations actually VARY along a transmission path?
//!
//! Campaign 2 measured a one-rung ceiling and named its cause: stance
//! partitions communities by their relation to a FIXED event subject, and
//! every such partition is closed under descent, so a path enters a class and
//! stays. Reading `variants_about` sharpens that — transmission steps are
//! strictly parent->child on a single-parent founding tree, so ANY relation
//! built only from lineage is constant across every step by construction.
//!
//! Campaign 3 needs a teller<->event relation that is NOT lineage-derived, so
//! it can flip more than once along one path. This probe measures the
//! candidates on one footing, before any hypothesis is frozen. The number that
//! decides each one is not its firing rate but its **flip ceiling**: the
//! maximum number of times it changes along the transmission paths that
//! actually occur.
//!
//! Reports only. Asserts nothing about outcomes; the two positive controls
//! below are assertions about the SUBSTRATE, reproducing published counts so a
//! zero can be told apart from a broken probe.

use hornvale_hearsay::derive::witnesses_of;
use hornvale_hearsay::lineage::{Lineage, lineage_of};
use hornvale_hearsay::stance;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use std::collections::{BTreeMap, BTreeSet};

/// The candidate relations, each a single axis taking a small integer label.
const RELATIONS: [&str; 5] = [
    "stance (campaign 2 control)",
    "coeval: did the teller exist at the event day",
    "raider: has the teller ever been named an attacker",
    "same-cause: teller's ending cause vs the subject's",
    "same-function: teller's function vs the subject's",
];

fn number(led: &Ledger, occ: EntityId, pred: &str) -> Option<f64> {
    match led.value_of(occ, pred) {
        Some(Value::Number(n)) => Some(*n),
        _ => None,
    }
}

/// `who`'s label on each candidate relation toward the event `(subject, day)`.
///
/// One entry per [`RELATIONS`] row, same order. Every relation is a single
/// axis: the labels are values ON that axis, never two unlike booleans
/// compared with `!=` — the type error campaign 2's rejected draft made.
fn labels(
    led: &Ledger,
    lin: &Lineage,
    raiders: &BTreeSet<EntityId>,
    subject: EntityId,
    day: f64,
    who: EntityId,
) -> [u8; 5] {
    // 0: stance, the control.
    let s = match stance::stance_of(led, lin, subject, who) {
        stance::Stance::Perpetrator => 0,
        stance::Stance::VictimLine => 1,
        stance::Stance::Bystander => 2,
    };

    // 1: coeval. Where the teller's own lifespan sits against the event day.
    // Not lineage-derived: a founding day is a node's own fact.
    let founded = number(led, who, hornvale_history::OCC_FOUNDED);
    let ended = number(led, who, hornvale_history::OCC_ENDED);
    let coeval = match founded {
        None => 3,
        Some(f) if f > day => 0, // not yet founded
        Some(_) => match ended {
            Some(e) if e < day => 2, // already gone
            _ => 1,                  // alive at the event
        },
    };

    // 2: raider. The teller's own history, independent of this event.
    let raider = u8::from(raiders.contains(&who));

    // 3: same-cause. Did the teller die the way the subject did?
    let same_cause = match (
        led.value_of(who, hornvale_history::OCC_CAUSE),
        led.value_of(subject, hornvale_history::OCC_CAUSE),
    ) {
        (Some(a), Some(b)) if a == b => 0,
        (Some(_), Some(_)) => 1,
        _ => 2,
    };

    // 4: same-function.
    let same_fn = match (
        led.value_of(who, hornvale_history::OCC_FUNCTION),
        led.value_of(subject, hornvale_history::OCC_FUNCTION),
    ) {
        (Some(a), Some(b)) if a == b => 0,
        (Some(_), Some(_)) => 1,
        _ => 2,
    };

    [s, coeval, raider, same_cause, same_fn]
}

/// SUBSTRATE ONLY: the scale of the amplitude actually being frozen — the
/// number of GENERATIONS a single retelling spans, and whether a lifespan rung
/// exists to cap the ladder.
///
/// This is deliberately a different quantity from the one measured in
/// `does_generation_length_vary_by_people_on_seed_42`, which used distance from
/// the EVENT. The frozen rule uses the gap between TELLER and HEARER, because
/// "a story handed across three generations blurs more than one handed across
/// half a generation" is a claim about date precision, where distance from the
/// event is really a claim about attitude. Measured before freezing, per the
/// rule this campaign broke once already.
///
/// claim: structural(seed: 42) — false-positive seed-loop flag.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn how_many_generations_does_one_retelling_span_on_seed_42() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let led = &world.ledger;
    let lin = lineage_of(led);
    let ladder = hornvale_hearsay::ladder::PrecisionLadder::of(led);
    let year_days = ladder
        .labels()
        .iter()
        .position(|l| *l == "year")
        .and_then(|i| ladder.span(hornvale_kernel::Precision(i as u8)))
        .map(|s| s.get())
        .expect("seed 42 has a year rung");

    // Per-occupation generation AND lifespan, in std days.
    let mut gen_days: BTreeMap<EntityId, f64> = BTreeMap::new();
    let mut life_days: BTreeMap<String, f64> = BTreeMap::new();
    for occ in lin.all() {
        let Some(Value::Text(people)) = led.value_of(occ, hornvale_history::OCC_PEOPLE) else {
            continue;
        };
        let Some(wc) = hornvale_worldgen::WorldComponents::assemble().ok() else {
            continue;
        };
        let Some(bio) = wc.biosphere.get_by_label(people) else {
            continue;
        };
        let lh = hornvale_species::life_history(bio.mass, bio.metabolic_class, bio.schedule);
        if let Some(g) = lh.generation_length {
            gen_days.insert(occ, g.get() * year_days);
        }
        if let Some(l) = lh.lifespan {
            life_days.insert(people.clone(), l.get() * year_days);
        }
    }

    println!("\n=== LIFESPAN RUNG AVAILABILITY ===");
    let mut ls: Vec<(&String, &f64)> = life_days.iter().collect();
    ls.sort_by(|a, b| a.1.total_cmp(b.1));
    for (p, d) in ls.iter().take(3).chain(ls.iter().rev().take(2)) {
        println!(
            "  {p:<14} lifespan = {:.1} std days ({:.1} yr)",
            d,
            *d / year_days
        );
    }
    println!("  peoples with a lifespan: {} of {}", life_days.len(), {
        let mut s: BTreeSet<&String> = BTreeSet::new();
        for occ in lin.all() {
            if let Some(Value::Text(p)) = led.value_of(occ, hornvale_history::OCC_PEOPLE) {
                s.insert(p);
            }
        }
        s.len()
    });

    // The frozen amplitude: |founded(hearer) - founded(teller)| in the
    // TELLER's generations, over every real transmission step.
    let mut events: Vec<(EntityId, f64)> = Vec::new();
    for f in led.find(hornvale_history::OCC_ENDED) {
        if let Value::Number(day) = &f.object {
            events.push((f.subject, *day));
        }
    }
    events.sort_by_key(|a| a.0);

    let mut amp: Vec<f64> = Vec::new();
    let mut path_totals_add: Vec<f64> = Vec::new();
    let mut path_totals_quad: Vec<f64> = Vec::new();
    for (subject, _day) in &events {
        let witnesses = witnesses_of(led, &lin, *subject, hornvale_history::OCC_ENDED);
        let wset: BTreeSet<EntityId> = witnesses.iter().copied().collect();
        for w in &witnesses {
            for d in lin.descendants_of(*w) {
                if wset.contains(&d) {
                    continue;
                }
                let ancestry = lin.ancestry(d);
                let Some(pos) = ancestry.iter().position(|a| a == w) else {
                    continue;
                };
                let mut path: Vec<EntityId> = ancestry[..=pos].to_vec();
                path.reverse();
                let (mut sum, mut sumsq) = (0.0f64, 0.0f64);
                for pair in path.windows(2) {
                    let (Some(ft), Some(fh)) = (
                        number(led, pair[0], hornvale_history::OCC_FOUNDED),
                        number(led, pair[1], hornvale_history::OCC_FOUNDED),
                    ) else {
                        continue;
                    };
                    let Some(g) = gen_days.get(&pair[0]) else {
                        continue;
                    };
                    let a = (fh - ft).abs() / g;
                    amp.push(a);
                    sum += a;
                    sumsq += a * a;
                }
                path_totals_add.push(sum);
                path_totals_quad.push(sumsq.sqrt());
            }
        }
    }

    let q = |v: &mut Vec<f64>, p: f64| -> f64 {
        v.sort_by(f64::total_cmp);
        if v.is_empty() {
            return f64::NAN;
        }
        v[((v.len() - 1) as f64 * p) as usize]
    };
    println!("\n=== AMPLITUDE: generations spanned by ONE retelling ===");
    println!("steps: {}", amp.len());
    println!(
        "  p10={:.3} p50={:.3} p75={:.3} p90={:.3} p99={:.3} max={:.3}",
        q(&mut amp, 0.10),
        q(&mut amp, 0.50),
        q(&mut amp, 0.75),
        q(&mut amp, 0.90),
        q(&mut amp, 0.99),
        amp.last().copied().unwrap_or(f64::NAN)
    );
    println!("\n=== ACCUMULATED OVER A WHOLE PATH (both candidate rules) ===");
    println!(
        "  additive    p50={:.2} p90={:.2} max={:.2}",
        q(&mut path_totals_add, 0.50),
        q(&mut path_totals_add, 0.90),
        path_totals_add.last().copied().unwrap_or(f64::NAN)
    );
    println!(
        "  quadrature  p50={:.2} p90={:.2} max={:.2}",
        q(&mut path_totals_quad, 0.50),
        q(&mut path_totals_quad, 0.90),
        path_totals_quad.last().copied().unwrap_or(f64::NAN)
    );

    assert!(!amp.is_empty(), "control: amplitude must be measurable");
    assert!(
        !life_days.is_empty(),
        "control: a lifespan rung must be derivable for at least one people"
    );
}

/// SUBSTRATE ONLY: does generation length actually vary by people on seed 42,
/// and does re-expressing the amplitude in GENERATIONS bring it onto the same
/// scale as a ladder?
///
/// The days-scale amplitude measured above is ~25 years at the median against a
/// ladder topping out at one year — two orders of magnitude apart. A generation
/// is the candidate unit because it is world-derived (allometric, from body
/// mass and metabolic class) AND people-varying, so the same gap in days is a
/// different number of generations for a long-lived people than a short-lived
/// one. This reports the input scale; it computes no rung and no outcome.
///
/// claim: structural(seed: 42) — false-positive seed-loop flag.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn does_generation_length_vary_by_people_on_seed_42() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let led = &world.ledger;
    let lin = lineage_of(led);
    let ladder = hornvale_hearsay::ladder::PrecisionLadder::of(led);

    // The world's year in std days, taken from the ladder's own year rung so
    // this probe and the ladder cannot disagree about what a year is.
    let year_days = ladder
        .labels()
        .iter()
        .position(|l| *l == "year")
        .and_then(|i| ladder.span(hornvale_kernel::Precision(i as u8)))
        .map(|s| s.get())
        .expect("seed 42 has a year rung");

    // people -> (occupation count, generation length in years)
    let mut peoples: BTreeMap<String, (u64, Option<f64>)> = BTreeMap::new();
    let mut gen_days_of: BTreeMap<EntityId, f64> = BTreeMap::new();
    for occ in lin.all() {
        let Some(Value::Text(people)) = led.value_of(occ, hornvale_history::OCC_PEOPLE) else {
            continue;
        };
        // Replicates the body of the (private) worldgen
        // `descent::generation_length_of`. If this design ships, that module
        // gets made public rather than this being copied into the crate.
        let gl = hornvale_worldgen::WorldComponents::assemble()
            .ok()
            .and_then(|wc| {
                wc.biosphere.get_by_label(people).map(|bio| {
                    hornvale_species::life_history(bio.mass, bio.metabolic_class, bio.schedule)
                        .generation_length
                })
            })
            .flatten()
            .map(|y| y.get());
        let entry = peoples.entry(people.clone()).or_insert((0, gl));
        entry.0 += 1;
        if let Some(g) = gl {
            gen_days_of.insert(occ, g * year_days);
        }
    }

    println!("\n=== GENERATION LENGTH BY PEOPLE (seed 42) ===");
    println!("year = {year_days:.3} std days");
    let mut lens: Vec<f64> = Vec::new();
    for (people, (count, gl)) in &peoples {
        match gl {
            Some(g) => {
                println!("  {people:<14} n={count:<5} generation = {g:.2} years");
                lens.push(*g);
            }
            None => println!("  {people:<14} n={count:<5} generation = NONE"),
        }
    }
    lens.sort_by(f64::total_cmp);
    if lens.len() >= 2 {
        println!(
            "  spread: min={:.2}  max={:.2}  ratio={:.2}x",
            lens[0],
            lens[lens.len() - 1],
            lens[lens.len() - 1] / lens[0]
        );
    }

    // The amplitude again, now divided by the TELLER's own generation length.
    let mut events: Vec<(EntityId, f64)> = Vec::new();
    for f in led.find(hornvale_history::OCC_ENDED) {
        if let Value::Number(day) = &f.object {
            events.push((f.subject, *day));
        }
    }
    events.sort_by_key(|a| a.0);

    let mut gens: Vec<f64> = Vec::new();
    for (subject, day) in &events {
        let witnesses = witnesses_of(led, &lin, *subject, hornvale_history::OCC_ENDED);
        let wset: BTreeSet<EntityId> = witnesses.iter().copied().collect();
        for w in &witnesses {
            for d in lin.descendants_of(*w) {
                if wset.contains(&d) {
                    continue;
                }
                let ancestry = lin.ancestry(d);
                let Some(pos) = ancestry.iter().position(|a| a == w) else {
                    continue;
                };
                let mut path: Vec<EntityId> = ancestry[..=pos].to_vec();
                path.reverse();
                for pair in path.windows(2) {
                    let (Some(a), Some(b)) = (
                        remove_from(led, pair[0], *day),
                        remove_from(led, pair[1], *day),
                    ) else {
                        continue;
                    };
                    if let Some(g) = gen_days_of.get(&pair[0]) {
                        gens.push((a - b).abs() / g);
                    }
                }
            }
        }
    }
    gens.sort_by(f64::total_cmp);
    let q = |v: &Vec<f64>, p: f64| -> f64 {
        if v.is_empty() {
            return f64::NAN;
        }
        v[((v.len() - 1) as f64 * p) as usize]
    };
    println!("\n=== AMPLITUDE IN GENERATIONS (teller's own people) ===");
    println!("steps: {}", gens.len());
    println!(
        "quantiles  p50={:.3}  p75={:.3}  p90={:.3}  p99={:.3}  max={:.3}",
        q(&gens, 0.50),
        q(&gens, 0.75),
        q(&gens, 0.90),
        q(&gens, 0.99),
        gens.last().copied().unwrap_or(f64::NAN)
    );

    assert!(
        peoples.len() >= 2,
        "control: seed 42 should carry several peoples"
    );
}

/// How far `who` stands from the event day, in std days.
///
/// Zero when the teller's own lifespan contains the day — it was there.
/// Otherwise the distance from the day to the nearer end of that lifespan.
/// This is the METRIC version of the `coeval` label above: the only measured
/// axis that carries a magnitude rather than a category, which is what an
/// amplitude-scaled damage rule needs.
fn remove_from(led: &Ledger, who: EntityId, day: f64) -> Option<f64> {
    let founded = number(led, who, hornvale_history::OCC_FOUNDED)?;
    let ended = number(led, who, hornvale_history::OCC_ENDED);
    if founded > day {
        return Some(founded - day);
    }
    match ended {
        Some(e) if e < day => Some(day - e),
        _ => Some(0.0),
    }
}

/// SUBSTRATE ONLY: the distribution of the amplitude the frozen rule will
/// consume, plus this world's ladder spans. Deliberately reports NOTHING about
/// resulting rungs — that is the readout's job, and computing it here would let
/// the rule be tuned against its own outcome before the hypothesis is frozen.
///
/// claim: structural(seed: 42) — false-positive seed-loop flag.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn what_scale_is_the_teller_remove_amplitude_on_seed_42() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let led = &world.ledger;
    let lin = lineage_of(led);
    let ladder = hornvale_hearsay::ladder::PrecisionLadder::of(led);

    println!("\n=== LADDER (seed 42) ===");
    for (i, label) in ladder.labels().iter().enumerate() {
        let span = ladder
            .span(hornvale_kernel::Precision(i as u8))
            .map(|s| s.get())
            .unwrap_or(f64::NAN);
        println!("  rung {i}: {label:<16} span = {span:.3} std days");
    }

    let mut events: Vec<(EntityId, f64)> = Vec::new();
    for f in led.find(hornvale_history::OCC_ENDED) {
        if let Value::Number(day) = &f.object {
            events.push((f.subject, *day));
        }
    }
    events.sort_by_key(|a| a.0);

    let mut deltas: Vec<f64> = Vec::new();
    let mut path_len: Vec<usize> = Vec::new();
    for (subject, day) in &events {
        let witnesses = witnesses_of(led, &lin, *subject, hornvale_history::OCC_ENDED);
        let wset: BTreeSet<EntityId> = witnesses.iter().copied().collect();
        for w in &witnesses {
            for d in lin.descendants_of(*w) {
                if wset.contains(&d) {
                    continue;
                }
                let ancestry = lin.ancestry(d);
                let Some(pos) = ancestry.iter().position(|a| a == w) else {
                    continue;
                };
                let mut path: Vec<EntityId> = ancestry[..=pos].to_vec();
                path.reverse();
                path_len.push(path.len() - 1);
                for pair in path.windows(2) {
                    let (rt, rh) = (
                        remove_from(led, pair[0], *day),
                        remove_from(led, pair[1], *day),
                    );
                    if let (Some(a), Some(b)) = (rt, rh) {
                        deltas.push((a - b).abs());
                    }
                }
            }
        }
    }

    deltas.sort_by(f64::total_cmp);
    path_len.sort_unstable();
    let zeros = deltas.iter().filter(|d| **d == 0.0).count();
    let q = |p: f64| -> f64 {
        if deltas.is_empty() {
            return f64::NAN;
        }
        deltas[((deltas.len() - 1) as f64 * p) as usize]
    };

    println!("\n=== AMPLITUDE |remove(teller) - remove(hearer)|, std days ===");
    println!("steps           : {}", deltas.len());
    println!(
        "exactly zero    : {} ({:.4}) <- steps the rule would NOT fire on",
        zeros,
        zeros as f64 / deltas.len().max(1) as f64
    );
    println!(
        "quantiles  p50={:.2}  p75={:.2}  p90={:.2}  p99={:.2}  max={:.2}",
        q(0.50),
        q(0.75),
        q(0.90),
        q(0.99),
        deltas.last().copied().unwrap_or(f64::NAN)
    );
    println!(
        "path length (steps)  p50={}  p90={}  max={}",
        path_len[path_len.len() / 2],
        path_len[path_len.len() * 9 / 10],
        path_len.last().copied().unwrap_or(0)
    );

    // Substrate control: the amplitude must not be inert the way same-function
    // was (0.0000). This asserts the INPUT varies, never that any outcome is
    // interesting.
    assert!(
        zeros < deltas.len(),
        "control: amplitude is inert — every step has zero remove-difference"
    );
    assert!(
        ladder.len() >= 2,
        "control: seed 42 should carry a ladder with at least two rungs"
    );
}

/// claim: structural(seed: 42) — false-positive seed-loop flag; the loops bind
/// occupation ids and relation indices, not seeds.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn which_teller_event_relations_flip_more_than_once_on_seed_42() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let led = &world.ledger;
    let lin = lineage_of(led);

    let mut raiders: BTreeSet<EntityId> = BTreeSet::new();
    for f in led.find(hornvale_history::OCC_ENDED_BY) {
        if let Value::Entity(a) = &f.object {
            raiders.insert(*a);
        }
    }

    // Every ending is an event a claim can be about.
    let mut events: Vec<(EntityId, f64)> = Vec::new();
    for f in led.find(hornvale_history::OCC_ENDED) {
        if let Value::Number(day) = &f.object {
            events.push((f.subject, *day));
        }
    }
    events.sort_by_key(|a| a.0);

    // Per relation: histogram of flip counts over transmission paths, and the
    // count of carrying (edge x event) pairs on which it fires.
    let mut flip_hist: [BTreeMap<u32, u64>; 5] = Default::default();
    let mut fires: [u64; 5] = [0; 5];
    let mut carrying_pairs: u64 = 0;
    let mut paths: u64 = 0;
    // What `variants_about` actually RETAINS: per (event, holder), the
    // least-corrupted path, i.e. the MINIMUM flip count across every path
    // reaching that holder. A relation can flip fifteen times somewhere and
    // still be invisible if every holder has a cleaner route.
    let mut held_hist: [BTreeMap<u32, u64>; 5] = Default::default();

    for (subject, day) in &events {
        let witnesses = witnesses_of(led, &lin, *subject, hornvale_history::OCC_ENDED);
        let wset: BTreeSet<EntityId> = witnesses.iter().copied().collect();
        // Label memo for this event, so a node on many paths is labelled once.
        let mut memo: BTreeMap<EntityId, [u8; 5]> = BTreeMap::new();
        // Best (fewest-flip) route to each holder, per relation.
        let mut best: BTreeMap<EntityId, [u32; 5]> = BTreeMap::new();

        for w in &witnesses {
            for d in lin.descendants_of(*w) {
                if wset.contains(&d) {
                    continue; // a witness is never demoted to an inheritor
                }
                let ancestry = lin.ancestry(d);
                let Some(pos) = ancestry.iter().position(|a| a == w) else {
                    continue;
                };
                let mut path: Vec<EntityId> = ancestry[..=pos].to_vec();
                path.reverse();

                paths += 1;
                let mut flips = [0u32; 5];
                for pair in path.windows(2) {
                    let (teller, hearer) = (pair[0], pair[1]);
                    carrying_pairs += 1;
                    let lt = *memo
                        .entry(teller)
                        .or_insert_with(|| labels(led, &lin, &raiders, *subject, *day, teller));
                    let lh = *memo
                        .entry(hearer)
                        .or_insert_with(|| labels(led, &lin, &raiders, *subject, *day, hearer));
                    for r in 0..5 {
                        if lt[r] != lh[r] {
                            flips[r] += 1;
                            fires[r] += 1;
                        }
                    }
                }
                for r in 0..5 {
                    *flip_hist[r].entry(flips[r]).or_insert(0) += 1;
                }
                let slot = best.entry(d).or_insert([u32::MAX; 5]);
                for r in 0..5 {
                    slot[r] = slot[r].min(flips[r]);
                }
            }
        }

        for held in best.values() {
            for r in 0..5 {
                *held_hist[r].entry(held[r]).or_insert(0) += 1;
            }
        }
    }

    println!("\n=== SUBSTRATE: seed 42 ===");
    println!("endings (events)          : {}", events.len());
    println!("raiders                   : {}", raiders.len());
    println!("transmission paths         : {paths}");
    println!("carrying (step x event)    : {carrying_pairs}");

    println!("\n=== FLIP CEILING PER RELATION ===");
    println!("a relation whose max flips is 1 has campaign 2's ceiling and");
    println!("cannot compound, however often it fires.\n");
    for r in 0..5 {
        let hist = &flip_hist[r];
        let max = hist.keys().copied().max().unwrap_or(0);
        let nonzero: u64 = hist.iter().filter(|(k, _)| **k > 0).map(|(_, v)| *v).sum();
        let total: u64 = hist.values().sum();
        let rate = if carrying_pairs == 0 {
            0.0
        } else {
            fires[r] as f64 / carrying_pairs as f64
        };
        println!("{}", RELATIONS[r]);
        println!(
            "  fires on {:>8} of {} steps ({:.4})   max flips/path = {}   paths with >=1 flip = {} of {}",
            fires[r], carrying_pairs, rate, max, nonzero, total
        );
        let mut shown: Vec<String> = Vec::new();
        for (k, v) in hist.iter().take(8) {
            shown.push(format!("{k}:{v}"));
        }
        println!("  flip histogram {{{}}}", shown.join(", "));

        let held = &held_hist[r];
        let held_max = held.keys().copied().max().unwrap_or(0);
        let held_total: u64 = held.values().sum();
        let held_lossy: u64 = held.iter().filter(|(k, _)| **k > 0).map(|(_, v)| *v).sum();
        let mut hshown: Vec<String> = Vec::new();
        for (k, v) in held.iter().take(8) {
            hshown.push(format!("{k}:{v}"));
        }
        println!(
            "  RETAINED (min over paths, what variants_about keeps): max = {held_max}, \
             {held_lossy} of {held_total} held claims lossy"
        );
        println!("  retained histogram {{{}}}\n", hshown.join(", "));
    }

    // POSITIVE CONTROLS on the substrate, not on any outcome. A zero flip
    // ceiling elsewhere means nothing unless these two reproduce.
    assert!(
        events.len() > 400,
        "control: seed 42 should carry >400 endings, got {}",
        events.len()
    );
    // Campaign 2's published ceiling is a claim about HELD claims, which are
    // the least-corrupted route per holder — NOT about every path that exists.
    // An earlier version of this control asserted the all-paths maximum was 1
    // and went red at 2: 209 paths cross stance twice, because the attacker can
    // sit INSIDE a witness's subtree, making `Bystander -> Perpetrator`
    // reachable after all. That is a correction to this probe's reasoning, not
    // to campaign 2's number, and the two measure different objects.
    let stance_retained_max = held_hist[0].keys().copied().max().unwrap_or(0);
    assert_eq!(
        stance_retained_max, 1,
        "control: campaign 2's one-rung ceiling on HELD claims should reproduce"
    );
    let stance_path_max = flip_hist[0].keys().copied().max().unwrap_or(0);
    assert!(
        stance_path_max >= 2,
        "control: some path should cross stance twice; got {stance_path_max}. \
         If this drops to 1 the retention finding below is vacuous."
    );
}
