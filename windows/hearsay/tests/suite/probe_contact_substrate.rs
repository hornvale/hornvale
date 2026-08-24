//! Substrate probe (The Parley, Myth campaign 4): how much cross-people
//! contact does the ledger actually carry, and what does the transmission
//! graph do with it?
//!
//! Campaigns 1-3 all wrote that the world is short of the contact that would
//! let accounts diverge across peoples. The Palimpsest's §7 corrected that to
//! a claim about the MODEL: `variants_about` and its accumulating sibling
//! only ever step parent->child down the founding tree, and descent never
//! crosses a people boundary. Before freezing any hypothesis about a
//! cross-people edge, this probe establishes the facts the design depends on,
//! none of which any campaign has measured: how wide the raid seam is (S1),
//! whether the far side has a line to tell anything to (S2), how far an
//! account reaches today and whether the two sides of a raid already disagree
//! (S3/S3b/S4), whether transmission already runs backwards in time (S5), what
//! reach a contact edge would actually buy (S6), and whether the shipped
//! stance rule already degrades the raider's account faster than the victim's
//! (S7).
//!
//! Reports only. Every assertion here is a POSITIVE CONTROL on the substrate
//! — reproducing a published count, or proving the probe can see the thing it
//! reports zero of — so that a zero can be told apart from a broken probe.
//! No assertion here is about an outcome.

use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::accumulate::Accumulation;
use hornvale_hearsay::contact::contact_of;
use hornvale_hearsay::derive::{variants_about_accumulating, witnesses_of};
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_hearsay::ladder::{PeopleLadders, PrecisionLadder};
use hornvale_hearsay::lineage::lineage_of;
use hornvale_hearsay::transmission::{Transmission, Walk};
use hornvale_kernel::Precision;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use std::collections::{BTreeMap, BTreeSet};

/// A 12-seed panel, a strict prefix of the census panel (`the-census` runs
/// seeds 0-999) and of The Palimpsest's 40-seed readout panel, so nothing
/// here samples a population the campaign's own readout will not.
///
/// Twelve rather than forty deliberately: this is a SUBSTRATE probe whose job
/// is to decide whether a mechanism is worth modelling, and the readout panel
/// is chosen later against a measured per-seed cost.
const PANEL: [u64; 12] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];

fn text(led: &Ledger, occ: EntityId, pred: &str) -> Option<String> {
    match led.value_of(occ, pred) {
        Some(Value::Text(t)) => Some(t.clone()),
        _ => None,
    }
}

fn number(led: &Ledger, occ: EntityId, pred: &str) -> Option<f64> {
    match led.value_of(occ, pred) {
        Some(Value::Number(n)) => Some(*n),
        _ => None,
    }
}

fn entity(led: &Ledger, occ: EntityId, pred: &str) -> Option<EntityId> {
    match led.value_of(occ, pred) {
        Some(Value::Entity(e)) => Some(*e),
        _ => None,
    }
}

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

/// Every occupation that ended, with its subject people and (when named and
/// Entity-valued) its attacker and the attacker's people.
struct Ending {
    subject: EntityId,
    people: String,
    day: f64,
    attacker: Option<EntityId>,
    attacker_people: Option<String>,
}

fn endings_of(led: &Ledger) -> Vec<Ending> {
    let mut out = Vec::new();
    for fact in led.find(hornvale_history::OCC_ENDED) {
        let subject = fact.subject;
        let Some(day) = number(led, subject, hornvale_history::OCC_ENDED) else {
            continue;
        };
        let people = text(led, subject, hornvale_history::OCC_PEOPLE).unwrap_or_default();
        let attacker = entity(led, subject, hornvale_history::OCC_ENDED_BY);
        let attacker_people = attacker.and_then(|a| text(led, a, hornvale_history::OCC_PEOPLE));
        out.push(Ending {
            subject,
            people,
            day,
            attacker,
            attacker_people,
        });
    }
    out.sort_by_key(|e| e.subject);
    out
}

fn median(mut v: Vec<f64>) -> f64 {
    if v.is_empty() {
        return f64::NAN;
    }
    v.sort_by(f64::total_cmp);
    v[v.len() / 2]
}

/// S1 + S2: how wide is the raid seam, and does the far side have a line to
/// tell anything to?
///
/// claim: structural(seed: panel) — false-positive seed-loop flag; the loop
/// binds a census-panel prefix, not a search over seeds.
#[test]
#[ignore = "probe: contact-substrate instrument over a live-worldgen battery; run by hand (decision 0148 took it off the heavy set)"]
fn the_raid_seam_and_the_far_sides_line() {
    println!("\n============ S1/S2: THE RAID SEAM ============");
    println!(
        "{:<6} {:>8} {:>8} {:>8} {:>9} {:>10} {:>10}",
        "seed", "endings", "named", "foreign", "foreign%", "atk-desc-med", "atk-desc-max"
    );

    let mut total_endings = 0usize;
    let mut total_named = 0usize;
    let mut total_foreign = 0usize;
    let mut all_attacker_descendants: Vec<f64> = Vec::new();
    let mut attackers_that_are_leaves = 0usize;
    let mut foreign_attackers_seen = 0usize;
    let mut seed42_foreign: Option<(usize, usize)> = None;

    for seed in PANEL.iter().copied().chain(std::iter::once(42)) {
        let world = build(seed);
        let led = &world.ledger;
        let lin = lineage_of(led);
        let endings = endings_of(led);

        let named = endings.iter().filter(|e| e.attacker.is_some()).count();
        let foreign: Vec<&Ending> = endings
            .iter()
            .filter(|e| {
                e.attacker_people
                    .as_ref()
                    .is_some_and(|ap| !ap.is_empty() && !e.people.is_empty() && *ap != e.people)
            })
            .collect();

        let mut desc_counts: Vec<f64> = Vec::new();
        for e in &foreign {
            let a = e.attacker.expect("foreign implies a named attacker");
            let n = lin.descendants_of(a).len();
            desc_counts.push(n as f64);
            if n == 0 {
                attackers_that_are_leaves += 1;
            }
            foreign_attackers_seen += 1;
        }
        all_attacker_descendants.extend(desc_counts.iter().copied());

        if seed == 42 {
            seed42_foreign = Some((foreign.len(), endings.len()));
        } else {
            total_endings += endings.len();
            total_named += named;
            total_foreign += foreign.len();
        }

        println!(
            "{:<6} {:>8} {:>8} {:>8} {:>8.1}% {:>10.1} {:>10.0}",
            seed,
            endings.len(),
            named,
            foreign.len(),
            if endings.is_empty() {
                0.0
            } else {
                100.0 * foreign.len() as f64 / endings.len() as f64
            },
            median(desc_counts.clone()),
            desc_counts.iter().cloned().fold(0.0, f64::max),
        );
    }

    println!("\npanel totals (seed 42 excluded, reported separately):");
    println!("  endings                 : {total_endings}");
    println!("  with a named attacker   : {total_named}");
    println!("  attacker of another people: {total_foreign}");
    println!(
        "  foreign share of endings: {:.2}%",
        100.0 * total_foreign as f64 / total_endings.max(1) as f64
    );
    println!(
        "  foreign attackers with NO descendants (leaves): {attackers_that_are_leaves} of \
         {foreign_attackers_seen}"
    );
    println!(
        "  attacker descendant count: median {:.1}, max {:.0}",
        median(all_attacker_descendants.clone()),
        all_attacker_descendants.iter().cloned().fold(0.0, f64::max),
    );

    let (f42, e42) = seed42_foreign.expect("seed 42 built");
    println!("\nseed 42 (campaign 2 published 17 of 474): {f42} of {e42}");

    // POSITIVE CONTROL. Not an outcome assertion: it proves the probe can see
    // a foreign attacker at all, so a zero elsewhere is data and not a bug.
    // Deliberately NOT `== 17`: main has moved since campaign 2 measured it
    // (campaign 2 itself reported its edge count moving 658 -> 780), so an
    // exact pin would fail for a reason that has nothing to do with contact.
    assert!(
        f42 > 0,
        "control: seed 42 must carry at least one cross-people attacker; \
         campaign 2 published 17 of 474"
    );
    assert!(
        total_endings > 0,
        "control: the panel must carry endings at all"
    );
}

/// S3 + S4: how far does one ending's account actually reach today, and do the
/// two sides of a raid already disagree?
///
/// claim: structural(seed: panel) — false-positive seed-loop flag; the loop
/// binds a census-panel prefix, not a search over seeds.
#[test]
#[ignore = "probe: contact-substrate instrument over a live-worldgen battery; run by hand (decision 0148 took it off the heavy set)"]
fn how_far_an_account_reaches_and_whether_the_sides_disagree() {
    println!("\n======== S3/S4: REACH, AND DISAGREEMENT AT THE SEAM ========");

    let components = hornvale_worldgen::WorldComponents::assemble().expect("components assemble");
    let mut peoples_per_claim: BTreeMap<usize, usize> = BTreeMap::new();
    let mut foreign_events = 0usize;
    let mut foreign_events_with_two_peoples = 0usize;
    let mut foreign_events_where_sides_differ = 0usize;
    let mut domestic_events = 0usize;
    let mut domestic_events_with_two_peoples = 0usize;

    for seed in PANEL {
        let world = build(seed);
        let led = &world.ledger;
        let lin = lineage_of(led);
        let graph = contact_of(led);
        let walk = Walk {
            ledger: led,
            lineage: &lin,
            contact: &graph,
            policy: Transmission::AS_SHIPPED,
        };
        let Some((ladders, durations)) = read_world(led, &components) else {
            println!("seed {seed}: no ladder; skipped");
            continue;
        };

        for e in endings_of(led) {
            let held = variants_about_accumulating(
                &walk,
                &ladders,
                &durations,
                Accumulation::Multiplicative,
                e.subject,
                hornvale_history::OCC_ENDED,
            );
            if held.is_empty() {
                continue;
            }
            let peoples: BTreeSet<String> = held
                .iter()
                .filter_map(|c| text(led, c.holder, hornvale_history::OCC_PEOPLE))
                .collect();
            *peoples_per_claim.entry(peoples.len()).or_default() += 1;

            let is_foreign = e
                .attacker_people
                .as_ref()
                .is_some_and(|ap| !ap.is_empty() && !e.people.is_empty() && *ap != e.people);
            if is_foreign {
                foreign_events += 1;
                if peoples.len() > 1 {
                    foreign_events_with_two_peoples += 1;
                }
                // Do the two sides hold DIFFERENT remembered days?
                let mut by_people: BTreeMap<String, BTreeSet<u64>> = BTreeMap::new();
                for c in &held {
                    let Some(p) = text(led, c.holder, hornvale_history::OCC_PEOPLE) else {
                        continue;
                    };
                    if let Value::Number(day) = &c.object {
                        by_people.entry(p).or_default().insert(day.to_bits());
                    }
                }
                let victim = by_people.get(&e.people);
                let raider = e.attacker_people.as_ref().and_then(|ap| by_people.get(ap));
                if let (Some(v), Some(r)) = (victim, raider)
                    && v != r
                {
                    foreign_events_where_sides_differ += 1;
                }
            } else {
                domestic_events += 1;
                if peoples.len() > 1 {
                    domestic_events_with_two_peoples += 1;
                }
            }
        }
    }

    println!("\nHOW MANY PEOPLES HOLD ONE ENDING'S ACCOUNT (all endings, all panel seeds):");
    let total: usize = peoples_per_claim.values().sum();
    for (n, count) in &peoples_per_claim {
        println!(
            "  {n} people(s): {count:>6}  ({:>5.2}%)",
            100.0 * *count as f64 / total.max(1) as f64
        );
    }

    println!("\nAT THE SEAM:");
    println!("  endings with a foreign attacker      : {foreign_events}");
    println!("    ... whose account reaches 2+ peoples: {foreign_events_with_two_peoples}");
    println!(
        "    ... where the two sides remember DIFFERENT days: {foreign_events_where_sides_differ}"
    );
    println!("  endings with no foreign attacker     : {domestic_events}");
    println!("    ... whose account reaches 2+ peoples: {domestic_events_with_two_peoples}");

    // POSITIVE CONTROL: the probe must have measured some accounts at all,
    // otherwise every zero above is vacuous.
    assert!(total > 0, "control: the panel must carry held accounts");
}

/// S5: does transmission already run backwards in time?
///
/// The accumulator's `gen_span` takes `(fh - ft).abs()`, which is total by
/// construction and therefore silent about ordering. A contact edge that
/// ignores the clock could deliver a claim to a community that no longer
/// exists, or about an event that has not happened. This measures whether the
/// EXISTING descent walk already does so, which decides whether the campaign
/// must add a clock or merely must not remove one.
///
/// claim: structural(seed: panel) — false-positive seed-loop flag; the loop
/// binds a census-panel prefix, not a search over seeds.
#[test]
#[ignore = "probe: contact-substrate instrument over a live-worldgen battery; run by hand (decision 0148 took it off the heavy set)"]
fn whether_transmission_already_runs_backwards() {
    println!("\n============ S5: THE CLOCK ============");

    let mut steps = 0usize;
    let mut steps_backwards = 0usize;
    let mut holders = 0usize;
    let mut holders_founded_before_the_event = 0usize;
    let mut holders_ended_before_the_event = 0usize;

    for seed in PANEL {
        let world = build(seed);
        let led = &world.ledger;
        let lin = lineage_of(led);

        // Every founding edge is a transmission step in every model shipped.
        for child in lin.all() {
            let Some(parent) = lin.parent(child) else {
                continue;
            };
            let (Some(fc), Some(fp)) = (
                number(led, child, hornvale_history::OCC_FOUNDED),
                number(led, parent, hornvale_history::OCC_FOUNDED),
            ) else {
                continue;
            };
            steps += 1;
            if fc < fp {
                steps_backwards += 1;
            }
        }

        for e in endings_of(led) {
            for w in witnesses_of(led, &lin, e.subject, hornvale_history::OCC_ENDED) {
                for d in lin.descendants_of(w).into_iter().chain(std::iter::once(w)) {
                    holders += 1;
                    if let Some(f) = number(led, d, hornvale_history::OCC_FOUNDED)
                        && f > e.day
                    {
                        holders_founded_before_the_event += 1;
                    }
                    if let Some(x) = number(led, d, hornvale_history::OCC_ENDED)
                        && x < e.day
                    {
                        holders_ended_before_the_event += 1;
                    }
                }
            }
        }
    }

    println!("founding steps                       : {steps}");
    println!(
        "  ... where the CHILD predates the parent: {steps_backwards} ({:.3}%)",
        100.0 * steps_backwards as f64 / steps.max(1) as f64
    );
    println!("claim holders (witness + descendants): {holders}");
    println!(
        "  ... founded AFTER the event they hold : {holders_founded_before_the_event} ({:.2}%)",
        100.0 * holders_founded_before_the_event as f64 / holders.max(1) as f64
    );
    println!(
        "  ... ENDED BEFORE the event they hold  : {holders_ended_before_the_event} ({:.2}%)",
        100.0 * holders_ended_before_the_event as f64 / holders.max(1) as f64
    );

    // POSITIVE CONTROL: the walk must have produced steps and holders,
    // otherwise both percentages above are vacuous zeros.
    assert!(steps > 0, "control: the panel must carry founding edges");
    assert!(holders > 0, "control: the panel must carry claim holders");
}

/// Assemble the per-people ladders and durations the accumulator needs.
///
/// A near-copy of the readout battery's `read_world`, deliberately: a probe
/// that assembled the ladder differently from the battery whose model it is
/// probing would be measuring a different model. Returns `None` when the
/// world has no year rung, exactly as that battery skips such a seed.
fn read_world(
    led: &Ledger,
    components: &hornvale_worldgen::WorldComponents,
) -> Option<(PeopleLadders, PeopleDurations)> {
    let lineage = lineage_of(led);
    let astronomical = PrecisionLadder::of(led);
    let year_days = astronomical
        .labels()
        .iter()
        .position(|label| *label == "year")
        .and_then(|i| astronomical.span(Precision(i as u8)))
        .map(|span| span.get())?;

    let mut named: BTreeSet<String> = BTreeSet::new();
    for occ in lineage.all() {
        if let Some(Value::Text(people)) = led.value_of(occ, hornvale_history::OCC_PEOPLE) {
            named.insert(people.clone());
        }
    }

    let mut durations = PeopleDurations::default();
    for people in &named {
        let Some(bio) = components.biosphere.get_by_label(people) else {
            continue;
        };
        let life = hornvale_species::life_history(bio.mass, bio.thermal_strategy, bio.schedule);
        let to_days = |years: hornvale_kernel::Years| StdDays::new(years.get() * year_days).ok();
        durations.insert(
            people,
            life.generation_length.and_then(to_days),
            life.lifespan.and_then(to_days),
        );
    }

    let ladders = PeopleLadders::of(led, &durations);
    Some((ladders, durations))
}

/// S3b: is the seam's disagreement REAL, or an artifact of set cardinality?
///
/// `how_far_an_account_reaches_and_whether_the_sides_disagree` compares the
/// two peoples' sets of remembered days and finds them unequal on most
/// events. Set inequality is a weak instrument: a victim line with fifty
/// holders and a raider line with two would differ for reasons that have
/// nothing to do with the account drifting apart. This asks the sharper
/// question — does EACH side hold a day the other side holds nowhere? — and
/// reports the holder counts alongside, so a reader can see the cardinality
/// the weak measure was exposed to.
///
/// It also verifies S5's dead-holder count on a named instance rather than in
/// aggregate, because a 1.19% figure computed by the same loop that reports it
/// is not independently checked by anything.
///
/// claim: structural(seed: panel) — false-positive seed-loop flag; the loop
/// binds a census-panel prefix, not a search over seeds.
#[test]
#[ignore = "probe: contact-substrate instrument over a live-worldgen battery; run by hand (decision 0148 took it off the heavy set)"]
fn is_the_seams_disagreement_real() {
    println!("\n======== S3b: IS THE DISAGREEMENT REAL? ========");

    let components = hornvale_worldgen::WorldComponents::assemble().expect("components assemble");
    let mut events = 0usize;
    let mut mutually_exclusive = 0usize;
    let mut one_sided = 0usize;
    let mut identical = 0usize;
    let mut victim_holders: Vec<f64> = Vec::new();
    let mut raider_holders: Vec<f64> = Vec::new();
    let mut example: Option<String> = None;

    // The dead-holder cross-check, on named instances.
    let mut dead_examples: Vec<String> = Vec::new();

    for seed in PANEL {
        let world = build(seed);
        let led = &world.ledger;
        let lin = lineage_of(led);
        let graph = contact_of(led);
        let walk = Walk {
            ledger: led,
            lineage: &lin,
            contact: &graph,
            policy: Transmission::AS_SHIPPED,
        };
        let Some((ladders, durations)) = read_world(led, &components) else {
            continue;
        };

        for e in endings_of(led) {
            let Some(ap) = e.attacker_people.clone() else {
                continue;
            };
            if ap.is_empty() || e.people.is_empty() || ap == e.people {
                continue;
            }
            let held = variants_about_accumulating(
                &walk,
                &ladders,
                &durations,
                Accumulation::Multiplicative,
                e.subject,
                hornvale_history::OCC_ENDED,
            );
            if held.is_empty() {
                continue;
            }

            let mut by_people: BTreeMap<String, BTreeSet<u64>> = BTreeMap::new();
            let mut count: BTreeMap<String, usize> = BTreeMap::new();
            for c in &held {
                let Some(p) = text(led, c.holder, hornvale_history::OCC_PEOPLE) else {
                    continue;
                };
                *count.entry(p.clone()).or_default() += 1;
                if let Value::Number(day) = &c.object {
                    by_people.entry(p).or_default().insert(day.to_bits());
                }
                // Dead-holder cross-check: a holder that ended before the event.
                if dead_examples.len() < 5
                    && let Some(x) = number(led, c.holder, hornvale_history::OCC_ENDED)
                    && x < e.day
                {
                    dead_examples.push(format!(
                        "seed {seed}: holder {:?} ended day {:.1} but holds the ending of {:?} \
                         on day {:.1} (hops {}, {} days dead)",
                        c.holder,
                        x,
                        e.subject,
                        e.day,
                        c.hops,
                        e.day - x
                    ));
                }
            }

            let (Some(v), Some(r)) = (by_people.get(&e.people), by_people.get(&ap)) else {
                continue;
            };
            events += 1;
            victim_holders.push(*count.get(&e.people).unwrap_or(&0) as f64);
            raider_holders.push(*count.get(&ap).unwrap_or(&0) as f64);

            let v_only = v.difference(r).count();
            let r_only = r.difference(v).count();
            if v_only > 0 && r_only > 0 {
                mutually_exclusive += 1;
                if example.is_none() {
                    example = Some(format!(
                        "seed {seed}: {:?} ({} holders, {} distinct days) vs {ap} ({} holders, \
                         {} distinct days) — {v_only} days only the victim line holds, {r_only} \
                         only the raiders",
                        e.people,
                        count.get(&e.people).unwrap_or(&0),
                        v.len(),
                        count.get(&ap).unwrap_or(&0),
                        r.len(),
                    ));
                }
            } else if v_only > 0 || r_only > 0 {
                one_sided += 1;
            } else {
                identical += 1;
            }
        }
    }

    println!("cross-people endings compared      : {events}");
    println!(
        "  MUTUALLY EXCLUSIVE (each side holds a day the other never does): {mutually_exclusive}"
    );
    println!("  one-sided (one set strictly contains the other)                : {one_sided}");
    println!("  identical day sets                                             : {identical}");
    println!(
        "victim-line holders per event: median {:.1}",
        median(victim_holders.clone())
    );
    println!(
        "raider-line holders per event: median {:.1}",
        median(raider_holders.clone())
    );
    if let Some(x) = &example {
        println!("\nexample: {x}");
    }

    println!("\nDEAD HOLDERS (S5 cross-check, named instances):");
    for d in &dead_examples {
        println!("  {d}");
    }

    // POSITIVE CONTROLS. Both prove the probe reached the population it
    // reports on; neither asserts an outcome.
    assert!(
        events > 0,
        "control: the panel must carry cross-people endings"
    );
    assert!(
        !victim_holders.is_empty(),
        "control: victim-line holder counts must have been measured"
    );
}

/// S6: THE DECIDING MEASUREMENT. If contact edges are added to the walk, how
/// many peoples can one account actually reach?
///
/// The raid seam looks thin when counted as "endings whose ATTACKER is of
/// another people" — 2.33%. But that counts only boundary crossings at hop
/// ZERO. 47% of endings name an attacker at all, so the contact graph is
/// dense; a claim crossing a boundary two contacts down the chain is invisible
/// to the hop-0 count. This measures reach under three walks:
///
/// - `descent`  — what ships today: parent->child only.
/// - `contact`  — descent plus an undirected edge between a victim and its
///   named attacker, ungated. The CEILING, not a proposal.
/// - `gated`    — the same, but a step is refused unless the receiver was
///   still alive after the event (killing the 1.19% dead holders S5 found)
///   and, for a contact edge, unless the event predates the contact.
///
/// Reports only. The gate here is a PROBE's gate, chosen to bound the answer;
/// nothing about it is proposed as the model.
///
/// claim: structural(seed: panel) — false-positive seed-loop flag; the loop
/// binds a census-panel prefix, not a search over seeds.
#[test]
#[ignore = "probe: contact-substrate instrument over a live-worldgen battery; run by hand (decision 0148 took it off the heavy set)"]
fn reach_under_descent_contact_and_a_clock() {
    println!("\n======== S6: REACH UNDER THREE WALKS ========");

    let mut named_attacker_endings = 0usize;
    let mut all_endings = 0usize;
    // walk -> peoples-reached -> count of events
    let mut dist: [BTreeMap<usize, usize>; 3] = Default::default();
    let mut holders: [usize; 3] = [0; 3];
    let labels = ["descent", "contact", "gated"];

    for seed in PANEL {
        let world = build(seed);
        let led = &world.ledger;
        let lin = lineage_of(led);
        let endings = endings_of(led);
        all_endings += endings.len();

        // The contact graph, undirected, from every named attacker.
        let mut contact: BTreeMap<EntityId, Vec<(EntityId, f64)>> = BTreeMap::new();
        for e in &endings {
            if let Some(a) = e.attacker {
                named_attacker_endings += 1;
                contact.entry(e.subject).or_default().push((a, e.day));
                contact.entry(a).or_default().push((e.subject, e.day));
            }
        }

        let people_of = |occ: EntityId| text(led, occ, hornvale_history::OCC_PEOPLE);
        let alive_after = |occ: EntityId, day: f64| {
            number(led, occ, hornvale_history::OCC_ENDED).is_none_or(|x| x > day)
        };

        for e in &endings {
            for (walk, dst) in dist.iter_mut().enumerate() {
                let use_contact = walk >= 1;
                let gated = walk == 2;
                let mut seen: BTreeSet<EntityId> = BTreeSet::new();
                let mut frontier: Vec<EntityId> =
                    witnesses_of(led, &lin, e.subject, hornvale_history::OCC_ENDED);
                for w in &frontier {
                    seen.insert(*w);
                }
                while let Some(node) = frontier.pop() {
                    for child in lin.children_of(node) {
                        if gated && !alive_after(*child, e.day) {
                            continue;
                        }
                        if seen.insert(*child) {
                            frontier.push(*child);
                        }
                    }
                    if use_contact && let Some(peers) = contact.get(&node) {
                        for (peer, contact_day) in peers {
                            if gated && (*contact_day < e.day || !alive_after(*peer, e.day)) {
                                continue;
                            }
                            if seen.insert(*peer) {
                                frontier.push(*peer);
                            }
                        }
                    }
                }
                holders[walk] += seen.len();
                let peoples: BTreeSet<String> = seen.iter().filter_map(|h| people_of(*h)).collect();
                *dst.entry(peoples.len()).or_default() += 1;
            }
        }
    }

    println!("endings                    : {all_endings}");
    println!(
        "  ... naming an attacker   : {named_attacker_endings} ({:.1}%)",
        100.0 * named_attacker_endings as f64 / all_endings.max(1) as f64
    );

    for (walk, label) in labels.iter().enumerate() {
        let d = &dist[walk];
        let total: usize = d.values().sum();
        let multi: usize = d.iter().filter(|(n, _)| **n > 1).map(|(_, c)| *c).sum();
        let three_plus: usize = d.iter().filter(|(n, _)| **n > 2).map(|(_, c)| *c).sum();
        let max = d.keys().last().copied().unwrap_or(0);
        println!("\n{label}:");
        println!(
            "  reaches 2+ peoples : {multi} of {total} ({:.2}%)",
            100.0 * multi as f64 / total.max(1) as f64
        );
        println!(
            "  reaches 3+ peoples : {three_plus} ({:.2}%)",
            100.0 * three_plus as f64 / total.max(1) as f64
        );
        println!("  most peoples reached by one account: {max}");
        println!("  total holders across all accounts  : {}", holders[walk]);
        let shown: Vec<String> = d.iter().take(8).map(|(n, c)| format!("{n}:{c}")).collect();
        println!("  peoples-reached histogram: {}", shown.join("  "));
    }

    // POSITIVE CONTROLS. The descent walk must reproduce the shipped model's
    // shape (2+ peoples on ~2.3% of endings, never 3), otherwise this probe's
    // graph is not the graph the model walks and every other row is noise.
    let descent_three_plus: usize = dist[0]
        .iter()
        .filter(|(n, _)| **n > 2)
        .map(|(_, c)| *c)
        .sum();
    assert_eq!(
        descent_three_plus, 0,
        "control: the shipped descent walk must never reach 3 peoples; if it does, \
         this probe is not walking the shipped graph"
    );
    assert!(
        holders[1] >= holders[0],
        "control: adding edges cannot reduce reach"
    );
}

/// S7: does the shipped stance rule already degrade the RAIDER's account
/// faster than the victim's, for a reason nobody chose?
///
/// `stance_of` labels only the exact `occ-ended-by` entity `Perpetrator`,
/// while `VictimLine` is closed under descent (`subject` or any descendant).
/// So the step perpetrator -> its own child is Perpetrator -> Bystander, which
/// `is_lossy` counts as a stance crossing, whereas victim -> its own child is
/// VictimLine -> VictimLine and costs nothing. If that holds, the perpetrator's
/// line forgets its own raid one rung faster than the victim's line does,
/// purely as an artifact of one label being a singleton and the other a closed
/// set — an asymmetry no campaign chose and none has reported.
///
/// Reports only; the control asserts the probe reached both lines.
///
/// claim: structural(seed: panel) — false-positive seed-loop flag; the loop
/// binds a census-panel prefix, not a search over seeds.
#[test]
#[ignore = "probe: contact-substrate instrument over a live-worldgen battery; run by hand (decision 0148 took it off the heavy set)"]
fn whether_the_raiders_line_forgets_faster() {
    println!("\n======== S7: THE PERPETRATOR/VICTIM ASYMMETRY ========");

    let mut victim_first_steps = 0usize;
    let mut victim_first_steps_lossy = 0usize;
    let mut raider_first_steps = 0usize;
    let mut raider_first_steps_lossy = 0usize;
    let mut raider_child_bystander = 0usize;
    let mut raider_child_victimline = 0usize;
    let mut raider_child_perp = 0usize;
    let mut victim_child_is_the_attacker = 0usize;

    for seed in PANEL {
        let world = build(seed);
        let led = &world.ledger;
        let lin = lineage_of(led);

        for e in endings_of(led) {
            let Some(attacker) = e.attacker else { continue };

            // The victim's first retelling step: subject -> each child.
            for child in lin.children_of(e.subject) {
                victim_first_steps += 1;
                if hornvale_hearsay::stance::is_lossy(
                    led,
                    &lin,
                    hornvale_hearsay::stance::Perpetration::Singleton,
                    e.subject,
                    e.subject,
                    *child,
                ) {
                    victim_first_steps_lossy += 1;
                }
            }
            // The raider's first retelling step: attacker -> each child.
            for child in lin.children_of(attacker) {
                raider_first_steps += 1;
                if hornvale_hearsay::stance::is_lossy(
                    led,
                    &lin,
                    hornvale_hearsay::stance::Perpetration::Singleton,
                    e.subject,
                    attacker,
                    *child,
                ) {
                    raider_first_steps_lossy += 1;
                }
                // WHICH label the raider's child lands on decides whether the
                // spec's stated mechanism is right. `Bystander` is NOT forced:
                // if the attacker itself descends from the subject, its child
                // does too and lands on `VictimLine`. Both are lossy against
                // `Perpetrator`, so the 100% survives either way -- but the
                // REASON differs, and a spec stating the wrong reason is the
                // defect class this campaign is trying not to repeat.
                match hornvale_hearsay::stance::stance_of(
                    led,
                    &lin,
                    hornvale_hearsay::stance::Perpetration::Singleton,
                    e.subject,
                    *child,
                ) {
                    hornvale_hearsay::stance::Stance::Bystander => raider_child_bystander += 1,
                    hornvale_hearsay::stance::Stance::VictimLine => raider_child_victimline += 1,
                    hornvale_hearsay::stance::Stance::Perpetrator => raider_child_perp += 1,
                }
            }
            // The victim line's residual: is a lossy `subject -> child` step
            // exactly the case where the child IS the named attacker?
            for child in lin.children_of(e.subject) {
                if *child == attacker {
                    victim_child_is_the_attacker += 1;
                }
            }
        }
    }

    let pct = |n: usize, d: usize| 100.0 * n as f64 / d.max(1) as f64;
    println!(
        "victim line, first step (subject -> child) : {victim_first_steps_lossy} lossy of \
         {victim_first_steps} ({:.2}%)",
        pct(victim_first_steps_lossy, victim_first_steps)
    );
    println!(
        "raider line, first step (attacker -> child): {raider_first_steps_lossy} lossy of \
         {raider_first_steps} ({:.2}%)",
        pct(raider_first_steps_lossy, raider_first_steps)
    );

    println!(
        "  raider's child lands on: Bystander {raider_child_bystander}, VictimLine \
         {raider_child_victimline}, Perpetrator {raider_child_perp}"
    );
    println!(
        "  victim's child IS the named attacker: {victim_child_is_the_attacker} (vs \
         {victim_first_steps_lossy} lossy victim steps)"
    );

    // POSITIVE CONTROLS: both lines must have been reached, or the two
    // percentages above are vacuous.
    assert!(
        victim_first_steps > 0,
        "control: victim line must have steps"
    );
    assert!(
        raider_first_steps > 0,
        "control: raider line must have steps"
    );
}
