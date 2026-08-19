//! The Parley's preregistered readout (spec §6), as a HEAVY BATTERY.
//!
//! Not registered lab metrics, and deliberately so: nine studies declare
//! `"metrics": "all"` with no opt-out, so a metric added here would run on
//! every world of `the-census` (~2000) forever. Campaigns 2 and 3 both put
//! their readouts in a heavy battery for the same reason
//! (`retelling_readout_seed42.rs`, `palimpsest_readout.rs`); this follows
//! them. What it costs: the readout is not tracked over time in the census.
//! Accepted — a campaign readout answers a frozen hypothesis once.
//!
//! **THE ONE THING A READER MUST KNOW BEFORE READING A COLUMN.** Two derive
//! functions produce the numbers here and they walk DIFFERENT GRAPHS:
//!
//! - [`variants_about`] steps parent->child down the founding tree and reads
//!   [`hornvale_hearsay::stance`] at every step. It honours the clock. It does
//!   **not** consult the contact seam at all — under every `Contact` arm it
//!   reports descent-only reach. It is therefore the only instrument that can
//!   answer H2 (§6.3, which is about stance) and the wrong instrument for
//!   everything else here.
//! - [`variants_about_accumulating`] walks the AUGMENTED graph — descent plus
//!   the raid seam — and resolves a rung from an accumulated width. It never
//!   consults `stance`. It is the instrument for H1, H3, H4 and the §6.6 null.
//!
//! A readout that reached for `variants_about` (or for `lib.rs`'s
//! `variant_count` / `finest_precision_hops`, which route through it) to
//! measure a `Contact` arm would print identical columns under both arms and
//! read as "contact does nothing" — a wrong attribution wearing a null's
//! clothes. The division above is deliberate and is not a style choice.
//!
//! **What varies, and what is held still.** Every arm is one step from
//! [`Transmission::AS_SHIPPED`] (`no-clock/singleton/descent/free`), never two:
//!
//! | § | quantity | arm varied | instrument |
//! |---|---|---|---|
//! | §6.2 H1 | holders removed by the clock | `Clock` | accumulating |
//! | §6.3 H2 | retained-rung gap, raider vs victim | `Perpetration` | `variants_about` |
//! | §6.4 H3 | endings whose account reaches 3+ peoples | `Contact` | accumulating |
//! | §6.5 H4 | mutually-exclusive cross-people day sets | `Contact` | accumulating |
//! | §6.6 | saturated fraction, per accumulation rule | `Contact` | accumulating |
//!
//! The combined `clock + contact` arm is NOT measured. It is a second step
//! from the baseline and no hypothesis in §6 asks for it; §3.3's `gated` row
//! (1.76% of endings reaching 3+ peoples) is that combination measured by the
//! substrate probe's own structural walk, and is quoted here only as context.
//!
//! **The width unit caveat carries over from campaign 3 unchanged.** The
//! accumulator seeds its width from `ladder.span(Precision::FINEST)` — std
//! days — and increments it by `amplitude::gen_span`, a dimensionless
//! generation count. Spec §2 freezes that erratum deliberately for a session
//! that has not read The Palimpsest's §6.7, so nothing here repairs it. It is
//! why `Additive` and `Quadrature` sit near the day rung and `Multiplicative`
//! climbs the ladder.
//!
//! The heavy battery **ASSERTS ONLY SUBSTRATE CONTROLS** (spec §6.7): that the
//! panel built, that held claims exist, that no claim reports a rung no ladder
//! in its world has, that the `descent` arm reproduces §3.3's shape, that the
//! clock never adds a holder and contact never removes one. **Every hypothesis
//! is REPORTED**, printed against its own §6 decision table. A falsified
//! prediction is a finding. This file must never be edited to rescue one.

use crate::common;

use common::{a_holder_that_died_before_the_event, two_peoples_joined_by_a_later_raid};
use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::accumulate::Accumulation;
use hornvale_hearsay::clock::Clock;
use hornvale_hearsay::contact::{Contact, ContactGraph, contact_of};
use hornvale_hearsay::derive::{variants_about, variants_about_accumulating};
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_hearsay::ladder::{PeopleLadders, PrecisionLadder};
use hornvale_hearsay::lineage::{Lineage, lineage_of};
use hornvale_hearsay::stance::Perpetration;
use hornvale_hearsay::transmission::{Transmission, Walk};
use hornvale_kernel::Precision;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use std::collections::{BTreeMap, BTreeSet};

/// The seed panel: the first 40 seeds of the census panel (`the-census` runs
/// seeds 0–999), the same panel campaign 3's readout used, so this readout is
/// a strict subset of the census population and directly comparable to its
/// predecessor.
const PANEL: [u64; 40] = [
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
];

/// Seeds 0–11 are exactly the panel spec §3 measured its substrate on, and
/// they are a prefix of [`PANEL`]. Every §3 figure this readout compares
/// against is therefore re-derivable on this run's own first twelve seeds,
/// rather than only against a published number from another instrument.
const BASELINE_PREFIX: u64 = 12;

/// Spec §3.4: mutually-exclusive cross-people day sets, 12 seeds, measured by
/// `probe_contact_substrate.rs` under `Accumulation::Multiplicative`. **19 as
/// published; 15 as re-derived at The Underworld's merge — see the
/// re-derivation block on [`BASELINE_ENDINGS_12`] below.**
const BASELINE_MUTUALLY_EXCLUSIVE_12: usize = 15;

/// # RE-DERIVED AT THE UNDERWORLD'S MERGE (2026-08-18)
///
/// The three `BASELINE_*_12` constants below are the ONLY figures this file
/// asserts, and all three moved when The Underworld merged. Re-derived by this
/// readout's own procedure on the merge product `main` `95cbaa70` +
/// `campaign/the-underworld` `ea95434f`, `PANEL`, `BASELINE_PREFIX` and the
/// scorer all untouched:
///
/// ```text
///                                 spec §3.1/§3.4   merge product    move
///   endings, seeds 0-11                    5913            4975    -15.9%
///   foreign-attacker endings                138             102    -26.1%
///   mutually-exclusive day sets              19              15    -21.1%
/// ```
///
/// **WHY, AND THE ATTRIBUTION IS NOT THE OBVIOUS ONE.** The Underworld re-keyed
/// the history bake's node index from `CellId` to `(CellId, DelveRung)` —
/// decision 0145, one community per *place* rather than per cell — which is the
/// change that most visibly touches settlement placement, and the natural
/// reading is that it accounts for the move. It does not. Neutralising it
/// alone (`Bake::rung_for` forced to `DelveRung::Surface`, which makes every
/// node-index key `(cell, Surface)` and so restores the old one-per-cell
/// semantics exactly, with everything else about the campaign intact) reads:
///
/// ```text
///                          main    re-key neutralised    as merged
///   endings                5913                  5038         4975
///   foreign                 138                   143          102
///   mutually exclusive       19                    13           15
/// ```
///
/// So the re-key explains **63 of the 938 lost endings (6.7%)**. The other
/// 93% is the rest of the campaign — the terrain, water-table, cave-depth and
/// species work that changed the *surface* world as well as adding the
/// underworld. The foreign-attacker count is the opposite case: neutralising
/// the re-key takes it to 143, *above* main's 138, so the re-key accounts for
/// the whole of that column's fall and slightly more.
///
/// **AND THE MOVE IS ~6x LARGER HERE THAN IN THE POPULATION IT SAMPLES.** Over
/// the full 1000-world census the same campaign moved settlement count -1.30%
/// (257.515 -> 254.171), total population -1.04% and standing tribute
/// relations -3.24%. Over seeds 0-11 specifically those same columns moved
/// -8.39%, -8.92% and -14.44%. The twelve-seed prefix is a small, high-variance
/// subsample, so a double-digit move in it is what a low-single-digit move in
/// the world looks like through this control. That is a fact about the
/// control's sensitivity, not a defect in it: it fired on a real world change,
/// which is its job.
///
/// Spec §3.1, seeds 0–11: endings in the panel. **5913 as published; 4975 as
/// re-derived above, which is what this constant now holds.**
const BASELINE_ENDINGS_12: usize = 4975;

/// Spec §3.1, seeds 0–11: endings whose attacker is of another people. **138
/// as published; 102 as re-derived at The Underworld's merge — see the
/// re-derivation block above.**
const BASELINE_FOREIGN_12: usize = 102;

/// Spec §3.5: the share of holders that had already ended when the event they
/// hold took place. H1's prediction is stated against this number.
const BASELINE_DEAD_HOLDER_PERCENT: f64 = 1.19;

/// Spec §6.4: the floor H3 predicts for endings reaching three or more
/// peoples under contact.
const H3_FLOOR_PERCENT: f64 = 1.0;

/// Spec §6.5: the factor by which contact is predicted to raise the
/// mutually-exclusive count.
const H4_FACTOR: f64 = 3.0;

/// **The panel size was chosen by measurement, per the plan's decision rule.**
/// A 3-seed pilot (seeds 0, 1, 2) was run before the full panel: 6.21 s of
/// test time, 7.60 s wall including the incremental build. That is
/// **2.07 s/seed**, comfortably under the plan's 5 s/seed bar, so the 40-seed
/// panel campaign 3 used was kept rather than shrunk. Recorded here, and
/// printed by the battery, so the number lives in the artifact and not only
/// in a report.
const PILOT_SEEDS: usize = 3;
/// Seconds of test time the 3-seed pilot took (7.60 s wall with the build).
const PILOT_TEST_SECONDS: f64 = 6.21;
/// Seconds of test time a full 40-seed run took, measured (2.28 s/seed —
/// the pilot's 2.07 s/seed held).
const FULL_RUN_TEST_SECONDS: f64 = 91.04;

/// The predicate every claim in this readout is about. Only an ending has
/// parties beyond its subject (campaign 2 spec §6.1), so it is the only
/// predicate a transmission tree can be built over.
const PREDICATE: &str = hornvale_history::OCC_ENDED;

/// One ending, with the two peoples the seam puts on either side of it.
struct Ending {
    /// The occupation that ended.
    subject: EntityId,
    /// The subject's own people, empty when it names none.
    people: String,
    /// The day it ended.
    day: f64,
    /// The people of its `occ-ended-by` attacker, when it named an
    /// `Entity`-valued one that itself names a people.
    attacker_people: Option<String>,
}

impl Ending {
    /// Whether the attacker is of another people — §3.1's 2.33% population,
    /// and H2's and H4's primary population.
    fn is_foreign(&self) -> bool {
        self.attacker_people
            .as_ref()
            .is_some_and(|ap| !ap.is_empty() && !self.people.is_empty() && *ap != self.people)
    }
}

/// Everything one world contributes, assembled once and shared by every arm
/// so the world is read for its people table exactly once.
struct WorldRead {
    /// The founding tree.
    lineage: Lineage,
    /// The raid seam.
    contact: ContactGraph,
    /// Per-people generation and lifespan, in std days.
    durations: PeopleDurations,
    /// One ladder per people — what the accumulating walk resolves against.
    ladders: PeopleLadders,
    /// The astronomical-only ladder — what `variants_about` coarsens against,
    /// as in campaign 2's own readout.
    astronomical: PrecisionLadder,
    /// Which people each occupation belongs to.
    people_of: BTreeMap<EntityId, String>,
    /// Every ending in the world, ascending by subject.
    endings: Vec<Ending>,
    /// The longest ladder any people in this world carries. A rung beyond it
    /// could have been resolved by no ladder here at all, which is the sound
    /// form of the "no claim reports a rung its ladder lacks" control — see
    /// [`SeedRow::rung_beyond_world`].
    max_rungs: usize,
}

/// How far one arm's accounts reach. Rule-independent: the accumulating walk's
/// REACH is graph reachability under the clock and the seam, and the
/// accumulation rule only orders rival tellings once they arrive. Asserted, not
/// assumed — see [`SeedRow::rule_disagreements`].
#[derive(Clone, Default)]
struct ReachRow {
    /// Endings with at least one holder.
    events: usize,
    /// Held claims summed over those endings.
    holders: usize,
    /// Endings whose account reaches two or more peoples.
    two_plus: usize,
    /// Endings whose account reaches three or more peoples — H3.
    three_plus: usize,
    /// The most peoples any one account reached.
    max_peoples: usize,
    /// peoples-reached -> how many endings.
    hist: BTreeMap<usize, usize>,
}

/// Where one arm's held claims sit on the ladder — the §6.6 null detector.
#[derive(Clone, Default)]
struct RungRow {
    /// Held claims.
    held: usize,
    /// Held claims at the coarsest rung of the HOLDER's own people's ladder,
    /// the same definition campaign 3's readout used.
    saturated: usize,
    /// Held claims whose rung lies past the holder's own people's ladder.
    /// Zero by construction under descent (descent never crosses a people
    /// boundary, so the resolving ladder IS the holder's); under contact it
    /// counts claims resolved against a longer foreign ladder, which is spec
    /// §5.4's originating-witness freeze made visible.
    beyond_own_ladder: usize,
    /// Rung index -> how many held claims.
    hist: BTreeMap<u8, usize>,
}

/// Whether the two sides of a raid remember different days — H4.
#[derive(Clone, Default)]
struct DivRow {
    /// Cross-people endings where both the victim's and the raider's people
    /// hold the account — §3.4's population.
    compared: usize,
    /// ... of which each side holds a day the other holds nowhere.
    mutually_exclusive: usize,
    /// ... of which one side's day set strictly contains the other's.
    one_sided: usize,
    /// ... of which the two day sets are identical.
    identical: usize,
    /// Endings where SOME pair of holding peoples is mutually exclusive.
    /// Broader than `mutually_exclusive`: contact can carry an account to a
    /// third people that was never a party to the raid, and the victim/raider
    /// pair cannot see that.
    any_pair_exclusive: usize,
}

/// The retained-rung gap between the raider's people and the victim's — H2.
#[derive(Clone, Default)]
struct H2Row {
    /// Cross-people endings where both sides hold at least one claim.
    events: usize,
    /// Per event, `median(raider rungs) - median(victim rungs)`.
    gaps: Vec<f64>,
    /// Every victim-people holder's rung, pooled over the seed.
    victim: Vec<f64>,
    /// Every raider-people holder's rung, pooled over the seed.
    raider: Vec<f64>,
}

/// One seed's whole contribution.
#[derive(Clone, Default)]
struct SeedRow {
    /// Which seed.
    seed: u64,
    /// Endings in the world.
    endings: usize,
    /// ... of which the attacker is of another people.
    foreign: usize,
    /// H1's denominator: holders under `AS_SHIPPED`.
    holders_no_clock: usize,
    /// H1's numerator's complement: holders under `clock = Alive`.
    holders_clock: usize,
    /// Holders under `AS_SHIPPED` that had themselves ended before the event
    /// — §3.5's 1.19%, re-derived on this panel.
    dead_holders: usize,
    /// Reach, indexed by [`Contact::ALL`].
    reach: [ReachRow; 2],
    /// Rungs, indexed by [`Contact::ALL`] then [`Accumulation::ALL`].
    rungs: [[RungRow; 3]; 2],
    /// Divergence, indexed the same way.
    div: [[DivRow; 3]; 2],
    /// H2, indexed by [`Perpetration::ALL`].
    h2: [H2Row; 2],
    /// CONTROL: endings where the clock arm reached somebody the no-clock arm
    /// did not. Must be zero — spec §5.1 calls the clock strictly removing.
    clock_added: usize,
    /// CONTROL: endings where the descent arm reached somebody the contact
    /// arm did not. Must be zero — spec §5.3 calls contact strictly adding.
    contact_removed: usize,
    /// CONTROL: endings where two accumulation rules disagreed on how many
    /// holders were reached. Must be zero; reach is rule-independent.
    rule_disagreements: usize,
    /// CONTROL: held claims on a rung longer than any ladder in their world.
    /// Must be zero — no ladder could have resolved one.
    rung_beyond_world: usize,
}

/// The median of `values`, or `NAN` when empty. Lower of the two central
/// values on an even population — deterministic, never interpolated, so the
/// result is always an observed rung.
fn median(values: &[f64]) -> f64 {
    if values.is_empty() {
        return f64::NAN;
    }
    let mut v = values.to_vec();
    v.sort_by(f64::total_cmp);
    v[v.len() / 2]
}

/// `n` as a percentage of `d`, or `0.0` when `d` is zero.
fn pct(n: usize, d: usize) -> f64 {
    100.0 * n as f64 / d.max(1) as f64
}

/// Whether some two peoples in `by_people` each hold a remembered day the
/// other holds nowhere.
fn any_pair_mutually_exclusive(by_people: &BTreeMap<String, BTreeSet<u64>>) -> bool {
    let sets: Vec<&BTreeSet<u64>> = by_people.values().collect();
    for (i, a) in sets.iter().enumerate() {
        for b in sets.iter().skip(i + 1) {
            if a.difference(b).count() > 0 && b.difference(a).count() > 0 {
                return true;
            }
        }
    }
    false
}

/// Every ending in a ledger, ascending by subject.
fn endings_of(led: &Ledger) -> Vec<Ending> {
    let mut out = Vec::new();
    for fact in led.find(PREDICATE) {
        let subject = fact.subject;
        let Some(Value::Number(day)) = led.value_of(subject, hornvale_history::OCC_ENDED) else {
            continue;
        };
        let people = match led.value_of(subject, hornvale_history::OCC_PEOPLE) {
            Some(Value::Text(p)) => p.clone(),
            _ => String::new(),
        };
        let attacker_people = match led.value_of(subject, hornvale_history::OCC_ENDED_BY) {
            Some(Value::Entity(a)) => match led.value_of(*a, hornvale_history::OCC_PEOPLE) {
                Some(Value::Text(p)) => Some(p.clone()),
                _ => None,
            },
            _ => None,
        };
        out.push(Ending {
            subject,
            people,
            day: *day,
            attacker_people,
        });
    }
    out.sort_by_key(|e| e.subject);
    out.dedup_by_key(|e| e.subject);
    out
}

/// Assemble one world's people table, ladders and endings.
///
/// `None` when the world offers no year rung to convert allometric years into
/// standard days — the readout reports such a seed as skipped rather than
/// guessing a year length. A near-copy of campaign 3's readout and of
/// `probe_contact_substrate.rs`, deliberately: a readout that assembled the
/// ladder differently from the probe whose substrate it is testing against
/// would be measuring a different model.
fn read_world(led: &Ledger, components: &hornvale_worldgen::WorldComponents) -> Option<WorldRead> {
    let lineage = lineage_of(led);
    let contact = contact_of(led);
    let astronomical = PrecisionLadder::of(led);
    let year_days = astronomical
        .labels()
        .iter()
        .position(|label| *label == "year")
        .and_then(|i| astronomical.span(Precision(i as u8)))
        .map(|span| span.get())?;

    let mut people_of: BTreeMap<EntityId, String> = BTreeMap::new();
    for occ in lineage.all() {
        if let Some(Value::Text(people)) = led.value_of(occ, hornvale_history::OCC_PEOPLE) {
            people_of.insert(occ, people.clone());
        }
    }

    let mut durations = PeopleDurations::default();
    let named: BTreeSet<&String> = people_of.values().collect();
    for people in named {
        let Some(bio) = components.biosphere.get_by_label(people) else {
            continue;
        };
        let life = hornvale_species::life_history(bio.mass, bio.metabolic_class, bio.schedule);
        let to_days = |years: hornvale_kernel::Years| StdDays::new(years.get() * year_days).ok();
        durations.insert(
            people,
            life.generation_length.and_then(to_days),
            life.lifespan.and_then(to_days),
        );
    }

    let ladders = PeopleLadders::of(led, &durations);
    let mut max_rungs = astronomical.len();
    for people in durations.peoples() {
        max_rungs = max_rungs.max(ladders.for_people(people).len());
    }

    Some(WorldRead {
        lineage,
        contact,
        durations,
        ladders,
        astronomical,
        people_of,
        endings: endings_of(led),
        max_rungs,
    })
}

/// Every quantity §6 asks for, over one world.
///
/// Nine walks per ending in the worst case — three rules under each of the two
/// `Contact` arms, one under `clock = Alive`, and two `variants_about` walks
/// that run only on the ~2.3% of endings with a foreign attacker (H2's whole
/// population, so paying for the other 97.7% would buy nothing).
fn measure_seed(seed: u64, led: &Ledger, read: &WorldRead) -> SeedRow {
    let mut row = SeedRow {
        seed,
        endings: read.endings.len(),
        ..Default::default()
    };

    for e in &read.endings {
        let foreign = e.is_foreign();
        if foreign {
            row.foreign += 1;
        }
        // The holder sets of each Contact arm's first rule, kept so the
        // strictly-adding and strictly-removing controls can be checked per
        // ending rather than only in aggregate (an aggregate count cannot see
        // one ending gaining a holder while another loses one).
        let mut sets: [BTreeSet<EntityId>; 2] = Default::default();

        for (ai, contact) in Contact::ALL.iter().enumerate() {
            let policy = Transmission {
                contact: *contact,
                ..Transmission::AS_SHIPPED
            };
            let walk = Walk {
                ledger: led,
                lineage: &read.lineage,
                contact: &read.contact,
                policy,
            };
            let mut first_len: Option<usize> = None;

            for (ri, rule) in Accumulation::ALL.iter().enumerate() {
                let held = variants_about_accumulating(
                    &walk,
                    &read.ladders,
                    &read.durations,
                    *rule,
                    e.subject,
                    PREDICATE,
                );
                match first_len {
                    None => first_len = Some(held.len()),
                    Some(n) if n != held.len() => row.rule_disagreements += 1,
                    _ => {}
                }

                let mut by_people: BTreeMap<String, BTreeSet<u64>> = BTreeMap::new();
                let rungs = &mut row.rungs[ai][ri];
                for c in &held {
                    rungs.held += 1;
                    let rung = c.precision.rung();
                    *rungs.hist.entry(rung).or_default() += 1;
                    if rung as usize >= read.max_rungs {
                        row.rung_beyond_world += 1;
                    }
                    let people = read.people_of.get(&c.holder);
                    let ladder = read.ladders.for_people(people.map_or("", |p| p.as_str()));
                    if !ladder.is_empty() {
                        if rung as usize >= ladder.len() {
                            rungs.beyond_own_ladder += 1;
                        } else if rung as usize == ladder.len() - 1 {
                            rungs.saturated += 1;
                        }
                    }
                    if let (Some(p), Value::Number(day)) = (people, &c.object) {
                        by_people
                            .entry(p.clone())
                            .or_default()
                            .insert(day.to_bits());
                    }
                }

                let div = &mut row.div[ai][ri];
                if any_pair_mutually_exclusive(&by_people) {
                    div.any_pair_exclusive += 1;
                }
                if foreign {
                    let ap = e
                        .attacker_people
                        .as_ref()
                        .expect("a foreign ending names an attacker people");
                    if let (Some(v), Some(r)) = (by_people.get(&e.people), by_people.get(ap)) {
                        div.compared += 1;
                        let v_only = v.difference(r).count();
                        let r_only = r.difference(v).count();
                        if v_only > 0 && r_only > 0 {
                            div.mutually_exclusive += 1;
                        } else if v_only > 0 || r_only > 0 {
                            div.one_sided += 1;
                        } else {
                            div.identical += 1;
                        }
                    }
                }

                if ri == 0 {
                    sets[ai] = held.iter().map(|c| c.holder).collect();
                    if !held.is_empty() {
                        let peoples: BTreeSet<&String> = held
                            .iter()
                            .filter_map(|c| read.people_of.get(&c.holder))
                            .collect();
                        let reach = &mut row.reach[ai];
                        reach.events += 1;
                        reach.holders += held.len();
                        *reach.hist.entry(peoples.len()).or_default() += 1;
                        if peoples.len() >= 2 {
                            reach.two_plus += 1;
                        }
                        if peoples.len() >= 3 {
                            reach.three_plus += 1;
                        }
                        reach.max_peoples = reach.max_peoples.max(peoples.len());
                    }
                    if ai == 0 {
                        row.holders_no_clock += held.len();
                        for c in &held {
                            if let Some(Value::Number(x)) =
                                led.value_of(c.holder, hornvale_history::OCC_ENDED)
                                && *x < e.day
                            {
                                row.dead_holders += 1;
                            }
                        }
                    }
                }
            }
        }

        // H1's arm: the clock, with contact left at descent so the two
        // changes cannot cancel (spec §5.1).
        let clocked = variants_about_accumulating(
            &Walk {
                ledger: led,
                lineage: &read.lineage,
                contact: &read.contact,
                policy: Transmission {
                    clock: Clock::Alive,
                    ..Transmission::AS_SHIPPED
                },
            },
            &read.ladders,
            &read.durations,
            Accumulation::ALL[0],
            e.subject,
            PREDICATE,
        );
        row.holders_clock += clocked.len();
        let clocked_set: BTreeSet<EntityId> = clocked.iter().map(|c| c.holder).collect();
        if !clocked_set.is_subset(&sets[0]) {
            row.clock_added += 1;
        }
        if !sets[0].is_subset(&sets[1]) {
            row.contact_removed += 1;
        }

        // H2's arms. `variants_about`, never the accumulating walk: it is the
        // only one that reads stance, and H2 is about stance. Its
        // seam-blindness does not bite because H2 varies `Perpetration` under
        // descent.
        if foreign {
            let ap = e
                .attacker_people
                .clone()
                .expect("a foreign ending names an attacker people");
            for (pi, perpetration) in Perpetration::ALL.iter().enumerate() {
                let walk = Walk {
                    ledger: led,
                    lineage: &read.lineage,
                    contact: &read.contact,
                    policy: Transmission {
                        perpetration: *perpetration,
                        ..Transmission::AS_SHIPPED
                    },
                };
                let held = variants_about(&walk, &read.astronomical, e.subject, PREDICATE);
                let mut victim: Vec<f64> = Vec::new();
                let mut raider: Vec<f64> = Vec::new();
                for c in &held {
                    let Some(p) = read.people_of.get(&c.holder) else {
                        continue;
                    };
                    let rung = f64::from(c.precision.rung());
                    if *p == e.people {
                        victim.push(rung);
                    } else if *p == ap {
                        raider.push(rung);
                    }
                }
                if !victim.is_empty() && !raider.is_empty() {
                    let h2 = &mut row.h2[pi];
                    h2.events += 1;
                    h2.gaps.push(median(&raider) - median(&victim));
                    h2.victim.extend(victim.iter().copied());
                    h2.raider.extend(raider.iter().copied());
                }
            }
        }
    }

    row
}

/// Print a `label -> count` distribution compactly.
fn show_hist<K: std::fmt::Debug + Ord>(hist: &BTreeMap<K, usize>) -> String {
    hist.iter()
        .map(|(k, v)| format!("{k:?}:{v}"))
        .collect::<Vec<_>>()
        .join(" ")
}

/// THE READOUT. Spec §6, over a fixed seed panel, one arm at a time, with no
/// arm nominated.
///
/// claim: readout(off-gate, heavy:) — the preregistered §6 readout over
/// `PANEL`; reports H1–H4 and the §6.6 null against their decision tables and
/// asserts only substrate controls. A falsified prediction is a finding here.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn the_parley_readout_over_a_seed_panel() {
    let components = hornvale_worldgen::WorldComponents::assemble().expect("components assemble");
    let mut rows: Vec<SeedRow> = Vec::new();
    let mut skipped: Vec<u64> = Vec::new();

    for seed in PANEL {
        let world = hornvale_worldgen::build_world(
            hornvale_kernel::Seed(seed),
            &hornvale_astronomy::SkyPins::default(),
            hornvale_worldgen::SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
        )
        .expect("panel seed builds");
        let led = &world.ledger;
        let Some(read) = read_world(led, &components) else {
            skipped.push(seed);
            continue;
        };
        rows.push(measure_seed(seed, led, &read));
    }

    println!("\n================ THE PARLEY READOUT ================");
    println!(
        "panel                : {} seeds (census seeds 0-39), {} measured, skipped {skipped:?}",
        PANEL.len(),
        rows.len()
    );
    println!("predicate            : {PREDICATE}");
    println!(
        "baseline             : AS_SHIPPED = {} (every arm is ONE step from it)",
        Transmission::AS_SHIPPED.label()
    );
    println!(
        "pilot cost           : {PILOT_SEEDS} seeds (0,1,2) cost {PILOT_TEST_SECONDS:.2} s of \
         test time = {:.2} s/seed; the plan's rule (<= 5 s/seed) kept the 40-seed panel. The \
         full run then measured {FULL_RUN_TEST_SECONDS:.2} s of test time.",
        PILOT_TEST_SECONDS / PILOT_SEEDS as f64
    );
    println!(
        "instruments          : H1/H3/H4/§6.6 via variants_about_accumulating (walks the seam); \
         H2 via variants_about (reads stance, never the seam)"
    );
    println!(
        "NOT measured         : the combined clock+contact arm — two steps from the baseline, \
         and no §6 hypothesis asks for it"
    );

    let endings: usize = rows.iter().map(|r| r.endings).sum();
    let foreign: usize = rows.iter().map(|r| r.foreign).sum();
    println!(
        "\nSUBSTRATE, THIS RUN  : {endings} endings, {foreign} with a foreign attacker \
         ({:.2}%; spec §3.1 measured 2.33% on seeds 0-11)",
        pct(foreign, endings)
    );
    let prefix_endings: usize = rows
        .iter()
        .filter(|r| r.seed < BASELINE_PREFIX)
        .map(|r| r.endings)
        .sum();
    let prefix_foreign: usize = rows
        .iter()
        .filter(|r| r.seed < BASELINE_PREFIX)
        .map(|r| r.foreign)
        .sum();
    println!(
        "  seeds 0-11 only    : {prefix_endings} endings, {prefix_foreign} foreign ({:.2}%) \
         — directly comparable to §3.1's 5,913 / 138 / 2.33%",
        pct(prefix_foreign, prefix_endings)
    );

    // =====================================================================
    // §6.2 — H1: the clock's blast radius.
    // =====================================================================
    println!("\n=== §6.2 H1 — THE CLOCK'S BLAST RADIUS ===");
    println!(
        "  {:<6} {:>10} {:>10} {:>10} {:>10} {:>10}",
        "seed", "holders", "w/ clock", "removed", "removed%", "dead%"
    );
    for r in &rows {
        println!(
            "  {:<6} {:>10} {:>10} {:>10} {:>9.2}% {:>9.2}%",
            r.seed,
            r.holders_no_clock,
            r.holders_clock,
            r.holders_no_clock.saturating_sub(r.holders_clock),
            pct(
                r.holders_no_clock.saturating_sub(r.holders_clock),
                r.holders_no_clock
            ),
            pct(r.dead_holders, r.holders_no_clock),
        );
    }
    let h1_total: usize = rows.iter().map(|r| r.holders_no_clock).sum();
    let h1_clocked: usize = rows.iter().map(|r| r.holders_clock).sum();
    let h1_dead: usize = rows.iter().map(|r| r.dead_holders).sum();
    let h1_removed = h1_total.saturating_sub(h1_clocked);
    let h1_removed_pct = pct(h1_removed, h1_total);
    let h1_dead_pct = pct(h1_dead, h1_total);
    println!("  --- panel ---");
    println!("    holders, no clock        : {h1_total}");
    println!("    holders, clock = Alive   : {h1_clocked}");
    println!("    removed                  : {h1_removed} ({h1_removed_pct:.2}%)");
    println!(
        "    of which DIRECTLY dead   : {h1_dead} ({h1_dead_pct:.2}%; spec §3.5 published \
         {BASELINE_DEAD_HOLDER_PERCENT}% on seeds 0-11)"
    );
    println!(
        "    blast-radius multiplier  : {:.2}x the directly-dead share",
        h1_removed_pct / h1_dead_pct.max(f64::MIN_POSITIVE)
    );
    println!(
        "  DECISION TABLE (§6.2): removal > {BASELINE_DEAD_HOLDER_PERCENT}% -> CONFIRMED; \
         removal <= {BASELINE_DEAD_HOLDER_PERCENT}% -> FALSIFIED (dead holders are \
         overwhelmingly leaves, itself a finding about the founding tree)"
    );
    println!(
        "  VERDICT: {} — {h1_removed_pct:.2}% removed",
        if h1_removed_pct > BASELINE_DEAD_HOLDER_PERCENT {
            "CONFIRMED"
        } else {
            "FALSIFIED"
        }
    );

    // =====================================================================
    // §6.3 — H2: the retained-rung gap.
    // =====================================================================
    println!("\n=== §6.3 H2 — RAIDER'S LINE vs VICTIM'S LINE, RETAINED RUNG ===");
    println!("  instrument: variants_about (the ONLY walk that reads stance), descent, no clock");
    println!(
        "  {:<6} {:>8} {:>10} {:>10} {:>8} {:>10} {:>10}",
        "seed", "sng ev", "sng gap", "sng v/r", "inh ev", "inh gap", "inh v/r"
    );
    for r in &rows {
        println!(
            "  {:<6} {:>8} {:>10.2} {:>4.1}/{:<5.1} {:>8} {:>10.2} {:>4.1}/{:<5.1}",
            r.seed,
            r.h2[0].events,
            median(&r.h2[0].gaps),
            median(&r.h2[0].victim),
            median(&r.h2[0].raider),
            r.h2[1].events,
            median(&r.h2[1].gaps),
            median(&r.h2[1].victim),
            median(&r.h2[1].raider),
        );
    }
    let mut h2_medians = [f64::NAN; 2];
    for (pi, perpetration) in Perpetration::ALL.iter().enumerate() {
        let mut gaps: Vec<f64> = Vec::new();
        let mut victim: Vec<f64> = Vec::new();
        let mut raider: Vec<f64> = Vec::new();
        for r in &rows {
            gaps.extend(r.h2[pi].gaps.iter().copied());
            victim.extend(r.h2[pi].victim.iter().copied());
            raider.extend(r.h2[pi].raider.iter().copied());
        }
        let mut gap_hist: BTreeMap<String, usize> = BTreeMap::new();
        for g in &gaps {
            *gap_hist.entry(format!("{g:+.1}")).or_default() += 1;
        }
        h2_medians[pi] = median(&gaps);
        println!("  --- arm: {} ---", perpetration.label());
        println!("    events compared          : {}", gaps.len());
        println!("    median rung, victim line : {:.2}", median(&victim));
        println!("    median rung, raider line : {:.2}", median(&raider));
        println!("    MEDIAN GAP (raider-victim): {:.4}", h2_medians[pi]);
        println!("    per-event gap histogram  : {}", show_hist(&gap_hist));
    }
    println!(
        "  DECISION TABLE (§6.3): under singleton, gap == +1 rung EXACTLY -> CONFIRMED; \
         gap absent or reversed -> FALSIFIED (the 100% lossy first step is absorbed downstream \
         and the geometry is cosmetic); gap > 1 rung -> WITHHELD, something other than stance \
         contributes and must be named first. Under inherited the gap is predicted to go to 0."
    );
    let h2_singleton = h2_medians[0];
    let h2_verdict = if !h2_singleton.is_finite() {
        "NO VERDICT (no cross-people ending had holders on both sides)"
    } else if (h2_singleton - 1.0).abs() < 1e-9 {
        "CONFIRMED"
    } else if h2_singleton > 1.0 {
        "WITHHELD — the gap exceeds one rung; campaign 2's held-claim ceiling caps stance at one \
         crossing, so something else contributes and H2 is not reported until it is named"
    } else {
        "FALSIFIED"
    };
    println!("  VERDICT (singleton): {h2_verdict} — median gap {h2_singleton:+.4}");
    println!(
        "  VERDICT (inherited): {} — median gap {:+.4}",
        if !h2_medians[1].is_finite() {
            "NO VERDICT"
        } else if h2_medians[1].abs() < 1e-9 {
            "as predicted, the gap goes to zero"
        } else {
            "NOT as predicted, the gap survives closing Perpetrator under descent"
        },
        h2_medians[1]
    );

    // =====================================================================
    // §6.4 — H3: accounts no tree can produce.
    // =====================================================================
    println!("\n=== §6.4 H3 — HOW MANY PEOPLES ONE ACCOUNT REACHES ===");
    println!(
        "  {:<6} {:>8} {:>8} {:>7} {:>8} {:>8} {:>8} {:>7} {:>8}",
        "seed", "d:events", "d:2+", "d:3+", "d:max", "c:events", "c:2+", "c:3+", "c:max"
    );
    for r in &rows {
        println!(
            "  {:<6} {:>8} {:>8} {:>7} {:>8} {:>8} {:>8} {:>7} {:>8}",
            r.seed,
            r.reach[0].events,
            r.reach[0].two_plus,
            r.reach[0].three_plus,
            r.reach[0].max_peoples,
            r.reach[1].events,
            r.reach[1].two_plus,
            r.reach[1].three_plus,
            r.reach[1].max_peoples,
        );
    }
    let mut three_plus_pct = [0.0f64; 2];
    for (ai, contact) in Contact::ALL.iter().enumerate() {
        let events: usize = rows.iter().map(|r| r.reach[ai].events).sum();
        let holders: usize = rows.iter().map(|r| r.reach[ai].holders).sum();
        let two: usize = rows.iter().map(|r| r.reach[ai].two_plus).sum();
        let three: usize = rows.iter().map(|r| r.reach[ai].three_plus).sum();
        let max = rows
            .iter()
            .map(|r| r.reach[ai].max_peoples)
            .max()
            .unwrap_or(0);
        let mut hist: BTreeMap<usize, usize> = BTreeMap::new();
        for r in &rows {
            for (k, v) in &r.reach[ai].hist {
                *hist.entry(*k).or_default() += v;
            }
        }
        three_plus_pct[ai] = pct(three, events);
        println!("  --- arm: {} ---", contact.label());
        println!("    endings with an account : {events}");
        println!("    total holders           : {holders}");
        println!(
            "    reaches 2+ peoples      : {two} ({:.2}%)",
            pct(two, events)
        );
        println!(
            "    reaches 3+ peoples      : {three} ({:.2}%)",
            pct(three, events)
        );
        println!("    most peoples reached    : {max}");
        println!("    peoples-reached hist    : {}", show_hist(&hist));
    }
    println!(
        "  CONTEXT: §3.3's structural walk measured descent 2.33% / 3+ = 0, contact 16.12% / \
         2.57%, gated 12.38% / 1.76%. Those walk the graph without the model's filters; these \
         walk the model."
    );
    println!(
        "  DECISION TABLE (§6.4): under contact, 3+ peoples on >= {H3_FLOOR_PERCENT}% of endings \
         -> CONFIRMED; below -> FALSIFIED"
    );
    println!(
        "  VERDICT: {} — {:.2}% under contact (descent {:.2}%)",
        if three_plus_pct[1] >= H3_FLOOR_PERCENT {
            "CONFIRMED"
        } else {
            "FALSIFIED"
        },
        three_plus_pct[1],
        three_plus_pct[0],
    );

    // §6.4's sharper half: seed 2, which has zero cross-people attackers.
    println!("\n  --- §6.4's SEED-LEVEL POSITIVE CONTROL: SEED 2 ---");
    match rows.iter().find(|r| r.seed == 2) {
        Some(r) => {
            println!(
                "    seed 2: {} endings, {} with a foreign attacker (§3.1 measured 0)",
                r.endings, r.foreign
            );
            println!(
                "    descent: {} endings reach 2+ peoples, {} reach 3+, {} holders, most \
                 peoples {}",
                r.reach[0].two_plus,
                r.reach[0].three_plus,
                r.reach[0].holders,
                r.reach[0].max_peoples,
            );
            println!(
                "    contact: {} endings reach 2+ peoples, {} reach 3+, {} holders, most \
                 peoples {}",
                r.reach[1].two_plus,
                r.reach[1].three_plus,
                r.reach[1].holders,
                r.reach[1].max_peoples,
            );
            println!(
                "    the seam IS live on seed 2 — it carries the account to {} holders where \
                 descent reaches {}, a {:.2}x widening — it simply never crosses a people \
                 boundary there.",
                r.reach[1].holders,
                r.reach[0].holders,
                r.reach[1].holders as f64 / r.reach[0].holders.max(1) as f64,
            );
            println!(
                "    DECISION TABLE (§6.4): under contact, seed 2 carries MORE THAN ZERO \
                 cross-people accounts -> CONFIRMED. This is the cleanest discriminator \
                 available between 'contact works' and 'contact re-describes co-witnessing': \
                 the mechanism is absent at hop 0 and present as a graph."
            );
            println!(
                "    VERDICT: {} — {} cross-people accounts under contact, {} under descent",
                if r.reach[1].two_plus > 0 {
                    "CONFIRMED"
                } else {
                    "FALSIFIED"
                },
                r.reach[1].two_plus,
                r.reach[0].two_plus,
            );
            if r.reach[1].two_plus == 0 {
                println!(
                    "    WHY, so the verdict is read for what it is: a contact edge joins a \
                     victim to its OWN named attacker, so an edge crosses a people boundary \
                     only where that attacker is foreign. Seed 2 has {} foreign attackers, \
                     therefore ZERO cross-people contact edges; descent never crosses a \
                     people boundary either. A cross-people account is STRUCTURALLY \
                     IMPOSSIBLE on this seed under either arm. §6.4 called seed 2 'absent at \
                     hop 0 but present as a graph' — but the graph's edges ARE the hop-0 \
                     seam, so the graph is absent too. The prediction is falsified because \
                     its premise was false; that says nothing about whether contact works, \
                     which H3's panel figure above is what answers. A seed with a foreign \
                     attacker but no 3+ account under descent would be the discriminator \
                     §6.4 wanted; seed 2 is not one.",
                    r.foreign
                );
            }
        }
        None => println!("    seed 2 was skipped; the control is unavailable"),
    }

    // =====================================================================
    // §6.5 — H4, and §6.6 — the null detector. Both are per accumulation
    // rule, so they share one pass over the table.
    // =====================================================================
    println!("\n=== §6.5 H4 / §6.6 THE NULL — DIVERGENCE AND SATURATION, PER RULE ===");
    println!(
        "  saturation is measured against the HOLDER's own people's ladder (campaign 3's \
         definition). Under contact a claim may have been resolved against a foreign, longer \
         ladder — the 'beyond' column counts exactly those."
    );
    println!(
        "  READ 'beyond' AS A TRIPWIRE, NOT AS EVIDENCE: every people on this panel carries \
         the same NUMBER of rungs (the astronomical rungs plus generation and lifespan), so \
         a foreign resolving ladder is never LONGER than the holder's own and the column is \
         0 throughout. It makes spec §5.4's originating-witness freeze VISIBLE only in a \
         world where ladder lengths differ; it says nothing about this one."
    );
    for (ri, rule) in Accumulation::ALL.iter().enumerate() {
        println!("\n  === RULE: {} ===", rule.label());
        println!(
            "    {:<10} {:>10} {:>10} {:>9} {:>7} {:>9} {:>7} {:>10} {:>10} {:>9}",
            "arm",
            "held",
            "saturated",
            "sat%",
            "beyond",
            "compared",
            "mutex",
            "one-sided",
            "identical",
            "any-pair"
        );
        let mut mutex = [0usize; 2];
        let mut one_sided = [0usize; 2];
        let mut identical = [0usize; 2];
        let mut any_pair = [0usize; 2];
        let mut sat_pct = [0.0f64; 2];
        for (ai, contact) in Contact::ALL.iter().enumerate() {
            let held: usize = rows.iter().map(|r| r.rungs[ai][ri].held).sum();
            let saturated: usize = rows.iter().map(|r| r.rungs[ai][ri].saturated).sum();
            let beyond: usize = rows.iter().map(|r| r.rungs[ai][ri].beyond_own_ladder).sum();
            let compared: usize = rows.iter().map(|r| r.div[ai][ri].compared).sum();
            let m: usize = rows.iter().map(|r| r.div[ai][ri].mutually_exclusive).sum();
            let one: usize = rows.iter().map(|r| r.div[ai][ri].one_sided).sum();
            let same: usize = rows.iter().map(|r| r.div[ai][ri].identical).sum();
            let any: usize = rows.iter().map(|r| r.div[ai][ri].any_pair_exclusive).sum();
            mutex[ai] = m;
            one_sided[ai] = one;
            identical[ai] = same;
            any_pair[ai] = any;
            sat_pct[ai] = pct(saturated, held);
            println!(
                "    {:<10} {held:>10} {saturated:>10} {:>8.2}% {beyond:>7} {compared:>9} \
                 {m:>7} {one:>10} {same:>10} {any:>9}",
                contact.label(),
                sat_pct[ai]
            );
            let mut hist: BTreeMap<u8, usize> = BTreeMap::new();
            for r in &rows {
                for (k, v) in &r.rungs[ai][ri].hist {
                    *hist.entry(*k).or_default() += v;
                }
            }
            println!("      rung histogram: {}", show_hist(&hist));
        }
        let prefix_mutex: usize = rows
            .iter()
            .filter(|r| r.seed < BASELINE_PREFIX)
            .map(|r| r.div[0][ri].mutually_exclusive)
            .sum();
        println!(
            "    seeds 0-11, descent, mutually exclusive: {prefix_mutex} under {}. \
             Spec §3.4's published figure is {BASELINE_MUTUALLY_EXCLUSIVE_12}, measured \
             under MULTIPLICATIVE only -- so it is the comparison figure on the \
             multiplicative line of this table and on no other.",
            rule.label()
        );
        let ratio = mutex[1] as f64 / mutex[0].max(1) as f64;
        println!(
            "    H4 (§6.5) under {}: descent {} -> contact {} = {ratio:.2}x  [predicted > \
             {H4_FACTOR}x] -> {}",
            rule.label(),
            mutex[0],
            mutex[1],
            if mutex[0] == 0 && mutex[1] == 0 {
                "NO VERDICT (no divergence on either arm)"
            } else if ratio > H4_FACTOR {
                "CONFIRMED"
            } else {
                "FALSIFIED"
            }
        );
        println!(
            "    WHERE THE FALSIFICATION WENT, over the same compared population: mutually \
             exclusive {} -> {}, one-sided {} -> {}, identical {} -> {}.",
            mutex[0], mutex[1], one_sided[0], one_sided[1], identical[0], identical[1],
        );
        // CONDITIONAL, like every other interpretive line here. An earlier
        // draft printed the pooling conclusion unconditionally beside these
        // six numbers, which would have read as a derivation on a future run
        // that contradicted it. A sentence that cannot be wrong is not a
        // finding.
        println!(
            "      {}",
            if identical[1] > identical[0] && mutex[1] < mutex[0] {
                "POOLING: identical day sets RISE while mutually-exclusive ones FALL, so \
                 contact does not merely fail to raise disagreement between the two sides of \
                 a raid — the two sides agree MORE often under it. A seam is a channel in \
                 BOTH directions, so each side receives the other's tellings and each keeps \
                 whichever it can reach least corrupted; the accounts pool rather than \
                 diverge. That is a mechanism §6.5 did not consider and §6.6's named null \
                 does not describe."
            } else {
                "NOT POOLING on this rule: identical day sets did not rise while \
                 mutually-exclusive ones fell, so whatever moved H4 here is not the two \
                 sides' accounts converging, and the pooling reading must not be carried \
                 over from another rule."
            }
        );
        // AS A RATE, NOT A COUNT. `any_pair_exclusive`'s eligible population
        // is "endings reaching 2+ peoples", and contact grows THAT population
        // by construction, because contact is strictly adding. A bare count
        // ratio here is therefore mostly the denominator moving, not the
        // phenomenon: on the 40-seed panel the counts read 15-33x while the
        // rates read 2.1-4.5x, a roughly four-fold inflation — and the count
        // was the number about to reach the chronicle.
        let eligible = [
            rows.iter().map(|r| r.reach[0].two_plus).sum::<usize>(),
            rows.iter().map(|r| r.reach[1].two_plus).sum::<usize>(),
        ];
        let any_rate = [pct(any_pair[0], eligible[0]), pct(any_pair[1], eligible[1])];
        println!(
            "    SECONDARY, NOT PREREGISTERED and therefore unable to discharge H4: endings \
             where SOME pair of holding peoples is mutually exclusive. AS A RATE over the \
             eligible population (endings reaching 2+ peoples): descent {}/{} = {:.2}% -> \
             contact {}/{} = {:.2}%, a {:.2}x RATE change.",
            any_pair[0],
            eligible[0],
            any_rate[0],
            any_pair[1],
            eligible[1],
            any_rate[1],
            any_rate[1] / any_rate[0].max(f64::MIN_POSITIVE),
        );
        println!(
            "      the bare COUNT ratio is {:.2}x and must not be quoted: the eligible \
             population itself grows {:.2}x under contact ({} -> {} endings) because contact \
             is strictly adding, so most of a count ratio is the denominator. §6.5's frozen \
             measure compares the VICTIM's people against the RAIDER's people and no other \
             pair — its population is pinned at {} on both arms — so it is blind by \
             definition to the pairs contact creates. Reported for interpretation only; the \
             verdict above stands as frozen.",
            any_pair[1] as f64 / any_pair[0].max(1) as f64,
            eligible[1] as f64 / eligible[0].max(1) as f64,
            eligible[0],
            eligible[1],
            rows.iter().map(|r| r.div[0][ri].compared).sum::<usize>(),
        );
        println!(
            "    §6.6 null under {}: saturated {:.2}% (descent) -> {:.2}% (contact); the null \
             is LIVE if saturation rises while divergence falls -> {}",
            rule.label(),
            sat_pct[0],
            sat_pct[1],
            if sat_pct[1] > sat_pct[0] && mutex[1] <= mutex[0] {
                "NULL FIRES: the mechanism works and the phenomenon vanishes"
            } else if sat_pct[1] > sat_pct[0] {
                "saturation rises but divergence rises too — the null does not explain H4"
            } else {
                "saturation does not rise; the named null is not what happened"
            }
        );
    }

    // =====================================================================
    // SUBSTRATE CONTROLS ONLY (spec §6.7). No hypothesis is asserted.
    // =====================================================================
    assert!(
        skipped.len() < PANEL.len(),
        "control: every panel seed was skipped for want of a year rung"
    );
    assert!(!rows.is_empty(), "control: the panel produced no seeds");
    for (ai, contact) in Contact::ALL.iter().enumerate() {
        for (ri, rule) in Accumulation::ALL.iter().enumerate() {
            let held: usize = rows.iter().map(|r| r.rungs[ai][ri].held).sum();
            assert!(
                held > 0,
                "control: arm {}/{} produced no held claims at all",
                contact.label(),
                rule.label()
            );
        }
    }
    let beyond_world: usize = rows.iter().map(|r| r.rung_beyond_world).sum();
    assert_eq!(
        beyond_world, 0,
        "control: {beyond_world} claims report a rung longer than any ladder in their own \
         world — no ladder could have resolved one"
    );
    let descent_beyond: usize = rows
        .iter()
        .flat_map(|r| r.rungs[0].iter())
        .map(|rr| rr.beyond_own_ladder)
        .sum();
    assert_eq!(
        descent_beyond, 0,
        "control: under descent the resolving ladder IS the holder's own people's ladder \
         (descent never crosses a people boundary), so a rung past it is impossible"
    );
    let descent_three_plus: usize = rows.iter().map(|r| r.reach[0].three_plus).sum();
    assert_eq!(
        descent_three_plus, 0,
        "control: the shipped descent walk must never reach 3 peoples (spec §3.3); if it does, \
         this readout is not walking the shipped graph"
    );
    let descent_events: usize = rows.iter().map(|r| r.reach[0].events).sum();
    let descent_two_plus: usize = rows.iter().map(|r| r.reach[0].two_plus).sum();
    let descent_two_pct = pct(descent_two_plus, descent_events);
    assert!(
        descent_two_plus > 0 && descent_two_pct < 10.0,
        "control: the descent arm must reproduce §3.3's THIN shape (2.33% of endings reach 2+ \
         peoples); measured {descent_two_pct:.2}% over {descent_events} endings"
    );
    let clock_added: usize = rows.iter().map(|r| r.clock_added).sum();
    assert_eq!(
        clock_added, 0,
        "control: the clock is strictly removing (spec §5.1); it added a holder on \
         {clock_added} endings"
    );
    let contact_removed: usize = rows.iter().map(|r| r.contact_removed).sum();
    assert_eq!(
        contact_removed, 0,
        "control: contact is strictly adding (spec §5.3); it removed a holder on \
         {contact_removed} endings"
    );
    let rule_disagreements: usize = rows.iter().map(|r| r.rule_disagreements).sum();
    assert_eq!(
        rule_disagreements, 0,
        "control: reach is graph reachability and cannot depend on the accumulation rule, \
         which only orders rival tellings; {rule_disagreements} endings disagreed"
    );

    // ---- THE TIGHTEST CONTROL THIS CAMPAIGN HAS ----
    //
    // Spec §3's substrate panel (seeds 0–11) is a strict PREFIX of this one,
    // so §3.1's and §3.4's published figures are re-derivable IN THIS RUN by
    // this readout's own instrument. They were printed before and asserted
    // now, because a printed figure that drifts reddens nothing: the only
    // asserted shape check above is `descent_two_pct < 10.0`, a 4x margin
    // against a measured 2.32%.
    //
    // WHAT A RED HERE MEANS, and it is not "the readout broke". These are
    // facts about the BAKE, and spec §3.1 already warns the denominator moves
    // with main (campaign 2 published 17 of 474 for seed 42 where §3 measured
    // 16 of 585). So a red is this ratchet firing: re-derive the figures,
    // decide deliberately whether the move was intended, and update these
    // constants IN THE COMMIT THAT MOVED THE BAKE. Never rebaseline them to
    // go green on a run whose cause is unexplained.
    let prefix_endings_measured: usize = rows
        .iter()
        .filter(|r| r.seed < BASELINE_PREFIX)
        .map(|r| r.endings)
        .sum();
    let prefix_foreign_measured: usize = rows
        .iter()
        .filter(|r| r.seed < BASELINE_PREFIX)
        .map(|r| r.foreign)
        .sum();
    assert_eq!(
        prefix_endings_measured, BASELINE_ENDINGS_12,
        "control: seeds 0-11 must re-derive spec §3.1's ending count exactly"
    );
    assert_eq!(
        prefix_foreign_measured, BASELINE_FOREIGN_12,
        "control: seeds 0-11 must re-derive spec §3.1's foreign-attacker count exactly"
    );
    let multiplicative = Accumulation::ALL
        .iter()
        .position(|r| *r == Accumulation::Multiplicative)
        .expect("Accumulation::ALL carries Multiplicative");
    let prefix_mutex_measured: usize = rows
        .iter()
        .filter(|r| r.seed < BASELINE_PREFIX)
        .map(|r| r.div[0][multiplicative].mutually_exclusive)
        .sum();
    assert_eq!(
        prefix_mutex_measured, BASELINE_MUTUALLY_EXCLUSIVE_12,
        "control: seeds 0-11 under descent + multiplicative must re-derive spec §3.4's \
         mutually-exclusive count exactly"
    );
}

// ===========================================================================
// THE NON-VACUITY CONTROLS. Cheap, hand-built, and NOT ignored.
//
// The panel's own controls prove the arms are ordered (clock removes, contact
// adds) but not that this file's ACCOUNTING can move: a readout that summed
// the wrong field would satisfy every subset assertion above and print a
// constant. These push each headline counter through the same `measure_seed`
// the panel runs.
// ===========================================================================

/// Assemble a [`WorldRead`] over a hand-built ledger, with one 50-day
/// generation and 150-day lifespan for every named people — the same
/// durations `tests/augmented_walk.rs` uses, so the fixtures' documented
/// widths carry over.
fn hand_read(led: &Ledger, peoples: &[&str]) -> WorldRead {
    let lineage = lineage_of(led);
    let contact = contact_of(led);
    let mut durations = PeopleDurations::default();
    for p in peoples {
        durations.insert(p, StdDays::new(50.0).ok(), StdDays::new(150.0).ok());
    }
    let ladders = PeopleLadders::of(led, &durations);
    let astronomical = PrecisionLadder::of(led);
    let mut people_of: BTreeMap<EntityId, String> = BTreeMap::new();
    for occ in lineage.all() {
        if let Some(Value::Text(p)) = led.value_of(occ, hornvale_history::OCC_PEOPLE) {
            people_of.insert(occ, p.clone());
        }
    }
    let mut max_rungs = astronomical.len();
    for p in durations.peoples() {
        max_rungs = max_rungs.max(ladders.for_people(p).len());
    }
    WorldRead {
        lineage,
        contact,
        durations,
        ladders,
        astronomical,
        people_of,
        endings: endings_of(led),
        max_rungs,
    }
}

/// H3's counter must be able to MOVE between the two `Contact` arms through
/// `measure_seed` itself. Without this, a panel showing no difference could be
/// a null or could be a readout that never varied the arm.
#[test]
fn the_contact_arm_moves_the_reach_counter_in_the_control() {
    let led = two_peoples_joined_by_a_later_raid();
    let read = hand_read(&led, &["human", "kobold"]);
    let row = measure_seed(0, &led, &read);

    assert_eq!(row.endings, 2, "the fixture carries two endings");
    assert!(
        row.reach[1].two_plus > row.reach[0].two_plus,
        "the seam must carry an account across a people boundary that descent does not: \
         descent {} vs contact {}",
        row.reach[0].two_plus,
        row.reach[1].two_plus,
    );
    assert_eq!(
        row.contact_removed, 0,
        "contact is strictly adding on the control too"
    );
    assert_eq!(
        row.rule_disagreements, 0,
        "reach must not depend on the accumulation rule on the control either"
    );
}

/// H1's counter must be able to move: the clock removes a holder, and the
/// directly-dead count is not the same thing as the removal count.
#[test]
fn the_clock_moves_the_holder_counter_in_the_control() {
    let led = a_holder_that_died_before_the_event();
    let read = hand_read(&led, &["human"]);
    let row = measure_seed(0, &led, &read);

    assert!(
        row.holders_clock < row.holders_no_clock,
        "the clock must remove somebody: {} with, {} without",
        row.holders_clock,
        row.holders_no_clock,
    );
    assert!(
        row.dead_holders > 0,
        "the fixture's dead community must be counted as directly dead"
    );
    assert!(
        row.holders_no_clock - row.holders_clock > row.dead_holders,
        "the blast radius must exceed the directly-dead count — 3 is dead and 4 is orphaned \
         below it: removed {} vs dead {}",
        row.holders_no_clock - row.holders_clock,
        row.dead_holders,
    );
    assert_eq!(row.clock_added, 0, "the clock never adds on the control");
}
