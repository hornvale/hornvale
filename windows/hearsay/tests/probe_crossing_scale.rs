//! Scale probe (The Undertow, Myth campaign 5): **is the crossing penalty
//! inert on a real world?**
//!
//! Task 2 shipped `Crossing::ContactWeighted`, which adds
//! `span(FINEST) / (1 + edges_between(a, b))` to the accumulating width at
//! every cross-people step. Task 2's own implementer and its reviewer then
//! found a defect in the spec's substrate reasoning, and it is the reason this
//! probe exists:
//!
//! - A cross-people step across the seam **is** a raid edge, and
//!   `contact::contact_of` builds the peers list and the people-pair tally in
//!   the same loop from the same ending record. So any seam crossing has
//!   already incremented its own pair's count: `edges_between >= 1` there, the
//!   denominator is `>= 2`, and the penalty is **at most `span(FINEST)/2`**.
//! - Spec §5.1's worked reading — that two peoples with no shared history pay
//!   the full rung — describes a step that, across the seam, cannot happen:
//!   zero edges between two peoples means no seam edge joins them at all.
//! - The reviewer's qualification, which this probe measures rather than
//!   assumes: that ceiling is a fact about the **bake**, not an invariant.
//!   `derive::tellable` also yields `lineage.children_of`, and nothing in code
//!   forbids a child carrying a different people from its parent. "Fission
//!   never crosses a people boundary" is a committed *measurement* (campaign
//!   2 §3.1, zero of 780 typed edges), not a rule. A descent crossing would be
//!   the one place `edges_between == 0` is reachable, so M1 counts
//!   crossings by which kind of edge carried them.
//!
//! The consequence, and the question: real ladders step from a day rung to a
//! next rung tens of times longer, while the penalty is at most half a day
//! rung. **The mechanism may be inert on real worlds while passing every
//! hand-built assertion in `tests/crossing.rs`.** If so, a 40-seed readout
//! built on top of it would report a null caused by the spec's arithmetic
//! rather than by the world — a finding-shaped non-finding.
//!
//! ## What is measured
//!
//! - **M1 — the realized penalty distribution.** At every crossing taken by a
//!   *winning* path under `Crossing::ContactWeighted`, the `edges_between` that
//!   priced it and the penalty that resulted, split by whether a seam edge or
//!   a descent edge carried it.
//! - **M2 — THE DECISIVE ONE.** `Crossing::Free` against
//!   `Crossing::ContactWeighted`, everything else at `AS_SHIPPED` with
//!   `Contact::WithRaidSeam`, per accumulation rule: how many holders' RETAINED
//!   RUNG differs, how many holders' REMEMBERED DAY differs, and how many
//!   holders' ACCUMULATED WIDTH differs at all. **The third against the first
//!   is the whole point**: a width that moves without moving a rung is the
//!   mechanism working and being invisible at emit.
//! - **M3 — the ladder context.** The rungs actually in play: how many distinct
//!   ladders the panel carries, `span(FINEST)`, the next rung above it, and
//!   the ratio between them. The arithmetic that would make the mechanism
//!   inert, measured rather than assumed.
//! - **M4 — the tercile split spec §6.2 (H2) preregisters.** Where the
//!   observed crossings sit in `edges_between`, and whether a tercile split
//!   over the observed people-pairs has members at all — a tercile with no
//!   members would make H2 unmeasurable before Task 4 spends a 40-seed run
//!   finding out.
//! - **M5 — WHAT WOULD MOVE A RUNG, MEASURED RATHER THAN EXTRAPOLATED.** This
//!   is the one place this probe goes beyond its brief, and the reason is that
//!   the analytic answer is not trustworthy: scaling the penalty changes which
//!   route wins, so "how much headroom is left to the next rung" is only an
//!   estimate of the multiplier needed. So the probe **runs the walk again at
//!   each of [`SWEEP`]'s multipliers** and reports where rung movement
//!   actually begins. The headroom distribution is reported beside it as the
//!   cheap analytic cross-check, not as the answer.
//!
//! ## Why this file reimplements the walk, and what holds it honest
//!
//! `variants_about_accumulating` returns `Vec<Claim>`, and a `Claim` carries
//! neither the accumulated width nor the route that produced it — so M1's
//! "which crossings did the winner take", M2's width column, and M5's scaled
//! penalty are all unaskable through the shipped signature. [`Probe::walk`] is
//! therefore a copy of that relaxation carrying three extra things: a penalty
//! SCALE, the width at emit, and the crossings of the telling each holder ends
//! up keeping.
//!
//! A reimplementation is a second instrument and could disagree with the first
//! silently, so it is held to the original on every ending and every rule, not
//! sampled: **scale 0.0 must reproduce `variants_about_accumulating` under
//! `Crossing::Free` holder for holder, and scale 1.0 must reproduce it under
//! `Crossing::ContactWeighted`** — same hop count, same rung, same remembered
//! day. `1.0 * x` and `0.0 * x` are exact in IEEE-754, so the scaled penalty
//! is bit-identical to the shipped one at those two multipliers and the
//! control is exact rather than approximate.
//!
//! `Contact::WithRaidSeam` and `Clock::Off` throughout, matching
//! `Transmission::AS_SHIPPED` and The Parley's own contact arms: this
//! campaign's arm is what a crossing COSTS, so holding the graph fixed is what
//! keeps the difference attributable.
//!
//! Reports only. Every assertion is a POSITIVE CONTROL — reproducing a
//! published count, or proving the probe reached the population it reports on.
//! No assertion here is about an outcome. **A zero is the expected worrying
//! answer here**, so telling one apart from a broken instrument matters more
//! than usual.
//!
//! ## Cost, measured, so the next reader budgets from a number and not a guess
//!
//! On this Mac (`test` profile, which this workspace builds optimized):
//! **54.1 s** of test time, 56.8 s wall including the incremental build, for
//! 12 seeds. That is 3 rules x (1 Free walk + 9 [`SWEEP`] walks + 2 shipped
//! equivalence-control walks) = 36 walks per ending over 5,913 endings, of
//! which a third of the wall time is `build_world` and none of it is the
//! walks' fault individually. A committed cost is a claim with a date;
//! re-measure rather than extrapolate.

use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::accumulate::{Accumulation, precision_at};
use hornvale_hearsay::amplitude::gen_span;
use hornvale_hearsay::contact::{Contact, ContactGraph, contact_of};
use hornvale_hearsay::derive::{variants_about_accumulating, witnesses_of};
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_hearsay::ladder::{PeopleLadders, PrecisionLadder};
use hornvale_hearsay::lineage::{Lineage, lineage_of};
use hornvale_hearsay::transmission::{Crossing, Transmission, Walk};
use hornvale_kernel::Claim;
use hornvale_kernel::Precision;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use hornvale_kernel::provenance::Provenance;
use std::collections::{BTreeMap, BTreeSet};

/// The 12-seed panel: a strict prefix of the census panel (`the-census` runs
/// seeds 0-999), of The Parley's 40-seed readout panel, and of both of this
/// campaign's earlier probes — so nothing here samples a population an earlier
/// instrument did not.
const PANEL: [u64; 12] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];

/// The predicate every claim here is about. Only an ending has parties beyond
/// its subject (The Palimpsest §6.1), so it is the only predicate a
/// transmission graph can be built over.
const PREDICATE: &str = hornvale_history::OCC_ENDED;

// ---------------------------------------------------------------------------
// The controls this probe is held to, READ OUT OF `parley_readout.rs`'s OWN
// CONSTANTS at this commit rather than out of any spec's prose. Main moves;
// the committed constant is the live claim.
// ---------------------------------------------------------------------------

/// `parley_readout.rs::BASELINE_ENDINGS_12`: endings over seeds 0-11.
///
/// **RE-DERIVED AGAINST THE MERGE PRODUCT.** This pair read 5,913 / 138 for
/// the whole of this campaign's execution. The Underworld changed settlement
/// placement and `parley_readout.rs`'s own controls were re-pinned at
/// `44ea8d5a`; the committed constant over there is still the live claim and
/// this file follows it.
const BASELINE_ENDINGS_12: usize = 4975;

/// `parley_readout.rs::BASELINE_FOREIGN_12`: endings over seeds 0-11 whose
/// attacker is of another people.
const BASELINE_FOREIGN_12: usize = 102;

/// The multipliers M5 re-walks the panel at. `1.0` is the shipped magnitude
/// and doubles as M2's `ContactWeighted` arm, so it is not walked twice.
///
/// Geometric because the question is an order of magnitude, not a value: the
/// ladder gap M3 measures is a ratio, so the answer "the numerator would have
/// to be ~N finest rungs" is what a spec correction needs.
const SWEEP: [f64; 9] = [1.0, 2.0, 4.0, 8.0, 16.0, 32.0, 64.0, 128.0, 256.0];

/// Which kind of edge carried a cross-people step.
///
/// The distinction is the reviewer's qualification made measurable: a SEAM
/// crossing has `edges_between >= 1` by construction, because `contact_of`
/// tallies the pair from the very ending that created the edge. A DESCENT
/// crossing — a child of another people than its parent — is the only place
/// `edges_between == 0`, and therefore the spec's full-price penalty, is
/// reachable at all.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Carrier {
    /// A parent -> child step down the founding tree.
    Descent,
    /// An undirected raid edge from `ContactGraph::peers_of`.
    Seam,
}

impl Carrier {
    /// This carrier's short name, used as a readout column.
    fn label(self) -> &'static str {
        match self {
            Carrier::Descent => "descent",
            Carrier::Seam => "seam",
        }
    }
}

/// One cross-people step taken by a winning path.
#[derive(Clone, Copy, Debug)]
struct Crossed {
    /// `ContactGraph::edges_between` for the two peoples, which prices it.
    edges: usize,
    /// Which kind of edge carried it.
    carrier: Carrier,
}

/// One node's best-known telling, as the relaxation converges toward it.
///
/// Identical to `derive.rs`'s private `Telling` but for `crossings`, which
/// carries the cross-people steps of the route that produced this telling.
/// Carrying the list forward rather than reconstructing it from predecessors
/// is deliberate: the key rises strictly along every edge, so a predecessor
/// chain would also be well-defined, but a list needs no argument to be right.
struct Telling<'a> {
    /// This telling's rank against any rival reaching the same holder.
    key: (u64, u32, EntityId),
    /// Accumulated damage width, at full precision.
    width: f64,
    /// The ORIGINATING witness's people's ladder, fixed for the whole path.
    ladder: &'a PrecisionLadder,
    /// The claim as this holder received it.
    claim: Claim,
    /// The cross-people steps of this telling's route, in traversal order.
    crossings: Vec<Crossed>,
}

/// One holder's held claim, with the width and route that reached it.
struct Held<'a> {
    /// The claim, exactly as the shipped walk would report it.
    claim: Claim,
    /// The accumulated width at emit — the quantity the shipped `Claim`
    /// discards once it has resolved a rung from it, and the quantity M2 asks
    /// about.
    width: f64,
    /// The ORIGINATING witness's people's ladder, which priced every crossing
    /// on this route and resolved this claim's rung.
    ladder: &'a PrecisionLadder,
    /// The cross-people steps of the winning route, in traversal order. Empty
    /// for a witness and for anyone reached without ever leaving its people.
    crossings: Vec<Crossed>,
}

/// The people `occ` belongs to, or `""` when the ledger does not say — the
/// same read `derive.rs`'s private `people_of` and `contact_of` both perform,
/// so the empty string is a people of its own name on both sides rather than a
/// sentinel.
fn people_of(led: &Ledger, occ: EntityId) -> &str {
    match led.value_of(occ, hornvale_history::OCC_PEOPLE) {
        Some(Value::Text(p)) => p.as_str(),
        _ => "",
    }
}

/// Everything one world's walks need, assembled once.
struct Probe<'a> {
    /// The committed ledger.
    led: &'a Ledger,
    /// The founding tree.
    lineage: &'a Lineage,
    /// The raid seam, exactly as `contact::contact_of` builds it — this probe
    /// needs no copy of it, unlike the direction probe, because it restricts
    /// nothing about the graph.
    contact: &'a ContactGraph,
    /// One ladder per people.
    ladders: &'a PeopleLadders,
    /// Per-people generation and lifespan, in std days.
    durations: &'a PeopleDurations,
}

impl<'a> Probe<'a> {
    /// Who `node` can tell under `Contact::WithRaidSeam`, ascending, each
    /// paired with the carrier it would be told across.
    ///
    /// **A hearer reachable BOTH ways is attributed to descent.** The shipped
    /// `tellable` sorts and dedups by `EntityId` alone, so it expands such a
    /// hearer exactly once; this keeps that, and resolves the tie toward the
    /// step that needs no seam. The attribution cannot change any width — the
    /// penalty reads the two peoples, never the carrier — so it affects only
    /// M1's split, and `Carrier::Descent` sorting before `Carrier::Seam` is
    /// what makes the sort do the resolution.
    fn tellable(&self, node: EntityId, event_day: Option<f64>) -> Vec<(EntityId, Carrier)> {
        let mut out: Vec<(EntityId, Carrier)> = self
            .lineage
            .children_of(node)
            .iter()
            .map(|c| (*c, Carrier::Descent))
            .collect();
        for (peer, day) in self.contact.peers_of(node) {
            if event_day.is_none_or(|event| *day >= event) {
                out.push((*peer, Carrier::Seam));
            }
        }
        out.sort();
        out.dedup_by_key(|(id, _)| *id);
        out
    }

    /// `derive.rs`'s private `crossing_penalty`, with a scale factor and the
    /// `edges_between` that priced it handed back.
    ///
    /// `scale == 0.0` is `Crossing::Free` and `scale == 1.0` is
    /// `Crossing::ContactWeighted`; both multiplications are exact in
    /// IEEE-754, which is what makes the equivalence controls exact.
    fn penalty(
        &self,
        ladder: &PrecisionLadder,
        teller: EntityId,
        hearer: EntityId,
        scale: f64,
    ) -> (f64, Option<usize>) {
        let (from, to) = (people_of(self.led, teller), people_of(self.led, hearer));
        if from == to {
            return (0.0, None);
        }
        let edges = self.contact.edges_between(from, to);
        let unit = ladder
            .span(Precision::FINEST)
            .map(|days| days.get())
            .unwrap_or(0.0);
        (scale * (unit / (1.0 + edges as f64)), Some(edges))
    }

    /// `variants_about_accumulating` with a penalty scale, carrying the width
    /// and the winning route's crossings out.
    ///
    /// Line for line the shipped relaxation, minus the clock (every arm here
    /// runs `Clock::Off`, so `admits` is unconditionally true and the branches
    /// would be dead) and plus the three things the shipped signature cannot
    /// express. The termination argument is unchanged and is the shipped one:
    /// width is non-decreasing so the key's primary component never falls, and
    /// at equal width `hops` rises by exactly one per edge, so the key rises
    /// strictly along every edge and a node is final when it is popped. A
    /// larger `scale` only ever makes a step wider, so it cannot weaken that.
    fn walk(&self, rule: Accumulation, scale: f64, subject: EntityId) -> Vec<Held<'a>> {
        let led = self.led;
        let Some(object) = led.value_of(subject, PREDICATE) else {
            return Vec::new();
        };
        let base = Claim {
            holder: subject,
            subject,
            predicate: PREDICATE.to_string(),
            object: object.clone(),
            grade: Provenance::Witnessed,
            hops: 0,
            precision: Precision::FINEST,
        };
        let witnesses = witnesses_of(led, self.lineage, subject, PREDICATE);
        let witness_set: BTreeSet<EntityId> = witnesses.iter().copied().collect();
        let event_day = match led.value_of(subject, PREDICATE) {
            Some(Value::Number(day)) => Some(*day),
            _ => None,
        };

        let mut reached: BTreeMap<EntityId, Telling<'a>> = BTreeMap::new();
        let mut frontier: BTreeSet<((u64, u32, EntityId), EntityId)> = BTreeSet::new();

        for w in &witnesses {
            let people = people_of(led, *w).to_string();
            let ladder = self.ladders.for_people(&people);
            let width = ladder
                .span(Precision::FINEST)
                .map(|days| days.get())
                .unwrap_or(0.0);
            let mut c = base.clone();
            c.holder = *w;
            let key = (0, 0, *w);
            reached.insert(
                *w,
                Telling {
                    key,
                    width,
                    ladder,
                    claim: c,
                    crossings: Vec::new(),
                },
            );
            frontier.insert((key, *w));
        }

        while let Some((key, node)) = frontier.iter().next().copied() {
            frontier.remove(&(key, node));
            let Some(telling) = reached.get(&node) else {
                continue;
            };
            if telling.key != key {
                continue; // a better telling replaced this one before it was relaxed
            }
            let (width, ladder, claim, witness) =
                (telling.width, telling.ladder, telling.claim.clone(), key.2);
            let carried = telling.crossings.clone();

            for (hearer, carrier) in self.tellable(node, event_day) {
                if witness_set.contains(&hearer) {
                    continue; // a witness is never demoted to an inheritor
                }
                let (penalty, edges) = self.penalty(ladder, node, hearer, scale);
                let span = gen_span(led, self.durations, node, hearer) + penalty;
                let next_width = rule.step(width, span);
                let precision = precision_at(ladder, next_width);
                let next_object = match &claim.object {
                    Value::Number(day) => Value::Number(ladder.apply(precision, *day)),
                    other => other.clone(),
                };
                let next_claim = claim.retold_by_lossy(hearer, precision, next_object);
                let next_key = (next_width.to_bits(), next_claim.hops, witness);
                match reached.get(&hearer) {
                    Some(held) if held.key <= next_key => continue,
                    Some(held) => {
                        frontier.remove(&(held.key, hearer));
                    }
                    None => {}
                }
                let mut crossings = carried.clone();
                if let Some(edges) = edges {
                    crossings.push(Crossed { edges, carrier });
                }
                reached.insert(
                    hearer,
                    Telling {
                        key: next_key,
                        width: next_width,
                        ladder,
                        claim: next_claim,
                        crossings,
                    },
                );
                frontier.insert((next_key, hearer));
            }
        }

        reached
            .into_values()
            .map(|t| Held {
                claim: t.claim,
                width: t.width,
                ladder: t.ladder,
                crossings: t.crossings,
            })
            .collect()
    }
}

// ---------------------------------------------------------------------------
// World reading. A near-copy of `parley_readout.rs::read_world` and of both
// earlier probes', deliberately: a probe that assembled the ladder differently
// from the readout whose numbers it re-derives would be measuring a different
// model.
// ---------------------------------------------------------------------------

/// One ending, with the two peoples the seam puts on either side of it.
struct Ending {
    /// The occupation that ended.
    subject: EntityId,
    /// The subject's own people, empty when it names none.
    people: String,
    /// The people of its `occ-ended-by` attacker, when it names an
    /// `Entity`-valued one that itself names a people.
    attacker_people: Option<String>,
}

impl Ending {
    /// Whether the attacker is of another people — The Parley §3.1's 2.33%
    /// population.
    fn is_foreign(&self) -> bool {
        self.attacker_people
            .as_ref()
            .is_some_and(|ap| !ap.is_empty() && !self.people.is_empty() && *ap != self.people)
    }
}

/// Every ending in a ledger, ascending by subject.
fn endings_of(led: &Ledger) -> Vec<Ending> {
    let mut out = Vec::new();
    for fact in led.find(PREDICATE) {
        let subject = fact.subject;
        if !matches!(
            led.value_of(subject, hornvale_history::OCC_ENDED),
            Some(Value::Number(_))
        ) {
            continue;
        }
        let people = people_of(led, subject).to_string();
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
            attacker_people,
        });
    }
    out.sort_by_key(|e| e.subject);
    out.dedup_by_key(|e| e.subject);
    out
}

/// One world, assembled once and shared by every arm.
struct WorldRead {
    /// The founding tree.
    lineage: Lineage,
    /// The raid seam.
    contact: ContactGraph,
    /// Per-people generation and lifespan, in std days.
    durations: PeopleDurations,
    /// One ladder per people.
    ladders: PeopleLadders,
    /// Every ending in the world, ascending by subject.
    endings: Vec<Ending>,
}

/// Assemble one world. `None` when it offers no year rung to convert
/// allometric years into standard days — such a seed is reported as skipped
/// rather than guessed at, exactly as The Parley's readout does.
fn read_world(led: &Ledger, components: &hornvale_worldgen::WorldComponents) -> Option<WorldRead> {
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
    Some(WorldRead {
        lineage,
        contact: contact_of(led),
        durations,
        ladders,
        endings: endings_of(led),
    })
}

/// Build one panel world at full depth.
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

// ---------------------------------------------------------------------------
// The accumulators.
// ---------------------------------------------------------------------------

/// One multiplier's effect against the `Crossing::Free` baseline.
#[derive(Clone, Copy, Default)]
struct SweepCell {
    /// Holders whose accumulated width differs from the Free arm's.
    width_moved: usize,
    /// ... of which the retained precision rung also differs.
    rung_moved: usize,
    /// ... of which the remembered day also differs.
    day_moved: usize,
}

/// Everything one rule contributes over one seed.
#[derive(Clone, Default)]
struct RuleRow {
    /// Held claims under the ContactWeighted arm, over every ending.
    holders: usize,

    // ---- M1 ----
    /// Cross-people steps taken by winning paths, summed over holders.
    crossings: usize,
    /// ... by carrier.
    by_carrier: BTreeMap<Carrier, usize>,
    /// `edges_between` -> how many crossings it priced.
    edges_hist: BTreeMap<usize, usize>,
    /// Every crossing's penalty as a fraction of `span(FINEST)`, i.e.
    /// `1/(1 + edges)`. Kept as values, not a histogram, so a median is exact.
    penalty_over_unit: Vec<f64>,
    /// Every crossing's penalty in std days — the absolute magnitude, which
    /// varies with the originating people's finest rung.
    penalty_days: Vec<f64>,
    /// Holders whose winning path crossed at least once.
    holders_with_crossing: usize,

    // ---- M2 ----
    /// Holders whose accumulated width differs between the two arms.
    width_moved: usize,
    /// ... whose retained precision rung differs.
    rung_moved: usize,
    /// ... whose remembered day differs.
    day_moved: usize,
    /// The `edges_between` of the crossings on the winning path of every
    /// rung-moved holder — M4's population.
    rung_moved_edges: BTreeMap<usize, usize>,

    // ---- the analytic cross-check M5 supersedes ----
    /// For each width-moved holder that is not already at its ladder's
    /// coarsest rung: `(next rung span - free width) / (weighted width - free
    /// width)`, the factor the penalty would need if the winning route did not
    /// change. An ESTIMATE — the route does change — reported beside M5's
    /// measured answer, never in place of it.
    headroom_factors: Vec<f64>,
    /// Width-moved holders already at their ladder's coarsest rung, for which
    /// no headroom is defined.
    at_coarsest: usize,

    // ---- M5 ----
    /// One cell per [`SWEEP`] multiplier.
    sweep: [SweepCell; SWEEP.len()],

    // ---- controls ----
    /// CONTROL: holders on which this probe's scale-0 walk disagreed with the
    /// shipped `Crossing::Free` walk. Must be zero.
    free_mismatch: usize,
    /// CONTROL: the same for scale 1 against `Crossing::ContactWeighted`.
    weighted_mismatch: usize,
    /// CONTROL: endings where the two arms reached different holder SETS. Must
    /// be zero — the penalty prices edges, it does not remove them, so
    /// reachability is identical (spec §6.4).
    holder_set_differs: usize,
    /// CONTROL: crossings whose recorded width was not finite. Must be zero at
    /// the shipped magnitude; reported for every multiplier so a saturating
    /// sweep cell is visible rather than silent.
    nonfinite_widths: usize,
}

/// One people's ladder, as M3 reports it.
#[derive(Clone)]
struct LadderShape {
    /// Which people.
    people: String,
    /// The finest rung's span, in std days.
    finest: f64,
    /// The next rung above it, when the ladder has one.
    next: Option<f64>,
    /// The finest rung's label.
    finest_label: String,
    /// The next rung's label, when there is one.
    next_label: Option<String>,
    /// How many rungs the ladder has.
    rungs: usize,
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
    /// Undirected raid edges in the seam.
    seam_edges: usize,
    /// Per-people ladders — M3.
    ladders: Vec<LadderShape>,
    /// Every unordered people-pair in this world with at least one raid edge
    /// between them, and how many — H2's OWN unit (spec §6.2 terciles
    /// people-pairs, not crossings). Read straight off `ContactGraph`, so it
    /// is the whole population and not only the pairs a winning path happened
    /// to use.
    pairs: Vec<(String, String, usize)>,
    /// Per accumulation rule, indexed by `Accumulation::ALL`.
    rules: [RuleRow; 3],
}

/// Compare two holder maps, holder for holder, on every reported field.
///
/// Returns the number of holders that differ in ANY of presence, hop count,
/// rung, or remembered day. A count rather than a bool so a red says how
/// wrong.
fn mismatches(mine: &[Held], shipped: &[Claim]) -> usize {
    let a: BTreeMap<EntityId, &Claim> = mine.iter().map(|h| (h.claim.holder, &h.claim)).collect();
    let b: BTreeMap<EntityId, &Claim> = shipped.iter().map(|c| (c.holder, c)).collect();
    let mut bad = 0;
    for (holder, x) in &a {
        match b.get(holder) {
            None => bad += 1,
            Some(y) => {
                let same_day = match (&x.object, &y.object) {
                    (Value::Number(p), Value::Number(q)) => p.to_bits() == q.to_bits(),
                    (p, q) => p == q,
                };
                if x.hops != y.hops || x.precision != y.precision || !same_day {
                    bad += 1;
                }
            }
        }
    }
    for holder in b.keys() {
        if !a.contains_key(holder) {
            bad += 1;
        }
    }
    bad
}

/// The remembered day of a claim as raw bits, so a comparison is exact.
fn day_bits(claim: &Claim) -> Option<u64> {
    match &claim.object {
        Value::Number(day) => Some(day.to_bits()),
        _ => None,
    }
}

/// Run the shipped walk under one `Crossing` arm.
fn shipped(
    read: &WorldRead,
    led: &Ledger,
    rule: Accumulation,
    arm: Crossing,
    e: &Ending,
) -> Vec<Claim> {
    variants_about_accumulating(
        &Walk {
            ledger: led,
            lineage: &read.lineage,
            contact: &read.contact,
            policy: Transmission {
                contact: Contact::WithRaidSeam,
                crossing: arm,
                ..Transmission::AS_SHIPPED
            },
        },
        &read.ladders,
        &read.durations,
        rule,
        e.subject,
        PREDICATE,
    )
}

/// Every quantity M1-M5 asks for, over one world.
fn measure_seed(seed: u64, led: &Ledger, read: &WorldRead) -> SeedRow {
    let probe = Probe {
        led,
        lineage: &read.lineage,
        contact: &read.contact,
        ladders: &read.ladders,
        durations: &read.durations,
    };

    let mut ladders: Vec<LadderShape> = Vec::new();
    for people in read.durations.peoples() {
        let l = read.ladders.for_people(people);
        let Some(finest) = l.span(Precision::FINEST) else {
            continue;
        };
        ladders.push(LadderShape {
            people: people.to_string(),
            finest: finest.get(),
            next: l.span(Precision(1)).map(|s| s.get()),
            finest_label: l.label(Precision::FINEST).unwrap_or("?").to_string(),
            next_label: l.label(Precision(1)).map(|s| s.to_string()),
            rungs: l.len(),
        });
    }

    let peoples: Vec<&str> = read.durations.peoples();
    let mut pairs: Vec<(String, String, usize)> = Vec::new();
    for (i, a) in peoples.iter().enumerate() {
        for b in &peoples[i + 1..] {
            let edges = read.contact.edges_between(a, b);
            if edges > 0 {
                pairs.push(((*a).to_string(), (*b).to_string(), edges));
            }
        }
    }

    let mut row = SeedRow {
        seed,
        endings: read.endings.len(),
        seam_edges: read.contact.edges(),
        ladders,
        pairs,
        ..Default::default()
    };

    for e in &read.endings {
        if e.is_foreign() {
            row.foreign += 1;
        }

        for (ri, rule) in Accumulation::ALL.iter().enumerate() {
            let r = &mut row.rules[ri];

            let free = probe.walk(*rule, 0.0, e.subject);
            let free_by: BTreeMap<EntityId, &Held> =
                free.iter().map(|h| (h.claim.holder, h)).collect();

            // ---- THE EQUIVALENCE CONTROLS, on every ending and every rule ----
            r.free_mismatch += mismatches(&free, &shipped(read, led, *rule, Crossing::Free, e));

            for (ki, k) in SWEEP.iter().enumerate() {
                let held = probe.walk(*rule, *k, e.subject);
                let cell = &mut r.sweep[ki];

                let mut set_differs = false;
                for h in &held {
                    if !h.width.is_finite() {
                        r.nonfinite_widths += 1;
                    }
                    let Some(base) = free_by.get(&h.claim.holder) else {
                        set_differs = true;
                        continue;
                    };
                    let width_moved = h.width.to_bits() != base.width.to_bits();
                    let rung_moved = h.claim.precision != base.claim.precision;
                    let day_moved = day_bits(&h.claim) != day_bits(&base.claim);
                    if width_moved {
                        cell.width_moved += 1;
                    }
                    if rung_moved {
                        cell.rung_moved += 1;
                    }
                    if day_moved {
                        cell.day_moved += 1;
                    }

                    // ---- M1, M2 and M4, at the SHIPPED magnitude only ----
                    if ki != 0 {
                        continue;
                    }
                    r.holders += 1;
                    if !h.crossings.is_empty() {
                        r.holders_with_crossing += 1;
                    }
                    let unit = h
                        .ladder
                        .span(Precision::FINEST)
                        .map(|s| s.get())
                        .unwrap_or(0.0);
                    for c in &h.crossings {
                        r.crossings += 1;
                        *r.by_carrier.entry(c.carrier).or_default() += 1;
                        *r.edges_hist.entry(c.edges).or_default() += 1;
                        let share = 1.0 / (1.0 + c.edges as f64);
                        r.penalty_over_unit.push(share);
                        r.penalty_days.push(unit * share);
                    }
                    if width_moved {
                        r.width_moved += 1;
                        // The rung ABOVE the one the Free arm resolved. `span`
                        // returns None past the ladder's end, which is exactly
                        // the "already at the coarsest rung, no headroom to
                        // measure" case.
                        match h.ladder.span(base.claim.precision.coarser()) {
                            Some(next) => {
                                let gap = next.get() - base.width;
                                let delta = h.width - base.width;
                                if delta > 0.0 && gap > 0.0 {
                                    r.headroom_factors.push(gap / delta);
                                }
                            }
                            None => r.at_coarsest += 1,
                        }
                    }
                    if rung_moved {
                        r.rung_moved += 1;
                        for c in &h.crossings {
                            *r.rung_moved_edges.entry(c.edges).or_default() += 1;
                        }
                    }
                    if day_moved {
                        r.day_moved += 1;
                    }
                }
                if ki == 0 {
                    if held.len() != free.len() || set_differs {
                        r.holder_set_differs += 1;
                    }
                    r.weighted_mismatch += mismatches(
                        &held,
                        &shipped(read, led, *rule, Crossing::ContactWeighted, e),
                    );
                }
            }
        }
    }

    row
}

// ---------------------------------------------------------------------------
// Reporting helpers.
// ---------------------------------------------------------------------------

/// `n` as a percentage of `d`, or `0.0` when `d` is zero.
fn pct(n: usize, d: usize) -> f64 {
    100.0 * n as f64 / d.max(1) as f64
}

/// Sum one `usize` field over the panel.
fn sum<F: Fn(&SeedRow) -> usize>(rows: &[SeedRow], f: F) -> usize {
    rows.iter().map(f).sum()
}

/// Min / median / max of a sample, or `None` when it is empty. The median
/// takes the lower of the two central values on an even length — the
/// deterministic tie-break `lib.rs::median_hops` already uses, never an
/// average, so the result is always an observed value.
fn quartet(xs: &mut [f64]) -> Option<(f64, f64, f64)> {
    if xs.is_empty() {
        return None;
    }
    xs.sort_by(|a, b| a.total_cmp(b));
    Some((xs[0], xs[(xs.len() - 1) / 2], xs[xs.len() - 1]))
}

/// Print a `key -> count` distribution compactly.
fn show_hist<K: std::fmt::Display + Ord>(hist: &BTreeMap<K, usize>) -> String {
    if hist.is_empty() {
        return "(none)".to_string();
    }
    hist.iter()
        .map(|(k, v)| format!("{k}={v}"))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Merge one seed's `RuleRow` into a panel-level accumulator.
fn merge(into: &mut RuleRow, from: &RuleRow) {
    into.holders += from.holders;
    into.crossings += from.crossings;
    into.holders_with_crossing += from.holders_with_crossing;
    into.width_moved += from.width_moved;
    into.rung_moved += from.rung_moved;
    into.day_moved += from.day_moved;
    into.at_coarsest += from.at_coarsest;
    into.free_mismatch += from.free_mismatch;
    into.weighted_mismatch += from.weighted_mismatch;
    into.holder_set_differs += from.holder_set_differs;
    into.nonfinite_widths += from.nonfinite_widths;
    for (k, v) in &from.by_carrier {
        *into.by_carrier.entry(*k).or_default() += v;
    }
    for (k, v) in &from.edges_hist {
        *into.edges_hist.entry(*k).or_default() += v;
    }
    for (k, v) in &from.rung_moved_edges {
        *into.rung_moved_edges.entry(*k).or_default() += v;
    }
    into.penalty_over_unit
        .extend_from_slice(&from.penalty_over_unit);
    into.penalty_days.extend_from_slice(&from.penalty_days);
    into.headroom_factors
        .extend_from_slice(&from.headroom_factors);
    for i in 0..SWEEP.len() {
        into.sweep[i].width_moved += from.sweep[i].width_moved;
        into.sweep[i].rung_moved += from.sweep[i].rung_moved;
        into.sweep[i].day_moved += from.sweep[i].day_moved;
    }
}

// ===========================================================================
// M1 / M2 / M3 / M4 / M5 — THE SCALE PROBE.
// ===========================================================================

/// The whole probe over the 12-seed panel.
///
/// claim: structural(seed: panel) — false-positive seed-loop flag; the loop
/// binds a census-panel prefix, not a search over seeds.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn whether_the_crossing_penalty_reaches_the_ladder() {
    let components = hornvale_worldgen::WorldComponents::assemble().expect("components assemble");
    let mut rows: Vec<SeedRow> = Vec::new();
    let mut skipped: Vec<u64> = Vec::new();

    for seed in PANEL {
        let world = build(seed);
        let led = &world.ledger;
        let Some(read) = read_world(led, &components) else {
            skipped.push(seed);
            continue;
        };
        rows.push(measure_seed(seed, led, &read));
    }

    println!("\n============ THE UNDERTOW SCALE PROBE ============");
    println!(
        "panel                : {} seeds (census seeds 0-11), {} measured, skipped {skipped:?}",
        PANEL.len(),
        rows.len()
    );
    println!("predicate            : {PREDICATE}");
    println!(
        "baseline             : {} with Contact::WithRaidSeam — the ONLY thing varied below \
         is Crossing, and (in M5) a scale factor on its penalty",
        Transmission::AS_SHIPPED.label()
    );
    println!(
        "method               : the shipped relaxation, reimplemented to expose the width and \
         the winning route's crossings. Nothing in windows/hearsay/src changes."
    );

    let endings = sum(&rows, |r| r.endings);
    let foreign = sum(&rows, |r| r.foreign);
    let seam_edges = sum(&rows, |r| r.seam_edges);
    println!(
        "\nSUBSTRATE, THIS RUN  : {endings} endings, {foreign} foreign ({:.2}%), {seam_edges} \
         undirected seam edges",
        pct(foreign, endings)
    );
    println!(
        "  controls           : parley_readout.rs pins {BASELINE_ENDINGS_12} endings / \
         {BASELINE_FOREIGN_12} foreign on this panel"
    );

    // =====================================================================
    // M3 — THE LADDER CONTEXT. Printed FIRST because it is the arithmetic
    // every later section is read against.
    // =====================================================================
    println!("\n=== M3 — THE LADDER CONTEXT ===");
    println!(
        "  A crossing's penalty is at most span(FINEST) (and, across the seam, at most half \
         of it). A rung MOVES only when the accumulated width reaches the NEXT rung's span. \
         The ratio below is therefore the arithmetic that decides whether the mechanism can \
         reach the ladder at all."
    );
    let mut shapes: Vec<&LadderShape> = rows.iter().flat_map(|r| r.ladders.iter()).collect();
    shapes.sort_by(|a, b| a.people.cmp(&b.people).then(a.finest.total_cmp(&b.finest)));
    let mut distinct: BTreeMap<(u64, Option<u64>), usize> = BTreeMap::new();
    for s in &shapes {
        *distinct
            .entry((s.finest.to_bits(), s.next.map(f64::to_bits)))
            .or_default() += 1;
    }
    let mut ratios: Vec<f64> = shapes
        .iter()
        .filter_map(|s| s.next.map(|n| n / s.finest))
        .collect();
    println!(
        "  per-people ladders  : {} over the panel, {} DISTINCT (finest, next) pairs",
        shapes.len(),
        distinct.len()
    );
    let rung_counts: BTreeMap<usize, usize> = shapes.iter().fold(BTreeMap::new(), |mut m, s| {
        *m.entry(s.rungs).or_default() += 1;
        m
    });
    println!("  rungs per ladder    : {}", show_hist(&rung_counts));
    println!(
        "\n  {:<6} {:<16} {:>12} {:<10} {:>12} {:<10} {:>9}",
        "seed", "people", "finest (d)", "label", "next (d)", "label", "next/fin"
    );
    for r in &rows {
        for s in &r.ladders {
            println!(
                "  {:<6} {:<16} {:>12.4} {:<10} {:>12} {:<10} {:>9}",
                r.seed,
                s.people,
                s.finest,
                s.finest_label,
                s.next
                    .map(|n| format!("{n:.4}"))
                    .unwrap_or_else(|| "-".to_string()),
                s.next_label.clone().unwrap_or_else(|| "-".to_string()),
                s.next
                    .map(|n| format!("{:.2}x", n / s.finest))
                    .unwrap_or_else(|| "-".to_string()),
            );
        }
    }
    match quartet(&mut ratios) {
        Some((lo, mid, hi)) => println!(
            "\n  THE GAP             : next/finest ranges {lo:.2}x .. {hi:.2}x, median {mid:.2}x \
             over {} ladders with a second rung",
            ratios.len()
        ),
        None => println!("\n  THE GAP             : no ladder on the panel has a second rung"),
    }

    // =====================================================================
    // M1 — THE REALIZED PENALTY DISTRIBUTION.
    // =====================================================================
    println!("\n=== M1 — THE REALIZED PENALTY DISTRIBUTION ===");
    println!(
        "  Every cross-people step taken by a WINNING path under Crossing::ContactWeighted. \
         edges=0 is the spec's full-price case; across the seam it is unreachable by \
         construction, so any edges=0 crossing here came down the founding TREE."
    );
    let mut panel: [RuleRow; 3] = Default::default();
    for r in &rows {
        for (ri, cell) in panel.iter_mut().enumerate() {
            merge(cell, &r.rules[ri]);
        }
    }
    for (ri, rule) in Accumulation::ALL.iter().enumerate() {
        let p = &mut panel[ri];
        println!("\n  --- rule: {} ---", rule.label());
        println!(
            "    held claims                     : {} over {} endings",
            p.holders, endings
        );
        println!(
            "    holders whose winner crossed    : {} ({:.4}% of holders)",
            p.holders_with_crossing,
            pct(p.holders_with_crossing, p.holders)
        );
        println!("    crossings on winning paths      : {}", p.crossings);
        println!(
            "      by carrier                    : {}",
            if p.by_carrier.is_empty() {
                "(none)".to_string()
            } else {
                p.by_carrier
                    .iter()
                    .map(|(k, v)| format!("{}={}", k.label(), v))
                    .collect::<Vec<_>>()
                    .join(" ")
            }
        );
        println!(
            "      edges_between histogram       : {}",
            show_hist(&p.edges_hist)
        );
        let at_zero = *p.edges_hist.get(&0).unwrap_or(&0);
        let at_one = *p.edges_hist.get(&1).unwrap_or(&0);
        println!(
            "      edges=0 (spec's full price)   : {at_zero} ({:.2}% of crossings) — {}",
            pct(at_zero, p.crossings),
            if at_zero == 0 {
                "UNREACHABLE, as the Task 2 review predicted"
            } else {
                "REACHED — a descent step crossed a people boundary; check the carrier split"
            }
        );
        println!(
            "      edges=1 (the reachable ceiling): {at_one} ({:.2}% of crossings)",
            pct(at_one, p.crossings)
        );
        match quartet(&mut p.penalty_over_unit.clone()) {
            Some((lo, mid, hi)) => println!(
                "    penalty / span(FINEST)          : min {lo:.6}, median {mid:.6}, max {hi:.6}"
            ),
            None => println!("    penalty / span(FINEST)          : NO CROSSING WAS EVER TAKEN"),
        }
        match quartet(&mut p.penalty_days.clone()) {
            Some((lo, mid, hi)) => println!(
                "    penalty, std days               : min {lo:.6}, median {mid:.6}, max {hi:.6}"
            ),
            None => println!("    penalty, std days               : NO CROSSING WAS EVER TAKEN"),
        }
    }

    // =====================================================================
    // M2 — THE DECISIVE ONE.
    // =====================================================================
    println!("\n=== M2 — DOES ANY REAL CLAIM'S RUNG MOVE? ===");
    println!(
        "  Crossing::Free against Crossing::ContactWeighted, holder for holder, everything \
         else held at AS_SHIPPED with Contact::WithRaidSeam. WIDTH MOVED without RUNG MOVED \
         is the mechanism working and being invisible at emit."
    );
    println!(
        "\n  {:<16} {:>10} {:>12} {:>12} {:>12} {:>12}",
        "rule", "holders", "width moved", "rung moved", "day moved", "rung/width"
    );
    for (ri, rule) in Accumulation::ALL.iter().enumerate() {
        let p = &panel[ri];
        println!(
            "  {:<16} {:>10} {:>12} {:>12} {:>12} {:>11.2}%",
            rule.label(),
            p.holders,
            p.width_moved,
            p.rung_moved,
            p.day_moved,
            pct(p.rung_moved, p.width_moved)
        );
    }
    let total_rung_moved: usize = panel.iter().map(|p| p.rung_moved).sum();
    let total_width_moved: usize = panel.iter().map(|p| p.width_moved).sum();
    println!(
        "\n  VERDICT             : {} holder-rungs moved across all three rules, out of {} \
         holders whose width moved.",
        total_rung_moved, total_width_moved
    );
    println!(
        "  READ IT AS          : {}",
        if total_width_moved == 0 {
            "NOTHING MOVED AT ALL — not even a width. Either no winning path ever crosses a \
             people boundary on this panel, or the instrument is not applying the penalty. \
             M1's crossing count separates those two."
        } else if total_rung_moved == 0 {
            "THE MECHANISM IS INERT WHERE IT COUNTS. It changes the accumulated width and \
             never once changes a reported rung, so nothing a reader of a world could \
             observe differs between the two arms. A readout built on this would report a \
             null caused by the penalty's magnitude, not by the world."
        } else {
            "THE MECHANISM REACHES THE LADDER. Some holders retain a different rung under \
             the penalty, so the readout has a live population to measure."
        }
    );
    for (ri, rule) in Accumulation::ALL.iter().enumerate() {
        let p = &mut panel[ri];
        match quartet(&mut p.headroom_factors.clone()) {
            Some((lo, mid, hi)) => println!(
                "  headroom, {:<14}: (next rung - free width) / (penalty delta) = min {lo:.1}x, \
                 median {mid:.1}x, max {hi:.1}x over {} width-moved holders ({} already at the \
                 coarsest rung). AN ESTIMATE of the multiplier needed — the winning ROUTE also \
                 changes, which is why M5 measures it instead.",
                rule.label(),
                p.headroom_factors.len(),
                p.at_coarsest
            ),
            None => println!(
                "  headroom, {:<14}: no width-moved holder had a next rung to reach",
                rule.label()
            ),
        }
    }

    // =====================================================================
    // M4 — THE TERCILE SPLIT SPEC §6.2 PREREGISTERS.
    // =====================================================================
    println!("\n=== M4 — IS H2's TERCILE SPLIT MEASURABLE? ===");
    println!(
        "  Spec §6.2 predicts a rise strictly larger in the BOTTOM tercile of contact_edges \
         than the TOP. A tercile split needs the observed edges_between values to VARY; a \
         degenerate distribution makes H2 unmeasurable however the readout is written."
    );
    // H2's unit is the PEOPLE-PAIR, not the crossing, so the pair population
    // is reported first and the crossing-weighted view second. They are
    // different questions and a tercile drawn on one does not transfer to the
    // other: the pair distribution is what §6.2's split partitions, and the
    // crossing distribution is how heavily each part is actually exercised.
    let mut pair_edges: Vec<usize> = rows
        .iter()
        .flat_map(|r| r.pairs.iter().map(|(_, _, e)| *e))
        .collect();
    pair_edges.sort_unstable();
    let pair_hist: BTreeMap<usize, usize> = pair_edges.iter().fold(BTreeMap::new(), |mut m, e| {
        *m.entry(*e).or_default() += 1;
        m
    });
    println!(
        "\n  people-pairs with a seam  : {} over the panel ({} distinct edge counts)",
        pair_edges.len(),
        pair_hist.len()
    );
    println!("  edges_between per pair    : {}", show_hist(&pair_hist));
    if pair_edges.len() >= 3 {
        let n = pair_edges.len();
        let (lo_cut, hi_cut) = (pair_edges[n / 3], pair_edges[(2 * n) / 3]);
        let bottom = pair_edges.iter().filter(|e| **e <= lo_cut).count();
        let top = pair_edges.iter().filter(|e| **e > hi_cut).count();
        let middle = n - bottom - top;
        println!(
            "  tercile cuts by rank      : bottom edges<={lo_cut} ({bottom} pairs) | middle \
             edges {}..={hi_cut} ({middle} pairs) | top edges>{hi_cut} ({top} pairs)",
            lo_cut + 1
        );
        println!(
            "  H2 measurable on pairs    : {}",
            if bottom > 0 && top > 0 && lo_cut != hi_cut {
                "YES — bottom and top terciles both have members and differ in edge count"
            } else {
                "NO — the rank cuts collapse; the pair distribution is too degenerate to                  separate a bottom tercile from a top one"
            }
        );
    } else {
        println!("  H2 measurable on pairs    : NO — fewer than three people-pairs carry a seam");
    }

    for (ri, rule) in Accumulation::ALL.iter().enumerate() {
        let p = &panel[ri];
        let distinct_edges = p.edges_hist.len();
        println!("\n  --- rule: {} ---", rule.label());
        println!(
            "    edges_between over crossings    : {} ({} distinct value(s))",
            show_hist(&p.edges_hist),
            distinct_edges
        );
        println!(
            "    edges_between over RUNG-MOVED   : {}",
            show_hist(&p.rung_moved_edges)
        );
        println!(
            "    tercile split                   : {}",
            if p.crossings == 0 {
                "UNMEASURABLE — no crossing was taken at all".to_string()
            } else if distinct_edges < 3 {
                format!(
                    "DEGENERATE — only {distinct_edges} distinct edges_between value(s) over \
                     {} crossings, so a bottom and top tercile cannot be separated",
                    p.crossings
                )
            } else {
                format!(
                    "POSSIBLE — {distinct_edges} distinct edges_between values over {} crossings",
                    p.crossings
                )
            }
        );
    }

    // =====================================================================
    // M5 — WHAT WOULD MOVE A RUNG, MEASURED.
    // =====================================================================
    println!("\n=== M5 — WHAT WOULD MOVE A RUNG (MEASURED, NOT EXTRAPOLATED) ===");
    println!(
        "  The same walk with the penalty numerator multiplied by k, against the same \
         Crossing::Free baseline. k=1 is the shipped magnitude and is M2's column. A \
         multiplier of k means a numerator of k * span(FINEST)."
    );
    for (ri, rule) in Accumulation::ALL.iter().enumerate() {
        let p = &panel[ri];
        println!("\n  --- rule: {} ---", rule.label());
        println!(
            "    {:>8} {:>14} {:>14} {:>14}",
            "k", "width moved", "rung moved", "day moved"
        );
        for (ki, k) in SWEEP.iter().enumerate() {
            let c = &p.sweep[ki];
            println!(
                "    {:>8} {:>14} {:>14} {:>14}",
                format!("{k:.0}x"),
                c.width_moved,
                c.rung_moved,
                c.day_moved
            );
        }
        let first = SWEEP
            .iter()
            .zip(p.sweep.iter())
            .find(|(_, c)| c.rung_moved > 0)
            .map(|(k, c)| (*k, c.rung_moved));
        println!(
            "    THRESHOLD                      : {}",
            match first {
                Some((1.0, n)) => format!(
                    "the SHIPPED magnitude already moves {n} rungs — no correction is needed \
                     for this rule"
                ),
                Some((k, n)) => format!(
                    "the first rung movement appears at k={k:.0} ({n} holders), i.e. a \
                     numerator of {k:.0} * span(FINEST) rather than one"
                ),
                None => format!(
                    "NO multiplier up to {:.0}x moved a single rung on this panel",
                    SWEEP[SWEEP.len() - 1]
                ),
            }
        );
    }

    // =====================================================================
    // POSITIVE CONTROLS ONLY. No assertion below is about an outcome.
    // =====================================================================
    println!("\n=== CONTROLS ===");
    assert!(!rows.is_empty(), "control: the panel produced no seeds");
    assert!(
        skipped.len() < PANEL.len(),
        "control: every panel seed was skipped for want of a year rung"
    );

    // (1) THE INSTRUMENT IS THE SHIPPED INSTRUMENT. Asserted on every ending
    // and every rule, not sampled — a reimplementation that drifted would make
    // every number above unattributable, and would do it silently.
    let free_mismatch: usize = panel.iter().map(|p| p.free_mismatch).sum();
    assert_eq!(
        free_mismatch, 0,
        "control: this probe's scale-0 walk must reproduce variants_about_accumulating under \
         Crossing::Free holder for holder; {free_mismatch} holders differed"
    );
    let weighted_mismatch: usize = panel.iter().map(|p| p.weighted_mismatch).sum();
    assert_eq!(
        weighted_mismatch, 0,
        "control: this probe's scale-1 walk must reproduce variants_about_accumulating under \
         Crossing::ContactWeighted holder for holder; {weighted_mismatch} holders differed"
    );
    println!("  shipped-walk equivalence   : 0 mismatches under BOTH Crossing arms, every ending");

    // (2) THE PENALTY PRICES EDGES, IT DOES NOT REMOVE THEM. Reachability is a
    // property of the graph, which neither arm touches, so the two arms must
    // reach identical holder SETS (spec §6.4).
    let holder_set_differs: usize = panel.iter().map(|p| p.holder_set_differs).sum();
    assert_eq!(
        holder_set_differs, 0,
        "control: the crossing penalty changes widths, never reachability; the two arms \
         reached different holder sets on {holder_set_differs} ending/rule pairs"
    );
    println!("  holder sets identical      : both arms, every ending and rule");

    // (3) THE PROBE REACHED THE POPULATION IT REPORTS ON. Without these, a
    // zero above is indistinguishable from an instrument that walked nothing.
    assert_eq!(
        endings, BASELINE_ENDINGS_12,
        "control: this panel must re-derive parley_readout.rs's pinned ending count exactly"
    );
    assert_eq!(
        foreign, BASELINE_FOREIGN_12,
        "control: this panel must re-derive parley_readout.rs's pinned foreign count exactly"
    );
    assert!(
        seam_edges > 0,
        "control: the panel must carry raid edges, or no crossing is even possible"
    );
    let holders: usize = panel.iter().map(|p| p.holders).sum();
    assert!(
        holders > 0,
        "control: the walk held no claim at all on the whole panel"
    );
    let crossings: usize = panel.iter().map(|p| p.crossings).sum();
    assert!(
        crossings > 0,
        "control: no winning path ever crossed a people boundary, so M1/M2/M4 report on an \
         EMPTY population and none of their zeros mean anything"
    );
    let pairs: usize = rows.iter().map(|r| r.pairs.len()).sum();
    assert!(
        pairs > 0,
        "control: no people-pair on the panel carries a raid edge, so M4's tercile report is \
         about an empty population"
    );
    println!(
        "  population reached         : {endings} endings, {holders} held claims, {crossings} \
         crossings on winning paths, {pairs} seamed people-pairs"
    );

    // (4) THE SWEEP IS A SWEEP. If the largest multiplier moves no more widths
    // than the smallest, the scale factor is not reaching the penalty at all
    // and M5's threshold row would be a false negative.
    let widest = SWEEP.len() - 1;
    let lo: usize = panel.iter().map(|p| p.sweep[0].width_moved).sum();
    let hi: usize = panel.iter().map(|p| p.sweep[widest].width_moved).sum();
    assert!(
        hi >= lo && hi > 0,
        "control: the penalty scale must reach the width — k={:.0} moved {hi} widths against \
         k=1's {lo}",
        SWEEP[widest]
    );
    println!(
        "  scale factor is live       : k=1 moves {lo} widths, k={:.0} moves {hi}",
        SWEEP[widest]
    );

    // (5) NOTHING SATURATED. A non-finite width would make a sweep cell report
    // a saturation artifact as a rung movement.
    let nonfinite: usize = panel.iter().map(|p| p.nonfinite_widths).sum();
    assert_eq!(
        nonfinite, 0,
        "control: {nonfinite} accumulated widths were not finite, so at least one sweep cell \
         is a floating-point artifact rather than a measurement"
    );
    println!("  all widths finite          : every rule, every multiplier");
}
