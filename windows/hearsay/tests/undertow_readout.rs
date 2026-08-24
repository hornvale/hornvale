//! The Undertow's preregistered readout (spec §6), as a HEAVY BATTERY.
//!
//! Not registered lab metrics, and deliberately so: nine studies declare
//! `"metrics": "all"` with no opt-out, so a metric added here would run on
//! every world of `the-census` (~2000) forever. Campaigns 2, 3 and 4 all put
//! their readouts in a heavy battery for the same reason
//! (`retelling_readout_seed42.rs`, `palimpsest_readout.rs`,
//! `parley_readout.rs`); this follows them, over the same 40-seed panel.
//!
//! **THE ARM VARIED HERE IS `Crossing`, AND EVERY NUMBER IS A DIFFERENCE FROM
//! `Transmission::AS_SHIPPED`** (`no-clock/singleton/descent/free`).
//! `Crossing::ContactWeighted` adds `span(FINEST) / (1 + edges_between(a, b))`
//! to the accumulating width at every cross-people step;
//! [`Crossing::Free`] is pre-campaign behaviour.
//!
//! ## The instrument, and why this file carries a copy of the walk
//!
//! `variants_about_accumulating` returns `Vec<Claim>`, and a `Claim` carries
//! neither the accumulated width nor the ROUTE that produced it. §6.2 (H2) is
//! a question about the route — "did the telling this holder kept cross a
//! people boundary?" — so it is unaskable through the shipped signature.
//! [`Probe::walk`] is therefore `derive.rs`'s relaxation with three extra
//! things carried out of it: the width at emit, the cross-people steps of the
//! winning route, and how many of the winning route's steps were seam edges.
//! It is exactly `probe_crossing_scale.rs`'s walk, which was committed at
//! `05b1565f` before this readout existed, minus its penalty SCALE (this file
//! measures the two shipped arms only, never a hypothetical multiplier).
//!
//! **A reimplementation is a second instrument and could disagree with the
//! first silently, so it is held to the original on every ending, every rule
//! and BOTH arms — not sampled.** Every reported contact-arm cell is produced
//! by the copy and checked holder-for-holder (presence, hops, rung,
//! remembered day) against `variants_about_accumulating` under the same
//! policy. That control is asserted, and a red there means this file's numbers
//! are not the model's numbers.
//!
//! The DESCENT cells use the shipped walk directly and no copy at all: no
//! route information is wanted there, because §5.5's erratum measured zero
//! descent-carried crossings on this bake, and the descent arm's own job here
//! is to be the unmoved reference H1's ratio divides by.
//!
//! ## THREE THINGS A READER MUST HOLD BEFORE READING A COLUMN
//!
//! **1. H2's terciles are cut on PEOPLE-PAIRS, never on crossings.**
//! `probe_crossing_scale.rs` measures the pair distribution on 12 seeds —
//! 42 seamed pairs, `edges_between` running `1:20 2:9 3:6 4:2 5:1 6:2 10:1
//! 11:1` — and, separately, that crossings are heavily CONCENTRATED: 41.6% of
//! them (additive) sit at `edges = 3`, and the two single-pair buckets at
//! `edges = 10` and `edges = 11` carry another 16.8% between them. A tercile
//! cut on the crossing population would therefore be dominated by a handful of
//! pairs on a handful of seeds, and would report their behaviour as the
//! world's. The cut here is over the per-seed seamed-pair population pooled
//! across the panel, and the cut values are printed with the result.
//!
//! **THE CONCENTRATION FIGURE IS RE-DERIVED AND IT MOVED.** Before this
//! campaign absorbed main, the same probe read `48% of all crossings at
//! edges = 25, a SINGLE pair`. The Underworld changed settlement placement, so
//! the seam's shape changed with it; the design decision (cut on pairs) was
//! frozen before either measurement and stands, but the number that argued for
//! it is now the one above. See the spec's §3 preamble.
//!
//! **2. `Additive` carries the non-argmin defect, and — re-derived against
//! this tree — the CROSSING ARM DOES NOT MOVE IT.**
//! `probe_argmin_defect_crossing_arms.rs` measures 49 of 9,531 contact
//! holder-rule cells off the argmin under `Crossing::Free` and **the same 49
//! under `Crossing::ContactWeighted`** — all of them additive; quadrature and
//! multiplicative are exactly 0 under both arms. The cause is structural:
//! `gen_span` telescopes within a same-people segment along a locally monotone
//! founding-day run, so additive's width is endpoint-determined and
//! path-length-blind BY CONSTRUCTION — which is exactly the defect's
//! signature. Quadrature (sums squares) and multiplicative (a running product)
//! do not telescope.
//!
//! **THIS IS THE OTHER FIGURE THAT MOVED, AND IT INVERTED.** On the
//! pre-absorption substrate the same probe measured `36 -> 90 of 13,164`, a
//! 2.5x rise concentrated on additive (32 -> 88), and this file's additive
//! columns were annotated CONFOUNDED because part of an additive difference
//! was that differential defect rate rather than the mechanism. On the merge
//! product it is 49 -> 49: the two arms sit at an identical defect rate, so an
//! arm-to-arm additive comparison is NOT differentially confounded here.
//! Additive still carries the whole of the defect (1.54% of its cells against
//! 0% for the other two rules), so it remains the rule with the most
//! degeneracy in it — but the confound this file was built to disclose is not
//! present on this substrate, and the annotation below says so rather than
//! repeating a number that is no longer true.
//!
//! **3. `Multiplicative` is still the rule to trust, on two counts.**
//! It is 0-on-the-defect under both arms (quadrature now is too), AND it is
//! the only rule whose `edges_between` buckets are all populated — over the
//! rung-moved population the 12-seed probe finds additive at 5 of 8 buckets
//! and quadrature at 3 of 8 (35 holders in total), against multiplicative's
//! full 8. The per-rule bucket populations are printed beside H2 so a thin
//! result reads as THIN rather than as ABSENT.
//!
//! ## On ratios
//!
//! Every ratio here prints its two populations. The Parley shipped a near-miss
//! where a 15-33x "effect" was really a 2.1-4.5x one, because the eligible
//! population had itself grown 7.5x by construction. Two of this readout's
//! populations are pinned by a control rather than by argument — `Crossing`
//! changes no holder's REACHABILITY, only what a route costs, so the Free and
//! ContactWeighted arms hold literally the same holders — and that identity is
//! asserted, not assumed.
//!
//! ## What is asserted
//!
//! The battery **ASSERTS ONLY SUBSTRATE CONTROLS** (spec §6.4): the panel
//! built; held claims exist in every cell; no claim reports a rung its own
//! world's ladders lack; `AS_SHIPPED` reproduces the pre-campaign walk and the
//! descent arm is unmoved by `Crossing`; the penalty never removes (or adds) a
//! holder; the copy of the walk agrees with the shipped one holder-for-holder;
//! and §3's published counts — 4,975 endings, 102 foreign, 15
//! mutually-exclusive, all on seeds 0-11 — re-derive from
//! `parley_readout.rs`'s committed constants.
//!
//! **Every hypothesis is REPORTED**, printed against its own §6 decision
//! table. A falsified prediction is a finding. This file must never be edited
//! to rescue one.

mod common;

use common::{a_people_boundary_no_raid_has_ever_crossed, two_peoples_joined_by_one_raid};
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

/// The seed panel: the first 40 seeds of the census panel (`the-census` runs
/// seeds 0-999), the same panel campaigns 3 and 4 used, so this readout is a
/// strict subset of the census population and directly comparable to its
/// predecessors.
const PANEL: [u64; 40] = [
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
];

/// Seeds 0-11 are exactly the panel spec §3 measured its substrate on, and
/// they are a prefix of [`PANEL`]. Every §3 figure this readout compares
/// against is therefore re-derivable in this run's own first twelve seeds.
const BASELINE_PREFIX: u64 = 12;

/// Spec §3.1, seeds 0-11: endings in the panel
/// (`parley_readout.rs::BASELINE_ENDINGS_12`).
///
/// **RE-DERIVED AGAINST THE MERGE PRODUCT.** This trio read 5,913 / 138 / 19
/// for the whole of this campaign's execution. The Underworld changed
/// settlement placement and `parley_readout.rs`'s own controls were re-pinned
/// at `44ea8d5a`; every figure in this file is measured against the tree that
/// actually lands, and the source of truth for these three is still
/// `parley_readout.rs`, not this file and not the spec's prose.
const BASELINE_ENDINGS_12: usize = 4975;

/// Spec §3.1, seeds 0-11: endings whose attacker is of another people
/// (`parley_readout.rs::BASELINE_FOREIGN_12`).
const BASELINE_FOREIGN_12: usize = 102;

/// Spec §3.4, seeds 0-11, descent + multiplicative: mutually-exclusive
/// cross-people day sets (`parley_readout.rs::BASELINE_MUTUALLY_EXCLUSIVE_12`).
const BASELINE_MUTUALLY_EXCLUSIVE_12: usize = 15;

/// Spec §6.1's decision threshold: H1 predicts the contact-over-descent
/// mutually-exclusive ratio exceeds this under at least one rule. The Parley
/// published 0.59x / 0.52x / 0.77x on the pre-Underworld substrate; this
/// readout's own `Crossing::Free` arm re-derives the same quantity on the
/// merge product, and it is printed beside every H1 line.
const H1_RATIO_FLOOR: f64 = 1.0;

/// The predicate every claim in this readout is about. Only an ending has
/// parties beyond its subject (campaign 2 spec §6.1), so it is the only
/// predicate a transmission graph can be built over.
const PREDICATE: &str = hornvale_history::OCC_ENDED;

/// **The panel size was chosen by measurement, per the plan's decision rule.**
/// A 3-seed pilot (seeds 0, 1, 2) ran before the full panel; the numbers below
/// are printed by the battery so they live in the artifact rather than only in
/// a task report.
///
/// **These two are the DECISION RECORD and are deliberately not re-measured.**
/// They are what the panel-size rule was applied to, on the pre-absorption
/// substrate; re-stamping them with a post-absorption timing would leave the
/// decision looking as though it had been taken against numbers that did not
/// exist when it was taken. The re-measured cost is in `docs/timings.md`.
const PILOT_SEEDS: usize = 3;
/// Seconds of test time the 3-seed pilot took.
const PILOT_TEST_SECONDS: f64 = 7.01;
/// Seconds of test time the full 40-seed run took.
const FULL_RUN_TEST_SECONDS: f64 = 112.77;

// ===========================================================================
// The copy of the walk (see the module doc for why one exists).
// ===========================================================================

/// Which kind of edge carried a cross-people step.
///
/// The distinction is spec §5.5's erratum made measurable: a SEAM crossing has
/// `edges_between >= 1` by construction, because `contact_of` tallies the pair
/// from the very ending that created the edge, so its penalty is at most half
/// a finest rung. A DESCENT crossing — a child of another people than its
/// parent — is the only place `edges_between == 0`, and therefore the spec's
/// full-price penalty, is reachable at all. The erratum measured zero of them
/// on 12 seeds; this re-derives that on 40.
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
/// Identical to `derive.rs`'s private `Telling` but for the two route fields,
/// which carry the winning route's history forward rather than reconstructing
/// it from predecessors: the key rises strictly along every edge so a
/// predecessor chain would also be well-defined, but a carried list needs no
/// argument to be right.
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
    /// How many steps of this route were carried by a seam edge, whether or
    /// not they crossed a people boundary.
    seam_steps: u32,
}

/// One holder's held claim, with the width and route that reached it.
struct Held {
    /// The claim, exactly as the shipped walk would report it.
    claim: Claim,
    /// The accumulated width at emit — the quantity the shipped `Claim`
    /// discards once it has resolved a rung from it.
    width: f64,
    /// The cross-people steps of the winning route, in traversal order. Empty
    /// for a witness and for anyone reached without ever leaving its people.
    crossings: Vec<Crossed>,
    /// Seam-carried steps on the winning route, people boundary or not.
    seam_steps: u32,
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

/// Everything one world's copied walks need, assembled once.
struct Probe<'a> {
    /// The committed ledger.
    led: &'a Ledger,
    /// The founding tree.
    lineage: &'a Lineage,
    /// The raid seam, exactly as `contact::contact_of` builds it.
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
    /// hearer exactly once; this keeps that and resolves the tie toward the
    /// step that needs no seam. The attribution cannot change any width — the
    /// penalty reads the two peoples, never the carrier — so it affects only
    /// the carrier split, and `Carrier::Descent` sorting before `Carrier::Seam`
    /// is what makes the sort do the resolution.
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

    /// `derive.rs`'s private `crossing_penalty`, with the `edges_between` that
    /// priced it handed back.
    ///
    /// Transcribed from `derive.rs:108-129`, not re-derived: zero under
    /// [`Crossing::Free`] and zero for a step within one people, otherwise
    /// `span(FINEST) / (1 + edges_between(a, b))` against the ORIGINATING
    /// witness's ladder.
    fn penalty(
        &self,
        ladder: &PrecisionLadder,
        arm: Crossing,
        teller: EntityId,
        hearer: EntityId,
    ) -> (f64, Option<usize>) {
        let (from, to) = (people_of(self.led, teller), people_of(self.led, hearer));
        if from == to {
            return (0.0, None);
        }
        let edges = self.contact.edges_between(from, to);
        if arm == Crossing::Free {
            return (0.0, Some(edges));
        }
        let unit = ladder
            .span(Precision::FINEST)
            .map(|days| days.get())
            .unwrap_or(0.0);
        (unit / (1.0 + edges as f64), Some(edges))
    }

    /// `variants_about_accumulating` under `Contact::WithRaidSeam`, carrying
    /// the width and the winning route's crossings out.
    ///
    /// Line for line the shipped relaxation, minus the clock (every arm here
    /// runs `Clock::Off`, so `admits` is unconditionally true and the branches
    /// would be dead) and plus the three things the shipped signature cannot
    /// express. The termination argument is unchanged and is the shipped one:
    /// width is non-decreasing so the key's primary component never falls, and
    /// at equal width `hops` rises by exactly one per edge, so the key rises
    /// strictly along every edge and a node is final when it is popped.
    ///
    /// **`Crossing::Free` here is not a separate code path**: the penalty
    /// returns exactly `0.0`, and `x + 0.0` is exact in IEEE-754 for every
    /// finite `x`, which is what makes the equivalence control against the
    /// shipped walk exact rather than approximate on both arms.
    fn walk(&self, rule: Accumulation, arm: Crossing, subject: EntityId) -> Vec<Held> {
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
                    seam_steps: 0,
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
            let carried_seam = telling.seam_steps;

            for (hearer, carrier) in self.tellable(node, event_day) {
                if witness_set.contains(&hearer) {
                    continue; // a witness is never demoted to an inheritor
                }
                let (penalty, edges) = self.penalty(ladder, arm, node, hearer);
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
                        seam_steps: carried_seam + u32::from(carrier == Carrier::Seam),
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
                crossings: t.crossings,
                seam_steps: t.seam_steps,
            })
            .collect()
    }
}

// ===========================================================================
// World reading. A near-copy of `parley_readout.rs::read_world` and of every
// probe on this branch, deliberately: a readout that assembled the ladder
// differently from the probes whose numbers it re-derives would be measuring
// a different model.
// ===========================================================================

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
    /// Whether the attacker is of another people — §3.1's 2.33% population,
    /// and H1's population.
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
        if led.value_of(subject, hornvale_history::OCC_ENDED).is_none() {
            continue;
        }
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
            attacker_people,
        });
    }
    out.sort_by_key(|e| e.subject);
    out.dedup_by_key(|e| e.subject);
    out
}

/// Everything one world contributes, assembled once and shared by every arm.
struct WorldRead {
    /// The founding tree.
    lineage: Lineage,
    /// The raid seam.
    contact: ContactGraph,
    /// Per-people generation and lifespan, in std days.
    durations: PeopleDurations,
    /// One ladder per people — what every walk resolves against.
    ladders: PeopleLadders,
    /// Which people each occupation belongs to.
    people_of: BTreeMap<EntityId, String>,
    /// Every ending in the world, ascending by subject.
    endings: Vec<Ending>,
    /// The longest ladder any people in this world carries. A rung beyond it
    /// could have been resolved by no ladder here at all.
    max_rungs: usize,
}

/// Assemble one world's people table, ladders and endings.
///
/// `None` when the world offers no year rung to convert allometric years into
/// standard days — the readout reports such a seed as skipped rather than
/// guessing a year length.
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
        let life = hornvale_species::life_history(bio.mass, bio.thermal_strategy, bio.schedule);
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
        people_of,
        endings: endings_of(led),
        max_rungs,
    })
}

// ===========================================================================
// The accumulators.
// ===========================================================================

/// Whether the two sides of a raid remember different days — H1's quantity,
/// the same shape `parley_readout.rs` measured it with.
#[derive(Clone, Default)]
struct DivRow {
    /// Cross-people endings where both the victim's and the raider's people
    /// hold the account — H1's DENOMINATOR, printed with every ratio.
    compared: usize,
    /// ... of which each side holds a day the other holds nowhere.
    mutually_exclusive: usize,
    /// ... of which one side's day set strictly contains the other's.
    one_sided: usize,
    /// ... of which the two day sets are identical.
    identical: usize,
}

/// H2's population, bucketed by the `edges_between` of the boundary the holder
/// sits across — never by the crossings taken, per the tercile ruling.
#[derive(Clone, Default)]
struct H2Row {
    /// `edges_between` -> (cross-people holders, of which the held telling
    /// crossed NO people boundary).
    by_edges: BTreeMap<usize, (usize, usize)>,
    /// The same numerator under the stricter reading: the held telling used no
    /// SEAM EDGE at all, whether or not that edge crossed a people boundary.
    /// Reported beside the first so the §6.2 wording ("without crossing a
    /// seam") cannot be read two ways without the reader seeing both.
    no_seam_edge: usize,
    /// `edges_between` -> how many crossings winning paths actually took, so
    /// a thin bucket reads as thin.
    crossing_edges: BTreeMap<usize, usize>,
    /// Carrier -> how many crossings — spec §5.5's descent/seam split.
    crossing_carrier: BTreeMap<&'static str, usize>,
}

/// §6.3's first level: how much the penalty rewrites, holder by holder.
#[derive(Clone, Default)]
struct ChangeRow {
    /// Holders under contact, identical on both arms by construction and
    /// asserted to be so — §6.3's denominator.
    holders: usize,
    /// ... whose held telling differs in ANY reported field.
    any: usize,
    /// ... whose remembered DAY differs.
    day: usize,
    /// ... whose retained RUNG differs.
    rung: usize,
    /// ... whose hop count differs (a different route won).
    hops: usize,
    /// ... whose accumulated WIDTH differs at all — the mechanism working,
    /// whether or not it is visible at emit.
    width: usize,
    /// ... of those, holders of the SUBJECT's own people. A width that moved
    /// here means the least-damaged route left the people and came back —
    /// spec §3.6's zigzag, priced.
    width_same_people: usize,
    /// ... of those, cross-people holders (H2's population).
    width_cross_people: usize,
    /// H2'S OWN POSITIVE CONTROL, and the reason it exists: an aggregate share
    /// that does not move cannot distinguish "the penalty reached this
    /// population and changed nothing" from "the penalty never reached it".
    /// Cross-people holders whose held telling differs between the arms.
    cross_changed: usize,
    /// Cross-people holders whose winning route's CROSSING COUNT differs —
    /// the route recomposed, whether or not it crossed zero times.
    cross_route_changed: usize,
    /// Cross-people holders that crossed under `Free` and cross NOT AT ALL
    /// under `ContactWeighted` — H2's numerator moving, holder by holder.
    /// This is the only direction the mechanism can push: the penalty widens
    /// crossing routes and never the ingroup one.
    cross_became_ingroup: usize,
    /// The opposite direction, which the mechanism cannot cause. Non-zero
    /// here would mean H2's numerator is being moved by something other than
    /// the penalty and the result must not be read as this campaign's.
    cross_left_ingroup: usize,
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
    /// Every unordered people-pair in this world with at least one raid edge
    /// between them, and how many — H2's tercile unit.
    pairs: Vec<(String, String, usize)>,
    /// Divergence, indexed `[Contact::ALL][Crossing::ALL][Accumulation::ALL]`.
    div: [[[DivRow; 3]; 2]; 2],
    /// Held claims, indexed the same way.
    held: [[[usize; 3]; 2]; 2],
    /// H2, indexed `[Crossing::ALL][Accumulation::ALL]` — contact arm only.
    h2: [[H2Row; 3]; 2],
    /// §6.3's first level, indexed by `Accumulation::ALL` — contact arm only.
    changed: [ChangeRow; 3],
    /// CONTROL: holders whose copied-walk claim differs from the shipped
    /// walk's, indexed `[Crossing::ALL][Accumulation::ALL]`. Must be zero.
    mismatches: [[usize; 3]; 2],
    /// CONTROL: holders whose DESCENT-arm claim moved between `Crossing`
    /// arms, per rule. Must be zero — descent crosses no people boundary on
    /// this bake, so the penalty cannot fire (spec §8's byte-identity item).
    descent_moved: [usize; 3],
    /// CONTROL: endings where `ContactWeighted` reached a holder `Free` did
    /// not. Must be zero — the penalty prices routes, never reachability.
    crossing_added: usize,
    /// CONTROL: endings where `ContactWeighted` LOST a holder `Free` had.
    /// Must be zero — spec §6.4 names this one explicitly.
    crossing_removed: usize,
    /// CONTROL: held claims on a rung longer than any ladder in their world.
    rung_beyond_world: usize,
}

/// `n` as a percentage of `d`, or `0.0` when `d` is zero.
fn pct(n: usize, d: usize) -> f64 {
    100.0 * n as f64 / d.max(1) as f64
}

/// Print a `label -> count` distribution compactly.
fn show_hist<K: std::fmt::Display + Ord>(hist: &BTreeMap<K, usize>) -> String {
    if hist.is_empty() {
        return "(empty)".to_string();
    }
    hist.iter()
        .map(|(k, v)| format!("{k}:{v}"))
        .collect::<Vec<_>>()
        .join(" ")
}

/// The remembered day of a claim as raw bits, so a comparison is exact.
fn day_bits(claim: &Claim) -> Option<u64> {
    match &claim.object {
        Value::Number(day) => Some(day.to_bits()),
        _ => None,
    }
}

/// Fold one arm's held claims into the per-people remembered-day sets H1
/// compares, and update `div`.
fn tally_divergence(
    e: &Ending,
    people_of: &BTreeMap<EntityId, String>,
    claims: &[(EntityId, Claim)],
    div: &mut DivRow,
) {
    if !e.is_foreign() {
        return;
    }
    let ap = e
        .attacker_people
        .as_ref()
        .expect("a foreign ending names an attacker people");
    let mut by_people: BTreeMap<&str, BTreeSet<u64>> = BTreeMap::new();
    for (holder, claim) in claims {
        let Some(p) = people_of.get(holder) else {
            continue;
        };
        if let Some(bits) = day_bits(claim) {
            by_people.entry(p.as_str()).or_default().insert(bits);
        }
    }
    let (Some(v), Some(r)) = (by_people.get(e.people.as_str()), by_people.get(ap.as_str())) else {
        return;
    };
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

/// Run the shipped walk under one (`Contact`, `Crossing`) pair.
fn shipped(
    read: &WorldRead,
    led: &Ledger,
    rule: Accumulation,
    contact: Contact,
    arm: Crossing,
    subject: EntityId,
) -> Vec<Claim> {
    variants_about_accumulating(
        &Walk {
            ledger: led,
            lineage: &read.lineage,
            contact: &read.contact,
            policy: Transmission {
                contact,
                crossing: arm,
                ..Transmission::AS_SHIPPED
            },
        },
        &read.ladders,
        &read.durations,
        rule,
        subject,
        PREDICATE,
    )
}

/// Whether two claims for the same holder agree on every reported field.
fn same_claim(x: &Claim, y: &Claim) -> bool {
    let same_day = match (&x.object, &y.object) {
        (Value::Number(p), Value::Number(q)) => p.to_bits() == q.to_bits(),
        (p, q) => p == q,
    };
    x.hops == y.hops && x.precision == y.precision && same_day
}

/// Every quantity §6 asks for, over one world.
///
/// Six walks per ending per rule: two shipped walks for the descent cells
/// (which need no route information), two copied walks for the contact cells,
/// and two more shipped walks holding those copies honest.
fn measure_seed(seed: u64, led: &Ledger, read: &WorldRead) -> SeedRow {
    let probe = Probe {
        led,
        lineage: &read.lineage,
        contact: &read.contact,
        ladders: &read.ladders,
        durations: &read.durations,
    };

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
        pairs,
        ..Default::default()
    };

    for e in &read.endings {
        if e.is_foreign() {
            row.foreign += 1;
        }

        for (ri, rule) in Accumulation::ALL.iter().enumerate() {
            // ---- the DESCENT cells: the shipped walk, both Crossing arms.
            let mut descent: [Vec<Claim>; 2] = Default::default();
            for (ci, arm) in Crossing::ALL.iter().enumerate() {
                let claims = shipped(read, led, *rule, Contact::Descent, *arm, e.subject);
                row.held[0][ci][ri] += claims.len();
                for c in &claims {
                    if c.precision.rung() as usize >= read.max_rungs {
                        row.rung_beyond_world += 1;
                    }
                }
                let pairs: Vec<(EntityId, Claim)> =
                    claims.iter().map(|c| (c.holder, c.clone())).collect();
                tally_divergence(e, &read.people_of, &pairs, &mut row.div[0][ci][ri]);
                descent[ci] = claims;
            }
            let free_by_holder: BTreeMap<EntityId, &Claim> =
                descent[0].iter().map(|c| (c.holder, c)).collect();
            for c in &descent[1] {
                match free_by_holder.get(&c.holder) {
                    Some(other) if same_claim(c, other) => {}
                    _ => row.descent_moved[ri] += 1,
                }
            }
            if descent[0].len() != descent[1].len() {
                row.descent_moved[ri] += descent[0].len().abs_diff(descent[1].len());
            }

            // ---- the CONTACT cells: the copied walk, held to the shipped one.
            let mut contact_arms: [Vec<Held>; 2] = Default::default();
            for (ci, arm) in Crossing::ALL.iter().enumerate() {
                let mine = probe.walk(*rule, *arm, e.subject);
                let theirs = shipped(read, led, *rule, Contact::WithRaidSeam, *arm, e.subject);
                let by_holder: BTreeMap<EntityId, &Claim> =
                    theirs.iter().map(|c| (c.holder, c)).collect();
                let mut bad = 0;
                for h in &mine {
                    match by_holder.get(&h.claim.holder) {
                        Some(other) if same_claim(&h.claim, other) => {}
                        _ => bad += 1,
                    }
                }
                let mine_holders: BTreeSet<EntityId> =
                    mine.iter().map(|h| h.claim.holder).collect();
                for c in &theirs {
                    if !mine_holders.contains(&c.holder) {
                        bad += 1;
                    }
                }
                row.mismatches[ci][ri] += bad;

                row.held[1][ci][ri] += mine.len();
                let h2 = &mut row.h2[ci][ri];
                let pairs: Vec<(EntityId, Claim)> = mine
                    .iter()
                    .map(|h| (h.claim.holder, h.claim.clone()))
                    .collect();
                tally_divergence(e, &read.people_of, &pairs, &mut row.div[1][ci][ri]);

                for h in &mine {
                    if h.claim.precision.rung() as usize >= read.max_rungs {
                        row.rung_beyond_world += 1;
                    }
                    for x in &h.crossings {
                        *h2.crossing_edges.entry(x.edges).or_default() += 1;
                        *h2.crossing_carrier.entry(x.carrier.label()).or_default() += 1;
                    }
                    // H2's population: a holder whose OWN people differs from
                    // the people whose ending this is. Arm-invariant by
                    // construction — it reads the ledger, never the route —
                    // which is what keeps the two arms' denominators equal.
                    let holder_people = read
                        .people_of
                        .get(&h.claim.holder)
                        .map_or("", |p| p.as_str());
                    if holder_people.is_empty() || e.people.is_empty() || holder_people == e.people
                    {
                        continue;
                    }
                    let edges = read.contact.edges_between(holder_people, &e.people);
                    let bucket = h2.by_edges.entry(edges).or_default();
                    bucket.0 += 1;
                    if h.crossings.is_empty() {
                        bucket.1 += 1;
                    }
                    if h.seam_steps == 0 {
                        h2.no_seam_edge += 1;
                    }
                }
                contact_arms[ci] = mine;
            }

            // ---- §6.3's first level, and the reachability controls.
            let free: BTreeMap<EntityId, &Held> = contact_arms[0]
                .iter()
                .map(|h| (h.claim.holder, h))
                .collect();
            let weighted: BTreeMap<EntityId, &Held> = contact_arms[1]
                .iter()
                .map(|h| (h.claim.holder, h))
                .collect();
            let ch = &mut row.changed[ri];
            ch.holders += free.len();
            for (holder, a) in &free {
                let Some(b) = weighted.get(holder) else {
                    continue; // counted by the removal control below
                };
                let holder_people = read.people_of.get(holder).map_or("", |p| p.as_str());
                let cross =
                    !holder_people.is_empty() && !e.people.is_empty() && holder_people != e.people;
                let changed = !same_claim(&a.claim, &b.claim);
                if changed {
                    ch.any += 1;
                }
                if day_bits(&a.claim) != day_bits(&b.claim) {
                    ch.day += 1;
                }
                if a.claim.precision != b.claim.precision {
                    ch.rung += 1;
                }
                if a.claim.hops != b.claim.hops {
                    ch.hops += 1;
                }
                if a.width.to_bits() != b.width.to_bits() {
                    ch.width += 1;
                    if cross {
                        ch.width_cross_people += 1;
                    } else {
                        ch.width_same_people += 1;
                    }
                }
                if !cross {
                    continue;
                }
                if changed {
                    ch.cross_changed += 1;
                }
                if a.crossings.len() != b.crossings.len() {
                    ch.cross_route_changed += 1;
                }
                match (a.crossings.is_empty(), b.crossings.is_empty()) {
                    (false, true) => ch.cross_became_ingroup += 1,
                    (true, false) => ch.cross_left_ingroup += 1,
                    _ => {}
                }
            }
            if weighted.keys().any(|h| !free.contains_key(h)) {
                row.crossing_added += 1;
            }
            if free.keys().any(|h| !weighted.contains_key(h)) {
                row.crossing_removed += 1;
            }
        }
    }

    row
}

/// Sum a per-seed field over the panel.
fn sum<F: Fn(&SeedRow) -> usize>(rows: &[SeedRow], f: F) -> usize {
    rows.iter().map(f).sum()
}

/// The tercile cuts over a sorted people-pair edge population: `(lo, hi)`,
/// read as bottom `<= lo`, middle `lo+1..=hi`, top `> hi`.
fn tercile_cuts(sorted: &[usize]) -> Option<(usize, usize)> {
    if sorted.len() < 3 {
        return None;
    }
    let n = sorted.len();
    Some((sorted[n / 3], sorted[(2 * n) / 3]))
}

/// THE READOUT. Spec §6, over a fixed seed panel, with no arm nominated.
///
/// claim: readout(off-gate, heavy:) — the preregistered §6 readout over
/// `PANEL`; reports H1, H2 and the §6.3 named null against their decision
/// tables and asserts only substrate controls. A falsified prediction is a
/// finding here.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn the_undertow_readout_over_a_seed_panel() {
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

    println!("\n================ THE UNDERTOW READOUT ================");
    println!(
        "panel                : {} seeds (census seeds 0-39), {} measured, skipped {skipped:?}",
        PANEL.len(),
        rows.len()
    );
    println!("predicate            : {PREDICATE}");
    println!(
        "baseline             : AS_SHIPPED = {} — every number below is a difference from it",
        Transmission::AS_SHIPPED.label()
    );
    println!(
        "arm varied           : Crossing::Free -> Crossing::ContactWeighted, under BOTH \
         Contact arms, on all three accumulation rules"
    );
    println!(
        "pilot cost           : {PILOT_SEEDS} seeds (0,1,2) cost {PILOT_TEST_SECONDS:.2} s of \
         test time = {:.2} s/seed; the plan's rule (<= 5 s/seed) kept the 40-seed panel. The \
         full run then measured {FULL_RUN_TEST_SECONDS:.2} s of test time. BOTH FIGURES ARE \
         THE DECISION RECORD, taken before main was absorbed; docs/timings.md carries the \
         re-measured cost.",
        PILOT_TEST_SECONDS / PILOT_SEEDS as f64
    );
    println!(
        "instrument           : contact cells walk a COPY of derive.rs's relaxation (a Claim \
         carries no route, and §6.2 is a question about the route); every contact cell is held \
         holder-for-holder to variants_about_accumulating under the same policy, asserted \
         below. Descent cells are the shipped walk itself."
    );

    let endings = sum(&rows, |r| r.endings);
    let foreign = sum(&rows, |r| r.foreign);
    let seam_edges = sum(&rows, |r| r.seam_edges);
    println!(
        "\nSUBSTRATE, THIS RUN  : {endings} endings, {foreign} with a foreign attacker \
         ({:.2}%), {seam_edges} undirected raid edges",
        pct(foreign, endings)
    );
    let prefix_endings = sum(&rows, |r| {
        if r.seed < BASELINE_PREFIX {
            r.endings
        } else {
            0
        }
    });
    let prefix_foreign = sum(&rows, |r| {
        if r.seed < BASELINE_PREFIX {
            r.foreign
        } else {
            0
        }
    });
    println!(
        "  seeds 0-11 only    : {prefix_endings} endings, {prefix_foreign} foreign \
         — re-derives parley_readout.rs's pinned {BASELINE_ENDINGS_12} / \
         {BASELINE_FOREIGN_12}, which is what §3 now publishes"
    );

    println!(
        "\n  {:<6} {:>9} {:>8} {:>7} {:>12} {:>12} {:>12}",
        "seed", "endings", "foreign", "edges", "d:holders", "c:holders", "pairs"
    );
    for r in &rows {
        println!(
            "  {:<6} {:>9} {:>8} {:>7} {:>12} {:>12} {:>12}",
            r.seed,
            r.endings,
            r.foreign,
            r.seam_edges,
            r.held[0][0][0],
            r.held[1][0][0],
            r.pairs.len(),
        );
    }

    // =====================================================================
    // THE CONFOUND, STATED BEFORE ANY NUMBER THAT CARRIES IT.
    // =====================================================================
    println!("\n=== READ THIS BEFORE THE ADDITIVE COLUMN (Task 3, re-derived at the close) ===");
    println!(
        "  `probe_argmin_defect_crossing_arms.rs` measures 49 of 9,531 contact holder-rule \
         cells off the argmin under `Crossing::Free` on seeds 0-11, and THE SAME 49 under \
         `Crossing::ContactWeighted` — all of them ADDITIVE (1.54% of its cells); quadrature \
         and multiplicative are exactly 0 under BOTH arms."
    );
    println!(
        "  The cause is structural, not incidental: `gen_span` TELESCOPES within a same-people \
         segment along a locally monotone founding-day run, so additive's width is \
         endpoint-determined and path-length-blind by construction — which is the defect's \
         signature. Quadrature sums squares and multiplicative runs a product; neither \
         telescopes."
    );
    println!(
        "  CONSEQUENCE FOR EVERY ADDITIVE NUMBER BELOW, AND IT IS NOT THE ONE AN EARLIER \
         DRAFT PRINTED. On the pre-absorption substrate the same probe read 36 -> 90 of \
         13,164 — a 2.5x rise, almost all additive (32 -> 88) — so an additive arm-to-arm \
         difference was partly a DIFFERENTIAL defect rate rather than the mechanism, and \
         every additive line here was annotated CONFOUNDED. Re-derived against the merge \
         product the arms sit at an IDENTICAL 49, so that differential confound is absent \
         and an additive comparison here is like-for-like. Additive still carries the whole \
         of the defect, so it is still the rule with the most degeneracy in it. \
         MULTIPLICATIVE remains the rule to trust, on two counts — 0-on-the-defect under \
         both arms (quadrature now is too), and the only rule whose edges_between buckets \
         are all populated over the rung-moved population."
    );

    // =====================================================================
    // §6.1 — H1: does the penalty break pooling?
    // =====================================================================
    println!("\n=== §6.1 H1 — MUTUALLY-EXCLUSIVE CROSS-PEOPLE ENDINGS ===");
    println!(
        "  H1 predicts that under contact the mutually-exclusive count rises ABOVE its descent \
         count under at least one rule — the ratio The Parley published at 0.59x / 0.52x / \
         0.77x exceeding {H1_RATIO_FLOOR:.1}x. THAT PUBLISHED TRIPLE IS PRE-UNDERWORLD; the \
         `free` rows below are the same quantity re-derived on the merge product, and they \
         are the ones to compare against."
    );
    println!(
        "  COUNTS WITH BOTH POPULATIONS NAMED, never a bare ratio: `compared` is the eligible \
         population — cross-people endings where BOTH the victim's people and the raider's \
         hold the account."
    );
    println!(
        "  AND HERE THAT POPULATION IS SATURATED, which is worth a sentence rather than the \
         caveat an earlier draft printed. `compared` measures the same in all nine rows below \
         and equals the foreign-ending count EXACTLY, so it is 100% of the eligible endings on \
         every arm and every rule. It is not that the arms happen to agree: the \
         `occ-ended-by` attacker is always a witness, so both sides hold the account by \
         construction and there is no room for an arm to move this denominator. A ratio built \
         on it is therefore a pure numerator comparison, which is the cleanest case and the \
         opposite of the denominator-inflation The Parley shipped."
    );
    let mut h1_ratio = [[0.0f64; 3]; 2];
    for (ri, rule) in Accumulation::ALL.iter().enumerate() {
        println!("\n  --- rule: {} ---", rule.label());
        println!(
            "    {:<10} {:<18} {:>9} {:>7} {:>10} {:>10} {:>9}",
            "contact", "crossing", "compared", "mutex", "one-sided", "identical", "mutex%"
        );
        for (ai, contact) in Contact::ALL.iter().enumerate() {
            for (ci, arm) in Crossing::ALL.iter().enumerate() {
                let compared = sum(&rows, |r| r.div[ai][ci][ri].compared);
                let mutex = sum(&rows, |r| r.div[ai][ci][ri].mutually_exclusive);
                let one = sum(&rows, |r| r.div[ai][ci][ri].one_sided);
                let same = sum(&rows, |r| r.div[ai][ci][ri].identical);
                println!(
                    "    {:<10} {:<18} {compared:>9} {mutex:>7} {one:>10} {same:>10} {:>8.2}%",
                    contact.label(),
                    arm.label(),
                    pct(mutex, compared)
                );
            }
        }
        for (ci, arm) in Crossing::ALL.iter().enumerate() {
            let d_mutex = sum(&rows, |r| r.div[0][ci][ri].mutually_exclusive);
            let c_mutex = sum(&rows, |r| r.div[1][ci][ri].mutually_exclusive);
            let d_cmp = sum(&rows, |r| r.div[0][ci][ri].compared);
            let c_cmp = sum(&rows, |r| r.div[1][ci][ri].compared);
            h1_ratio[ci][ri] = c_mutex as f64 / d_mutex.max(1) as f64;
            println!(
                "    H1 under {:<18}: descent {d_mutex}/{d_cmp} -> contact {c_mutex}/{c_cmp} \
                 = {:.2}x  [predicted > {H1_RATIO_FLOOR:.1}x]",
                arm.label(),
                h1_ratio[ci][ri]
            );
        }
        println!(
            "    the penalty's own move, contact arm only: {} -> {} mutually exclusive \
             (compared {} -> {})",
            sum(&rows, |r| r.div[1][0][ri].mutually_exclusive),
            sum(&rows, |r| r.div[1][1][ri].mutually_exclusive),
            sum(&rows, |r| r.div[1][0][ri].compared),
            sum(&rows, |r| r.div[1][1][ri].compared),
        );
        if *rule == Accumulation::Additive {
            println!(
                "    ^ ADDITIVE carries the whole non-argmin defect (49 of 3,177 cells); \
                 re-derived, the two arms sit at the SAME 49, so this comparison is not \
                 differentially confounded — see the caveat above."
            );
        }
    }
    let h1_confirmed = h1_ratio[1].iter().any(|x| *x > H1_RATIO_FLOOR);
    println!(
        "\n  DECISION TABLE (§6.1): under Crossing::ContactWeighted, the contact-over-descent \
         mutually-exclusive ratio exceeds {H1_RATIO_FLOOR:.1}x on at least one rule -> \
         CONFIRMED; every rule stays at or below it -> FALSIFIED (pooling is a property of \
         seam VOLUME that selection cannot reach, and the thread needs a different instrument)"
    );
    println!(
        "  VERDICT: {} — ratios under the penalty: additive {:.2}x, quadrature {:.2}x, \
         multiplicative {:.2}x (pre-campaign, Free: {:.2}x / {:.2}x / {:.2}x)",
        if h1_confirmed {
            "CONFIRMED"
        } else {
            "FALSIFIED"
        },
        h1_ratio[1][0],
        h1_ratio[1][1],
        h1_ratio[1][2],
        h1_ratio[0][0],
        h1_ratio[0][1],
        h1_ratio[0][2],
    );

    // =====================================================================
    // §6.2 — H2: ingroup preference as an output, split by tercile.
    // =====================================================================
    println!("\n=== §6.2 H2 — INGROUP PREFERENCE, BY CONTACT TERCILE ===");
    println!(
        "  POPULATION: holders whose OWN people differs from the people whose ending this is \
         (`cross-people holders`). Read from the ledger, never from the route, so it is \
         IDENTICAL under both Crossing arms — which is asserted below, not assumed, and is why \
         these shares share a denominator."
    );
    println!(
        "  NUMERATOR: those whose held telling crossed NO people boundary — the ingroup \
         account, kept in preference to the one that came across the seam."
    );
    println!(
        "  TERCILES ARE CUT ON PEOPLE-PAIRS, NOT ON CROSSINGS. Crossings are heavily \
         CONCENTRATED — 41.6% of them on the 12-seed probe sit at edges=3, and the two \
         single-pair buckets at edges=10 and edges=11 carry another 16.8% between them — so a \
         crossing-weighted tercile would report a handful of pairs' behaviour as the world's. \
         The cut below is over the per-seed seamed-pair population pooled across the panel. \
         (Re-derived: before main was absorbed the same probe read 48% at edges=25, a SINGLE \
         pair. The cut rule was frozen before either measurement.)"
    );

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
        "\n  seamed people-pairs      : {} over the panel, {} distinct edge counts",
        pair_edges.len(),
        pair_hist.len()
    );
    println!("  edges_between per pair   : {}", show_hist(&pair_hist));
    let mut prefix_pairs: Vec<usize> = rows
        .iter()
        .filter(|r| r.seed < BASELINE_PREFIX)
        .flat_map(|r| r.pairs.iter().map(|(_, _, e)| *e))
        .collect();
    prefix_pairs.sort_unstable();
    println!(
        "  seeds 0-11 prefix        : {} pairs, cuts {:?} — the scale probe re-derives 42 \
         pairs with cuts (1, 2) on the same twelve seeds",
        prefix_pairs.len(),
        tercile_cuts(&prefix_pairs)
    );

    let cuts = tercile_cuts(&pair_edges);
    match cuts {
        None => {
            println!("  TERCILE CUTS             : UNAVAILABLE — fewer than three seamed pairs")
        }
        Some((lo, hi)) => {
            let bottom = pair_edges.iter().filter(|e| **e <= lo).count();
            let top = pair_edges.iter().filter(|e| **e > hi).count();
            println!(
                "  TERCILE CUTS             : bottom edges<={lo} ({bottom} pairs) | middle \
                 {}..={hi} ({} pairs) | top edges>{hi} ({top} pairs)",
                lo + 1,
                pair_edges.len() - bottom - top,
            );
        }
    }

    let names = ["bottom", "middle", "top"];
    let mut h2_rise = [[f64::NAN; 3]; 3]; // [rule][tercile]
    for (ri, rule) in Accumulation::ALL.iter().enumerate() {
        println!("\n  --- rule: {} ---", rule.label());
        let mut merged: [BTreeMap<usize, (usize, usize)>; 2] = Default::default();
        let mut crossing_edges: BTreeMap<usize, usize> = BTreeMap::new();
        let mut crossing_carrier: BTreeMap<&'static str, usize> = BTreeMap::new();
        for (ci, _) in Crossing::ALL.iter().enumerate() {
            for r in &rows {
                for (k, v) in &r.h2[ci][ri].by_edges {
                    let slot = merged[ci].entry(*k).or_default();
                    slot.0 += v.0;
                    slot.1 += v.1;
                }
                if ci == 1 {
                    for (k, v) in &r.h2[ci][ri].crossing_edges {
                        *crossing_edges.entry(*k).or_default() += v;
                    }
                    for (k, v) in &r.h2[ci][ri].crossing_carrier {
                        *crossing_carrier.entry(k).or_default() += v;
                    }
                }
            }
        }
        // RULING C's requirement: the bucket populations, printed beside the
        // result, so a thin result reads as THIN rather than as ABSENT.
        let bucket_pop: BTreeMap<usize, usize> = merged[0].iter().map(|(k, v)| (*k, v.0)).collect();
        println!(
            "    cross-people holders by edges_between : {}",
            show_hist(&bucket_pop)
        );
        println!(
            "    crossings taken by winning paths      : {} [NOTE: a DIFFERENT population \
             from the line above — tallied over ALL contact holders, cross-people or not, \
             because §3.6's zigzag has same-people holders taking crossings too. Do not read \
             the two lines as numerator and denominator of each other.]",
            show_hist(&crossing_edges)
        );
        println!(
            "    ... by carrier                        : {}",
            show_hist(&crossing_carrier)
        );

        println!(
            "    {:<8} {:>10} {:>14} {:>9} {:>14} {:>9} {:>7} {:>9}",
            "tercile", "holders", "free:ingroup", "free%", "cw:ingroup", "cw%", "d(n)", "rise(pp)"
        );
        let mut tercile_pop: [[(usize, usize); 2]; 3] = Default::default();
        for (ti, name) in names.iter().enumerate() {
            let in_bucket = |edges: usize| -> bool {
                match cuts {
                    None => ti == 0,
                    Some((lo, hi)) => match ti {
                        0 => edges <= lo,
                        1 => edges > lo && edges <= hi,
                        _ => edges > hi,
                    },
                }
            };
            let pop: [(usize, usize); 2] = [0, 1].map(|ci| {
                merged[ci]
                    .iter()
                    .filter(|(k, _)| in_bucket(**k))
                    .fold((0, 0), |acc, (_, v)| (acc.0 + v.0, acc.1 + v.1))
            });
            tercile_pop[ti] = pop;
            let free_pct = pct(pop[0].1, pop[0].0);
            let cw_pct = pct(pop[1].1, pop[1].0);
            h2_rise[ri][ti] = cw_pct - free_pct;
            println!(
                "    {name:<8} {:>10} {:>14} {free_pct:>8.2}% {:>14} {cw_pct:>8.2}% \
                 {:>+7} {:>+8.2}",
                pop[0].0,
                pop[0].1,
                pop[1].1,
                pop[1].1 as i64 - pop[0].1 as i64,
                h2_rise[ri][ti]
            );
            if pop[0].0 != pop[1].0 {
                println!(
                    "      NOTE: the two arms' denominators differ ({} vs {}), which the \
                     no-add/no-remove control forbids — read this row with suspicion",
                    pop[0].0, pop[1].0
                );
            }
        }
        let overall: [(usize, usize); 2] = [0, 1].map(|ci| {
            merged[ci]
                .values()
                .fold((0, 0), |acc, v| (acc.0 + v.0, acc.1 + v.1))
        });
        let overall_rise = pct(overall[1].1, overall[1].0) - pct(overall[0].1, overall[0].0);
        println!(
            "    ALL      {:>10} {:>14} {:>8.2}% {:>14} {:>8.2}% {:>+7} {:>+8.2}",
            overall[0].0,
            overall[0].1,
            pct(overall[0].1, overall[0].0),
            overall[1].1,
            pct(overall[1].1, overall[1].0),
            overall[1].1 as i64 - overall[0].1 as i64,
            overall_rise,
        );
        // THE COST OF RULING A, DISCLOSED BESIDE ITS OWN RESULT. Cutting on
        // PAIRS is right — crossings concentrate on a few pairs (41.6% at
        // edges=3), so a crossing-weighted cut would report them as the world
        // — but the
        // three terciles hold equal numbers of PAIRS and wildly unequal
        // numbers of HOLDERS. The `rise` column is already a rate over each
        // tercile's own denominator, so the imbalance does not distort it;
        // it does mean the bottom tercile's zero rests on far fewer holders
        // than the top tercile's positive, and a reader is owed that.
        println!(
            "    tercile holder populations (a consequence of cutting on pairs, not holders): \
             bottom {} | middle {} | top {} — equal pair counts, unequal holder counts",
            merged[0]
                .iter()
                .filter(|(k, _)| cuts.is_none_or(|(lo, _)| **k <= lo))
                .map(|(_, v)| v.0)
                .sum::<usize>(),
            merged[0]
                .iter()
                .filter(|(k, _)| cuts.is_some_and(|(lo, hi)| **k > lo && **k <= hi))
                .map(|(_, v)| v.0)
                .sum::<usize>(),
            merged[0]
                .iter()
                .filter(|(k, _)| cuts.is_some_and(|(_, hi)| **k > hi))
                .map(|(_, v)| v.0)
                .sum::<usize>(),
        );
        // IMPORTANT 3: THE BUCKET AXIS IS NOT THE AXIS THE PENALTY IS LEVIED
        // ON, and the discrepancy is concentrated exactly where the verdict
        // lives. A holder is bucketed by edges_between(its people, the
        // subject's people); the penalties it actually paid were priced at
        // whatever boundaries its ROUTE crossed, which need not include that
        // pair at all. The edges=0 bucket is the extreme case and it is not
        // small.
        let zero_bucket = merged[0].get(&0).map_or(0, |v| v.0);
        println!(
            "    THE TERCILE AXIS IS NOT THE AXIS THE PENALTY IS LEVIED ON: a holder is \
             bucketed by edges_between(holder's people, subject's people), but the penalties \
             it paid were priced at whatever boundaries its ROUTE crossed. {} of the bottom \
             tercile's {} holders ({:.1}%) sit in the edges=0 bucket — NO direct edge exists \
             between those two peoples at all, so every one of them was reached through a \
             CHAIN and every penalty it paid was priced by some OTHER pair. The bottom \
             tercile is where the verdict lives, so this is a caveat on the verdict and not \
             a footnote.",
            zero_bucket,
            tercile_pop[0][0].0,
            pct(zero_bucket, tercile_pop[0][0].0),
        );
        println!(
            "    stricter reading (held telling used NO seam edge at all, boundary or not): \
             free {} -> contact-weighted {} of {} cross-people holders",
            sum(&rows, |r| r.h2[0][ri].no_seam_edge),
            sum(&rows, |r| r.h2[1][ri].no_seam_edge),
            overall[0].0,
        );
        // THE REACH CONTROL THAT LEADS, BECAUSE IT IS EXACT AND PER-TERCILE.
        // `width_cross_people` (§6.3's split, counted over a different loop
        // entirely) equals `cross-people holders MINUS free:ingroup` to the
        // unit on all three rules: every cross-people holder whose Free route
        // crossed had its width moved by the penalty. Two things follow. The
        // mechanism reached the whole crossing population, not a sample of
        // it; and no ladder on this panel returned a zero span(FINEST),
        // because a zero unit would have left some crossing holder's width
        // untouched.
        let crossed_panel = overall[0].0 - overall[0].1;
        let width_cross = sum(&rows, |r| r.changed[ri].width_cross_people);
        println!(
            "    REACH, EXACTLY (the strongest control on this page): {crossed_panel} \
             cross-people holders had their Free route cross at least one boundary \
             ({} holders minus {} that already held an ingroup telling). §6.3 counted \
             {width_cross} cross-people holders whose WIDTH the penalty moved, over a \
             separate loop — and {}, so EVERY crossing holder paid. That also proves no \
             ladder here returned a zero span(FINEST): a zero unit would have left some \
             crossing holder untouched.",
            overall[0].0,
            overall[0].1,
            if crossed_panel == width_cross {
                "the two agree to the unit".to_string()
            } else {
                format!("they DISAGREE by {}", crossed_panel.abs_diff(width_cross))
            },
        );
        // IMPORTANT 4: the same identity, per tercile — because the exact
        // zero this control exists to defend is the BOTTOM TERCILE's, and a
        // panel-wide aggregate is one granularity too coarse to defend it.
        // Valid as an inference: the panel-level identity says every crossing
        // holder paid, so within any subset the count that paid is that
        // subset's own crossing count.
        println!(
            "    ... AND PER TERCILE, which is the granularity the verdict actually needs: \
             bottom {} of {} holders crossed and therefore demonstrably paid a penalty, \
             middle {} of {}, top {} of {}. So the bottom tercile's +0.00 pp is NOT the \
             mechanism failing to reach it — {} of its holders paid and none of them changed \
             which telling they hold.",
            tercile_pop[0][0].0 - tercile_pop[0][0].1,
            tercile_pop[0][0].0,
            tercile_pop[1][0].0 - tercile_pop[1][0].1,
            tercile_pop[1][0].0,
            tercile_pop[2][0].0 - tercile_pop[2][0].1,
            tercile_pop[2][0].0,
            tercile_pop[0][0].0 - tercile_pop[0][0].1,
        );
        // H2'S POSITIVE CONTROL. A share that does not move is two different
        // findings wearing one number: the penalty reached this population and
        // changed nothing, or it never reached it at all. These separate them,
        // holder by holder, and the last column is the direction the mechanism
        // CANNOT cause.
        println!(
            "    SECONDARY, AND PANEL-AGGREGATE — the exact per-tercile reach control above \
             is the one to read: of {} cross-people holders, {} had their \
             held telling changed by the penalty, {} had their route's crossing count change, \
             {} went crossed -> ingroup, {} went ingroup -> crossed. THE LAST COLUMN IS \
             REPORTED, NOT ASSERTED, AND HERE IS WHY BOTH HALVES MATTER: `step` is monotone \
             non-decreasing in `span` on all three rules, and an ingroup route pays no \
             penalty, so a route that won with zero crossings under Free still wins under \
             ContactWeighted — the mechanism CANNOT push this direction. But the relaxation \
             is not always its own argmin (spec §3.5, re-derived: 49 of 9,531 holder-rule \
             cells, IDENTICAL under both arms), so a \
             non-zero here would be that DEFECT surfacing, not the mechanism, and asserting \
             zero would conflate the two",
            overall[0].0,
            sum(&rows, |r| r.changed[ri].cross_changed),
            sum(&rows, |r| r.changed[ri].cross_route_changed),
            sum(&rows, |r| r.changed[ri].cross_became_ingroup),
            sum(&rows, |r| r.changed[ri].cross_left_ingroup),
        );
        if *rule == Accumulation::Additive {
            println!(
                "    ^ ADDITIVE carries the whole non-argmin defect (49 of 3,177 cells); \
                 re-derived, the two arms sit at the SAME 49, so this comparison is not \
                 differentially confounded — see the caveat above."
            );
        }
    }
    println!(
        "\n  DECISION TABLE (§6.2, UNCHANGED): the ingroup share RISES under the penalty, AND \
         the rise is strictly larger in the BOTTOM tercile than the TOP -> CONFIRMED. \
         Otherwise (bottom <= top) -> FALSIFIED. No rise at all -> FALSIFIED on clause 1 \
         before clause 2 is reached."
    );
    println!(
        "  WHAT THE FALSIFICATION DOES **NOT** LICENCE, AND THIS IS THE SINGLE MOST IMPORTANT \
         SENTENCE ON THIS PAGE. §6.2 glossed its own falsification as 'contact_edges is inert \
         and §5.2's derivation is decorative'. THAT GLOSS DOES NOT APPLY TO THIS RESULT, for a \
         reason §6.2 did not anticipate: the measured ordering is INVERTED, not UNIFORM, and \
         an inverted ordering is equally consistent with a CONSTANT penalty. 48% of the \
         crossings on the 12-seed probe concentrate on a few pairs (41.6% at edges=3), so \
         flips concentrate \
         wherever the crossings are — which is where contact is highest — WHATEVER the \
         magnitude rule is. A constant penalty would produce the same shape."
    );
    println!(
        "  SO: THIS READOUT CARRIES NO ARM THAT SEPARATES A DERIVED MAGNITUDE FROM A CONSTANT \
         ONE, and §5.5's k-multiplier sweep is not that arm either — it varied GLOBAL SCALE, \
         which cannot reorder pairs relative to one another. §5.2's derivation — the \
         campaign's licence under decision 0021 — is therefore NEITHER CONFIRMED NOR REFUTED \
         by this campaign. `tests/crossing.rs` proves the FORMULA responds to edge count; \
         nothing here proves that responsiveness matters on real data more than a constant \
         would. That is a LIMITATION OF THE INSTRUMENT, not a finding against the model, and \
         the arm that would settle it — ContactWeighted against a constant-denominator \
         control at matched mean penalty — is a campaign of its own."
    );
    for (ri, rule) in Accumulation::ALL.iter().enumerate() {
        let (b, t) = (h2_rise[ri][0], h2_rise[ri][2]);
        let verdict = if !b.is_finite() || !t.is_finite() {
            "NO VERDICT (a tercile has no members)"
        } else if b <= 0.0 && t <= 0.0 {
            "FALSIFIED on clause 1 — no rise in either tercile"
        } else if b > t {
            "CONFIRMED — the rise is larger where the peoples are stranger"
        } else {
            "FALSIFIED on clause 2 — the rise is not larger in the bottom tercile"
        };
        let m = h2_rise[ri][1];
        println!(
            "  VERDICT ({:<14}): {verdict} — bottom {b:+.2} pp, middle {m:+.2} pp, top \
             {t:+.2} pp",
            rule.label(),
        );
    }
    println!(
        "  THE VERDICT ABOVE IS ROBUST; ANY CAUSAL GLOSS ON ITS DIRECTION IS NOT, AND AN \
         EARLIER DRAFT OF THIS LINE OVERREACHED. Clause 2 fails however anyone argues about \
         exposure: the bottom tercile's rise is +0.00 pp and the top's is positive on all \
         three rules, so `bottom > top` is false outright. What must NOT be read off that is \
         a DIRECTION — 'the rise grows with contact rather than with strangeness'. It rests on \
         d(n) counts of +0/+5/+32, +0/+5/+51 and +0/+8/+38, with no eligibility denominator \
         stated, and the reading is not robust to which denominator is chosen: against ALL \
         holders a zero in the bottom tercile is surprising, but against the holders that \
         could actually flip — those whose Free route crossed, and that hold a rival ingroup \
         telling at all — it is unremarkable. The ordering is REPORTED as an observation. It \
         is not established as a finding, and the chronicle must not promote it to one."
    );

    // =====================================================================
    // §6.3 — THE NAMED NULL, BOTH LEVELS SIDE BY SIDE.
    // =====================================================================
    println!("\n=== §6.3 THE NAMED NULL — BOTH LEVELS, SIDE BY SIDE ===");
    println!(
        "  §3.4 measured a SELECTION change that rewrote 45.7% of holders and moved the \
         aggregate by <= 2 events of 124 — PRE-ABSORPTION FIGURES, quoted verbatim from \
         frozen §6.3; re-derived §3.4 reads 41.9% and <= 4 of 100, and the shape §6.3 names \
         is unchanged by the difference. The live null is that §5.1 does the same. A readout \
         printing only the aggregate cannot tell a working mechanism from an inert one, and \
         this substrate has produced that shape once already."
    );
    println!(
        "\n  {:<14} {:>10} {:>9} {:>8} {:>8} {:>8} {:>9} | {:>7} {:>7} {:>7}",
        "rule",
        "holders",
        "width!=",
        "any!=",
        "day!=",
        "rung!=",
        "hops!=",
        "mutex0",
        "mutex1",
        "delta"
    );
    for (ri, rule) in Accumulation::ALL.iter().enumerate() {
        let holders = sum(&rows, |r| r.changed[ri].holders);
        let width = sum(&rows, |r| r.changed[ri].width);
        let any = sum(&rows, |r| r.changed[ri].any);
        let day = sum(&rows, |r| r.changed[ri].day);
        let rung = sum(&rows, |r| r.changed[ri].rung);
        let hops = sum(&rows, |r| r.changed[ri].hops);
        let m0 = sum(&rows, |r| r.div[1][0][ri].mutually_exclusive);
        let m1 = sum(&rows, |r| r.div[1][1][ri].mutually_exclusive);
        println!(
            "  {:<14} {holders:>10} {width:>9} {any:>8} {day:>8} {rung:>8} {hops:>9} | \
             {m0:>7} {m1:>7} {:>+7}",
            rule.label(),
            m1 as i64 - m0 as i64
        );
        println!(
            "    as shares of the {holders} contact holders: width {:.3}%, any {:.3}%, day \
             {:.3}%, rung {:.3}%, hops {:.3}%",
            pct(width, holders),
            pct(any, holders),
            pct(day, holders),
            pct(rung, holders),
            pct(hops, holders),
        );
        println!(
            "    WHERE THE WIDTH MOVED: {} at holders of the SUBJECT's own people, {} at \
             cross-people holders. The first is spec §3.6's ZIGZAG being priced — the \
             least-damaged route left the people and came back, so a holder that never \
             looks foreign is paying a crossing anyway.",
            sum(&rows, |r| r.changed[ri].width_same_people),
            sum(&rows, |r| r.changed[ri].width_cross_people),
        );
        println!(
            "    -> {}",
            if any == 0 && width == 0 {
                "INERT AT BOTH LEVELS: the penalty changed no holder's width at all, so \
                 nothing downstream could move. This is not the named null — it is the \
                 mechanism failing to reach the population."
            } else if any == 0 {
                "INVISIBLE AT EMIT: widths move but no held telling changes. The mechanism \
                 works and the ladder absorbs all of it — §5.5's 'a bigger penalty buys \
                 resolution, never reach', at k = 1."
            } else if m0 == m1 {
                "THE NAMED NULL FIRES: held tellings change while the aggregate does not \
                 move at all. §3.4's dissociation repeating one level up, exactly as §6.3 \
                 preregistered."
            } else {
                "BOTH LEVELS MOVE: the mechanism changes which account is held AND the \
                 aggregate the campaign is about. The named null does not fire here."
            }
        );
        if *rule == Accumulation::Additive {
            println!(
                "    ^ ADDITIVE carries the whole non-argmin defect (49 of 3,177 cells); \
                 re-derived, the two arms sit at the SAME 49, so this comparison is not \
                 differentially confounded — see the caveat above."
            );
        }
    }
    println!(
        "  READ THE TWO LEVELS TOGETHER: `width!=` is the mechanism firing, `any!=` is it \
         surviving to emit, and `delta` is the campaign's headline. A large first column with \
         a zero last one is the null; a zero first column is not the null but an inert \
         mechanism, and the two must never be reported as the same result."
    );

    // =====================================================================
    // SUBSTRATE CONTROLS ONLY (spec §6.4). No hypothesis is asserted.
    // =====================================================================
    println!("\n=== SUBSTRATE CONTROLS (asserted; no hypothesis is) ===");
    let mismatches = sum(&rows, |r| r.mismatches.iter().flatten().sum::<usize>());
    let descent_moved = sum(&rows, |r| r.descent_moved.iter().sum::<usize>());
    let added = sum(&rows, |r| r.crossing_added);
    let removed = sum(&rows, |r| r.crossing_removed);
    let beyond = sum(&rows, |r| r.rung_beyond_world);
    println!("  copy vs shipped walk, holders differing : {mismatches}");
    println!("  descent arm moved by Crossing           : {descent_moved}");
    println!("  endings where the penalty ADDED a holder: {added}");
    println!("  endings where the penalty LOST a holder : {removed}");
    println!("  claims on a rung no ladder in the world has: {beyond}");

    assert!(
        skipped.len() < PANEL.len(),
        "control: every panel seed was skipped for want of a year rung"
    );
    assert!(!rows.is_empty(), "control: the panel produced no seeds");
    for (ai, contact) in Contact::ALL.iter().enumerate() {
        for (ci, arm) in Crossing::ALL.iter().enumerate() {
            for (ri, rule) in Accumulation::ALL.iter().enumerate() {
                let held = sum(&rows, |r| r.held[ai][ci][ri]);
                assert!(
                    held > 0,
                    "control: cell {}/{}/{} produced no held claims at all",
                    contact.label(),
                    arm.label(),
                    rule.label()
                );
            }
        }
    }
    assert_eq!(
        mismatches, 0,
        "control: this file's copy of the relaxation disagreed with \
         variants_about_accumulating on {mismatches} holders — the reported contact-arm \
         numbers are then not the model's numbers"
    );
    assert_eq!(
        descent_moved, 0,
        "control: Crossing must not move the DESCENT arm — a descent step across a people \
         boundary is open in code and never walked by this bake (spec §5.5), so \
         AS_SHIPPED's walk must reproduce byte for byte under ContactWeighted; \
         {descent_moved} holders moved"
    );
    assert_eq!(
        removed, 0,
        "control: the penalty prices routes and must never REMOVE a holder (spec §6.4); it \
         removed one on {removed} endings"
    );
    assert_eq!(
        added, 0,
        "control: the penalty enters through the width only and must not change \
         reachability, so it can add no holder either; it added one on {added} endings"
    );
    assert_eq!(
        beyond, 0,
        "control: {beyond} claims report a rung longer than any ladder in their own world — \
         no ladder could have resolved one"
    );
    assert_eq!(
        Transmission::AS_SHIPPED.crossing,
        Crossing::Free,
        "control: AS_SHIPPED must still be the pre-campaign policy; every number here is a \
         difference from it"
    );

    // ---- §3's published counts, re-derived on this run's own prefix ----
    //
    // Seeds 0-11 are a strict PREFIX of this panel, so parley_readout.rs's
    // committed constants are re-derivable IN THIS RUN by this readout's own
    // instrument. A red here is this ratchet firing: re-derive the figures,
    // decide deliberately whether the bake moved on purpose, and update the
    // constants IN THE COMMIT THAT MOVED IT. Never rebaseline to go green on
    // a run whose cause is unexplained.
    assert_eq!(
        prefix_endings, BASELINE_ENDINGS_12,
        "control: seeds 0-11 must re-derive spec §3.1's ending count exactly"
    );
    assert_eq!(
        prefix_foreign, BASELINE_FOREIGN_12,
        "control: seeds 0-11 must re-derive spec §3.1's foreign-attacker count exactly"
    );
    let multiplicative = Accumulation::ALL
        .iter()
        .position(|r| *r == Accumulation::Multiplicative)
        .expect("Accumulation::ALL carries Multiplicative");
    let prefix_mutex = sum(&rows, |r| {
        if r.seed < BASELINE_PREFIX {
            r.div[0][0][multiplicative].mutually_exclusive
        } else {
            0
        }
    });
    assert_eq!(
        prefix_mutex, BASELINE_MUTUALLY_EXCLUSIVE_12,
        "control: seeds 0-11 under descent + multiplicative + Crossing::Free must re-derive \
         spec §3.4's mutually-exclusive count exactly"
    );
    println!(
        "  §3 re-derived on seeds 0-11             : {prefix_endings} endings, \
         {prefix_foreign} foreign, {prefix_mutex} mutually exclusive — all three match"
    );
}

// ===========================================================================
// THE NON-VACUITY CONTROLS. Cheap, hand-built, and NOT ignored.
//
// The panel's own controls prove the two arms hold the same holders and that
// the copy agrees with the shipped walk — but not that this file's ACCOUNTING
// can MOVE. A readout that summed the wrong field would satisfy every
// assertion above and print a constant. These push each headline counter
// through the same `measure_seed` the panel runs.
// ===========================================================================

/// Assemble a [`WorldRead`] over a hand-built ledger, with one 50-day
/// generation and 150-day lifespan for every named people — the same durations
/// `tests/augmented_walk.rs` and `parley_readout.rs` use, so the fixtures'
/// documented widths carry over.
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
        people_of,
        endings: endings_of(led),
        max_rungs,
    }
}

/// §6.3's first level must be able to MOVE through `measure_seed` itself.
/// Without this, a panel showing no change could be a null or could be a
/// readout that never varied the arm.
#[test]
fn the_crossing_arm_moves_the_change_counter_in_the_control() {
    let led = two_peoples_joined_by_one_raid();
    let read = hand_read(&led, &["human", "kobold"]);
    let row = measure_seed(0, &led, &read);

    let moved: usize = row.changed.iter().map(|c| c.width).sum();
    assert!(
        moved > 0,
        "the penalty must widen somebody's held telling on the near-strangers fixture: \
         widths moved on {moved} holders"
    );
    assert_eq!(
        row.crossing_removed, 0,
        "the penalty removes no holder on the control either"
    );
    assert_eq!(
        row.crossing_added, 0,
        "the penalty adds no holder on the control either"
    );
    assert_eq!(
        row.mismatches.iter().flatten().sum::<usize>(),
        0,
        "the copy of the relaxation must agree with the shipped walk on the control too"
    );
}

/// H2's counter must be able to move, and its numerator must be able to be
/// NEITHER zero NOR everybody — a share pinned at either end by construction
/// would make the tercile split unreadable however the panel came out.
///
/// The fixture carries both cases at once, which is why it is the right
/// control and why an earlier draft of this test asserted the wrong thing.
/// It has two endings:
///
/// - **occ 1 ends with no named attacker.** Its only witness is itself
///   (human), so the kobolds hear it ACROSS the seam and every kobold holder
///   of it crossed a people boundary — numerator 0.
/// - **occ 3 ends AT KOBOLD HANDS.** `witnesses_of` admits the attacker, so a
///   kobold is a witness, and the kobold line below it holds a telling that
///   never crossed anything — numerator 1 per holder.
///
/// The second is the mechanism §6.2 is about, in miniature: an account of the
/// same event exists on both sides, and one side's copy came down its own
/// line. A control that demanded 0 would have been asserting the mechanism
/// away.
#[test]
fn h2s_population_is_reached_in_the_control() {
    let led = two_peoples_joined_by_one_raid();
    let read = hand_read(&led, &["human", "kobold"]);
    let row = measure_seed(0, &led, &read);

    let population: usize = row.h2[0]
        .iter()
        .flat_map(|h| h.by_edges.values())
        .map(|v| v.0)
        .sum();
    let ingroup: usize = row.h2[0]
        .iter()
        .flat_map(|h| h.by_edges.values())
        .map(|v| v.1)
        .sum();
    println!("cross-people holders {population}, of which ingroup {ingroup}");
    assert!(
        population > 0,
        "the fixture must produce cross-people holders for H2 to be about"
    );
    assert!(
        ingroup > 0 && ingroup < population,
        "H2's numerator must be able to sit strictly between its two ends on a fixture \
         carrying both cases: {ingroup} of {population}"
    );
    let crossings: usize = row.h2[1]
        .iter()
        .flat_map(|h| h.crossing_edges.values())
        .sum();
    assert!(
        crossings > 0,
        "the winning routes must take at least one crossing for H2's buckets to fill"
    );
}

/// The `edges_between == 0` bucket — the only place spec §5.1's FULL-price
/// rung is reachable — must be reachable by this file's accounting, or H2's
/// bottom tercile would be silently unable to hold its most extreme member.
///
/// Constructed, not producible by the bake: the boundary sits on a DESCENT
/// edge, which campaign 2 measured as never happening in real data (zero of
/// 780 typed edges), and which spec §5.5 names as the one route to a zero
/// denominator.
#[test]
fn the_zero_edge_bucket_is_reachable_by_the_accounting() {
    let led = a_people_boundary_no_raid_has_ever_crossed();
    let read = hand_read(&led, &["human", "drow"]);
    let row = measure_seed(0, &led, &read);

    let zero_bucket_crossings: usize = row.h2[1]
        .iter()
        .filter_map(|h| h.crossing_edges.get(&0))
        .sum();
    assert!(
        zero_bucket_crossings > 0,
        "the full-price crossing must land in the edges=0 bucket; it did not, so H2's \
         bottom tercile could never hold one"
    );
    let by_descent: usize = row.h2[1]
        .iter()
        .filter_map(|h| h.crossing_carrier.get("descent"))
        .sum();
    assert!(
        by_descent > 0,
        "this fixture's crossing is carried by a DESCENT edge, which is the whole reason \
         the zero bucket is reachable at all"
    );
}
