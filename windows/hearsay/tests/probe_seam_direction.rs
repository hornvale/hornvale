//! Substrate probe (The Undertow, Myth campaign 5): **did the world pool the
//! accounts, or did the undirected edge?**
//!
//! The Parley added a cross-people contact edge along `occ-ended-by` and found
//! that accounts POOL rather than diverge — identical remembered-day sets rose
//! under contact on every accumulation rule, and its frozen §6.5 divergence
//! measure FELL (0.59x / 0.52x / 0.77x). Its spec §5.3 froze the edge
//! **undirected**, on the ground that asserting a direction would be
//! authoring. So the pooling has two candidate causes and nothing published
//! separates them:
//!
//! - **the world pools** — a raid puts two peoples in contact and the account
//!   converges however news flows across it; or
//! - **the undirected edge pools** — symmetry is the mechanism, each side
//!   receiving the other's telling and keeping whichever it can reach least
//!   corrupted, and a one-way seam would not do it.
//!
//! This probe is the discriminator. It **simulates direction on the existing
//! undirected graph** rather than implementing a directed arm, which is how
//! The Parley's own S6 decided whether the contact edge was worth modelling
//! before building it. Every raid edge has a definable direction already: the
//! `occ-ended` subject is the VICTIM and its `occ-ended-by` object is the
//! RAIDER, and `contact_of` simply writes both directions of that one fact.
//!
//! - **S1** — the direction decomposition. Under the undirected seam, which
//!   way did each cross-people holder's WINNING path cross, and how often does
//!   a winning path cross more than once?
//! - **S2** — does pooling survive a directed restriction? The Parley's §6.5
//!   quantities under both directions, victim->raider only, and raider->victim
//!   only, per accumulation rule.
//! - **S3** — reach under each direction.
//! - **S4** — a loose end from the merge queue: does `endings` carry its own
//!   12-seed amplification? Measured as a 12-seed prefix against a 100-seed
//!   panel. **A PROXY for the census comparison, not the census comparison.**
//!
//! ## Why this file reimplements the walk, and what holds it honest
//!
//! `variants_about_accumulating` returns `Vec<Claim>`, and a `Claim` carries
//! no route — so neither "which way did it cross" nor "restrict the seam to
//! one direction" is askable through the shipped signature. [`Probe::walk`] is
//! therefore a copy of that relaxation carrying two extra things: a per-edge
//! direction filter, and the seam crossings of the telling each holder ends up
//! keeping.
//!
//! A reimplementation is a second instrument and could disagree with the first
//! silently, so it is held to the original: [`Arm::Both`] must reproduce
//! `variants_about_accumulating` under `Contact::WithRaidSeam` HOLDER FOR
//! HOLDER — same hop count, same rung, same remembered day — and [`Arm::None`]
//! must reproduce it under `Contact::Descent`. That equivalence is asserted on
//! every ending of the panel, not sampled.
//!
//! The clock is `Off` throughout, matching `Transmission::AS_SHIPPED` and
//! The Parley's own contact arms, so nothing here varies two things at once.
//!
//! Reports only. Every assertion is a POSITIVE CONTROL — reproducing a
//! published count, or proving the probe reached the population it reports on
//! — so a zero can be told apart from a broken instrument. No assertion here
//! is about an outcome.
//!
//! ## Cost, measured, so the next reader budgets from a number and not a guess
//!
//! On this Mac (`test` profile, which this workspace builds optimized), first
//! authoring run: [`which_way_the_account_crosses_the_seam`] **30.7 s** for 12
//! seeds — 18 walks per ending, of which 6 are the shipped walk run purely as
//! the equivalence control — and [`how_much_the_twelve_seed_prefix_over_reads`]
//! **218.8 s** for 100 seeds, which is almost entirely `build_world` and only
//! incidentally the one descent walk it runs per foreign ending. A committed
//! cost is a claim with a date; re-measure rather than extrapolate.

mod common;

use common::a_round_trip_through_another_people;
use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::accumulate::{Accumulation, precision_at};
use hornvale_hearsay::amplitude::gen_span;
use hornvale_hearsay::contact::{Contact, contact_of};
use hornvale_hearsay::derive::{variants_about_accumulating, witnesses_of};
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_hearsay::ladder::{PeopleLadders, PrecisionLadder};
use hornvale_hearsay::lineage::{Lineage, lineage_of};
use hornvale_hearsay::transmission::{Transmission, Walk};
use hornvale_kernel::Claim;
use hornvale_kernel::Precision;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use hornvale_kernel::provenance::Provenance;
use std::collections::{BTreeMap, BTreeSet};

/// The 12-seed panel: a strict prefix of the census panel (`the-census` runs
/// seeds 0-999), of The Parley's 40-seed readout panel, and of its substrate
/// probe's own panel — so nothing here samples a population an earlier
/// campaign's instrument did not.
const PANEL: [u64; 12] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];

/// S4's wide panel: census seeds 0-99, of which [`PANEL`] is the first twelve.
const WIDE: u64 = 100;

/// The predicate every claim here is about. Only an ending has parties beyond
/// its subject (The Palimpsest §6.1), so it is the only predicate a
/// transmission graph can be built over.
const PREDICATE: &str = hornvale_history::OCC_ENDED;

// ---------------------------------------------------------------------------
// The controls this probe is held to, READ OUT OF `parley_readout.rs`'s OWN
// CONSTANTS at this commit rather than out of The Parley's spec prose. Main
// has moved since that spec was written (The Underworld and The Illumination
// landed, and The Parley's controls were re-pinned at 44ea8d5a), so the
// committed constants are the live claim and the spec is the historical one.
// AT THIS COMMIT THE TWO DISAGREE, AND THE COMMITTED CONSTANT WINS. The
// Parley's spec §3.1 publishes 5,913 / 138 and its §3.4 publishes 19; those
// were measured before The Underworld changed settlement placement.
// `parley_readout.rs` was re-pinned at `44ea8d5a` to 4,975 / 102 / 15, and
// this file follows it, because a spec is a record of what a campaign
// measured on the day and a committed constant is a claim about the tree it
// sits in. The probe reports both sources, so the divergence is visible in
// the output and not only in a red.
// ---------------------------------------------------------------------------

/// The Parley spec §3.1's PUBLISHED ending count, kept so the probe can print
/// the two sources side by side and say whether they still agree. Historical:
/// measured before The Underworld moved settlement placement.
const PARLEY_SPEC_ENDINGS_12: usize = 5913;

/// The Parley spec §3.1's PUBLISHED foreign-attacker count. Historical, for
/// the same reason as [`PARLEY_SPEC_ENDINGS_12`].
const PARLEY_SPEC_FOREIGN_12: usize = 138;

/// `parley_readout.rs::BASELINE_ENDINGS_12`: endings over seeds 0-11.
const BASELINE_ENDINGS_12: usize = 4975;

/// `parley_readout.rs::BASELINE_FOREIGN_12`: endings over seeds 0-11 whose
/// attacker is of another people.
const BASELINE_FOREIGN_12: usize = 102;

/// `parley_readout.rs::BASELINE_MUTUALLY_EXCLUSIVE_12`: mutually-exclusive
/// cross-people day sets over seeds 0-11, under DESCENT and
/// `Accumulation::Multiplicative` only. The Parley spec §3.4 published 19 on
/// the pre-Underworld substrate.
const BASELINE_MUTUALLY_EXCLUSIVE_12: usize = 15;

/// Which side of a raid a seam edge points AWAY from.
///
/// The ledger writes one fact — `occ-ended(victim) = day` with
/// `occ-ended-by(victim) = raider` — and `contact::contact_of` turns it into
/// two entries, one on each party. This names the two halves so a traversal
/// can be attributed to one of them.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum EdgeKind {
    /// The destroyed community's account passes to those who took the site.
    VictimToRaider,
    /// The survivors' account passes to the people who did it, or back.
    RaiderToVictim,
}

impl EdgeKind {
    /// This half's short name, used as a readout column.
    fn label(self) -> &'static str {
        match self {
            EdgeKind::VictimToRaider => "victim->raider",
            EdgeKind::RaiderToVictim => "raider->victim",
        }
    }
}

/// Which seam traversals a walk admits.
///
/// [`Arm::None`] and [`Arm::Both`] have shipped counterparts
/// (`Contact::Descent` and `Contact::WithRaidSeam`) and are asserted equal to
/// them; the two directed arms have none, and simulating them on the
/// undirected graph is this probe's whole method.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Arm {
    /// Descent only — the graph every model before The Parley walked.
    None,
    /// Both directions: The Parley as it shipped, and the BASELINE here.
    Both,
    /// Only the victim may tell the raider.
    VictimToRaider,
    /// Only the raider may tell the victim.
    RaiderToVictim,
}

impl Arm {
    /// Every arm, in a fixed order so the readout's columns are stable.
    const ALL: [Arm; 4] = [
        Arm::None,
        Arm::Both,
        Arm::VictimToRaider,
        Arm::RaiderToVictim,
    ];

    /// This arm's short name.
    fn label(self) -> &'static str {
        match self {
            Arm::None => "descent",
            Arm::Both => "both",
            Arm::VictimToRaider => "v->r only",
            Arm::RaiderToVictim => "r->v only",
        }
    }

    /// Whether a traversal of `kind` is admitted.
    fn admits(self, kind: EdgeKind) -> bool {
        match self {
            Arm::None => false,
            Arm::Both => true,
            Arm::VictimToRaider => kind == EdgeKind::VictimToRaider,
            Arm::RaiderToVictim => kind == EdgeKind::RaiderToVictim,
        }
    }
}

/// The raid seam with its direction kept.
///
/// A near-copy of `contact::contact_of`, differing only in that each entry
/// records which half of the raid it is. The copy is held to the original by
/// [`the_directed_seam_reproduces_the_shipped_contact_graph`]: projecting the
/// direction away must give `ContactGraph::peers_of` exactly.
#[derive(Clone, Debug, Default)]
struct DirectedSeam {
    /// teller -> (hearer, day of the raid, which half), ascending.
    out: BTreeMap<EntityId, Vec<(EntityId, f64, EdgeKind)>>,
}

impl DirectedSeam {
    /// Everyone `occ` may tell across the seam, ascending. Empty when `occ`
    /// never met anybody.
    fn out_of(&self, occ: EntityId) -> &[(EntityId, f64, EdgeKind)] {
        self.out.get(&occ).map_or(&[], Vec::as_slice)
    }

    /// How many directed traversals exist, by half.
    fn counts(&self) -> (usize, usize) {
        let mut v = 0;
        let mut r = 0;
        for edges in self.out.values() {
            for (_, _, kind) in edges {
                match kind {
                    EdgeKind::VictimToRaider => v += 1,
                    EdgeKind::RaiderToVictim => r += 1,
                }
            }
        }
        (v, r)
    }
}

/// Read the raid seam out of a ledger, keeping direction.
///
/// Every branch here mirrors `contact_of`: the same `find`/`value_of` reads,
/// the same refusal of an ending missing a day or an `Entity`-valued attacker,
/// the same refusal of a self-raid, the same sort and dedup.
fn directed_seam_of(led: &Ledger) -> DirectedSeam {
    let mut out = DirectedSeam::default();
    for fact in led.find(PREDICATE) {
        let victim = fact.subject;
        let Some(Value::Number(day)) = led.value_of(victim, hornvale_history::OCC_ENDED) else {
            continue;
        };
        let Some(Value::Entity(raider)) = led.value_of(victim, hornvale_history::OCC_ENDED_BY)
        else {
            continue;
        };
        let (day, raider) = (*day, *raider);
        if raider == victim {
            continue; // a community cannot meet itself
        }
        out.out
            .entry(victim)
            .or_default()
            .push((raider, day, EdgeKind::VictimToRaider));
        out.out
            .entry(raider)
            .or_default()
            .push((victim, day, EdgeKind::RaiderToVictim));
    }
    for edges in out.out.values_mut() {
        edges.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.total_cmp(&b.1)).then(a.2.cmp(&b.2)));
        edges.dedup();
    }
    out
}

/// One node's best-known telling, as the relaxation converges toward it.
///
/// Identical to `derive.rs`'s private `Telling` but for `seam`, which carries
/// the crossings of the route that produced this telling. Carrying the list
/// forward rather than reconstructing it from predecessors is deliberate: the
/// key rises strictly along every edge, so a predecessor chain would also be
/// well-defined, but a list needs no argument to be right.
struct Telling<'a> {
    /// This telling's rank against any rival reaching the same holder.
    key: (u64, u32, EntityId),
    /// Accumulated damage width, at full precision.
    width: f64,
    /// The ORIGINATING witness's people's ladder, fixed for the whole path.
    ladder: &'a PrecisionLadder,
    /// The claim as this holder received it.
    claim: Claim,
    /// The seam crossings of this telling's route, in traversal order.
    seam: Vec<EdgeKind>,
}

/// One holder's held claim, with the route that reached it.
struct Held {
    /// The claim, exactly as the shipped walk would report it.
    claim: Claim,
    /// The seam crossings of the winning route, in traversal order. Empty for
    /// a witness and for anyone reached down the founding tree alone.
    seam: Vec<EdgeKind>,
}

/// Everything one world's walks need, assembled once.
struct Probe<'a> {
    /// The committed ledger.
    led: &'a Ledger,
    /// The founding tree.
    lineage: &'a Lineage,
    /// The raid seam, with direction.
    seam: &'a DirectedSeam,
    /// One ladder per people.
    ladders: &'a PeopleLadders,
    /// Per-people generation and lifespan, in std days.
    durations: &'a PeopleDurations,
}

impl<'a> Probe<'a> {
    /// Who `node` can tell under `arm`, ascending, each paired with the seam
    /// half it would be told across (`None` for a founding-tree step).
    ///
    /// **A hearer reachable BOTH ways is attributed to descent.** The shipped
    /// `tellable` sorts and dedups by `EntityId` alone, so it expands such a
    /// hearer exactly once; this keeps that, and resolves the tie toward the
    /// step that needs no seam — an account that could have travelled down the
    /// tree is not evidence for a crossing. `Option`'s ordering puts `None`
    /// first, so the sort does it.
    fn tellable(
        &self,
        arm: Arm,
        node: EntityId,
        event_day: Option<f64>,
    ) -> Vec<(EntityId, Option<EdgeKind>)> {
        let mut out: Vec<(EntityId, Option<EdgeKind>)> = self
            .lineage
            .children_of(node)
            .iter()
            .map(|c| (*c, None))
            .collect();
        if arm != Arm::None {
            for (peer, day, kind) in self.seam.out_of(node) {
                if arm.admits(*kind) && event_day.is_none_or(|event| *day >= event) {
                    out.push((*peer, Some(*kind)));
                }
            }
        }
        out.sort();
        out.dedup_by_key(|(id, _)| *id);
        out
    }

    /// `variants_about_accumulating`, with a direction filter and the winning
    /// route's seam crossings carried out.
    ///
    /// Line for line the shipped relaxation, minus the clock (every arm here
    /// runs `Clock::Off`, so `admits` is unconditionally true and the branches
    /// would be dead) and plus the two things the shipped signature cannot
    /// express. The termination argument is unchanged and is the shipped one:
    /// width is non-decreasing so the key's primary component never falls, and
    /// at equal width `hops` rises by exactly one per edge, so the key rises
    /// strictly along every edge and a node is final when it is popped.
    fn walk(&self, rule: Accumulation, arm: Arm, subject: EntityId) -> Vec<Held> {
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
            let people = match led.value_of(*w, hornvale_history::OCC_PEOPLE) {
                Some(Value::Text(p)) => p.clone(),
                _ => String::new(),
            };
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
                    seam: Vec::new(),
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
            let crossings = telling.seam.clone();

            for (hearer, via) in self.tellable(arm, node, event_day) {
                if witness_set.contains(&hearer) {
                    continue; // a witness is never demoted to an inheritor
                }
                let next_width = rule.step(width, gen_span(led, self.durations, node, hearer));
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
                let mut seam = crossings.clone();
                if let Some(kind) = via {
                    seam.push(kind);
                }
                reached.insert(
                    hearer,
                    Telling {
                        key: next_key,
                        width: next_width,
                        ladder,
                        claim: next_claim,
                        seam,
                    },
                );
                frontier.insert((next_key, hearer));
            }
        }

        reached
            .into_values()
            .map(|t| Held {
                claim: t.claim,
                seam: t.seam,
            })
            .collect()
    }
}

// ---------------------------------------------------------------------------
// World reading. A near-copy of `parley_readout.rs::read_world` and of
// `probe_contact_substrate.rs`'s, deliberately: a probe that assembled the
// ladder differently from the readout whose numbers it is testing against
// would be measuring a different model.
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
    /// Whether the attacker is of another people — §3.1's 2.33% population and
    /// §6.5's whole population.
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

/// One world, assembled once and shared by every arm.
struct WorldRead {
    /// The founding tree.
    lineage: Lineage,
    /// The raid seam, with direction.
    seam: DirectedSeam,
    /// Per-people generation and lifespan, in std days.
    durations: PeopleDurations,
    /// One ladder per people.
    ladders: PeopleLadders,
    /// Which people each occupation belongs to.
    people_of: BTreeMap<EntityId, String>,
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
    Some(WorldRead {
        lineage,
        seam: directed_seam_of(led),
        durations,
        ladders,
        people_of,
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

/// `n` as a percentage of `d`, or `0.0` when `d` is zero.
fn pct(n: usize, d: usize) -> f64 {
    100.0 * n as f64 / d.max(1) as f64
}

// ---------------------------------------------------------------------------
// The accumulators.
// ---------------------------------------------------------------------------

/// S1: how the winning paths crossed the seam, over one arm and rule.
#[derive(Clone, Default)]
struct DirectionRow {
    /// Held claims, over every ending.
    holders: usize,
    /// ... whose winning path used at least one seam edge.
    crossers: usize,
    /// ... whose people differs from the ending subject's.
    cross_people: usize,
    /// Cross-people holders reached with NO seam crossing at all — the
    /// hop-0 co-witness (the named attacker) and its line under descent.
    cross_people_no_crossing: usize,
    /// Cross-people holders whose winning path crossed exactly once, by half.
    cross_people_once: BTreeMap<EdgeKind, usize>,
    /// Cross-people holders whose winning path crossed more than once.
    cross_people_multi: usize,
    /// The first crossing of every cross-people crosser, by half.
    first_crossing: BTreeMap<EdgeKind, usize>,
    /// Every seam traversal on every winning path, by half — the edge-level
    /// decomposition, which the holder-level one cannot show.
    traversals: BTreeMap<EdgeKind, usize>,
    /// crossings-on-the-winning-path -> how many cross-people holders.
    depth_hist: BTreeMap<usize, usize>,
}

/// S2: The Parley's §6.5 quantities, over one arm and rule.
#[derive(Clone, Default)]
struct DivRow {
    /// Cross-people endings where both sides' peoples hold the account.
    compared: usize,
    /// ... of which each side holds a day the other holds nowhere.
    mutually_exclusive: usize,
    /// ... of which one side's day set strictly contains the other's.
    one_sided: usize,
    /// ... of which the two day sets are identical.
    identical: usize,
}

/// S3: how far one arm's accounts reach. Rule-independent, and asserted so.
#[derive(Clone, Default)]
struct ReachRow {
    /// Endings with at least one holder.
    events: usize,
    /// Held claims summed over those endings.
    holders: usize,
    /// Endings whose account reaches two or more peoples.
    two_plus: usize,
    /// Endings whose account reaches three or more peoples.
    three_plus: usize,
    /// The most peoples any one account reached.
    max_peoples: usize,
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
    /// Directed seam traversals available, victim->raider then raider->victim.
    edges: (usize, usize),
    /// Reach, indexed by [`Arm::ALL`].
    reach: [ReachRow; 4],
    /// Divergence, indexed by [`Arm::ALL`] then [`Accumulation::ALL`].
    div: [[DivRow; 3]; 4],
    /// Direction decomposition, indexed by [`Accumulation::ALL`], under
    /// [`Arm::Both`] alone — the arm S1 asks about.
    dir: [DirectionRow; 3],
    /// CONTROL: holders on which this probe's [`Arm::Both`] walk disagreed
    /// with the shipped `Contact::WithRaidSeam` walk. Must be zero.
    both_mismatch: usize,
    /// CONTROL: the same for [`Arm::None`] against `Contact::Descent`.
    descent_mismatch: usize,
    /// CONTROL: endings where a directed arm reached somebody [`Arm::Both`]
    /// did not. Must be zero — a restriction cannot add.
    restriction_added: usize,
    /// CONTROL: endings where descent reached somebody a directed arm did not.
    /// Must be zero — an added edge cannot remove.
    edge_removed: usize,
    /// CONTROL: endings where two accumulation rules disagreed on reach. Must
    /// be zero; reach is graph reachability.
    rule_disagreements: usize,
}

/// Compare a probe walk against the shipped one, holder for holder.
///
/// Returns the number of holders that differ in ANY reported field — presence,
/// hop count, rung, or remembered day. A count rather than a bool so a red
/// says how wrong, and so a per-seed column can show it.
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

/// The remembered days each people holds, and how many holders each has.
fn by_people(read: &WorldRead, held: &[Held]) -> BTreeMap<String, BTreeSet<u64>> {
    let mut out: BTreeMap<String, BTreeSet<u64>> = BTreeMap::new();
    for h in held {
        let Some(p) = read.people_of.get(&h.claim.holder) else {
            continue;
        };
        if let Value::Number(day) = &h.claim.object {
            out.entry(p.clone()).or_default().insert(day.to_bits());
        }
    }
    out
}

/// Every quantity S1-S3 asks for, over one world.
fn measure_seed(seed: u64, led: &Ledger, read: &WorldRead) -> SeedRow {
    let probe = Probe {
        led,
        lineage: &read.lineage,
        seam: &read.seam,
        ladders: &read.ladders,
        durations: &read.durations,
    };
    let shipped_graph = contact_of(led);
    let mut row = SeedRow {
        seed,
        endings: read.endings.len(),
        edges: read.seam.counts(),
        ..Default::default()
    };

    for e in &read.endings {
        let foreign = e.is_foreign();
        if foreign {
            row.foreign += 1;
        }
        // Holder sets per arm at rule 0, kept so the ordering controls can be
        // checked PER ENDING — an aggregate count cannot see one ending
        // gaining a holder while another loses one.
        let mut sets: [BTreeSet<EntityId>; 4] = Default::default();

        for (ai, arm) in Arm::ALL.iter().enumerate() {
            let mut first_len: Option<usize> = None;
            for (ri, rule) in Accumulation::ALL.iter().enumerate() {
                let held = probe.walk(*rule, *arm, e.subject);
                match first_len {
                    None => first_len = Some(held.len()),
                    Some(n) if n != held.len() => row.rule_disagreements += 1,
                    _ => {}
                }

                // ---- S1, under Arm::Both only ----
                if *arm == Arm::Both {
                    let d = &mut row.dir[ri];
                    for h in &held {
                        d.holders += 1;
                        if !h.seam.is_empty() {
                            d.crossers += 1;
                        }
                        for kind in &h.seam {
                            *d.traversals.entry(*kind).or_default() += 1;
                        }
                        let holder_people = read.people_of.get(&h.claim.holder);
                        let is_cross = match (holder_people, e.people.as_str()) {
                            (Some(p), subject) => {
                                !p.is_empty() && !subject.is_empty() && p != subject
                            }
                            _ => false,
                        };
                        if !is_cross {
                            continue;
                        }
                        d.cross_people += 1;
                        *d.depth_hist.entry(h.seam.len()).or_default() += 1;
                        match h.seam.len() {
                            0 => d.cross_people_no_crossing += 1,
                            1 => *d.cross_people_once.entry(h.seam[0]).or_default() += 1,
                            _ => d.cross_people_multi += 1,
                        }
                        if let Some(first) = h.seam.first() {
                            *d.first_crossing.entry(*first).or_default() += 1;
                        }
                    }
                }

                // ---- S2 ----
                if foreign {
                    let ap = e
                        .attacker_people
                        .as_ref()
                        .expect("a foreign ending names an attacker people");
                    let sets_by_people = by_people(read, &held);
                    if let (Some(v), Some(r)) =
                        (sets_by_people.get(&e.people), sets_by_people.get(ap))
                    {
                        let div = &mut row.div[ai][ri];
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

                // ---- S3, plus the equivalence controls ----
                if ri == 0 {
                    sets[ai] = held.iter().map(|h| h.claim.holder).collect();
                    if !held.is_empty() {
                        let peoples: BTreeSet<&String> = held
                            .iter()
                            .filter_map(|h| read.people_of.get(&h.claim.holder))
                            .collect();
                        let reach = &mut row.reach[ai];
                        reach.events += 1;
                        reach.holders += held.len();
                        if peoples.len() >= 2 {
                            reach.two_plus += 1;
                        }
                        if peoples.len() >= 3 {
                            reach.three_plus += 1;
                        }
                        reach.max_peoples = reach.max_peoples.max(peoples.len());
                    }
                }

                // THE EQUIVALENCE CONTROL, on every ending and every rule for
                // the two arms that HAVE a shipped counterpart.
                if *arm == Arm::Both || *arm == Arm::None {
                    let contact = if *arm == Arm::Both {
                        Contact::WithRaidSeam
                    } else {
                        Contact::Descent
                    };
                    let shipped = variants_about_accumulating(
                        &Walk {
                            ledger: led,
                            lineage: &read.lineage,
                            contact: &shipped_graph,
                            policy: Transmission {
                                contact,
                                ..Transmission::AS_SHIPPED
                            },
                        },
                        &read.ladders,
                        &read.durations,
                        *rule,
                        e.subject,
                        PREDICATE,
                    );
                    let bad = mismatches(&held, &shipped);
                    if *arm == Arm::Both {
                        row.both_mismatch += bad;
                    } else {
                        row.descent_mismatch += bad;
                    }
                }
            }
        }

        // Ordering controls, per ending: descent <= each directed arm <= both.
        for ai in [2usize, 3] {
            if !sets[ai].is_subset(&sets[1]) {
                row.restriction_added += 1;
            }
            if !sets[0].is_subset(&sets[ai]) {
                row.edge_removed += 1;
            }
        }
    }

    row
}

/// Print a `label -> count` distribution compactly.
fn show_hist<K: std::fmt::Debug + Ord>(hist: &BTreeMap<K, usize>) -> String {
    if hist.is_empty() {
        return "(none)".to_string();
    }
    hist.iter()
        .map(|(k, v)| format!("{k:?}:{v}"))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Sum one field over the panel.
fn sum<F: Fn(&SeedRow) -> usize>(rows: &[SeedRow], f: F) -> usize {
    rows.iter().map(f).sum()
}

// ===========================================================================
// S1 / S2 / S3 — THE DISCRIMINATOR.
// ===========================================================================

/// S1, S2 and S3 over the 12-seed panel: the direction decomposition, whether
/// pooling survives a directed restriction, and reach under each direction.
///
/// claim: structural(seed: panel) — false-positive seed-loop flag; the loop
/// binds a census-panel prefix, not a search over seeds.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn which_way_the_account_crosses_the_seam() {
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

    println!("\n============ THE UNDERTOW SUBSTRATE PROBE ============");
    println!(
        "panel                : {} seeds (census seeds 0-11), {} measured, skipped {skipped:?}",
        PANEL.len(),
        rows.len()
    );
    println!("predicate            : {PREDICATE}");
    println!(
        "baseline             : {} — every arm here restricts the SEAM only; the clock is Off \
         throughout, exactly as The Parley's contact arms were",
        Transmission::AS_SHIPPED.label()
    );
    println!(
        "method               : direction SIMULATED on the shipped undirected graph. No \
         directed arm is implemented; nothing in windows/hearsay/src changes."
    );

    let endings = sum(&rows, |r| r.endings);
    let foreign = sum(&rows, |r| r.foreign);
    let v_edges: usize = rows.iter().map(|r| r.edges.0).sum();
    let r_edges: usize = rows.iter().map(|r| r.edges.1).sum();
    println!(
        "\nSUBSTRATE, THIS RUN  : {endings} endings, {foreign} foreign ({:.2}%)",
        pct(foreign, endings)
    );
    println!(
        "  controls           : parley_readout.rs pins {BASELINE_ENDINGS_12} endings / \
         {BASELINE_FOREIGN_12} foreign on this panel; The Parley spec §3.1 published \
         {} / {}. {}",
        PARLEY_SPEC_ENDINGS_12,
        PARLEY_SPEC_FOREIGN_12,
        if (BASELINE_ENDINGS_12, BASELINE_FOREIGN_12)
            == (PARLEY_SPEC_ENDINGS_12, PARLEY_SPEC_FOREIGN_12)
        {
            "The two AGREE at this commit."
        } else {
            "THE TWO DISAGREE — read the committed constant, not the spec: The Underworld \
             moved settlement placement after that spec was written."
        }
    );
    println!(
        "  directed seam      : {v_edges} victim->raider traversals, {r_edges} raider->victim \
         (equal by construction — contact_of writes both halves of one fact)"
    );

    // PER SEED, so a panel total is never read as if it were tight.
    println!(
        "\n  {:<6} {:>9} {:>9} {:>9} {:>10} {:>10} {:>10} {:>10}",
        "seed", "endings", "foreign", "v->r ed", "d:holders", "b:holders", "v:holders", "r:holders"
    );
    for r in &rows {
        println!(
            "  {:<6} {:>9} {:>9} {:>9} {:>10} {:>10} {:>10} {:>10}",
            r.seed,
            r.endings,
            r.foreign,
            r.edges.0,
            r.reach[0].holders,
            r.reach[1].holders,
            r.reach[2].holders,
            r.reach[3].holders,
        );
    }

    // =====================================================================
    // S1 — THE DIRECTION DECOMPOSITION.
    // =====================================================================
    println!("\n=== S1 — WHICH WAY THE WINNING PATH CROSSED ===");
    println!(
        "  Under Arm::Both (The Parley as it shipped). A holder is CROSS-PEOPLE when its \
         people differs from the ending subject's."
    );
    for (ri, rule) in Accumulation::ALL.iter().enumerate() {
        let mut d = DirectionRow::default();
        for r in &rows {
            let s = &r.dir[ri];
            d.holders += s.holders;
            d.crossers += s.crossers;
            d.cross_people += s.cross_people;
            d.cross_people_no_crossing += s.cross_people_no_crossing;
            d.cross_people_multi += s.cross_people_multi;
            for (k, v) in &s.cross_people_once {
                *d.cross_people_once.entry(*k).or_default() += v;
            }
            for (k, v) in &s.first_crossing {
                *d.first_crossing.entry(*k).or_default() += v;
            }
            for (k, v) in &s.traversals {
                *d.traversals.entry(*k).or_default() += v;
            }
            for (k, v) in &s.depth_hist {
                *d.depth_hist.entry(*k).or_default() += v;
            }
        }
        let once_v = *d
            .cross_people_once
            .get(&EdgeKind::VictimToRaider)
            .unwrap_or(&0);
        let once_r = *d
            .cross_people_once
            .get(&EdgeKind::RaiderToVictim)
            .unwrap_or(&0);
        let first_v = *d
            .first_crossing
            .get(&EdgeKind::VictimToRaider)
            .unwrap_or(&0);
        let first_r = *d
            .first_crossing
            .get(&EdgeKind::RaiderToVictim)
            .unwrap_or(&0);
        println!("\n  --- rule: {} ---", rule.label());
        println!("    held claims                        : {}", d.holders);
        println!(
            "    ... whose winning path used the seam: {} ({:.2}%)",
            d.crossers,
            pct(d.crossers, d.holders)
        );
        println!(
            "    CROSS-PEOPLE holders               : {}",
            d.cross_people
        );
        println!(
            "      reached with NO crossing         : {} ({:.2}%) — the hop-0 co-witness \
             (the named attacker) and its line under descent",
            d.cross_people_no_crossing,
            pct(d.cross_people_no_crossing, d.cross_people)
        );
        println!(
            "      crossed EXACTLY once, {:<14}: {once_v} ({:.2}% of cross-people)",
            EdgeKind::VictimToRaider.label(),
            pct(once_v, d.cross_people)
        );
        println!(
            "      crossed EXACTLY once, {:<14}: {once_r} ({:.2}% of cross-people)",
            EdgeKind::RaiderToVictim.label(),
            pct(once_r, d.cross_people)
        );
        println!(
            "      crossed MORE THAN ONCE           : {} ({:.2}% of cross-people)",
            d.cross_people_multi,
            pct(d.cross_people_multi, d.cross_people)
        );
        println!(
            "    THE SPLIT (single-crossing holders) : {once_v} {} vs {once_r} {} = {}",
            EdgeKind::VictimToRaider.label(),
            EdgeKind::RaiderToVictim.label(),
            if once_v + once_r == 0 {
                "no single-crossing holder at all".to_string()
            } else {
                format!(
                    "{:.1}% / {:.1}%",
                    pct(once_v, once_v + once_r),
                    pct(once_r, once_v + once_r)
                )
            }
        );
        println!(
            "    FIRST crossing of every crosser     : {first_v} {} vs {first_r} {}",
            EdgeKind::VictimToRaider.label(),
            EdgeKind::RaiderToVictim.label(),
        );
        println!(
            "    seam TRAVERSALS on winning paths    : {}",
            show_hist(&d.traversals)
        );
        println!(
            "    crossings-per-cross-people-holder   : {}",
            show_hist(&d.depth_hist)
        );
    }

    // =====================================================================
    // S3 — REACH UNDER EACH DIRECTION. (Printed before S2 because S2's
    // `compared` population is easier to read once reach is on the page.)
    // =====================================================================
    println!("\n=== S3 — REACH UNDER EACH DIRECTION ===");
    println!(
        "  {:<11} {:>9} {:>10} {:>9} {:>9} {:>9} {:>9}",
        "arm", "events", "holders", "2+", "2+%", "3+", "3+%"
    );
    let mut three_pct = [0.0f64; 4];
    for (ai, arm) in Arm::ALL.iter().enumerate() {
        let events = sum(&rows, |r| r.reach[ai].events);
        let holders = sum(&rows, |r| r.reach[ai].holders);
        let two = sum(&rows, |r| r.reach[ai].two_plus);
        let three = sum(&rows, |r| r.reach[ai].three_plus);
        let max = rows
            .iter()
            .map(|r| r.reach[ai].max_peoples)
            .max()
            .unwrap_or(0);
        three_pct[ai] = pct(three, events);
        println!(
            "  {:<11} {events:>9} {holders:>10} {two:>9} {:>8.2}% {three:>9} {:>8.2}%   max \
             peoples {max}",
            arm.label(),
            pct(two, events),
            three_pct[ai],
        );
    }
    println!(
        "  The Parley's readout measures 2.33% of endings reaching 3+ peoples under the \
         undirected seam, with a maximum of 5, on its 40-seed panel (re-derived on the merge \
         product; it published 2.35% and a maximum of 6 before The Underworld moved \
         settlement placement); descent reaches 3 on exactly 0. This panel is its first 12 \
         seeds."
    );

    // =====================================================================
    // S2 — DOES POOLING SURVIVE A DIRECTED RESTRICTION?
    // =====================================================================
    println!("\n=== S2 — THE PARLEY'S §6.5 QUANTITIES, PER DIRECTION ===");
    println!(
        "  Population: cross-people endings where BOTH the victim's people and the raider's \
         people hold the account. `identical` is the pooling signal; `mutex` is §6.5's frozen \
         divergence measure."
    );
    for (ri, rule) in Accumulation::ALL.iter().enumerate() {
        println!("\n  === RULE: {} ===", rule.label());
        println!(
            "    {:<11} {:>10} {:>8} {:>10} {:>10} {:>9} {:>9}",
            "arm", "compared", "mutex", "one-sided", "identical", "mutex%", "ident%"
        );
        let mut mutex = [0usize; 4];
        let mut ident = [0usize; 4];
        for (ai, arm) in Arm::ALL.iter().enumerate() {
            let c = sum(&rows, |r| r.div[ai][ri].compared);
            let m = sum(&rows, |r| r.div[ai][ri].mutually_exclusive);
            let o = sum(&rows, |r| r.div[ai][ri].one_sided);
            let s = sum(&rows, |r| r.div[ai][ri].identical);
            mutex[ai] = m;
            ident[ai] = s;
            println!(
                "    {:<11} {c:>10} {m:>8} {o:>10} {s:>10} {:>8.2}% {:>8.2}%",
                arm.label(),
                pct(m, c),
                pct(s, c)
            );
        }
        println!(
            "    vs BOTH (the baseline): mutex {} -> v->r {:.2}x, r->v {:.2}x  |  identical \
             {} -> v->r {:.2}x, r->v {:.2}x",
            mutex[1],
            mutex[2] as f64 / mutex[1].max(1) as f64,
            mutex[3] as f64 / mutex[1].max(1) as f64,
            ident[1],
            ident[2] as f64 / ident[1].max(1) as f64,
            ident[3] as f64 / ident[1].max(1) as f64,
        );
        println!(
            "    vs DESCENT (The Parley's own comparison): mutex {} -> both {:.2}x, v->r \
             {:.2}x, r->v {:.2}x  |  identical {} -> both {:.2}x, v->r {:.2}x, r->v {:.2}x",
            mutex[0],
            mutex[1] as f64 / mutex[0].max(1) as f64,
            mutex[2] as f64 / mutex[0].max(1) as f64,
            mutex[3] as f64 / mutex[0].max(1) as f64,
            ident[0],
            ident[1] as f64 / ident[0].max(1) as f64,
            ident[2] as f64 / ident[0].max(1) as f64,
            ident[3] as f64 / ident[0].max(1) as f64,
        );
        // CONDITIONAL, never a sentence that cannot be wrong.
        let pooled_v = ident[2] > ident[0] && mutex[2] < mutex[0];
        let pooled_r = ident[3] > ident[0] && mutex[3] < mutex[0];
        let pooled_both = ident[1] > ident[0] && mutex[1] < mutex[0];
        println!(
            "    READING under {}: descent->both {}; descent->v->r {}; descent->r->v {}. {}",
            rule.label(),
            if pooled_both {
                "POOLS"
            } else {
                "does not pool"
            },
            if pooled_v { "POOLS" } else { "does not pool" },
            if pooled_r { "POOLS" } else { "does not pool" },
            if pooled_both && (pooled_v || pooled_r) {
                "Pooling survives a ONE-WAY seam, so symmetry is not the mechanism — the \
                 world pools."
            } else if pooled_both {
                "Pooling appears only with BOTH directions live, which is what an \
                 artifact-of-symmetry reading predicts."
            } else {
                "The baseline arm did not pool on this rule, so neither directed arm can \
                 discriminate here; read another rule."
            }
        );
    }

    // =====================================================================
    // POSITIVE CONTROLS ONLY. No assertion below is about an outcome.
    // =====================================================================
    assert!(!rows.is_empty(), "control: the panel produced no seeds");
    assert!(
        skipped.len() < PANEL.len(),
        "control: every panel seed was skipped for want of a year rung"
    );

    // (1) THE INSTRUMENT IS THE SHIPPED INSTRUMENT. Asserted on every ending
    // and every rule, not sampled — a reimplementation that drifted would make
    // every number above unattributable.
    let both_mismatch = sum(&rows, |r| r.both_mismatch);
    assert_eq!(
        both_mismatch, 0,
        "control: this probe's Arm::Both walk must reproduce \
         variants_about_accumulating under Contact::WithRaidSeam holder for holder; \
         {both_mismatch} holders differed"
    );
    let descent_mismatch = sum(&rows, |r| r.descent_mismatch);
    assert_eq!(
        descent_mismatch, 0,
        "control: this probe's Arm::None walk must reproduce \
         variants_about_accumulating under Contact::Descent holder for holder; \
         {descent_mismatch} holders differed"
    );

    // (2) THE ARMS ARE ORDERED. A restriction of the seam cannot add a holder,
    // and adding a seam edge to descent cannot remove one.
    let restriction_added = sum(&rows, |r| r.restriction_added);
    assert_eq!(
        restriction_added, 0,
        "control: a directed arm is a RESTRICTION of the undirected one and cannot reach \
         somebody it does not; it did on {restriction_added} endings"
    );
    let edge_removed = sum(&rows, |r| r.edge_removed);
    assert_eq!(
        edge_removed, 0,
        "control: adding a one-way seam edge to descent cannot remove a holder; it did on \
         {edge_removed} endings"
    );
    let rule_disagreements = sum(&rows, |r| r.rule_disagreements);
    assert_eq!(
        rule_disagreements, 0,
        "control: reach is graph reachability and cannot depend on the accumulation rule; \
         {rule_disagreements} endings disagreed"
    );

    // (3) THE PROBE REACHED THE POPULATION IT REPORTS ON.
    assert_eq!(
        endings, BASELINE_ENDINGS_12,
        "control: this panel must re-derive parley_readout.rs's pinned ending count exactly"
    );
    assert_eq!(
        foreign, BASELINE_FOREIGN_12,
        "control: this panel must re-derive parley_readout.rs's pinned foreign count exactly"
    );
    let multiplicative = Accumulation::ALL
        .iter()
        .position(|r| *r == Accumulation::Multiplicative)
        .expect("Accumulation::ALL carries Multiplicative");
    let descent_mutex = sum(&rows, |r| r.div[0][multiplicative].mutually_exclusive);
    assert_eq!(
        descent_mutex, BASELINE_MUTUALLY_EXCLUSIVE_12,
        "control: descent x multiplicative must re-derive parley_readout.rs's pinned \
         mutually-exclusive count exactly"
    );
    assert!(
        v_edges > 0 && r_edges > 0,
        "control: the panel must carry seam traversals in BOTH directions, or no direction \
         arm measures anything: {v_edges} victim->raider, {r_edges} raider->victim"
    );
    let crossers = sum(&rows, |r| r.dir[multiplicative].crossers);
    assert!(
        crossers > 0,
        "control: some holder's winning path must have used the seam, or the crossing \
         attribution never fired at all"
    );
    let compared = sum(&rows, |r| r.div[1][multiplicative].compared);
    assert!(
        compared > 0,
        "control: the panel must carry cross-people endings where both sides hold"
    );
}

// ===========================================================================
// S4 — THE 12-SEED PREFIX AGAINST A 100-SEED PANEL.
// ===========================================================================

/// One seed's S4 contribution.
struct Wide {
    /// Which seed.
    seed: u64,
    /// Endings in the world.
    endings: usize,
    /// ... of which the attacker is of another people.
    foreign: usize,
    /// Cross-people endings where both sides' peoples hold the account.
    compared: usize,
    /// ... of which each side holds a day the other holds nowhere.
    mutex: usize,
}

/// A named quantity of a [`Wide`] row. A plain `fn` pointer rather than a
/// boxed closure so the table below stays a simple array type.
type Quantity = (&'static str, fn(&Wide) -> usize);

/// S4: how much does the 12-seed panel over-read relative to 100 seeds?
///
/// **THIS IS A PROXY FOR THE CENSUS COMPARISON, NOT THE CENSUS COMPARISON.**
/// The merge queue established that The Parley's 12-seed control amplifies
/// against the census by 4.5-8.6x across three census metrics, with `endings`
/// retaining an unexplained 1.89x above the amplified figure — and nobody has
/// measured whether `endings` carries its own amplification. A census cannot
/// be run here (it is authored on the canonical box, decisions 0063/0079), so
/// the measurable half is prefix-vs-population SENSITIVITY: the same three
/// quantities over seeds 0-11 against seeds 0-99, as per-seed means.
///
/// A 100-seed panel is not the census's 1,000 seeds either, so this bounds the
/// sampling sensitivity rather than resolving the 1.89x. If the prefix reads
/// close to 1.00x here, the 1.89x is NOT a small-panel artifact and the
/// explanation lies elsewhere (the census's own build depth, its pin sets, or
/// a genuinely different denominator). If it reads far from 1.00x, small-panel
/// sampling is a live candidate and a wider control is worth its cost.
///
/// ## The answer this probe returned, so the conditional above is not left dangling
///
/// **The prefix reads LOW, not high** — per-seed means over seeds 0-11 against
/// seeds 0-99: `endings` **0.700x**, `foreign` 0.623x, `compared` 0.623x,
/// `mutex` 0.616x. So the first branch is the one that fired, in the direction
/// that makes the residual WORSE: sampling cannot be the source of a 12-seed
/// control reading *high* against the census, and correcting for seed count
/// would push the 1.89x to roughly **2.7x** (1.89 / 0.70). The residual lives
/// in what the census does DIFFERENTLY — build depth, pin sets, or the
/// denominator it divides by — not in how many seeds it draws.
///
/// **Both halves of that are needed, and the second bounds the first.** All
/// four z-scores sit within ~1.7 standard errors of the wide mean (|z| <= 1.62),
/// so the 0.70x is NOT itself a demonstrated downward bias — it is what a
/// 12-seed draw from a distribution this skewed looks like (per-seed `endings`
/// over 100 seeds: min 38, median 543, max 2,472). The honest statement is that
/// the 12-seed panel shows no measurable UPWARD amplification and the data
/// cannot support a claim of downward amplification either. Quoting the ratio
/// without the z would manufacture a finding; the probe prints both.
///
/// claim: structural(seed: panel) — false-positive seed-loop flag; the loop
/// binds a census-panel prefix, not a search over seeds.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn how_much_the_twelve_seed_prefix_over_reads() {
    let components = hornvale_worldgen::WorldComponents::assemble().expect("components assemble");
    let multiplicative = Accumulation::ALL
        .iter()
        .position(|r| *r == Accumulation::Multiplicative)
        .expect("Accumulation::ALL carries Multiplicative");

    let mut wide: Vec<Wide> = Vec::new();
    let mut skipped: Vec<u64> = Vec::new();

    for seed in 0..WIDE {
        let world = build(seed);
        let led = &world.ledger;
        let Some(read) = read_world(led, &components) else {
            skipped.push(seed);
            continue;
        };
        let shipped_graph = contact_of(led);
        let walk = Walk {
            ledger: led,
            lineage: &read.lineage,
            contact: &shipped_graph,
            policy: Transmission::AS_SHIPPED, // descent, no clock — §3.4's own instrument
        };
        let mut w = Wide {
            seed,
            endings: read.endings.len(),
            foreign: 0,
            compared: 0,
            mutex: 0,
        };
        for e in &read.endings {
            if !e.is_foreign() {
                continue;
            }
            w.foreign += 1;
            let ap = e
                .attacker_people
                .as_ref()
                .expect("a foreign ending names an attacker people");
            let held = variants_about_accumulating(
                &walk,
                &read.ladders,
                &read.durations,
                Accumulation::ALL[multiplicative],
                e.subject,
                PREDICATE,
            );
            let mut sets: BTreeMap<String, BTreeSet<u64>> = BTreeMap::new();
            for c in &held {
                let Some(p) = read.people_of.get(&c.holder) else {
                    continue;
                };
                if let Value::Number(day) = &c.object {
                    sets.entry(p.clone()).or_default().insert(day.to_bits());
                }
            }
            if let (Some(v), Some(r)) = (sets.get(&e.people), sets.get(ap)) {
                w.compared += 1;
                if v.difference(r).count() > 0 && r.difference(v).count() > 0 {
                    w.mutex += 1;
                }
            }
        }
        wide.push(w);
    }

    let prefix: Vec<&Wide> = wide
        .iter()
        .filter(|w| w.seed < PANEL.len() as u64)
        .collect();
    let all: Vec<&Wide> = wide.iter().collect();

    let per_seed = |rows: &[&Wide], f: fn(&Wide) -> usize| -> f64 {
        if rows.is_empty() {
            return f64::NAN;
        }
        rows.iter().map(|w| f(w)).sum::<usize>() as f64 / rows.len() as f64
    };

    println!("\n=== S4 — 12-SEED PREFIX vs 100-SEED PANEL ===");
    println!(
        "  A PROXY, NOT THE CENSUS COMPARISON. The census cannot be run here (authored on the \
         canonical box, decisions 0063/0079). This measures prefix-vs-population sensitivity \
         only, and 100 seeds is not the census's 1,000 either."
    );
    println!(
        "  seeds measured: {} of {WIDE}, skipped {skipped:?}; prefix = seeds 0-{}",
        wide.len(),
        PANEL.len() - 1
    );
    println!(
        "\n  {:<24} {:>10} {:>10} {:>10} {:>10} {:>10} {:>9} {:>8}",
        "quantity", "12 total", "12/seed", "100 total", "100/seed", "prefix/100", "SE(n=12)", "z"
    );
    let quantities: [Quantity; 4] = [
        ("endings", |w| w.endings),
        ("foreign", |w| w.foreign),
        ("compared (both hold)", |w| w.compared),
        ("mutually exclusive", |w| w.mutex),
    ];
    for (name, f) in quantities {
        let p12: usize = prefix.iter().map(|w| f(w)).sum();
        let p100: usize = all.iter().map(|w| f(w)).sum();
        let m12 = per_seed(&prefix, f);
        let m100 = per_seed(&all, f);
        // Per-seed spread over the wide panel, and the standard error a
        // 12-seed mean drawn from it would carry.
        let var = all
            .iter()
            .map(|w| {
                let d = f(w) as f64 - m100;
                d * d
            })
            .sum::<f64>()
            / (all.len().max(2) - 1) as f64;
        let se = var.sqrt() / (prefix.len().max(1) as f64).sqrt();
        println!(
            "  {name:<24} {p12:>10} {m12:>10.3} {p100:>10} {m100:>10.3} {:>9.3}x {se:>9.2} \
             {:>8.2}",
            m12 / if m100 == 0.0 { f64::MIN_POSITIVE } else { m100 },
            (m12 - m100) / if se == 0.0 { f64::MIN_POSITIVE } else { se },
        );
    }
    println!(
        "\n  HOW TO READ 'prefix/100': the 12-seed per-seed mean over the 100-seed per-seed \
         mean. 1.00x means the prefix is representative on that quantity at this sample \
         size; ABOVE 1.00x means the prefix reads HIGH, below it reads LOW."
    );
    println!(
        "  READ THE z COLUMN BESIDE IT OR NOT AT ALL. These per-seed distributions are \
         heavily skewed (see the quartiles below), so a 12-seed mean is noisy and a ratio \
         printed alone invites a reader to treat sampling noise as a bias. `SE(n=12)` is the \
         standard error of a 12-seed mean drawn from the wide panel's own spread and `z` is \
         how many of those the prefix sits from the wide mean. It is a CALIBRATION, not a \
         test: the prefix is a subset of the 100, so the two are not independent, and |z| \
         near or below 1 means the prefix is indistinguishable from a representative draw."
    );
    println!(
        "  WHAT IT BEARS ON: the merge queue's unexplained 1.89x on `endings` above its \
         amplified figure — a figure in which the 12-seed control reads HIGH against the \
         census. If `endings` reads at or BELOW 1.00x here, seed-count sampling cannot be \
         the source of an upward amplification, and the residual lies in what the census \
         does DIFFERENTLY (build depth, pin sets, denominator), not in how many seeds it \
         draws. That would make the residual larger, not smaller."
    );

    // Per-seed spread, so a mean is not read as if it were tight.
    let mut ends: Vec<f64> = all.iter().map(|w| w.endings as f64).collect();
    ends.sort_by(f64::total_cmp);
    println!(
        "\n  endings per seed over 100: min {:.0}, q1 {:.0}, median {:.0}, q3 {:.0}, max {:.0}",
        ends[0],
        ends[ends.len() / 4],
        ends[ends.len() / 2],
        ends[3 * ends.len() / 4],
        ends[ends.len() - 1]
    );
    let mut fors: Vec<f64> = all.iter().map(|w| w.foreign as f64).collect();
    fors.sort_by(f64::total_cmp);
    println!(
        "  foreign per seed over 100: min {:.0}, q1 {:.0}, median {:.0}, q3 {:.0}, max {:.0}",
        fors[0],
        fors[fors.len() / 4],
        fors[fors.len() / 2],
        fors[3 * fors.len() / 4],
        fors[fors.len() - 1]
    );

    // POSITIVE CONTROLS. The prefix must be the panel The Parley pinned, which
    // is what makes the comparison a comparison and not two different reads.
    let p_endings: usize = prefix.iter().map(|w| w.endings).sum();
    let p_foreign: usize = prefix.iter().map(|w| w.foreign).sum();
    let p_mutex: usize = prefix.iter().map(|w| w.mutex).sum();
    assert_eq!(
        p_endings, BASELINE_ENDINGS_12,
        "control: the prefix of this wide panel must BE seeds 0-11 and re-derive their pinned \
         ending count"
    );
    assert_eq!(
        p_foreign, BASELINE_FOREIGN_12,
        "control: ... and their pinned foreign count"
    );
    assert_eq!(
        p_mutex, BASELINE_MUTUALLY_EXCLUSIVE_12,
        "control: ... and their pinned mutually-exclusive count under descent x multiplicative"
    );
    assert!(
        all.len() > prefix.len(),
        "control: the wide panel must be wider than its own prefix"
    );
}

// ===========================================================================
// THE NON-VACUITY CONTROLS. Cheap, hand-built, and NOT ignored.
//
// The panel's controls prove this probe's walk IS the shipped walk, but not
// that its direction filter can move anything: an `Arm` that silently admitted
// every edge would satisfy every subset assertion above and print a table of
// four identical columns. These push the filter through a fixture whose two
// seam crossings are both victim->raider, so the two directed arms MUST
// disagree.
// ===========================================================================

/// Assemble the fixture world's ladders and durations — one 50-day generation
/// and 150-day lifespan per people, the same values `tests/augmented_walk.rs`
/// and `parley_readout.rs`'s own controls use.
fn hand_read(led: &Ledger, peoples: &[&str]) -> WorldRead {
    let lineage = lineage_of(led);
    let mut durations = PeopleDurations::default();
    for p in peoples {
        durations.insert(p, StdDays::new(50.0).ok(), StdDays::new(150.0).ok());
    }
    let ladders = PeopleLadders::of(led, &durations);
    let mut people_of: BTreeMap<EntityId, String> = BTreeMap::new();
    for occ in lineage.all() {
        if let Some(Value::Text(p)) = led.value_of(occ, hornvale_history::OCC_PEOPLE) {
            people_of.insert(occ, p.clone());
        }
    }
    WorldRead {
        lineage,
        seam: directed_seam_of(led),
        durations,
        ladders,
        people_of,
        endings: endings_of(led),
    }
}

/// The directed seam must project back onto the shipped undirected one exactly
/// — otherwise every direction attributed above is attributed on a graph the
/// model does not walk.
#[test]
fn the_directed_seam_reproduces_the_shipped_contact_graph() {
    let led = a_round_trip_through_another_people();
    let shipped = contact_of(&led);
    let directed = directed_seam_of(&led);
    let lineage = lineage_of(&led);

    let mut checked = 0usize;
    let mut with_edges = 0usize;
    for occ in lineage.all() {
        let mut mine: Vec<(EntityId, u64)> = directed
            .out_of(occ)
            .iter()
            .map(|(peer, day, _)| (*peer, day.to_bits()))
            .collect();
        mine.sort();
        mine.dedup();
        let theirs: Vec<(EntityId, u64)> = shipped
            .peers_of(occ)
            .iter()
            .map(|(peer, day)| (*peer, day.to_bits()))
            .collect();
        assert_eq!(
            mine, theirs,
            "the directed seam must project onto contact_of's undirected one at {occ:?}"
        );
        checked += 1;
        if !theirs.is_empty() {
            with_edges += 1;
        }
    }

    // POSITIVE CONTROLS: the comparison must not be vacuous.
    assert!(checked > 0, "control: no occupation was compared at all");
    assert!(
        with_edges > 0,
        "control: the fixture must carry at least one contact edge, or the equality above \
         compares empty against empty"
    );
    let (v, r) = directed.counts();
    assert_eq!(
        (v, r),
        (2, 2),
        "control: the fixture's two raids give two traversals of each half"
    );
}

/// The direction filter must be able to MOVE the holder set, and the crossing
/// attribution must be able to fire.
///
/// The fixture's two seam crossings are `2 -> 5` (2 is the victim, 5 the
/// raider) and `6 -> 4` (6 the victim, 4 the raider), so the account's route
/// out of the human line and back is victim->raider both times. Under
/// `r->v only` neither is open and the walk must collapse to descent.
#[test]
fn the_direction_filter_moves_the_holder_set_in_the_control() {
    let led = a_round_trip_through_another_people();
    let read = hand_read(&led, &["human", "kobold"]);
    let probe = Probe {
        led: &led,
        lineage: &read.lineage,
        seam: &read.seam,
        ladders: &read.ladders,
        durations: &read.durations,
    };
    let subject = read.endings[0].subject;

    let holders = |arm: Arm| -> BTreeSet<EntityId> {
        probe
            .walk(Accumulation::Multiplicative, arm, subject)
            .iter()
            .map(|h| h.claim.holder)
            .collect()
    };
    let descent = holders(Arm::None);
    let both = holders(Arm::Both);
    let v2r = holders(Arm::VictimToRaider);
    let r2v = holders(Arm::RaiderToVictim);

    assert!(
        both.len() > descent.len(),
        "control: the seam must add holders on this fixture: descent {} vs both {}",
        descent.len(),
        both.len()
    );
    assert!(
        v2r.len() > r2v.len(),
        "control: the fixture's crossings are victim->raider, so restricting to that half \
         must keep more holders than restricting to the other: v->r {} vs r->v {}",
        v2r.len(),
        r2v.len()
    );
    assert!(
        v2r.is_subset(&both) && r2v.is_subset(&both),
        "control: a restriction of the seam cannot reach outside the undirected walk"
    );
    assert!(
        descent.is_subset(&v2r) && descent.is_subset(&r2v),
        "control: adding a one-way edge to descent cannot remove a holder"
    );

    // The crossing attribution itself: at least one holder's winning path must
    // record a victim->raider step, and none may record a raider->victim one.
    let held = probe.walk(Accumulation::Multiplicative, Arm::Both, subject);
    let v_steps: usize = held
        .iter()
        .flat_map(|h| h.seam.iter())
        .filter(|k| **k == EdgeKind::VictimToRaider)
        .count();
    let r_steps: usize = held
        .iter()
        .flat_map(|h| h.seam.iter())
        .filter(|k| **k == EdgeKind::RaiderToVictim)
        .count();
    assert!(
        v_steps > 0,
        "control: the crossing attribution never fired on a fixture built around two \
         victim->raider crossings"
    );
    assert_eq!(
        r_steps, 0,
        "control: the fixture has no raider->victim crossing to attribute, yet {r_steps} were \
         recorded"
    );
}

/// This probe's walk must reproduce the shipped one on the hand-built fixture
/// too, not only on the panel — so a red in the heavy battery can be told
/// apart from a red in the walk itself without a live worldgen.
#[test]
fn the_probe_walk_reproduces_the_shipped_walk_on_the_control() {
    let led = a_round_trip_through_another_people();
    let read = hand_read(&led, &["human", "kobold"]);
    let probe = Probe {
        led: &led,
        lineage: &read.lineage,
        seam: &read.seam,
        ladders: &read.ladders,
        durations: &read.durations,
    };
    let shipped_graph = contact_of(&led);
    let subject = read.endings[0].subject;

    let mut compared = 0usize;
    for (arm, contact) in [
        (Arm::None, Contact::Descent),
        (Arm::Both, Contact::WithRaidSeam),
    ] {
        for rule in Accumulation::ALL {
            let mine = probe.walk(rule, arm, subject);
            let shipped = variants_about_accumulating(
                &Walk {
                    ledger: &led,
                    lineage: &read.lineage,
                    contact: &shipped_graph,
                    policy: Transmission {
                        contact,
                        ..Transmission::AS_SHIPPED
                    },
                },
                &read.ladders,
                &read.durations,
                rule,
                subject,
                PREDICATE,
            );
            assert!(
                !shipped.is_empty(),
                "control: the shipped walk must hold somebody under {}/{}, or the comparison \
                 is empty against empty",
                arm.label(),
                rule.label()
            );
            assert_eq!(
                mismatches(&mine, &shipped),
                0,
                "the probe walk must reproduce the shipped one under {}/{}",
                arm.label(),
                rule.label()
            );
            compared += 1;
        }
    }
    assert_eq!(compared, 6, "control: six arm/rule pairs must be compared");
}
