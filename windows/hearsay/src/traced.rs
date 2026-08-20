//! The seam-aware relaxation, with the route and width the shipped `Claim`
//! discards carried out alongside it.
//!
//! [`crate::derive::variants_about_accumulating`] computes which witness a
//! holder's telling descends from and the accumulated damage width at every
//! step, and drops both at the return (`reached.into_values().map(|t|
//! t.claim).collect()`) — a `Claim` has no field for either. A question about
//! the ROUTE (did this holder's account cross a people boundary? whose line
//! did it come down?) is therefore unaskable through the shipped signature.
//!
//! This module is a SIBLING of that relaxation, not a replacement: it does
//! not change `variants_about_accumulating`'s behaviour, and the two are held
//! to agreement holder-for-holder by `tests/traced_walk.rs`'s heavy battery.
//! It began life as a private copy inside
//! `windows/hearsay/tests/undertow_readout.rs` (that file's own module doc
//! explains why the copy existed); this promotes it to a public, `pub`
//! first-class walk so later work does not need to keep re-deriving it.
//!
//! Two differences from the readout's private copy, both restorations of
//! behaviour the readout was entitled to skip because it only ever called its
//! copy under one policy:
//!
//! - **The clock is honoured.** The readout's copy dropped
//!   [`crate::clock::Clock`] entirely because every arm it measured ran
//!   `Clock::Off`, where the check is dead code. This walk is called under
//!   whatever policy its caller supplies, so it applies
//!   [`crate::clock::admits`] exactly as `derive.rs` does — to every witness
//!   and every hearer, refusing a step whose hearer had already ended
//!   (spec §6.2's H1).
//! - **`Contact::Descent` truly means descent-only.** The readout's private
//!   `Probe::tellable` always walked the raid seam, because the readout never
//!   called it under `Contact::Descent` (the descent cells there use the
//!   shipped walk directly). This walk reads `walk.policy.contact` the same
//!   way `derive.rs`'s `tellable` does, so it agrees with the shipped walk
//!   under BOTH `Contact` arms, which is what the agreement battery checks.

use crate::accumulate::{Accumulation, precision_at};
use crate::amplitude::gen_span;
use crate::clock;
use crate::contact::Contact;
use crate::derive::witnesses_of;
use crate::durations::PeopleDurations;
use crate::ladder::{PeopleLadders, PrecisionLadder};
use crate::transmission::{Crossing, Walk};
use hornvale_kernel::Claim;
use hornvale_kernel::Precision;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use hornvale_kernel::provenance::Provenance;
use std::collections::{BTreeMap, BTreeSet};

/// Whether a winning-route step descended the founding tree or rode a raid
/// seam.
///
/// A hearer reachable both ways (a child who is also a raid peer) is
/// attributed to descent — the same resolution [`tellable`] documents, and
/// the reason `Descent` is declared before `Seam`: sorting the pair
/// `(EntityId, Carrier)` and keeping the first of a duplicate id keeps the
/// carrier that needs no seam.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Carrier {
    /// A parent -> child step down the founding tree.
    Descent,
    /// An undirected raid edge from [`crate::contact::ContactGraph::peers_of`].
    Seam,
}

/// One cross-people step of a winning route.
///
/// Recorded whenever the step's teller and hearer belong to different
/// peoples, independent of [`Crossing`] — under [`Crossing::Free`] the
/// penalty this step paid is exactly `0.0`, but the crossing itself still
/// happened and is still reported, because the route is what this type
/// exists to expose.
///
/// type-audit: bare-ok(count: edges)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Crossed {
    /// `ContactGraph::edges_between` for the two peoples this step crossed —
    /// the count that priced it (spec §5.1).
    pub edges: u32,
    /// Which kind of edge carried this step.
    pub carrier: Carrier,
}

/// A held telling with the route and width the shipped `Claim` discards.
///
/// type-audit: bare-ok(count: width)
#[derive(Clone, Debug, PartialEq)]
pub struct HeldTelling {
    /// The account exactly as the shipped walk reports it.
    pub claim: Claim,
    /// The originating witness this telling descends from — the walk's
    /// `key.2`, fixed once per path and carried forward at every step.
    pub witness: EntityId,
    /// Accumulated damage width at emit, at full precision — the quantity
    /// the shipped `Claim` discards once a rung has been resolved from it.
    pub width: f64,
    /// The winning route's cross-people steps, in traversal order. Empty for
    /// a witness and for anyone reached without ever leaving their people.
    pub crossings: Vec<Crossed>,
}

/// One node's best-known telling, as the relaxation converges toward it.
///
/// Identical in shape to `derive.rs`'s private `Telling`, plus the one field
/// that carries the route forward: the key rises strictly along every edge
/// (see `derive.rs`'s termination argument, which this walk shares
/// unchanged), so a predecessor chain would also be well-defined, but a
/// carried list needs no separate reconstruction to be right.
struct Telling<'a> {
    /// This telling's rank against any rival telling reaching the same
    /// holder — `(width.to_bits(), hops, witness)`, ascending.
    key: (u64, u32, EntityId),
    /// The accumulated damage width, at full precision.
    width: f64,
    /// The originating witness's people's ladder, fixed for the whole path.
    ladder: &'a PrecisionLadder,
    /// The claim as this holder received it.
    claim: Claim,
    /// The cross-people steps of this telling's route, in traversal order.
    crossings: Vec<Crossed>,
}

/// The people `occ` belongs to, or `""` when the ledger does not say.
///
/// A duplicate of `derive.rs`'s private `people_of` (that function is not
/// `pub(crate)`), reading the same predicate the same way so both walks agree
/// about what an unlabelled occupation's people is: one nameless people, not
/// a sentinel to branch on.
fn people_of(ledger: &Ledger, occ: EntityId) -> &str {
    match ledger.value_of(occ, hornvale_history::OCC_PEOPLE) {
        Some(Value::Text(p)) => p.as_str(),
        _ => "",
    }
}

/// The day `(subject, predicate)` happened, when the ledger states it as a
/// number — a duplicate of `derive.rs`'s private `event_day_of`.
fn event_day_of(ledger: &Ledger, subject: EntityId, predicate: &str) -> Option<f64> {
    match ledger.value_of(subject, predicate) {
        Some(Value::Number(day)) => Some(*day),
        _ => None,
    }
}

/// Whether `walk`'s clock lets `occ` hold a claim about an event on
/// `event_day` — a duplicate of `derive.rs`'s private `admits`.
fn admits(walk: &Walk, event_day: Option<f64>, occ: EntityId) -> bool {
    match event_day {
        Some(day) => clock::admits(walk.policy.clock, walk.ledger, occ, day),
        None => true,
    }
}

/// Who `node` can tell, each paired with the carrier that would tell them:
/// its children in the founding tree always (`Carrier::Descent`), plus —
/// only under [`Contact::WithRaidSeam`] — every peer it met on or after
/// `event_day` (`Carrier::Seam`).
///
/// Matches `derive.rs`'s private `tellable` in which hearers it admits (the
/// day condition and the `Contact` gate are identical), and additionally
/// resolves a hearer reachable BOTH ways toward `Carrier::Descent`: the
/// shipped `tellable` sorts and dedups plain `EntityId`s, so it is blind to
/// which edge carried a doubly-reachable hearer and the choice cannot change
/// any width (the penalty reads the two peoples, never the carrier) —
/// dedup keeps only the FIRST of a run of equal ids, and `Descent` sorting
/// before `Seam` is what makes that first entry the descent one.
fn tellable(walk: &Walk, node: EntityId, event_day: Option<f64>) -> Vec<(EntityId, Carrier)> {
    let mut out: Vec<(EntityId, Carrier)> = walk
        .lineage
        .children_of(node)
        .iter()
        .map(|c| (*c, Carrier::Descent))
        .collect();
    if walk.policy.contact == Contact::WithRaidSeam {
        for (peer, day) in walk.contact.peers_of(node) {
            if event_day.is_none_or(|event| *day >= event) {
                out.push((*peer, Carrier::Seam));
            }
        }
    }
    out.sort();
    out.dedup_by_key(|(id, _)| *id);
    out
}

/// What the step `teller -> hearer` pays for crossing a people boundary
/// (`derive.rs`'s private `crossing_penalty`), with the `edges_between` that
/// priced it handed back so the winning route can carry it.
///
/// `Some(edges)` whenever the step's teller and hearer belong to different
/// peoples, whatever [`Crossing`] arm is in force — under [`Crossing::Free`]
/// the returned penalty is `0.0`, but the crossing itself still happened.
/// `x + 0.0` is exact in IEEE-754 for every finite `x`, which is what keeps
/// the two arms' widths identical to the penny under `Free` and lets the
/// agreement battery hold both arms to the shipped walk exactly.
fn crossing_info(
    walk: &Walk,
    ladder: &PrecisionLadder,
    teller: EntityId,
    hearer: EntityId,
) -> (f64, Option<u32>) {
    let (from, to) = (
        people_of(walk.ledger, teller),
        people_of(walk.ledger, hearer),
    );
    if from == to {
        return (0.0, None);
    }
    let edges = walk.contact.edges_between(from, to);
    if walk.policy.crossing == Crossing::Free {
        return (0.0, Some(edges as u32));
    }
    let unit = ladder
        .span(Precision::FINEST)
        .map(|days| days.get())
        .unwrap_or(0.0);
    (unit / (1.0 + edges as f64), Some(edges as u32))
}

/// The seam-aware relaxation, carrying the route and width the shipped
/// [`crate::derive::variants_about_accumulating`] drops. Projects to that
/// function exactly: `traced_variants_about_accumulating(..).into_iter()
/// .map(|t| t.claim).collect()` equals `variants_about_accumulating(..)`
/// holder-for-holder under every policy, rule, and predicate the two share —
/// `tests/traced_walk.rs`'s heavy agreement battery is the guard.
///
/// Line for line the shipped relaxation (same width-first key, same strict
/// replacement, same clock and witness rules — see `derive.rs`'s doc comment
/// for the termination argument, which is unchanged and not repeated here),
/// plus the two things the shipped signature cannot express: the accumulated
/// width at emit, and the winning route's cross-people steps.
///
/// type-audit: bare-ok(identifier-text: predicate)
pub fn traced_variants_about_accumulating(
    walk: &Walk,
    ladders: &PeopleLadders,
    durations: &PeopleDurations,
    rule: Accumulation,
    subject: EntityId,
    predicate: &str,
) -> Vec<HeldTelling> {
    let ledger = walk.ledger;
    let lineage = walk.lineage;
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
    let witness_set: BTreeSet<EntityId> = witnesses.iter().copied().collect();
    let event_day = event_day_of(ledger, subject, predicate);

    let mut reached: BTreeMap<EntityId, Telling> = BTreeMap::new();
    let mut frontier: BTreeSet<((u64, u32, EntityId), EntityId)> = BTreeSet::new();

    for w in &witnesses {
        if !admits(walk, event_day, *w) {
            continue; // the clock refuses even a witness that had already ended
        }
        let people = match ledger.value_of(*w, hornvale_history::OCC_PEOPLE) {
            Some(Value::Text(p)) => p.clone(),
            _ => String::new(),
        };
        let ladder = ladders.for_people(&people);
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

        for (hearer, carrier) in tellable(walk, node, event_day) {
            if witness_set.contains(&hearer) {
                continue; // a witness is never demoted to an inheritor
            }
            if !admits(walk, event_day, hearer) {
                continue; // the clock refuses the STEP, which orphans the line below it
            }
            let (penalty, edges) = crossing_info(walk, ladder, node, hearer);
            let span = gen_span(ledger, durations, node, hearer) + penalty;
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
        .map(|t| HeldTelling {
            claim: t.claim,
            witness: t.key.2,
            width: t.width,
            crossings: t.crossings,
        })
        .collect()
}
