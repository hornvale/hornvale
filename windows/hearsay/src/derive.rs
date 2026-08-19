//! Turning committed facts into held claims.

use crate::clock;
use crate::contact::Contact;
use crate::ladder::PrecisionLadder;
use crate::lineage::Lineage;
use crate::stance;
use crate::transmission::Walk;
use hornvale_kernel::Claim;
use hornvale_kernel::Precision;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use hornvale_kernel::provenance::Provenance;
use std::collections::{BTreeMap, BTreeSet};

/// The day the event under test happened, when the ledger states it as a
/// number.
///
/// `None` for a non-`Number` object — a `Text` or `Entity` predicate has no
/// day, so neither the clock nor the seam's contact-day condition has anything
/// to compare against and both must fall open rather than guess. Same posture
/// [`crate::amplitude::gen_span`] takes toward an unreadable duration.
fn event_day_of(ledger: &Ledger, subject: EntityId, predicate: &str) -> Option<f64> {
    match ledger.value_of(subject, predicate) {
        Some(Value::Number(day)) => Some(*day),
        _ => None,
    }
}

/// Whether `walk`'s clock lets `occ` hold a claim about an event on
/// `event_day`. A dateless event admits everybody, as does [`clock::Clock::Off`].
fn admits(walk: &Walk, event_day: Option<f64>, occ: EntityId) -> bool {
    match event_day {
        Some(day) => clock::admits(walk.policy.clock, walk.ledger, occ, day),
        None => true,
    }
}

/// Who `node` can tell, under this walk's topology: its children in the
/// founding tree always, plus — only under [`Contact::WithRaidSeam`] — every
/// peer it met on or after `event_day`.
///
/// The day condition is spec §5.3's first: a meeting cannot carry news of
/// something that has not happened. Two tests in `tests/augmented_walk.rs`
/// pin it, and they pin different halves:
/// `the_seam_refuses_a_raid_that_predates_the_event` is the only test in the
/// crate that reddens when the condition is DELETED, and
/// `the_seam_admits_a_raid_on_the_event_day` is the only one that reddens
/// when `>=` is weakened to `>` — every other fixture straddles the
/// comparison without landing on it, so the whole suite stayed green under
/// either operator until that boundary case was added.
fn tellable(walk: &Walk, node: EntityId, event_day: Option<f64>) -> Vec<EntityId> {
    let mut out: Vec<EntityId> = walk.lineage.children_of(node).to_vec();
    if walk.policy.contact == Contact::WithRaidSeam {
        for (peer, day) in walk.contact.peers_of(node) {
            if event_day.is_none_or(|event| *day >= event) {
                out.push(*peer);
            }
        }
    }
    out.sort();
    out.dedup();
    out
}

/// Everyone present when `(subject, predicate)` happened.
///
/// Only an ENDING has parties beyond its subject (spec §6.1). For an ending on
/// day `d`, that is the subject itself, every child of the subject founded on
/// exactly day `d` — the survivors who fled and refounded — and the occupation
/// named by `occ-ended-by` when it is `Entity`-valued. Any other predicate has
/// the subject alone.
///
/// The day comparison is exact equality, and deliberately so: the bake writes
/// a refounding at precisely its parent's ending day (477 of 562 such pairs on
/// seed 42, with all three gap quartiles at 0.0), so there is no threshold to
/// tune and no near-miss band to argue about.
///
/// type-audit: bare-ok(identifier-text: predicate)
pub fn witnesses_of(
    ledger: &Ledger,
    lineage: &Lineage,
    subject: EntityId,
    predicate: &str,
) -> Vec<EntityId> {
    let mut out = vec![subject];
    if predicate == hornvale_history::OCC_ENDED {
        if let Some(Value::Number(day)) = ledger.value_of(subject, hornvale_history::OCC_ENDED) {
            let day = *day;
            // Survivors are DIRECT children, not deeper kin — so ask for them
            // directly rather than filtering the whole descendant set down to
            // one hop, which is what this used to do.
            for child in lineage.children_of(subject) {
                if let Some(Value::Number(f)) =
                    ledger.value_of(*child, hornvale_history::OCC_FOUNDED)
                    && *f == day
                {
                    out.push(*child);
                }
            }
        }
        if let Some(Value::Entity(attacker)) =
            ledger.value_of(subject, hornvale_history::OCC_ENDED_BY)
        {
            out.push(*attacker);
        }
    }
    out.sort();
    out.dedup();
    out
}

/// Every claim held about `(subject, predicate)`, ascending by holder.
///
/// Every witness of the event (`witnesses_of`) holds it at `hops = 0` with
/// grade `Witnessed`; every witness's descendants inherit it, downgraded and
/// one hop further per retelling. Two rules keep the walk from producing a
/// wrong answer at the seams (spec §6.1): a holder that is itself a witness
/// is never demoted to an inheritor (a survivor saw the raid; it does not
/// merely hear about it from the village it fled), and a holder reachable
/// from two witnesses takes the nearer telling — the minimum hop count, not
/// whichever witness's walk happened to reach it first. Nothing here draws,
/// mutates content, or consults anything but the ledger.
///
/// type-audit: bare-ok(identifier-text: predicate)
pub fn claims_about(
    ledger: &Ledger,
    lineage: &Lineage,
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
        // A witness holds a claim at the finest rung; Task 2b/3/4 wire the
        // ladder and the two filters that coarsen it on a lossy retelling.
        precision: Precision::FINEST,
    };
    let witnesses = witnesses_of(ledger, lineage, subject, predicate);
    // BTreeMap keeps the result ascending and deterministic.
    let mut held: BTreeMap<EntityId, Claim> = BTreeMap::new();
    for w in &witnesses {
        let mut c = base.clone();
        c.holder = *w;
        held.insert(*w, c); // hops 0, Witnessed
    }
    for w in &witnesses {
        // The walk carries the hop count out with it, so the depth no longer
        // costs a per-descendant re-derivation of that descendant's ancestry.
        for (d, hops) in lineage.descendants_with_hops(*w) {
            if witnesses.contains(&d) {
                continue; // a witness is never demoted to an inheritor
            }
            let entry = held.entry(d).or_insert_with(|| {
                let mut c = base.clone();
                c.holder = d;
                c.grade = base.grade.on_transmission();
                c.hops = hops;
                c
            });
            // reachable from two witnesses: the NEARER telling is the one held
            if entry.hops > hops {
                entry.hops = hops;
            }
        }
    }
    held.into_values().collect()
}

/// Every VARIANT of a claim about `(subject, predicate)`, ascending by
/// holder — campaign 2's path-aware sibling of [`claims_about`], which this
/// function does not modify (its tests pin campaign 1's no-decay baseline).
///
/// Witnesses are seeded exactly as in `claims_about`: `hops = 0`,
/// `Provenance::Witnessed`, `Precision::FINEST`, never demoted to an
/// inheritor. From each witness, the claim is retold step by step down the
/// ancestry chain to every descendant: a step where teller and hearer share a
/// [`stance::Stance`] toward `subject` is frictionless
/// ([`Claim::retold_by`]); a step that crosses a stance boundary coarsens the
/// remembered day one rung ([`PrecisionLadder::coarser`]) and hands the
/// already-snapped value to [`Claim::retold_by_lossy`] — this function does
/// the day arithmetic via [`PrecisionLadder::apply`], never the kernel.
///
/// Once content varies by path, hop count alone no longer orders two
/// tellings of the same claim, so a holder reachable by more than one path
/// keeps **the least-corrupted telling**, ordered by (1) fewest lossy steps,
/// (2) fewest hops, (3) smallest witness `EntityId`. The first is the
/// semantic rule — a community holds the clearest version it can reach; the
/// third exists only to make the order total so two equally-good paths
/// cannot race, and carries no meaning of its own.
///
/// **This walk IGNORES `walk.policy.contact` — it takes the arm and throws it
/// away.** It steps parent->child down the founding tree, honouring the clock
/// and [`stance::Perpetration`] and nothing else; it never consults the raid
/// seam, so it reports descent-only reach under [`Contact::Descent`] and
/// [`Contact::WithRaidSeam`] alike. **A readout that varies `Contact` through
/// this function measures nothing**: it will print identical columns per arm,
/// which reads as "contact does nothing" — a wrong attribution wearing a
/// null's clothes. [`variants_about_accumulating`] is the seam-aware walk and
/// is the one to reach for. Leaving this one descent-only is deliberate, not
/// an omission: campaign 2's pinned baseline is measured against it.
///
/// type-audit: bare-ok(identifier-text: predicate)
pub fn variants_about(
    walk: &Walk,
    ladder: &PrecisionLadder,
    subject: EntityId,
    predicate: &str,
) -> Vec<Claim> {
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
    let event_day = event_day_of(ledger, subject, predicate);

    // Best candidate per holder, keyed by (lossy_steps, hops, witness)
    // ascending -- the ordering the doc comment states. BTreeMap keeps the
    // result ascending by holder for free, as in `claims_about`.
    let mut best: BTreeMap<EntityId, ((u32, u32, EntityId), Claim)> = BTreeMap::new();

    for w in &witnesses {
        if !admits(walk, event_day, *w) {
            continue; // the clock refuses even a witness that had already ended
        }
        let mut c = base.clone();
        c.holder = *w;
        best.insert(*w, ((0, 0, *w), c)); // hops 0, Witnessed, nothing lossy
    }

    for w in &witnesses {
        if !admits(walk, event_day, *w) {
            continue; // nobody can be told by a community the clock refused
        }
        for d in lineage.descendants_of(*w) {
            if witnesses.contains(&d) {
                continue; // a witness is never demoted to an inheritor
            }
            let ancestry = lineage.ancestry(d);
            let pos = ancestry
                .iter()
                .position(|a| a == w)
                .expect("descendants_of(w) guarantees w is in d's ancestry");
            // ancestry(d) is [d, parent(d), ..., root]; the walk needs the
            // other direction, witness down to descendant.
            let mut path: Vec<EntityId> = ancestry[..=pos].to_vec();
            path.reverse();
            // The clock refuses a STEP whose hearer had already ended, which
            // orphans every community below that step even when it is itself
            // alive at the event (spec §6.2's H1). On the founding tree this
            // path is the only route, so refusing it here is that rule.
            if path[1..].iter().any(|h| !admits(walk, event_day, *h)) {
                continue;
            }

            let mut c = base.clone();
            c.holder = *w;
            let mut lossy_steps: u32 = 0;
            for pair in path.windows(2) {
                let (teller, hearer) = (pair[0], pair[1]);
                if stance::is_lossy(
                    ledger,
                    lineage,
                    walk.policy.perpetration,
                    subject,
                    teller,
                    hearer,
                ) {
                    lossy_steps += 1;
                    let precision = ladder.coarser(c.precision);
                    let object = match &c.object {
                        Value::Number(day) => Value::Number(ladder.apply(precision, *day)),
                        other => other.clone(),
                    };
                    c = c.retold_by_lossy(hearer, precision, object);
                } else {
                    c = c.retold_by(hearer);
                }
            }

            let key = (lossy_steps, c.hops, *w);
            match best.get(&d) {
                Some((best_key, _)) if *best_key <= key => {} // the held telling is at least as good
                _ => {
                    best.insert(d, (key, c));
                }
            }
        }
    }

    best.into_values().map(|(_, c)| c).collect()
}

/// One node's best-known telling, as the augmented walk relaxes toward it.
///
/// `key` is the ordering [`variants_about_accumulating`] documents —
/// `(width.to_bits(), hops, witness)` — carried beside the state it ranks so
/// the relaxation never has to re-derive it. `ladder` is the ORIGINATING
/// witness's people's ladder, fixed for the whole path (spec §5.4).
struct Telling<'a> {
    /// This telling's rank against any rival telling reaching the same holder.
    key: (u64, u32, EntityId),
    /// The accumulated damage width, at full precision. The rung is resolved
    /// from it at emit, never stored in its place.
    width: f64,
    /// The originating witness's people's ladder.
    ladder: &'a PrecisionLadder,
    /// The claim as this holder received it.
    claim: Claim,
}

/// Campaign 3's accumulating sibling of [`variants_about`].
///
/// Each retelling widens a continuous damage width by the step's generational
/// span ([`crate::amplitude::gen_span`]) under `rule`, and the reported
/// [`Precision`] is resolved from that width at emit against the ladder of the
/// ORIGINATING WITNESS's people — fixed once per path, before the walk, not
/// re-read at each step. Campaign 3 noted that this is equivalent to "the
/// teller's people's ladder" only for as long as a transmission path never
/// crosses a people boundary, and that a later campaign adding a cross-people
/// edge would turn it into a real choice. THAT CAMPAIGN IS THIS ONE
/// ([`Contact::WithRaidSeam`]), and the choice is made explicitly: the ladder
/// stays the originating witness's (`KNOW-teller-ladder-at-emit`, carried
/// forward), because re-reading it per step would confound this campaign's
/// headline with a second change nothing measured.
///
/// The width starts at the finest rung's span rather than zero, because
/// `Accumulation::Multiplicative` cannot move a zero width.
///
/// **A BEST-FIRST RELAXATION, NOT A PATH ENUMERATION, AND THE REASON IS THE
/// TOPOLOGY.** Until this campaign the graph was the founding tree, where a
/// witness reaches a descendant by exactly one route, so the walk could
/// recover it by slicing that descendant's ancestry. [`Contact::WithRaidSeam`]
/// adds an undirected edge, which makes the graph CYCLIC — the slice is not
/// merely inconvenient there, it is undefined, and a walk that patched around
/// it would report plausible widths that were wrong. So the walk relaxes
/// instead: each node keeps its best-known telling, and is re-expanded only
/// when a STRICTLY smaller key reaches it.
///
/// **Termination** rests on the key rising strictly along every edge, and the
/// key is `(width.to_bits(), hops, witness)` compared LEXICOGRAPHICALLY WITH
/// WIDTH FIRST. That shape is why it takes two halves, both load-bearing:
///
/// 1. **width is non-decreasing**
///    ([`crate::accumulate::Accumulation::step`], pinned by
///    `tests/accumulate.rs::every_rule_is_non_decreasing`), so the PRIMARY
///    component can never fall. This is the half that makes that test
///    load-bearing here rather than belt-and-braces, and it was measured, not
///    argued: a rule that halved the width instead — hops still rising by one
///    per edge — relaxed the cycle fixture **1,078 times against the shipped
///    5**, halting only once the `f64` underflowed to 0.0 and the primary
///    component finally tied. That is floating-point exhaustion, not
///    termination.
/// 2. **at equal width, `hops` rises by exactly one per edge**, so a lap of a
///    cycle still strictly increases the key rather than tying forever.
///
/// Together they give a strict increase on every edge, which is what makes a
/// node final when it is popped and drains the frontier. **The comparison's
/// DIRECTION is load-bearing too**: accept a strictly larger key instead and a
/// two-cycle widens forever, which is a genuine hang and was measured as one.
/// Strictness itself — `<=` rather than `<` — is the one part that is NOT
/// about termination: it buys a stable first-arrival tie-break and cheap
/// insurance. `tests/augmented_walk.rs` pins draining on a real cycle.
///
/// **The clock** ([`crate::clock::Clock`]) is applied to every holder as it is
/// admitted, WITNESSES INCLUDED, and refusing a hearer refuses the step: a
/// community below a refused teller is orphaned even when it was itself alive
/// at the event, because nobody could have told it. That is spec §6.2's H1
/// mechanism and it is why the clock removes strictly more than the holders
/// that fail it.
///
/// Multi-path holders keep the LEAST-corrupted telling, ordered by (1)
/// smallest final width, (2) fewest hops, (3) smallest witness `EntityId` —
/// the same shape as `variants_about`, with width replacing lossy-step count
/// because width is now the thing that varies. A witness is never demoted to
/// an inheritor and is never re-expanded from a passing telling: its own seed
/// is the finest telling any route can hand it, so a route through it can only
/// ever be worse for everyone below it too. **That last argument assumes every
/// people's ladder shares a rung-0 span**, which is true wherever rung 0 is the
/// world's day length — but [`crate::ladder::PrecisionLadder::with_social`]
/// sorts rungs by length, so a people whose generation ran shorter than a day
/// would seed a narrower base and a route through a witness could genuinely
/// win. Unreachable from today's bake; noted because the argument, not the
/// code, is what would be wrong.
///
/// type-audit: bare-ok(identifier-text: predicate)
pub fn variants_about_accumulating(
    walk: &Walk,
    ladders: &crate::ladder::PeopleLadders,
    durations: &crate::durations::PeopleDurations,
    rule: crate::accumulate::Accumulation,
    subject: EntityId,
    predicate: &str,
) -> Vec<Claim> {
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

    // BTreeMap keeps the result ascending by holder for free, as in
    // `claims_about`; the BTreeSet beside it is the frontier, ordered by the
    // same key so the cheapest telling is always relaxed first.
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
        // NOT `.map(|s| s.get())` — a bare `s` closure parameter
        // false-positives `cli/tests/claim_shape.rs`'s seed-loop detector,
        // which then demands a `claim:` tag that would be inaccurate here.
        // Task 1 hit this in the plan's own sample code.
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

        for hearer in tellable(walk, node, event_day) {
            if witness_set.contains(&hearer) {
                continue; // a witness is never demoted to an inheritor
            }
            if !admits(walk, event_day, hearer) {
                continue; // the clock refuses the STEP, which orphans the line below it
            }
            let next_width = rule.step(
                width,
                crate::amplitude::gen_span(ledger, durations, node, hearer),
            );
            let precision = crate::accumulate::precision_at(ladder, next_width);
            let next_object = match &claim.object {
                Value::Number(day) => Value::Number(ladder.apply(precision, *day)),
                other => other.clone(),
            };
            let next_claim = claim.retold_by_lossy(hearer, precision, next_object);
            // f64 has no total order for a BTreeMap key; `to_bits` on a
            // non-negative finite width is monotone, so it orders correctly.
            let next_key = (next_width.to_bits(), next_claim.hops, witness);
            match reached.get(&hearer) {
                // Strictly smaller, never equal. This is a tie-break, not the
                // termination argument (see the doc comment, which needs BOTH
                // a non-falling width and a rising hop count): given those, an
                // equal key re-expanded would still not loop. What strictness
                // buys is that the FIRST telling to arrive at a given key is
                // the one kept, so two exactly-equal routes cannot race.
                Some(held) if held.key <= next_key => continue,
                Some(held) => {
                    frontier.remove(&(held.key, hearer));
                }
                None => {}
            }
            reached.insert(
                hearer,
                Telling {
                    key: next_key,
                    width: next_width,
                    ladder,
                    claim: next_claim,
                },
            );
            frontier.insert((next_key, hearer));
        }
    }

    reached.into_values().map(|t| t.claim).collect()
}
