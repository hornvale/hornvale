//! Turning committed facts into held claims.

use crate::ladder::PrecisionLadder;
use crate::lineage::Lineage;
use crate::stance;
use hornvale_kernel::Claim;
use hornvale_kernel::Precision;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use hornvale_kernel::provenance::Provenance;
use std::collections::BTreeMap;

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
/// type-audit: bare-ok(identifier-text: predicate)
pub fn variants_about(
    ledger: &Ledger,
    lineage: &Lineage,
    ladder: &PrecisionLadder,
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

    // Best candidate per holder, keyed by (lossy_steps, hops, witness)
    // ascending -- the ordering the doc comment states. BTreeMap keeps the
    // result ascending by holder for free, as in `claims_about`.
    let mut best: BTreeMap<EntityId, ((u32, u32, EntityId), Claim)> = BTreeMap::new();

    for w in &witnesses {
        let mut c = base.clone();
        c.holder = *w;
        best.insert(*w, ((0, 0, *w), c)); // hops 0, Witnessed, nothing lossy
    }

    for w in &witnesses {
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

            let mut c = base.clone();
            c.holder = *w;
            let mut lossy_steps: u32 = 0;
            for pair in path.windows(2) {
                let (teller, hearer) = (pair[0], pair[1]);
                if stance::is_lossy(ledger, lineage, subject, teller, hearer) {
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

/// Campaign 3's accumulating sibling of [`variants_about`].
///
/// Each retelling widens a continuous damage width by the step's generational
/// span ([`crate::amplitude::gen_span`]) under `rule`, and the reported
/// [`Precision`] is resolved from that width at emit against the TELLER's
/// people's ladder. Where campaign 2 spends a whole rung per lossy step —
/// welding firing rate to firing depth — this separates them, so a frequent
/// small-amplitude step costs little and a rare large one costs a lot.
///
/// The width starts at the finest rung's span rather than zero, because
/// `Accumulation::Multiplicative` cannot move a zero width.
///
/// Multi-path holders keep the LEAST-corrupted telling, ordered by (1)
/// smallest final width, (2) fewest hops, (3) smallest witness `EntityId` —
/// the same shape as `variants_about`, with width replacing lossy-step count
/// because width is now the thing that varies.
///
/// type-audit: bare-ok(identifier-text: predicate)
pub fn variants_about_accumulating(
    ledger: &Ledger,
    lineage: &Lineage,
    ladders: &crate::ladder::PeopleLadders,
    durations: &crate::durations::PeopleDurations,
    rule: crate::accumulate::Accumulation,
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
            // NOT `.map(|s| s.get())` — a bare `s` closure parameter
            // false-positives `cli/tests/claim_shape.rs`'s seed-loop detector,
            // which then demands a `claim:` tag that would be inaccurate here.
            // Task 1 hit this in the plan's own sample code.
            let mut width = ladder
                .span(Precision::FINEST)
                .map(|days| days.get())
                .unwrap_or(0.0);

            let mut c = base.clone();
            c.holder = *w;
            for pair in path.windows(2) {
                let (teller, hearer) = (pair[0], pair[1]);
                width = rule.step(
                    width,
                    crate::amplitude::gen_span(ledger, durations, teller, hearer),
                );
                let precision = crate::accumulate::precision_at(ladder, width);
                let object = match &c.object {
                    Value::Number(day) => Value::Number(ladder.apply(precision, *day)),
                    other => other.clone(),
                };
                c = c.retold_by_lossy(hearer, precision, object);
            }

            // f64 has no total order for a BTreeMap key; `to_bits` on a
            // non-negative finite width is monotone, so it orders correctly.
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
