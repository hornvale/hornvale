//! The Undertow, Task 3: reproduce the non-argmin defect `probe_tiebreak_rules.rs`
//! found (spec §3.5, commit `95cbdd33`) independently, and ask the question that
//! probe predates and cannot answer: **does Task 2's contact-weighted crossing
//! penalty (`Crossing::ContactWeighted`) change how many holders sit at a tie?**
//!
//! ## Why this is a new file and not an edit to the existing one
//!
//! `probe_tiebreak_rules.rs` was committed before `Crossing` existed
//! (`52dc0045` lands after `95cbdd33`), so its `Transmission` is pinned to
//! `Transmission::AS_SHIPPED` with only `contact` varied — it can only ever
//! see `Crossing::Free`. Extending its already-large S0/S1 matrix with a
//! fourth axis would multiply its scope and risk the established S1 numbers
//! it is pinned against. This file asks the narrower S0b question only, over
//! both `Crossing` arms, and is deliberately silent about S1.
//!
//! ## Why this is an EXTENSION of the audited enumerator, not a third one
//!
//! The task brief warns that two earlier probes each reimplemented a
//! *relaxation* (best-first, one telling per node) and therefore inherited
//! the defect instead of detecting it — only a full enumeration of the
//! candidate set can see a shipped answer that is not that set's minimum.
//! The [`Enumerator`] below is `probe_tiebreak_rules.rs`'s enumerator,
//! copied, kept as a full enumeration exactly as there, and given exactly
//! one new capability the shipped relaxation already has: the crossing
//! penalty `derive.rs::crossing_penalty` adds to a cross-people step's width
//! under `Crossing::ContactWeighted`. The formula is not re-derived here —
//! it is transcribed from `derive.rs:108-129` (`span(FINEST) /
//! (1 + edges_between(a, b))`, zero under `Crossing::Free` or a same-people
//! step), the exact term `variants_about_accumulating` adds to `gen_span`
//! before the accumulation rule sees it (`derive.rs:556-558`).
//!
//! Everything else — world reading, `witnesses_of`, simple-path enumeration,
//! the `least_damage` control comparing the shipped answer against the
//! enumerated set's minimum key — is unchanged from the audited instrument.
//! Only `Contact::WithRaidSeam` is enumerated (foreign endings only): the
//! descent arm's argmin question does not depend on `Crossing` at all — see
//! the note beside [`PANEL`] — and `probe_tiebreak_rules.rs` already holds it
//! to zero at 0/246,627.
//!
//! **One more thing this copy omits, harmless today, worth a line for
//! whoever varies `clock` next (fix round 1, Minor 5):** the shipped
//! relaxation (`variants_about_accumulating`) gates every witness and hearer
//! through `admits(walk, event_day, occ)` as well, which reads
//! `walk.policy.clock`; this `Enumerator` never calls `admits` at all. That
//! is exact under `Transmission::AS_SHIPPED` and every policy this file
//! constructs, all of which fix `clock: Clock::Off` (an always-true gate),
//! so nothing here is under-enumerating today. A future
//! campaign that reuses this `Enumerator` under a live clock would silently
//! enumerate routes the shipped walk could never take — `admits` is not
//! optional plumbing, it just happens to be a no-op at every policy this file
//! ever builds.
//!
//! ## Why descent needs no re-measurement under `Crossing::ContactWeighted`
//!
//! `crossing_penalty` is added identically regardless of which `Contact` arm
//! is walked — it fires whenever a step crosses a people boundary, however
//! that step was reached. But the founding tree gives every holder EXACTLY
//! ONE telling (`probe_tiebreak_rules.rs`'s own control, reproduced above:
//! 246,627 holders, 0 not-argmin), which is a structural fact about the tree
//! — one path in, always — independent of any edge's width. A route that is
//! the only candidate at its holder is trivially that holder's argmin no
//! matter what the edges along it cost. So `Crossing` cannot move the descent
//! row, and re-running 246,627 holders' worth of free enumeration to confirm
//! a structural argument would spend real cost to learn nothing.
//!
//! ## Why `multiplicative` sits at zero under both arms, `additive` carries the whole defect
//!
//! **The scale-probe explanation this file's task report first gave
//! ("the penalty's realized magnitude is small relative to real rung gaps")
//! cannot be the mechanism: `multiplicative` reads exactly 0 under
//! `Crossing::Free` too, where the penalty is identically zero.** The real
//! cause is structural, in [`gen_span`] and [`Accumulation::step`], and has
//! nothing to do with `Crossing` at all.
//!
//! **Re-derived against the merge product, and the two halves of that
//! reading fared differently. Only one of them is a vindication.** On the
//! pre-absorption substrate the two arms differed — 36 under `Free` against
//! 90 under `ContactWeighted`, almost all of the rise on `additive` — so the
//! arm looked like it moved the defect and every `additive` figure downstream
//! carried a CONFOUNDED annotation. On the tree this campaign lands on, the
//! two arms are IDENTICAL at 49, all of it `additive`, with `quadrature` and
//! `multiplicative` at exactly 0 under both, and the two arms' defective
//! `(holder, rule)` SETS are identical too (symmetric difference 0, measured
//! below rather than inferred from equal counts).
//!
//! - **ADDITIVE-ONLY is what telescoping predicts, and it now holds
//!   exactly.** As executed it was VIOLATED: `quadrature` ran 4 -> 2, which a
//!   structurally additive-only account does not allow. At 0/0 it holds. That
//!   half is a genuine strengthening.
//! - **ARM-INVARIANCE IS NOT PREDICTED BY TELESCOPING AND MUST NOT BE CLAIMED
//!   AS SUCH.** Telescoping says additive width is hop-blind WITHIN a
//!   same-people segment. `Crossing` reprices CROSS-people steps, which
//!   changes which route wins and therefore which holders get scored at all —
//!   so a count that moved would have been perfectly compatible with
//!   telescoping. The pre-absorption 36 -> 90 was UNEXPLAINED BY the account,
//!   not evidence AGAINST it, and the present 49 -> 49 is an observation the
//!   account is silent about rather than a confirmation of it.
//!
//! `gen_span(teller, hearer) = |founded(hearer) - founded(teller)| / g`
//! (`amplitude.rs:31-51`), where `g` is read from `teller` — the CURRENT
//! node of each step, not a value fixed once from the originating witness
//! (`amplitude.rs:37`, `led.value_of(teller, OCC_PEOPLE)`, called fresh on
//! every hop). **The witness identity never changes along a route — it is
//! the ordering key's `witness` tie-break field — but that is a different
//! invariant from "which people determines `g`", and only the first one is
//! guaranteed.** `tellable` offers `children_of(node)` at every node
//! regardless of people, plus raid-seam peers, so a route CAN cross a people
//! boundary and keep descending inside the new people, at which point `g`
//! changes mid-route.
//!
//! So the telescoping argument is PIECEWISE, not whole-route: within any
//! SAME-PEOPLE SEGMENT of a route — a run of steps whose teller stays in one
//! people, hence one fixed `g` — the segment's `gen_span` terms are
//! consecutive founding-day differences over that fixed constant, and along
//! any LOCALLY MONOTONE run of founding days within the segment (each step's
//! founding day moving the same direction as the last) they **telescope**:
//! the segment's contribution collapses to
//! `(founded(segment end) - founded(segment start)) / g`, depending only on
//! the segment's endpoints, never on how many hops it took inside the
//! segment. A route that never crosses a people boundary (the entire descent
//! population, and the great majority of a contact route's length — the
//! scale probe found 0 of 26,798 winning-path crossings by descent) is a
//! single segment end to end, which is where this file's own not-argmin
//! samples sit.
//!
//! `Accumulation::Additive::step` is literally `width + span`
//! (`accumulate.rs:49-56`) — a running sum — so additive width inherits the
//! segment's telescoping identity directly: two routes between the same two
//! same-people-segment endpoints, differing only in hop count WITHIN that
//! segment, can accumulate to the exact same real width. `Quadrature`
//! (`sqrt(w^2 + s^2)`, summing SQUARES) and `Multiplicative`
//! (`w * (1 + s)`, a running PRODUCT) do neither — breaking one span into
//! several pieces changes the accumulated total under both, generically.
//!
//! **That telescoping identity is exactly the defect's own signature.**
//! Every not-argmin sample this file and `probe_tiebreak_rules.rs` print
//! shows the shipped holder at the SAME width bits and the SAME remembered
//! day as the enumerated argmin, differing only in hop count — precisely the
//! shape additive's degeneracy produces and the other two rules structurally
//! cannot. So the defect is not merely MORE common under `additive` — it is,
//! to first order, an `additive`-only phenomenon that `quadrature` and
//! `multiplicative` are close to immune to by construction.
//!
//! **Verified independently** (not merely asserted from the code): a
//! synthetic check outside this crate, replicating `gen_span`'s and each
//! `Accumulation::step` rule's exact formulas over 100,000 random
//! four-node monotone founding-day chains (fixed generation length, no seam
//! crossing — isolating the width formula from the graph search itself),
//! comparing a 3-hop route against a 1-hop route between the SAME two
//! endpoints:
//!
//! ```text
//! additive          64819   64.82%   bit-identical width (3-hop vs 1-hop)
//! quadrature            0    0.00%
//! multiplicative        0    0.00%
//! ```
//!
//! Telescoping is an identity over the REAL numbers, true on every route
//! regardless of length; it is not a guarantee of BIT-identical `f64` totals,
//! because IEEE 754 addition is not perfectly associative across a different
//! number and order of terms — which is why the additive figure above is a
//! large majority (real ties are common) rather than all 100,000 (some real
//! ties are lost to summation-order rounding). The exact percentage is
//! RNG-dependent and not a claim this file pins; the STRUCTURAL asymmetry
//! between additive and the other two rules is the load-bearing fact, and it
//! is a property of `amplitude.rs` and `accumulate.rs`, unrelated to
//! `Crossing` or to anything this campaign changed.
//!
//! ## Reports only, with one exception
//!
//! Every assertion below is a POSITIVE CONTROL — the enumeration reaches the
//! same population `probe_tiebreak_rules.rs` did, and it contains the shipped
//! answer everywhere. The **one non-control assertion** pins this file's own
//! `Crossing::Free` aggregate to the number this task measured directly from
//! `probe_tiebreak_rules.rs` in the same session (49 of 9,531, 0.5141%) —
//! not because the defect is expected to hold at that exact count forever,
//! but because two independently-restructured instruments landing on the
//! same integer is the strongest evidence this file can offer that it is
//! measuring the same thing the audited probe measured, not a drifted copy
//! of it. If the campaign's `Crossing::ContactWeighted` count differs, that
//! is the finding this task exists to report — see the module doc's closing
//! table in the task report, not an assertion here.
//!
//! Nothing in `windows/hearsay/src` changes and no fix is applied — the task
//! brief forbids it in this campaign, to keep `Transmission::AS_SHIPPED`
//! (which does not include this defect's correction either way) attributable.
//!
//! ## Cost, measured, so the next reader budgets from a number and not a guess
//!
//! On this Mac (`test` profile, which this workspace builds optimized):
//! [`does_the_crossing_penalty_change_the_non_argmin_defect`] cost **728.24 s**
//! (~12.1 min) on its authoring run and **432.03 s** on a re-run later the same
//! day — **this is the most expensive test this campaign wrote**, and the
//! spread between the two runs is the honest width of the estimate, not noise
//! to average away. The cost is the exhaustive enumeration itself: 10,589,340
//! simple routes over 3,177 holders across 12 seeds and both [`Crossing`] arms,
//! where every sibling probe relaxes instead. **Both the cost figures and the
//! route count above are PRE-ABSORPTION**: The Underworld moved settlement
//! placement, the substrate shrank with it, and the re-measured cost is in
//! `docs/timings.md`. A committed cost is a claim with a date; re-measure
//! rather than extrapolate.
//!
//! Neither figure is paid by any gate. This test is `#[ignore]`d into the heavy
//! set, and `hornvale-hearsay` has no entries in `gate-commit`'s subfloor
//! roster at all — so nothing here runs until someone runs it by hand.

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
use std::collections::{BTreeMap, BTreeSet};

/// The 12-seed panel: the same strict prefix `probe_tiebreak_rules.rs` and
/// `parley_readout.rs` use, so this file samples no population an earlier
/// campaign's instrument did not.
const PANEL: [u64; 12] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];

/// The predicate every claim here is about (spec §6.1: only an ending has
/// parties beyond its subject).
const PREDICATE: &str = hornvale_history::OCC_ENDED;

/// Node expansions one ending's enumeration may spend before it is declared
/// capped. Identical to `probe_tiebreak_rules.rs`'s bound: the graph being
/// walked (topology and population) is unchanged by `Crossing`, only its
/// edge weights are, so the same ceiling applies for the same reason.
const MAX_EXPANSIONS: u64 = 4_000_000;

/// Route length one ending's enumeration may reach before it is declared
/// capped.
const MAX_DEPTH: u32 = 2_048;

/// `parley_readout.rs::BASELINE_ENDINGS_12`, re-read as a control that this
/// file's `read_world` reaches the same population `probe_tiebreak_rules.rs`
/// did.
///
/// **RE-DERIVED AGAINST THE MERGE PRODUCT** (5,913 / 138 before The
/// Underworld moved settlement placement).
const BASELINE_ENDINGS_12: usize = 4975;

/// `parley_readout.rs::BASELINE_FOREIGN_12`, the same control for the foreign
/// population this file enumerates over.
const BASELINE_FOREIGN_12: usize = 102;

/// This task's own direct measurement of `probe_tiebreak_rules.rs`'s S0b
/// contact row, run against the merge product: `compared 9531, not the argmin
/// 49, share 0.5141%`. Used below as the one non-control assertion — see the
/// module doc.
///
/// **RE-DERIVED AGAINST THE MERGE PRODUCT.** It read `13164 / 36 / 0.2735%`
/// on the pre-absorption substrate. The two instruments still land on the
/// same integer, which is the whole point of the assertion.
const REPRO_FREE_COMPARED: usize = 9531;

/// Companion to [`REPRO_FREE_COMPARED`].
const REPRO_FREE_NOT_ARGMIN: usize = 49;

// ===========================================================================
// THE ENUMERATOR — `probe_tiebreak_rules.rs`'s, extended with the crossing
// penalty `derive.rs::crossing_penalty` adds under `Crossing::ContactWeighted`.
// Kept as a full enumeration throughout: see the module doc for why that
// distinction is the whole point.
// ===========================================================================

/// One telling that reached a holder, deduplicated to its observable content.
/// Identical in shape to `probe_tiebreak_rules.rs::Telling`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Telling {
    /// Accumulated damage width at this holder, as `f64::to_bits`.
    width_bits: u64,
    /// Retellings between the originating witness and this holder.
    hops: u32,
    /// The originating witness. Fixes the ladder for the whole route.
    witness: EntityId,
    /// The remembered day, as `f64::to_bits`.
    day_bits: u64,
    /// The rung this holder remembers the day at.
    rung: u8,
    /// How many distinct simple routes delivered exactly this telling.
    routes: u32,
}

impl Telling {
    /// The shipped ordering key: smallest width, then fewest hops, then
    /// smallest witness. Spec §5.3: unchanged by the crossing penalty, which
    /// enters through `width` only.
    fn least_damage_key(&self) -> (u64, u32, EntityId) {
        (self.width_bits, self.hops, self.witness)
    }
}

/// One ending's whole candidate set under one `(Crossing, Accumulation)` pair.
#[derive(Default)]
struct Candidates {
    /// holder -> every telling that reached it, ascending by the shipped key.
    per_holder: BTreeMap<EntityId, Vec<Telling>>,
    /// Whether a bound bound. A capped ending is excluded from every reported
    /// statistic and counted separately.
    capped: bool,
}

/// The mutable half of one enumeration.
struct Accum {
    /// (holder, telling) -> route count, ascending in the shipped key's order.
    seen: BTreeMap<EntityId, BTreeMap<Telling, u32>>,
    /// Expansions spent so far.
    expansions: u64,
    /// Whether a bound bound.
    capped: bool,
}

/// Everything one ending's enumeration needs, assembled once per world.
struct Enumerator<'a> {
    /// The committed ledger.
    led: &'a Ledger,
    /// The founding tree.
    lineage: &'a Lineage,
    /// The SHIPPED raid seam — `contact::contact_of`, not a copy of it.
    contact: &'a ContactGraph,
    /// Per-people generation and lifespan, in std days.
    durations: &'a PeopleDurations,
}

/// One node's state on the route currently being walked.
#[derive(Clone, Copy)]
struct Step {
    /// Who currently holds the telling.
    node: EntityId,
    /// Accumulated damage width at full precision.
    width: f64,
    /// The day as this node remembers it, already coarsened by every step
    /// behind it.
    day: f64,
    /// The rung `day` currently sits at.
    rung: Precision,
    /// Retellings behind this node.
    hops: u32,
}

/// The people `occ` belongs to, or `""` when the ledger does not say.
/// `derive.rs::people_of`, transcribed (that function is private to the
/// crate).
fn people_of(led: &Ledger, occ: EntityId) -> String {
    match led.value_of(occ, hornvale_history::OCC_PEOPLE) {
        Some(Value::Text(p)) => p.clone(),
        _ => String::new(),
    }
}

impl<'a> Enumerator<'a> {
    /// Who `node` may tell, over the raid-seam-augmented graph. Line for line
    /// `probe_tiebreak_rules.rs::tellable` restricted to `Contact::WithRaidSeam`
    /// (this file never enumerates descent — see the module doc).
    fn tellable(&self, node: EntityId, event_day: Option<f64>) -> Vec<(EntityId, bool)> {
        let mut out: Vec<(EntityId, bool)> = self
            .lineage
            .children_of(node)
            .iter()
            .map(|c| (*c, false))
            .collect();
        for (peer, day) in self.contact.peers_of(node) {
            if event_day.is_none_or(|event| *day >= event) {
                out.push((*peer, true));
            }
        }
        out.sort();
        out.dedup_by_key(|(id, _)| *id);
        out
    }

    /// Every telling that reaches every holder, over simple paths from every
    /// witness, under one `(Crossing, Accumulation)` pair.
    ///
    /// The per-step arithmetic is the shipped relaxation's, verbatim,
    /// **plus** the one term `derive.rs::crossing_penalty` adds: see the
    /// module doc.
    #[allow(clippy::too_many_arguments)]
    fn enumerate(
        &self,
        ladders: &PeopleLadders,
        rule: Accumulation,
        crossing: Crossing,
        subject: EntityId,
    ) -> Candidates {
        let led = self.led;
        let Some(Value::Number(event)) = led.value_of(subject, PREDICATE) else {
            return Candidates::default();
        };
        let event = *event;
        let witnesses = witnesses_of(led, self.lineage, subject, PREDICATE);
        let witness_set: BTreeSet<EntityId> = witnesses.iter().copied().collect();

        let mut acc = Accum {
            seen: BTreeMap::new(),
            expansions: 0,
            capped: false,
        };
        for w in &witnesses {
            let people = people_of(led, *w);
            let ladder = ladders.for_people(&people);
            let width = ladder
                .span(Precision::FINEST)
                .map(|days| days.get())
                .unwrap_or(0.0);
            let mut visited: BTreeSet<EntityId> = BTreeSet::new();
            visited.insert(*w);
            self.descend(
                &mut acc,
                &witness_set,
                ladder,
                rule,
                crossing,
                Some(event),
                *w,
                Step {
                    node: *w,
                    width,
                    day: event,
                    rung: Precision::FINEST,
                    hops: 0,
                },
                &mut visited,
            );
        }

        let mut per_holder: BTreeMap<EntityId, Vec<Telling>> = BTreeMap::new();
        for (holder, rows) in acc.seen {
            let mut v: Vec<Telling> = rows
                .into_iter()
                .map(|(mut t, routes)| {
                    t.routes = routes;
                    t
                })
                .collect();
            v.sort();
            per_holder.insert(holder, v);
        }
        Candidates {
            per_holder,
            capped: acc.capped,
        }
    }

    /// Walk every simple path out of `step.node`, recording the telling that
    /// reached it. `probe_tiebreak_rules.rs::descend`, with the crossing
    /// penalty term added to the step's span before the accumulation rule
    /// sees it — `derive.rs:556-558`'s own order.
    #[allow(clippy::too_many_arguments)]
    fn descend(
        &self,
        acc: &mut Accum,
        witness_set: &BTreeSet<EntityId>,
        ladder: &PrecisionLadder,
        rule: Accumulation,
        crossing: Crossing,
        event_day: Option<f64>,
        witness: EntityId,
        step: Step,
        visited: &mut BTreeSet<EntityId>,
    ) {
        acc.expansions += 1;
        if acc.expansions > MAX_EXPANSIONS {
            acc.capped = true;
            return;
        }
        let telling = Telling {
            width_bits: step.width.to_bits(),
            hops: step.hops,
            witness,
            day_bits: step.day.to_bits(),
            rung: step.rung.rung(),
            routes: 0,
        };
        *acc.seen
            .entry(step.node)
            .or_default()
            .entry(telling)
            .or_default() += 1;

        if step.hops >= MAX_DEPTH {
            acc.capped = true;
            return;
        }
        for (hearer, _is_seam) in self.tellable(step.node, event_day) {
            if witness_set.contains(&hearer) {
                continue; // a witness is never demoted to an inheritor
            }
            if visited.contains(&hearer) {
                continue; // simple paths only
            }
            // THE ONE ADDITION: derive.rs:551-557's crossing penalty, added
            // to the step's ordinary generational span BEFORE the
            // accumulation rule sees it — same order, same units
            // (span(FINEST) of the ORIGINATING witness's ladder), same zero
            // cases (Crossing::Free, or a same-people step).
            let penalty = if crossing == Crossing::Free {
                0.0
            } else {
                let (from, to) = (people_of(self.led, step.node), people_of(self.led, hearer));
                if from == to {
                    0.0
                } else {
                    let unit = ladder
                        .span(Precision::FINEST)
                        .map(|d| d.get())
                        .unwrap_or(0.0);
                    unit / (1.0 + self.contact.edges_between(&from, &to) as f64)
                }
            };
            let span = gen_span(self.led, self.durations, step.node, hearer) + penalty;
            let next_width = rule.step(step.width, span);
            let rung = precision_at(ladder, next_width);
            let next = Step {
                node: hearer,
                width: next_width,
                day: ladder.apply(rung, step.day),
                rung,
                hops: step.hops + 1,
            };
            visited.insert(hearer);
            self.descend(
                acc,
                witness_set,
                ladder,
                rule,
                crossing,
                event_day,
                witness,
                next,
                visited,
            );
            visited.remove(&hearer);
            if acc.expansions > MAX_EXPANSIONS {
                return;
            }
        }
    }
}

// ---------------------------------------------------------------------------
// World reading. `probe_tiebreak_rules.rs`'s own copy, verbatim — see that
// file's module doc: it is itself "a near-copy of `parley_readout.rs`'s,
// deliberately", the established pattern every probe in this crate follows
// rather than sharing modules across separate test-binary crates.
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
    /// Whether the attacker is of another people — this file's whole
    /// population (only foreign endings carry a seam crossing to weigh).
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
    /// The shipped raid seam.
    contact: ContactGraph,
    /// Per-people generation and lifespan, in std days.
    durations: PeopleDurations,
    /// One ladder per people.
    ladders: PeopleLadders,
    /// Every ending in the world, ascending by subject.
    endings: Vec<Ending>,
}

/// Assemble one world. `None` when it offers no year rung, exactly as
/// `probe_tiebreak_rules.rs::read_world`.
fn read_world(led: &Ledger, components: &hornvale_worldgen::WorldComponents) -> Option<WorldRead> {
    let lineage = lineage_of(led);
    let astronomical = PrecisionLadder::of(led);
    let year_days = astronomical
        .labels()
        .iter()
        .position(|label| *label == "year")
        .and_then(|i| astronomical.span(Precision(i as u8)))
        .map(|span| span.get())?;

    let mut people_of_map: BTreeMap<EntityId, String> = BTreeMap::new();
    for occ in lineage.all() {
        if let Some(Value::Text(people)) = led.value_of(occ, hornvale_history::OCC_PEOPLE) {
            people_of_map.insert(occ, people.clone());
        }
    }

    let mut durations = PeopleDurations::default();
    let named: BTreeSet<&String> = people_of_map.values().collect();
    for people in named {
        let Some(bio) = components.biosphere.get_by_label(people) else {
            continue;
        };
        let life = hornvale_species::life_history(bio.mass, bio.metabolic_class, bio.schedule);
        let to_days = |years: hornvale_kernel::Years| {
            hornvale_astronomy::units::StdDays::new(years.get() * year_days).ok()
        };
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

/// `n` as a percentage of `d`, or `0.0` when `d` is zero.
fn pct(n: usize, d: usize) -> f64 {
    100.0 * n as f64 / d.max(1) as f64
}

/// The remembered day a claim carries, as bits.
fn day_bits_of(c: &Claim) -> Option<u64> {
    match &c.object {
        Value::Number(day) => Some(day.to_bits()),
        _ => None,
    }
}

// ===========================================================================
// THE MEASUREMENT — S0b, over both `Crossing` arms, contact only.
// ===========================================================================

/// One (Crossing, Accumulation) vertex's S0b counts.
#[derive(Clone, Copy, Default)]
struct Vertex {
    /// Holders where the shipped answer was found in the enumerated set.
    compared: usize,
    /// ... of which it was not that set's argmin under the shipped key.
    not_argmin: usize,
    /// CONTROL: holders where the shipped answer was absent from the
    /// enumerated set entirely. Must be zero.
    shipped_absent: usize,
    /// CONTROL: endings whose enumeration reached a different holder set
    /// from the shipped walk. Must be zero.
    holder_set_mismatch: usize,
    /// Endings whose enumeration hit a bound, excluded from every other
    /// count above.
    capped_endings: usize,
}

/// One defective `(holder, rule)` vertex's IDENTITY: `(seed, rule index,
/// subject, holder)`.
///
/// The seed is part of the key because [`EntityId`]s are minted per world and
/// repeat across seeds — two different worlds' holders can carry the same id,
/// and a set keyed without the seed would silently merge them.
type DefectId = (u64, usize, EntityId, EntityId);

/// One seed's whole contribution: [`Vertex`], indexed by [`Crossing::ALL`] then
/// [`Accumulation::ALL`].
#[derive(Clone, Default)]
struct SeedRow {
    /// Endings in the world (control population).
    endings: usize,
    /// ... of which the attacker is of another people (control population).
    foreign: usize,
    /// The vertices.
    vertices: [[Vertex; 3]; 2],
    /// Up to a few not-argmin samples, for the readout.
    samples: Vec<String>,
    /// **WHICH** vertices were defective, per [`Crossing`] arm — not how many.
    ///
    /// [`Vertex::not_argmin`] is a COUNT, and a count cannot distinguish "the
    /// same vertices are defective under both arms" from "two disjoint sets of
    /// the same size are". The readout draws a set-level conclusion, so it
    /// needs a set-level measurement; this is it.
    defects: [BTreeSet<DefectId>; 2],
}

/// Every quantity this file asks for, over one world.
fn measure_seed(seed: u64, led: &Ledger, read: &WorldRead) -> SeedRow {
    let enumerator = Enumerator {
        led,
        lineage: &read.lineage,
        contact: &read.contact,
        durations: &read.durations,
    };
    let mut row = SeedRow {
        endings: read.endings.len(),
        foreign: 0,
        ..Default::default()
    };

    for e in &read.endings {
        if !e.is_foreign() {
            continue;
        }
        row.foreign += 1;

        for (ci, cross) in Crossing::ALL.iter().enumerate() {
            for (ri, rule) in Accumulation::ALL.iter().enumerate() {
                let cands = enumerator.enumerate(&read.ladders, *rule, *cross, e.subject);
                let vertex = &mut row.vertices[ci][ri];
                if cands.capped {
                    vertex.capped_endings += 1;
                    continue;
                }

                let shipped = variants_about_accumulating(
                    &Walk {
                        ledger: led,
                        lineage: &read.lineage,
                        contact: &read.contact,
                        policy: Transmission {
                            contact: Contact::WithRaidSeam,
                            crossing: *cross,
                            ..Transmission::AS_SHIPPED
                        },
                    },
                    &read.ladders,
                    &read.durations,
                    *rule,
                    e.subject,
                    PREDICATE,
                );
                let shipped_by_holder: BTreeMap<EntityId, &Claim> =
                    shipped.iter().map(|c| (c.holder, c)).collect();
                let mine_set: BTreeSet<EntityId> = cands.per_holder.keys().copied().collect();
                if shipped_by_holder.keys().copied().collect::<BTreeSet<_>>() != mine_set {
                    vertex.holder_set_mismatch += 1;
                }

                for (holder, set) in &cands.per_holder {
                    let Some(cl) = shipped_by_holder.get(holder) else {
                        vertex.shipped_absent += 1;
                        continue;
                    };
                    let Some(d) = day_bits_of(cl) else {
                        vertex.shipped_absent += 1;
                        continue;
                    };
                    let matched_present = set.iter().any(|t| {
                        t.hops == cl.hops && t.rung == cl.precision.rung() && t.day_bits == d
                    });
                    if !matched_present {
                        vertex.shipped_absent += 1;
                        continue;
                    }
                    vertex.compared += 1;

                    let min_key = set[0].least_damage_key();
                    let tied_has_shipped = set
                        .iter()
                        .take_while(|t| t.least_damage_key() == min_key)
                        .any(|t| {
                            t.hops == cl.hops && t.rung == cl.precision.rung() && t.day_bits == d
                        });
                    if !tied_has_shipped {
                        vertex.not_argmin += 1;
                        row.defects[ci].insert((seed, ri, e.subject, *holder));
                        if row.samples.len() < 6 {
                            row.samples.push(format!(
                                "seed {seed} {}/{} subj {:?} holder {:?}: shipped hops={} rung={} \
                                 | argmin hops={} rung={} (same day: {})",
                                cross.label(),
                                rule.label(),
                                e.subject,
                                holder,
                                cl.hops,
                                cl.precision.rung(),
                                set[0].hops,
                                set[0].rung,
                                set[0].day_bits == d,
                            ));
                        }
                    }
                }
            }
        }
    }
    row
}

fn sum<F: Fn(&SeedRow) -> usize>(rows: &[SeedRow], f: F) -> usize {
    rows.iter().map(f).sum()
}

fn fold_vertex(rows: &[SeedRow], ci: usize, ri: usize) -> Vertex {
    let mut out = Vertex::default();
    for r in rows {
        let c = r.vertices[ci][ri];
        out.compared += c.compared;
        out.not_argmin += c.not_argmin;
        out.shipped_absent += c.shipped_absent;
        out.holder_set_mismatch += c.holder_set_mismatch;
        out.capped_endings += c.capped_endings;
    }
    out
}

/// The S0b non-argmin defect over the 12-seed panel, under both `Crossing`
/// arms.
///
/// claim: structural(seed: panel) — false-positive seed-loop flag; the loop
/// binds a census-panel prefix, not a search over seeds (same shape as
/// `probe_tiebreak_rules.rs`'s own claim tag, which this file's panel is a
/// strict copy of).
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn does_the_crossing_penalty_change_the_non_argmin_defect() {
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

    println!("\n=== Task 3 (The Undertow) — does Crossing move the non-argmin defect? ===");
    println!("  panel seeds skipped for want of a year rung: {skipped:?}");

    let endings = sum(&rows, |r| r.endings);
    let foreign = sum(&rows, |r| r.foreign);
    println!(
        "  endings {endings} (control: {BASELINE_ENDINGS_12}), foreign {foreign} (control: {BASELINE_FOREIGN_12})"
    );

    println!(
        "\n  {:<20} {:>9} {:>12} {:>14} {:>9} {:>8}",
        "arm", "rule", "compared", "not the argmin", "share", "capped"
    );
    let mut totals: [Vertex; 2] = [Vertex::default(); 2];
    for (ci, cross) in Crossing::ALL.iter().enumerate() {
        for (ri, rule) in Accumulation::ALL.iter().enumerate() {
            let c = fold_vertex(&rows, ci, ri);
            println!(
                "  {:<20} {:>9} {:>12} {:>14} {:>8.4}% {:>8}",
                cross.label(),
                rule.label(),
                c.compared,
                c.not_argmin,
                pct(c.not_argmin, c.compared),
                c.capped_endings,
            );
            totals[ci].compared += c.compared;
            totals[ci].not_argmin += c.not_argmin;
            totals[ci].shipped_absent += c.shipped_absent;
            totals[ci].holder_set_mismatch += c.holder_set_mismatch;
            totals[ci].capped_endings += c.capped_endings;
        }
        println!(
            "  {:<20} {:>9} {:>12} {:>14} {:>8.4}%   (aggregate over all three accumulation rules)",
            cross.label(),
            "ALL",
            totals[ci].compared,
            totals[ci].not_argmin,
            pct(totals[ci].not_argmin, totals[ci].compared),
        );
    }

    // CAVEAT (fix round 1, Important 3): capped_endings drops the SAME small
    // set of foreign endings from every vertex above, on BOTH arms — and a
    // capped ending is by construction the densest one, the one with the MOST
    // routes reaching it, which is exactly where a route-count-dependent
    // defect like this one is most likely to bite. So every rate printed
    // above is a rate over the reachable, non-capped subpopulation, not over
    // the full foreign population — the absolute shares should be read as
    // FLOORS, not point estimates. The COMPARISON between the two arms is
    // unaffected: both arms drop the identical endings, so the ratio stays
    // like-for-like even though neither side's absolute number is complete.
    // (Re-derived against the merge product the cap binds 2 of 102 foreign
    // endings, where it bound 14 of 138 before main was absorbed.)
    let capped_per_rule = fold_vertex(&rows, 0, 0).capped_endings;
    println!(
        "\n  CAVEAT: {capped_per_rule} of {foreign} foreign endings ({:.1}%) are capped on EVERY vertex above and excluded before any comparison runs. Capped endings are the densest ones by construction (most routes reaching them), which is where this defect is most likely -- so the absolute shares above are FLOORS over the reachable subpopulation, not point estimates over the full population. The free-vs-contact-weighted RATIO is unaffected: both arms drop the identical {capped_per_rule} endings, so the comparison stays like-for-like even though neither side's absolute count is complete.",
        pct(capped_per_rule, foreign)
    );

    let samples: Vec<&String> = rows.iter().flat_map(|r| r.samples.iter()).collect();
    for line in samples.iter().take(6) {
        println!("    {line}");
    }

    // =====================================================================
    // THE SET-LEVEL COMPARISON. A COUNT CANNOT ANSWER A SET-LEVEL QUESTION.
    //
    // Every column above is an aggregate, and `n == n` between the two arms
    // is consistent with two DISJOINT sets of size n. Reporting "the penalty
    // does not move this defect" off equal counts would be exactly the
    // right-measurement/wrong-attribution shape this thread keeps hitting.
    // So the identities are carried out of `measure_seed` and diffed here:
    // the symmetric difference is the number a set-level claim needs, and it
    // costs one BTreeSet per arm on a population of tens.
    //
    // REPORTED, NOT ASSERTED. Whether the penalty moves this defect is a
    // finding, not a control, and the file's rule is that only controls are
    // asserted.
    // =====================================================================
    let free_set: BTreeSet<DefectId> = rows
        .iter()
        .flat_map(|r| r.defects[0].iter().copied())
        .collect();
    let cw_set: BTreeSet<DefectId> = rows
        .iter()
        .flat_map(|r| r.defects[1].iter().copied())
        .collect();
    let both = free_set.intersection(&cw_set).count();
    let only_free = free_set.difference(&cw_set).count();
    let only_cw = cw_set.difference(&free_set).count();
    println!(
        "\n  WHICH VERTICES, NOT HOW MANY (the set-level comparison the counts above cannot make): \
         free {} defective (holder, rule) vertices, contact-weighted {}, in BOTH {}, free-only {}, \
         contact-weighted-only {} — symmetric difference {}. Two equal counts are consistent \
         with two disjoint sets, so this is the measurement a claim about the penalty NOT \
         MOVING the defect actually rests on.",
        free_set.len(),
        cw_set.len(),
        both,
        only_free,
        only_cw,
        only_free + only_cw,
    );

    println!(
        "\n  READING: Crossing::Free's aggregate is this task's own reproduction of \
         probe_tiebreak_rules.rs's S0b contact row ({REPRO_FREE_NOT_ARGMIN} of \
         {REPRO_FREE_COMPARED}, {:.4}%). Crossing::ContactWeighted's aggregate is Task 2's \
         mechanism applied to the SAME instrument, SAME population, SAME comparison — the \
         first measurement of whether the campaign's own penalty moves this pre-existing \
         defect. See the task report for the disposition.",
        pct(REPRO_FREE_NOT_ARGMIN, REPRO_FREE_COMPARED)
    );

    // =====================================================================
    // CONTROLS.
    // =====================================================================
    assert!(!rows.is_empty(), "control: the panel produced no seeds");
    assert!(
        skipped.len() < PANEL.len(),
        "control: every panel seed was skipped for want of a year rung"
    );
    assert_eq!(
        endings, BASELINE_ENDINGS_12,
        "control: this panel must re-derive parley_readout.rs's pinned ending count exactly"
    );
    assert_eq!(
        foreign, BASELINE_FOREIGN_12,
        "control: this panel must re-derive parley_readout.rs's pinned foreign count exactly"
    );

    for (ci, cross) in Crossing::ALL.iter().enumerate() {
        assert_eq!(
            totals[ci].shipped_absent,
            0,
            "control: the shipped walk's own answer must appear SOMEWHERE in the enumerated \
             candidate set under Crossing::{}, on every holder and every accumulation rule — \
             that is what proves the enumeration is a superset of the route the shipped walk \
             took; it was absent on {} holders",
            cross.label(),
            totals[ci].shipped_absent
        );
        assert_eq!(
            totals[ci].holder_set_mismatch,
            0,
            "control: the enumeration must reach exactly the shipped walk's holder set under \
             Crossing::{} — a selection rule reorders tellings and can never change WHO holds; \
             {} endings differed",
            cross.label(),
            totals[ci].holder_set_mismatch
        );
    }

    // Both Crossing arms walk the identical graph TOPOLOGY (only edge WEIGHTS
    // differ), so the same routes are visited and the same endings should hit
    // MAX_EXPANSIONS under either arm — a structural invariant, not a
    // coincidence, and a mismatch would mean the two enumerations silently
    // diverged on which ending they even attempted.
    assert_eq!(
        totals[0].capped_endings, totals[1].capped_endings,
        "control: Crossing changes edge weights, not the graph's shape, so both arms must cap \
         the same endings under every accumulation rule (capped_endings summed over all three \
         rules must match arm to arm; {capped_per_rule} endings per rule, times three rules): \
         free {}, contact-weighted {}",
        totals[0].capped_endings, totals[1].capped_endings
    );

    // THE ONE NON-CONTROL ASSERTION — see the module doc. This pins
    // Crossing::Free's aggregate to this task's own direct measurement of
    // probe_tiebreak_rules.rs, taken in this session.
    assert_eq!(
        (totals[0].compared, totals[0].not_argmin),
        (REPRO_FREE_COMPARED, REPRO_FREE_NOT_ARGMIN),
        "Crossing::Free must reproduce probe_tiebreak_rules.rs's own S0b contact row exactly — \
         a mismatch means this file's restructuring diverged from the audited instrument, which \
         the task brief says matters more than this task: {:?} vs the recorded ({REPRO_FREE_COMPARED}, {REPRO_FREE_NOT_ARGMIN})",
        (totals[0].compared, totals[0].not_argmin)
    );
}
