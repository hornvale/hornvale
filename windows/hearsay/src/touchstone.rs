//! The belief-delta module: diffing two transmission arms' traced tellings
//! into per-holder, per-component change flags.
//!
//! [`crate::traced::traced_variants_about_accumulating`] (Task 2) returns one
//! [`crate::traced::HeldTelling`] per holder reached under a given policy
//! arm — the route, width, and downstream `Claim` all bundled together. This
//! module answers the question a campaign comparing two arms actually asks:
//! for the SAME holder, walked under arm A and arm B, which components of
//! what they hold moved? A holder's `Claim` alone cannot answer this on its
//! own (two `Claim`s can differ in `object` while agreeing everywhere else,
//! or agree in `object` while arriving down different routes), so the diff
//! is taken component-by-component rather than by a single `PartialEq`.
//!
//! Both inputs are the ascending-by-holder `Vec<HeldTelling>` the traced walk
//! returns (the walk's `reached: BTreeMap<EntityId, Telling>` iterates in key
//! order), so alignment is a single merge-join, never a lookup structure of
//! its own.

use crate::traced::HeldTelling;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use std::cmp::Ordering;
use std::collections::BTreeMap;

/// Which components of one holder's held telling changed between two arms.
///
/// A holder appears here only when the merge-join in [`belief_deltas`] found
/// them in BOTH arms — a holder reached under only one arm has nothing to
/// diff and is tallied by [`tail_counts`]'s `only_a`/`only_b` instead.
///
/// type-audit: bare-ok(flag: route_changed), bare-ok(flag: day_changed), bare-ok(flag: rung_changed), bare-ok(flag: hops_changed), bare-ok(flag: width_changed)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BeliefDelta {
    /// The holder this delta is about (`HeldTelling::claim::holder`, which
    /// agrees between the two arms by construction — it is the merge-join
    /// key).
    pub holder: EntityId,
    /// The winning witness or crossing sequence differs: `a.witness !=
    /// b.witness || a.crossings != b.crossings`. Route identity is the PAIR,
    /// not either half alone — two routes can share a witness while crossing
    /// different peoples, or share a crossing count while descending from
    /// different witnesses.
    pub route_changed: bool,
    /// The remembered day (`claim.object`) differs, bit-exact: for a
    /// `Value::Number`, the two `f64`s' `to_bits()` differ; for any other
    /// `Value`, ordinary `Value` equality decides it. A float `==` would call
    /// two `NaN`s unequal (silently hiding a real difference between two
    /// non-comparable values) and `-0.0`/`0.0` equal (silently hiding a real
    /// sign difference); `to_bits()` is the determinism-safe comparison this
    /// repo already uses (`undertow_readout.rs`'s `day_bits`).
    pub day_changed: bool,
    /// The remembered rung (`claim.precision`) differs.
    pub rung_changed: bool,
    /// The hop depth (`claim.hops`) differs.
    pub hops_changed: bool,
    /// The accumulated width differs, bit-exact (`to_bits()`, same rationale
    /// as `day_changed`).
    pub width_changed: bool,
}

impl BeliefDelta {
    /// Any tracked component moved — the OR of the five flags.
    /// type-audit: bare-ok(flag: return)
    pub fn any_changed(&self) -> bool {
        self.route_changed
            || self.day_changed
            || self.rung_changed
            || self.hops_changed
            || self.width_changed
    }
}

/// Tail counts: the denominator and each component's mover count.
///
/// type-audit: bare-ok(count: reached_both), bare-ok(count: only_a), bare-ok(count: only_b), bare-ok(count: any), bare-ok(count: route), bare-ok(count: day), bare-ok(count: rung), bare-ok(count: hops), bare-ok(count: width)
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct TailCounts {
    /// Holders reached under BOTH arms — the denominator [`changed_tail`]
    /// divides by.
    pub reached_both: usize,
    /// Holders reached under arm A alone (present in A, absent from B).
    pub only_a: usize,
    /// Holders reached under arm B alone (present in B, absent from A).
    pub only_b: usize,
    /// Holders (among `reached_both`) where [`BeliefDelta::any_changed`].
    pub any: usize,
    /// Holders where `route_changed`.
    pub route: usize,
    /// Holders where `day_changed`.
    pub day: usize,
    /// Holders where `rung_changed`.
    pub rung: usize,
    /// Holders where `hops_changed`.
    pub hops: usize,
    /// Holders where `width_changed`.
    pub width: usize,
}

/// The people `occ` belongs to, or `""` when the ledger does not say — the
/// same resolution [`crate::traced::traced_variants_about_accumulating`]
/// uses (`OCC_PEOPLE`, `Value::Text` or nothing).
fn people_of(ledger: &Ledger, occ: EntityId) -> String {
    match ledger.value_of(occ, hornvale_history::OCC_PEOPLE) {
        Some(Value::Text(p)) => p.clone(),
        _ => String::new(),
    }
}

/// Whether the remembered day differs, bit-exact.
///
/// `Value::Number` compares by `to_bits()`; any other pairing (including a
/// mismatched variant, which the walk never actually produces for a fixed
/// predicate but which this function does not assume) falls back to ordinary
/// `Value` equality.
fn day_changed(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Number(da), Value::Number(db)) => da.to_bits() != db.to_bits(),
        _ => a != b,
    }
}

/// The delta between two `HeldTelling`s already known to belong to the same
/// holder.
fn delta_of(a: &HeldTelling, b: &HeldTelling) -> BeliefDelta {
    BeliefDelta {
        holder: a.claim.holder,
        route_changed: a.witness != b.witness || a.crossings != b.crossings,
        day_changed: day_changed(&a.claim.object, &b.claim.object),
        rung_changed: a.claim.precision != b.claim.precision,
        hops_changed: a.claim.hops != b.claim.hops,
        width_changed: a.width.to_bits() != b.width.to_bits(),
    }
}

/// The result of aligning two ascending-by-holder arms: the deltas for every
/// holder present in both, plus the holders present in only one side (kept as
/// `EntityId`s, not just counts, so [`tail_by_people_pair`] can still bucket
/// them by the missing side's own people).
struct Join {
    /// One delta per holder found in both arms, ascending by holder.
    deltas: Vec<BeliefDelta>,
    /// Holders found in arm A only.
    only_a: Vec<EntityId>,
    /// Holders found in arm B only.
    only_b: Vec<EntityId>,
}

/// Align two ascending-by-holder arms with a single merge-join.
fn merge_join(arm_a: &[HeldTelling], arm_b: &[HeldTelling]) -> Join {
    let mut deltas = Vec::new();
    let mut only_a = Vec::new();
    let mut only_b = Vec::new();
    let (mut i, mut j) = (0, 0);
    while i < arm_a.len() && j < arm_b.len() {
        let (a, b) = (&arm_a[i], &arm_b[j]);
        match a.claim.holder.cmp(&b.claim.holder) {
            Ordering::Less => {
                only_a.push(a.claim.holder);
                i += 1;
            }
            Ordering::Greater => {
                only_b.push(b.claim.holder);
                j += 1;
            }
            Ordering::Equal => {
                deltas.push(delta_of(a, b));
                i += 1;
                j += 1;
            }
        }
    }
    only_a.extend(arm_a[i..].iter().map(|t| t.claim.holder));
    only_b.extend(arm_b[j..].iter().map(|t| t.claim.holder));
    Join {
        deltas,
        only_a,
        only_b,
    }
}

/// Per-holder deltas over the holders reached under BOTH arms, ascending by
/// holder. Holders reached under only one arm are reported separately by
/// [`tail_counts`], never silently dropped.
pub fn belief_deltas(arm_a: &[HeldTelling], arm_b: &[HeldTelling]) -> Vec<BeliefDelta> {
    merge_join(arm_a, arm_b).deltas
}

/// Tail counts over the two arms: the denominator, the appeared/vanished
/// counts, and each component's mover count.
pub fn tail_counts(arm_a: &[HeldTelling], arm_b: &[HeldTelling]) -> TailCounts {
    let joined = merge_join(arm_a, arm_b);
    let mut t = TailCounts {
        reached_both: joined.deltas.len(),
        only_a: joined.only_a.len(),
        only_b: joined.only_b.len(),
        ..TailCounts::default()
    };
    for d in &joined.deltas {
        if d.route_changed {
            t.route += 1;
        }
        if d.day_changed {
            t.day += 1;
        }
        if d.rung_changed {
            t.rung += 1;
        }
        if d.hops_changed {
            t.hops += 1;
        }
        if d.width_changed {
            t.width += 1;
        }
        if d.any_changed() {
            t.any += 1;
        }
    }
    t
}

/// `any as f64 / reached_both as f64` — the campaign's `changed_tail`.
///
/// `0.0` when `reached_both == 0`: with no denominator there is nothing to
/// report a rate over, and `0.0` reads as "no measured movement" rather than
/// fabricating a rate from an empty population (the same "not measurable"
/// caution [`crate::spearman`] documents for its own `None`, though this
/// function returns a bare `f64` rather than an `Option` because a caller
/// tallying `changed_tail` across many subjects wants a plain number to sum,
/// not a hole in the series).
/// type-audit: bare-ok(ratio: return)
pub fn changed_tail(t: &TailCounts) -> f64 {
    if t.reached_both == 0 {
        return 0.0;
    }
    t.any as f64 / t.reached_both as f64
}

/// The same counts as [`tail_counts`], but bucketed by `(people_of(holder),
/// people_of(subject))` rather than summed over every holder at once.
///
/// `subject` is fixed for one call (one event's traced tellings), so every
/// key in the returned map shares the same second coordinate — the shape
/// exists so a caller comparing several subjects can merge these maps and
/// see the second coordinate vary. A holder present in only one arm is still
/// bucketed (by its own people, paired with the subject's), matching
/// [`tail_counts`]'s promise that a one-sided holder is counted, never
/// dropped.
/// type-audit: bare-ok(identifier-text: return)
pub fn tail_by_people_pair(
    ledger: &Ledger,
    subject: EntityId,
    arm_a: &[HeldTelling],
    arm_b: &[HeldTelling],
) -> BTreeMap<(String, String), TailCounts> {
    let subject_people = people_of(ledger, subject);
    let mut out: BTreeMap<(String, String), TailCounts> = BTreeMap::new();
    let joined = merge_join(arm_a, arm_b);
    for d in &joined.deltas {
        let key = (people_of(ledger, d.holder), subject_people.clone());
        let t = out.entry(key).or_default();
        t.reached_both += 1;
        if d.route_changed {
            t.route += 1;
        }
        if d.day_changed {
            t.day += 1;
        }
        if d.rung_changed {
            t.rung += 1;
        }
        if d.hops_changed {
            t.hops += 1;
        }
        if d.width_changed {
            t.width += 1;
        }
        if d.any_changed() {
            t.any += 1;
        }
    }
    for holder in &joined.only_a {
        let key = (people_of(ledger, *holder), subject_people.clone());
        out.entry(key).or_default().only_a += 1;
    }
    for holder in &joined.only_b {
        let key = (people_of(ledger, *holder), subject_people.clone());
        out.entry(key).or_default().only_b += 1;
    }
    out
}
