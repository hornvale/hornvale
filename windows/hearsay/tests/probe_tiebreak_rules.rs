//! Substrate probe (The Undertow, Myth campaign 5): **did contact pool the
//! accounts, or did the ARGMIN?**
//!
//! The Parley added a cross-people contact edge and found that accounts POOL
//! rather than diverge — identical remembered-day sets rose under contact on
//! every accumulation rule, and its frozen §6.5 divergence measure FELL
//! (0.59x additive / 0.52x quadrature / 0.77x multiplicative, on its 40-seed
//! panel, before The Underworld moved settlement placement; re-derived on the
//! merge product the same triple is 0.52x / 0.55x / 0.90x). Its own reading
//! blamed the edge's SYMMETRY. This campaign's first
//! probe (`probe_seam_direction.rs`) ruled that out: divergence falls under a
//! one-way seam too, on every rule and in both directions, so pooling tracks
//! seam VOLUME and not seam symmetry.
//!
//! One candidate cause is left standing, and it is a modelling choice nobody
//! has questioned since campaign 2. `derive.rs`'s ordering key is
//! `(width.to_bits(), hops, witness)` in `variants_about_accumulating` and
//! `(lossy_steps, hops, witness)` in `variants_about`. **Neither consults
//! `occ-people`.** So every community keeps the least-damaged telling it can
//! reach, regardless of who told it: hand a community a better-sourced
//! account from the people who just burned its village and it adopts that
//! over its own grandmother's. If pooling is that rule's doing, The Parley's
//! headline is a statement about an argmin rather than about contact.
//!
//! This probe SIMULATES alternative tie-breaks on the shipped graph. Nothing
//! in `windows/hearsay/src` changes and no new arm is implemented — the same
//! method The Parley's own §6 used to decide whether the contact edge was
//! worth modelling before building it.
//!
//! - **S0** — **is there a choice at all?** For each holder, how many
//!   distinct remembered values reached it? If most holders only ever receive
//!   one value, no tie-break can be responsible for anything and every rule
//!   agrees trivially. This is the anti-vacuity measurement for the whole
//!   probe and it is printed first.
//! - **S1** — The Parley's §6.5 quantities (identical day sets,
//!   mutually-exclusive pairs, and the contact-vs-descent ratio) under each
//!   selection rule x each `Accumulation` rule x each `Contact` arm.
//!
//! ## The method, and the three bounds it had to state out loud
//!
//! The shipped walk is a best-first RELAXATION: it keeps one telling per
//! node, the argmin of the key, and its termination rests on that key rising
//! strictly along every edge. Three of the four rules under test cannot be
//! expressed that way (see [`Selection`]), so this probe does not relax at
//! all — it **enumerates the whole candidate set** reaching each holder and
//! then applies each selection rule to it.
//!
//! **Bound 1, simple paths.** The seam makes the graph cyclic, so the set of
//! WALKS reaching a node is infinite; no route here may revisit a community.
//! That is an assumption this probe makes, not something the substrate
//! supplies, and it is stated rather than buried because one of the four
//! rules (`recency`) is *only* well-defined under it. For the shipped rule it
//! costs nothing and that is provable: `Accumulation::step` is non-decreasing
//! AND monotone in its incoming width for all three rules, and `hops` rises by
//! one per edge, so deleting a cycle from a route can only lower the key —
//! the argmin is always attained on a simple path.
//!
//! **Bound 2, the population.** Simple paths alone do not make this
//! affordable. A first pilot enumerated both arms on every ending and walked
//! **63,054,018 routes over three seeds**, exhausting a four-million
//! expansion ceiling on 12 endings per contact vertex — silently truncating
//! precisely the best-connected endings. The descent arm is not the problem
//! (the founding tree is a forest; its largest ending costs **338**
//! expansions on the merge product), so the contact arm is enumerated on
//! **§6.5's own population** — the ~2.1% of endings whose attacker is of
//! another people — which is the
//! population every S1 number is reported over anyway. The descent arm is
//! still enumerated on everything, because it is free and because the fact it
//! establishes is the load-bearing half of S0.
//!
//! **Bound 3 was tried and REJECTED, which is a finding.** Capping seam
//! crossings per route at two looked like the obvious cheap bound; under it
//! the shipped winner's crossing histogram read `0:13670 1:16491 2:11512`,
//! piled up at the ceiling, and 21,476 holders lost their shipped answer
//! outright because it needed three crossings or more. The least-damaged
//! route does not step across the seam once and run down a tree — it zigzags.
//! There is no crossing cap here, and the winners' crossing histogram is
//! reported instead.
//!
//! **That histogram is a PRE-UNDERWORLD pilot measurement and is not
//! re-derivable from this file**, because the capped arm was removed with the
//! bound it rejected. The design decision it supports stands on the live
//! histogram this probe still prints — crossings run out to 8 on the merge
//! product — and the three numbers above are kept as the record of what was
//! tried, not as a current claim.
//!
//! ## What holds the instrument honest, and the ambiguity it uncovered
//!
//! The `least-damage` row **is** `variants_about_accumulating`'s own output,
//! not a reconstruction of it, and the enumeration is held to CONTAINING that
//! output at the minimum key on every holder, every accumulation rule and
//! both arms. That control is stronger than a re-derivation in the way that
//! matters — it proves the enumerated set holds the shipped answer *at the
//! right rank* — and it is also the thing that would redden if any bound
//! above were quietly excluding a route the shipped walk needs.
//!
//! It is stated that way because a first draft asserted an argmin-for-argmin
//! re-derivation and **failed on 481 holders of a 3-seed pilot** — and the
//! cause was in the SHIPPED MODEL, not in either instrument. Every
//! `Accumulation` rule is order-independent in the multiset of generational
//! spans it consumes, so two routes from one witness, of equal length, over
//! the same spans in a different ORDER, arrive with a bit-identical width and
//! a bit-identical key — and a DIFFERENT remembered day, because the rungs
//! were applied in a different order along the way. At such a holder the
//! shipped answer is settled by the relaxation's arrival order, which its key
//! does not determine. Those holders are counted (`key_ties`) and printed;
//! nothing here asserts them away.
//!
//! The clock is `Off` throughout, matching `Transmission::AS_SHIPPED` and The
//! Parley's own contact arms, so nothing here varies two things at once. The
//! seam is the SHIPPED `contact::contact_of` graph, not a copy of it.
//!
//! Reports only. Every assertion is a POSITIVE CONTROL — reproducing a
//! published count, or proving the probe reached the population it reports on
//! — so a zero can be told apart from a broken instrument. No assertion here
//! is about an outcome.
//!
//! ## Cost, measured, so the next reader budgets from a number and not a guess
//!
//! A 3-seed pilot was run before the full panel, exactly as
//! `parley_readout.rs` did: see [`PILOT_SEEDS`] and [`PILOT_TEST_SECONDS`].
//! The full 12-seed figure is [`FULL_RUN_TEST_SECONDS`]. Both are for the
//! probe in its FINAL shape; the discarded unrestricted first draft cost
//! 274.2 s on three seeds and would not have finished the panel honestly at
//! any price. A committed cost is a claim with a date; re-measure rather than
//! extrapolate.

use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::accumulate::{Accumulation, precision_at};
use hornvale_hearsay::amplitude::gen_span;
use hornvale_hearsay::contact::{Contact, ContactGraph, contact_of};
use hornvale_hearsay::derive::{variants_about_accumulating, witnesses_of};
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_hearsay::ladder::{PeopleLadders, PrecisionLadder};
use hornvale_hearsay::lineage::{Lineage, lineage_of};
use hornvale_hearsay::transmission::{Transmission, Walk};
use hornvale_kernel::Claim;
use hornvale_kernel::Precision;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use std::collections::{BTreeMap, BTreeSet};

/// The 12-seed panel: a strict prefix of the census panel (`the-census` runs
/// seeds 0-999), of The Parley's 40-seed readout panel, and of this
/// campaign's first probe — so nothing here samples a population an earlier
/// campaign's instrument did not.
const PANEL: [u64; 12] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];

/// The predicate every claim here is about. Only an ending has parties beyond
/// its subject (The Palimpsest §6.1), so it is the only predicate a
/// transmission graph can be built over.
const PREDICATE: &str = hornvale_history::OCC_ENDED;

/// Seeds in the cost pilot run before the full panel.
const PILOT_SEEDS: usize = 3;

/// Seconds of nextest time the 3-seed pilot took, measured on this Mac.
///
/// **The DECISION RECORD, deliberately not re-measured after main was
/// absorbed**: it is what the panel-size rule was applied to, and re-stamping
/// it would make the decision look as though it had been taken against
/// numbers that did not exist when it was taken.
const PILOT_TEST_SECONDS: f64 = 6.38;

/// Seconds of nextest time the full 12-seed panel took, measured on this Mac.
///
/// Two green runs of identical content measured **264.00 s** and **348.61 s**
/// on the same box minutes apart, so this figure carries roughly 25%
/// run-to-run variance from whatever else the Mac was doing; the larger is
/// recorded because a budget wants the pessimistic one.
const FULL_RUN_TEST_SECONDS: f64 = 348.61;

// ---------------------------------------------------------------------------
// ENUMERATION BOUNDS. Both are reported, and a run that binds either is
// reported as capped rather than quietly truncated — a capped ending's
// candidate sets would be a subset of the real ones and every rule but
// `least-damage` would read off a population that is not the one described.
// ---------------------------------------------------------------------------

// NO SEAM-CROSSING CAP. One was tried at two crossings and REJECTED BY THE
// CONTROL, which is worth recording because it is itself a fact about the
// model: the shipped least-damage argmin routes across the seam far more than
// anyone would guess. Under the cap, the winner's crossing histogram on a
// 3-seed pilot read `0:13670 1:16491 2:11512` — piled up AT the ceiling — and
// 21,476 holders lost their shipped answer entirely because it needed three
// crossings or more. So the account does not merely step across the seam once
// and continue down a tree; the least-damaged route zigzags. What made the
// probe affordable instead is the POPULATION restriction below, not a route
// restriction.

/// Node expansions one ending's enumeration may spend before it is declared
/// capped. A backstop under [`MAX_CROSSINGS`], kept so that a future world
/// whose seam is denser than this panel's fails LOUDLY rather than slowly.
const MAX_EXPANSIONS: u64 = 4_000_000;

/// Route length one ending's enumeration may reach before it is declared
/// capped. Guards the recursion depth as well as the combinatorics.
const MAX_DEPTH: u32 = 2_048;

// ---------------------------------------------------------------------------
// The controls this probe is held to, READ OUT OF `parley_readout.rs`'s OWN
// CONSTANTS at this commit rather than out of The Parley's spec prose, for
// the reason `probe_seam_direction.rs` gives: main has moved since that spec
// was written, so the committed constant is the live claim.
// ---------------------------------------------------------------------------

/// `parley_readout.rs::BASELINE_ENDINGS_12`: endings over seeds 0-11.
///
/// **RE-DERIVED AGAINST THE MERGE PRODUCT.** This trio read 5,913 / 138 / 19
/// for the whole of this campaign's execution; The Underworld changed
/// settlement placement and `parley_readout.rs` was re-pinned at `44ea8d5a`.
const BASELINE_ENDINGS_12: usize = 4975;

/// `parley_readout.rs::BASELINE_FOREIGN_12`: endings over seeds 0-11 whose
/// attacker is of another people.
const BASELINE_FOREIGN_12: usize = 102;

/// `parley_readout.rs::BASELINE_MUTUALLY_EXCLUSIVE_12`: mutually-exclusive
/// cross-people day sets over seeds 0-11, under DESCENT and
/// `Accumulation::Multiplicative` only.
const BASELINE_MUTUALLY_EXCLUSIVE_12: usize = 15;

/// The Parley's published contact/descent ratio for the mutually-exclusive
/// count, indexed by [`Accumulation::ALL`], from `book/src/chronicle/the-
/// parley.md`. **A 40-SEED FIGURE, AND A PRE-UNDERWORLD ONE.** It is a
/// citation of what that chapter published and is therefore not re-derived;
/// [`UNDERTOW_MUTEX_RATIO_40`] is the same quantity measured on the tree this
/// campaign lands on. This panel is the first twelve seeds of both, so this
/// probe's own `least-damage` row is the comparable quantity and both arrays
/// are printed only so the three are visible side by side.
const PARLEY_MUTEX_RATIO_40: [f64; 3] = [0.59, 0.52, 0.77];

/// The same 40-seed quantity re-derived on the merge product by
/// `undertow_readout.rs`'s `Crossing::Free` arm — the pre-campaign behaviour,
/// which is exactly what The Parley measured.
const UNDERTOW_MUTEX_RATIO_40: [f64; 3] = [0.52, 0.55, 0.90];

// ===========================================================================
// THE SELECTION RULES.
// ===========================================================================

/// Which telling a holder keeps, out of every telling that reached it.
///
/// All five are **people-blind and stateless** — none consults `occ-people`,
/// which is the property this probe is testing the consequences of, not
/// relaxing. What separates them is only which feature of a telling wins.
///
/// **Two of them have no relaxation form, and that is a finding about the
/// question rather than an obstacle to route around.** A best-first walk of
/// the shipped shape can implement a rule only if the rule is the argmin of a
/// key that rises along every edge:
///
/// - [`Selection::LeastDamage`] and [`Selection::Primacy`] are such keys
///   (`width` is non-decreasing; `hops` rises by exactly one per edge), so
///   both could ship as written.
/// - [`Selection::Recency`] MAXIMISES hops, which is a longest-path problem.
///   On the acyclic founding tree it is well-defined; on the seam-augmented
///   graph it is **not defined at all as a walk rule** — hops is unbounded
///   around a cycle, so "the telling that arrived last" never arrives. It is
///   computable here only because this probe restricted routes to simple
///   paths, and longest-simple-path is NP-hard in general. Read its numbers
///   as "what a recency preference would do IF the substrate could express
///   one", never as a shippable arm.
/// - [`Selection::Frequency`] and [`Selection::FrequencyByWitness`] are not
///   argmins of anything: they need the whole arriving population at a node,
///   which a relaxation that keeps one telling per node has thrown away.
///   Shipping either means carrying a multiset per holder.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Selection {
    /// Today's behaviour: smallest final width, then fewest hops, then
    /// smallest witness `EntityId`. The BASELINE, and it must reproduce the
    /// shipped walk exactly.
    LeastDamage,
    /// The telling that arrived first: fewest hops, damage only as a
    /// tie-break.
    Primacy,
    /// The remembered value the most distinct tellings agree on, one vote per
    /// arriving telling. Ties, and the hop/rung reported for the winning
    /// value, resolve by `least-damage` among the tellings carrying it.
    Frequency,
    /// The same vote, one vote per distinct WITNESS rather than per arriving
    /// telling — each witness votes for the value of its own least-damaged
    /// telling. Reported beside [`Selection::Frequency`] because the seam
    /// gives one witness many routes to the same holder, so a per-route vote
    /// silently weights a well-connected witness more heavily and the two
    /// readings can differ. Not in the brief; kept because it is the
    /// defensible half of the same idea and costs nothing.
    FrequencyByWitness,
    /// The telling that arrived last: most hops, then least damage. See the
    /// type doc — **not implementable as a walk on this graph**.
    Recency,
}

impl Selection {
    /// Every rule, in a fixed order so the readout's rows are stable.
    const ALL: [Selection; 5] = [
        Selection::LeastDamage,
        Selection::Primacy,
        Selection::Frequency,
        Selection::FrequencyByWitness,
        Selection::Recency,
    ];

    /// This rule's short name, used as a readout row label.
    fn label(self) -> &'static str {
        match self {
            Selection::LeastDamage => "least-damage",
            Selection::Primacy => "primacy",
            Selection::Frequency => "frequency",
            Selection::FrequencyByWitness => "frequency-w",
            Selection::Recency => "recency",
        }
    }
}

/// One telling that reached a holder, deduplicated to its observable content.
///
/// Two routes that deliver the same value from the same witness at the same
/// hop count and rung are indistinguishable to every rule but
/// [`Selection::Frequency`], which counts votes — so they are collapsed to
/// one row carrying `routes`, rather than stored twice.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Telling {
    /// Accumulated damage width at this holder, as `f64::to_bits`. Monotone
    /// on a non-negative finite width, so it orders as the float does — the
    /// same trick the shipped key uses.
    width_bits: u64,
    /// Retellings between the originating witness and this holder.
    hops: u32,
    /// The ORIGINATING witness. Fixes the ladder for the whole route.
    witness: EntityId,
    /// The remembered day, as `f64::to_bits` — the quantity §6.5 compares.
    day_bits: u64,
    /// The rung this holder remembers the day at.
    rung: u8,
    /// Seam traversals on the route that delivered this telling. Part of the
    /// telling's identity: an account that came round through another people
    /// is a different telling from one that came down the line, even when it
    /// happens to agree.
    crossings: u32,
    /// How many distinct simple routes delivered exactly this telling.
    routes: u32,
}

impl Telling {
    /// The shipped ordering key: smallest width, then fewest hops, then
    /// smallest witness.
    fn least_damage_key(&self) -> (u64, u32, EntityId) {
        (self.width_bits, self.hops, self.witness)
    }

    /// Primacy's key: fewest hops, then smallest width, then smallest witness.
    fn primacy_key(&self) -> (u32, u64, EntityId) {
        (self.hops, self.width_bits, self.witness)
    }
}

/// Apply one selection rule to one holder's candidate set.
///
/// `cands` is non-empty and ascending by [`Telling::least_damage_key`] (the
/// enumerator's own storage order), so `cands[0]` is already the shipped
/// answer and every tie-break below resolves deterministically.
fn select(rule: Selection, cands: &[Telling]) -> Telling {
    let least = |xs: &[Telling]| {
        *xs.iter()
            .min_by_key(|t| t.least_damage_key())
            .expect("a holder's candidate set is never empty")
    };
    match rule {
        Selection::LeastDamage => least(cands),
        Selection::Primacy => *cands
            .iter()
            .min_by_key(|t| t.primacy_key())
            .expect("a holder's candidate set is never empty"),
        Selection::Recency => *cands
            .iter()
            .max_by_key(|t| (t.hops, std::cmp::Reverse(t.least_damage_key())))
            .expect("a holder's candidate set is never empty"),
        Selection::Frequency | Selection::FrequencyByWitness => {
            let mut votes: BTreeMap<u64, usize> = BTreeMap::new();
            if rule == Selection::Frequency {
                for t in cands {
                    *votes.entry(t.day_bits).or_default() += t.routes as usize;
                }
            } else {
                // One vote per witness, cast for that witness's own least
                // damaged telling.
                let mut per_witness: BTreeMap<EntityId, Telling> = BTreeMap::new();
                for t in cands {
                    per_witness
                        .entry(t.witness)
                        .and_modify(|b| {
                            if t.least_damage_key() < b.least_damage_key() {
                                *b = *t;
                            }
                        })
                        .or_insert(*t);
                }
                for t in per_witness.values() {
                    *votes.entry(t.day_bits).or_default() += 1;
                }
            }
            // The winning VALUE, ties broken by the least-damaged telling
            // carrying each tied value — so the tie-break never depends on
            // BTreeMap iteration order dressed up as a decision.
            let best_day = votes
                .iter()
                .map(|(day, n)| {
                    let rep = least(
                        &cands
                            .iter()
                            .copied()
                            .filter(|t| t.day_bits == *day)
                            .collect::<Vec<_>>(),
                    );
                    (*n, rep)
                })
                .max_by(|a, b| {
                    a.0.cmp(&b.0)
                        .then(b.1.least_damage_key().cmp(&a.1.least_damage_key()))
                })
                .expect("votes is non-empty when cands is");
            best_day.1
        }
    }
}

// ===========================================================================
// THE ENUMERATOR.
// ===========================================================================

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

/// One ending's whole candidate set, plus whether it is trustworthy.
#[derive(Default)]
struct Candidates {
    /// holder -> every telling that reached it, ascending by the shipped key.
    per_holder: BTreeMap<EntityId, Vec<Telling>>,
    /// Whether a bound bound. A capped ending is excluded from every
    /// reported statistic and counted separately.
    capped: bool,
    /// Node expansions this enumeration spent.
    expansions: u64,
}

/// The mutable half of one enumeration, kept apart from the borrowed world so
/// the recursion can hold `&self` and `&mut` state at once.
struct Accum {
    /// (holder, telling) -> route count, ascending in the shipped key's order
    /// because `Telling`'s field order IS that key's order.
    seen: BTreeMap<EntityId, BTreeMap<Telling, u32>>,
    /// Expansions spent so far.
    expansions: u64,
    /// Whether a bound bound.
    capped: bool,
}

impl<'a> Enumerator<'a> {
    /// Who `node` may tell under `arm`. Line for line `derive.rs`'s private
    /// `tellable`, minus the clock (every arm here runs `Clock::Off`, so its
    /// branches would be dead).
    fn tellable(
        &self,
        arm: Contact,
        node: EntityId,
        event_day: Option<f64>,
    ) -> Vec<(EntityId, bool)> {
        let mut out: Vec<(EntityId, bool)> = self
            .lineage
            .children_of(node)
            .iter()
            .map(|c| (*c, false))
            .collect();
        if arm == Contact::WithRaidSeam {
            for (peer, day) in self.contact.peers_of(node) {
                if event_day.is_none_or(|event| *day >= event) {
                    out.push((*peer, true));
                }
            }
        }
        // A hearer reachable BOTH ways is attributed to descent, exactly as
        // `probe_seam_direction.rs` attributes it: the shipped `tellable`
        // sorts and dedups by `EntityId` alone and expands such a hearer once,
        // and `false` sorts before `true`, so the sort resolves it.
        out.sort();
        out.dedup_by_key(|(id, _)| *id);
        out
    }

    /// Every telling that reaches every holder, over simple paths from every
    /// witness.
    ///
    /// The per-step arithmetic is the shipped relaxation's, verbatim: the same
    /// `gen_span`, the same `Accumulation::step`, the same `precision_at`
    /// against the ORIGINATING witness's ladder, and the same cumulative
    /// `ladder.apply` on the day the previous step already coarsened. Only the
    /// bookkeeping differs — the shipped walk keeps the argmin and this keeps
    /// everything.
    #[allow(clippy::too_many_arguments)]
    fn enumerate(
        &self,
        ladders: &PeopleLadders,
        rule: Accumulation,
        arm: Contact,
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
            let people = match led.value_of(*w, hornvale_history::OCC_PEOPLE) {
                Some(Value::Text(p)) => p.clone(),
                _ => String::new(),
            };
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
                arm,
                Some(event),
                *w,
                Step {
                    node: *w,
                    width,
                    day: event,
                    rung: Precision::FINEST,
                    hops: 0,
                    crossings: 0,
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
            expansions: acc.expansions,
        }
    }

    /// Walk every simple path out of `step.node`, recording the telling that
    /// reached it first.
    ///
    /// The route is carried in [`Step`] rather than as five more parameters
    /// because this already takes more than clippy's `too_many_arguments`
    /// threshold with the world, the bounds and the accumulator.
    #[allow(clippy::too_many_arguments)]
    fn descend(
        &self,
        acc: &mut Accum,
        witness_set: &BTreeSet<EntityId>,
        ladder: &PrecisionLadder,
        rule: Accumulation,
        arm: Contact,
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
        // `routes` is filled in from the multiplicity when the map is drained,
        // so it is zeroed here and never part of the dedup key.
        let telling = Telling {
            width_bits: step.width.to_bits(),
            hops: step.hops,
            witness,
            day_bits: step.day.to_bits(),
            rung: step.rung.rung(),
            crossings: step.crossings,
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
        for (hearer, is_seam) in self.tellable(arm, step.node, event_day) {
            if witness_set.contains(&hearer) {
                continue; // a witness is never demoted to an inheritor
            }
            if visited.contains(&hearer) {
                continue; // simple paths only — see the module doc
            }
            let next_width = rule.step(
                step.width,
                gen_span(self.led, self.durations, step.node, hearer),
            );
            let rung = precision_at(ladder, next_width);
            let next = Step {
                node: hearer,
                width: next_width,
                day: ladder.apply(rung, step.day),
                rung,
                hops: step.hops + 1,
                crossings: step.crossings + u32::from(is_seam),
            };
            visited.insert(hearer);
            self.descend(
                acc,
                witness_set,
                ladder,
                rule,
                arm,
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
    /// Seam traversals behind this node.
    crossings: u32,
    /// Retellings behind this node.
    hops: u32,
}

// ---------------------------------------------------------------------------
// World reading. A near-copy of `parley_readout.rs::read_world` and of
// `probe_seam_direction.rs`'s, deliberately: a probe that assembled the
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
    /// Whether the attacker is of another people — §6.5's whole population.
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
        contact: contact_of(led),
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

// ===========================================================================
// THE ACCUMULATORS.
// ===========================================================================

/// S0: how much choice a holder actually had, over one arm and rule.
#[derive(Clone, Default)]
struct ChoiceRow {
    /// Holders across every ending.
    holders: usize,
    /// Tellings that reached them, counting distinct simple routes.
    routes: usize,
    /// Distinct tellings (deduplicated to observable content).
    tellings: usize,
    /// Holders reached by two or more distinct tellings.
    multi_telling: usize,
    /// Holders reached by two or more distinct remembered VALUES — the only
    /// holders at which a tie-break can decide anything.
    multi_value: usize,
    /// distinct-remembered-values -> how many holders.
    value_hist: BTreeMap<usize, usize>,
    /// Holders on FOREIGN endings only — §6.5's own population, and the only
    /// population on which the two arms are like-for-like comparable, since
    /// the contact arm is enumerated on nothing else.
    foreign_holders: usize,
    /// ... of which two or more distinct remembered values reached them.
    foreign_multi_value: usize,
    /// Holders where the five rules did not all pick the same remembered
    /// value.
    rules_disagree: usize,
    /// Per rule, holders whose selected value differs from `least-damage`'s,
    /// indexed by [`Selection::ALL`].
    differs_from_shipped: [usize; 5],
    /// The largest candidate set seen at any one holder.
    max_tellings: usize,
    /// seam-crossings-on-the-winning-route -> how many holders, taken over
    /// the SHIPPED winner. The evidence that `MAX_CROSSINGS` does not bind on
    /// the answer this probe is held to.
    winner_crossings: BTreeMap<usize, usize>,
    /// Holders where the minimum key is attained by two or more DIFFERENT
    /// remembered days. Reported, never asserted: it is a property of the
    /// SHIPPED model, not of this instrument — see the test's own note.
    key_ties: usize,
    /// Endings whose enumeration hit a bound. Excluded from every statistic.
    capped_endings: usize,
    /// The largest expansion count any one ending's enumeration spent.
    max_expansions: u64,
}

/// S1: The Parley's §6.5 quantities, over one arm, accumulation rule and
/// selection rule.
#[derive(Clone, Copy, Default)]
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

/// One seed's whole contribution.
#[derive(Clone, Default)]
struct SeedRow {
    /// Which seed.
    seed: u64,
    /// Endings in the world.
    endings: usize,
    /// ... of which the attacker is of another people.
    foreign: usize,
    /// Undirected seam edges the world carries.
    seam_edges: usize,
    /// S0, indexed by [`Contact::ALL`] then [`Accumulation::ALL`].
    choice: [[ChoiceRow; 3]; 2],
    /// S1, indexed by [`Contact::ALL`], [`Accumulation::ALL`],
    /// [`Selection::ALL`].
    div: [[[DivRow; 5]; 3]; 2],
    /// S1 over the MATCHED population — the foreign endings whose contact
    /// enumeration ran to completion, on BOTH arms. [`SeedRow::div`]'s descent
    /// arm covers every foreign ending (which is what the published control
    /// is pinned against) while its contact arm cannot, so the two are not
    /// like-for-like and only this pair may be divided.
    div_matched: [[[DivRow; 5]; 3]; 2],
    /// Foreign endings dropped from [`SeedRow::div_matched`] because their
    /// contact enumeration exhausted [`MAX_EXPANSIONS`].
    unmatched_endings: usize,
    /// DIAGNOSTIC: up to a few holders where the shipped answer was absent
    /// from the minimum-key set, rendered for the readout so a red says WHAT
    /// differed rather than only how many.
    absent_samples: Vec<String>,
    /// CONTROL: holders where the shipped walk's own answer was ABSENT from
    /// the enumerated candidate set entirely. Must be zero — it proves the
    /// enumeration is a SUPERSET of the route the shipped walk actually took.
    shipped_absent: usize,
    /// FINDING, not a control: holders where the shipped answer is present in
    /// the candidate set but is NOT its argmin under the shipped key,
    /// indexed by [`Contact::ALL`]. See the readout's own note.
    not_argmin: [usize; 2],
    /// Holders compared for [`SeedRow::not_argmin`], indexed by
    /// [`Contact::ALL`] — the denominator that share needs.
    argmin_compared: [usize; 2],
    /// CONTROL: endings whose enumeration reached a different holder SET from
    /// the shipped walk. Must be zero.
    holder_set_mismatch: usize,
}

/// The remembered day a claim carries, as bits. `None` for a non-`Number`
/// object, which an ending never has.
fn day_bits_of(c: &Claim) -> Option<u64> {
    match &c.object {
        Value::Number(day) => Some(day.to_bits()),
        _ => None,
    }
}

/// The remembered days each people holds, under one selection.
fn by_people(read: &WorldRead, held: &BTreeMap<EntityId, u64>) -> BTreeMap<String, BTreeSet<u64>> {
    let mut out: BTreeMap<String, BTreeSet<u64>> = BTreeMap::new();
    for (holder, day_bits) in held {
        let Some(p) = read.people_of.get(holder) else {
            continue;
        };
        out.entry(p.clone()).or_default().insert(*day_bits);
    }
    out
}

/// Every quantity S0 and S1 ask for, over one world.
fn measure_seed(seed: u64, led: &Ledger, read: &WorldRead) -> SeedRow {
    let enumerator = Enumerator {
        led,
        lineage: &read.lineage,
        contact: &read.contact,
        durations: &read.durations,
    };
    let mut row = SeedRow {
        seed,
        endings: read.endings.len(),
        seam_edges: read.contact.edges(),
        ..Default::default()
    };

    for e in &read.endings {
        let foreign = e.is_foreign();
        if foreign {
            row.foreign += 1;
        }
        // This ending's §6.5 verdict per (arm, accumulation, selection), held
        // back until both arms are done so a foreign ending whose CONTACT
        // enumeration capped can be dropped from BOTH arms at once. 0 =
        // mutually exclusive, 1 = one-sided, 2 = identical.
        let mut verdict: [[[Option<u8>; 5]; 3]; 2] = Default::default();
        let mut contact_capped = [false; 3];
        for (ci, arm) in Contact::ALL.iter().enumerate() {
            // THE POPULATION RESTRICTION, AND THE WHOLE REASON THIS PROBE IS
            // AFFORDABLE. Under descent the graph is a forest and the
            // enumeration is free (338 expansions on the largest ending of
            // the merge product's panel); under contact it is not (63,054,018
            // routes on a pre-absorption 3-seed pilot, with 12 endings
            // exhausting a four-million expansion ceiling; the pilot's own
            // figures are not re-derivable because the capped arm went with
            // the bound it rejected). So the contact arm is enumerated only on
            // §6.5's OWN population — the ~2.1% of endings whose attacker is
            // of another people — which is the population every number in S1
            // is reported over anyway. The descent arm is enumerated on
            // everything, because it costs nothing and because the fact it
            // establishes (one telling per holder, always) is the load-bearing
            // half of S0.
            if *arm == Contact::WithRaidSeam && !foreign {
                continue;
            }
            for (ri, rule) in Accumulation::ALL.iter().enumerate() {
                let cands = enumerator.enumerate(&read.ladders, *rule, *arm, e.subject);
                let c = &mut row.choice[ci][ri];
                c.max_expansions = c.max_expansions.max(cands.expansions);
                if cands.capped {
                    c.capped_endings += 1;
                    if *arm == Contact::WithRaidSeam {
                        contact_capped[ri] = true;
                    }
                    continue;
                }

                // ---- THE SHIPPED WALK. Run FIRST, because it is the
                // baseline itself and not a reconstruction of one: the
                // `least-damage` row below IS `variants_about_accumulating`'s
                // output, and the enumeration is held to CONTAINING it at the
                // minimum key rather than to re-deriving it.
                //
                // The distinction is not pedantry, and an earlier draft of
                // this probe learned it the expensive way. Its `least-damage`
                // argmin disagreed with the shipped walk on 481 holders of a
                // 3-seed pilot, and the cause was in the SHIPPED model rather
                // than in either instrument: every `Accumulation` rule is
                // order-independent in the multiset of generational spans it
                // consumes, so two routes from the same witness, of the same
                // length, over the same spans in a different ORDER, arrive
                // with a bit-identical width and a bit-identical key — and a
                // DIFFERENT remembered day, because the rungs were applied in
                // a different order along the way. At such a holder the
                // shipped walk's answer is decided by its relaxation ORDER,
                // which the key does not determine. Those holders are counted
                // as `key_ties` and reported; nothing here asserts them away.
                let shipped = variants_about_accumulating(
                    &Walk {
                        ledger: led,
                        lineage: &read.lineage,
                        contact: &read.contact,
                        policy: Transmission {
                            contact: *arm,
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
                    row.holder_set_mismatch += 1;
                }

                // ---- the five selections, one pass ----
                let mut picks: [BTreeMap<EntityId, u64>; 5] = Default::default();
                for (holder, set) in &cands.per_holder {
                    c.holders += 1;
                    c.tellings += set.len();
                    c.routes += set.iter().map(|t| t.routes as usize).sum::<usize>();
                    c.max_tellings = c.max_tellings.max(set.len());
                    if set.len() > 1 {
                        c.multi_telling += 1;
                    }
                    let values: BTreeSet<u64> = set.iter().map(|t| t.day_bits).collect();
                    *c.value_hist.entry(values.len()).or_default() += 1;
                    if values.len() > 1 {
                        c.multi_value += 1;
                    }
                    if foreign {
                        c.foreign_holders += 1;
                        if values.len() > 1 {
                            c.foreign_multi_value += 1;
                        }
                    }

                    // The candidates at the MINIMUM key. `set` is sorted and
                    // `Telling`'s first three fields ARE that key, so this is
                    // a prefix.
                    let min_key = set[0].least_damage_key();
                    let tied: Vec<&Telling> = set
                        .iter()
                        .take_while(|t| t.least_damage_key() == min_key)
                        .collect();
                    if tied
                        .iter()
                        .map(|t| t.day_bits)
                        .collect::<BTreeSet<_>>()
                        .len()
                        > 1
                    {
                        c.key_ties += 1;
                    }

                    // THE CONTROL: the shipped answer must be SOMEWHERE in
                    // the enumerated set — that is what proves the
                    // enumeration is a superset of the route the shipped walk
                    // took, and it is the strongest claim this probe can make
                    // without re-implementing the relaxation's arrival order.
                    // Whether it is also the set's ARGMIN is recorded, not
                    // asserted; see the readout.
                    let mut shipped_day: Option<u64> = None;
                    match shipped_by_holder.get(holder) {
                        Some(cl) => {
                            let d = day_bits_of(cl);
                            let matched: Vec<&Telling> = d
                                .map(|d| {
                                    set.iter()
                                        .filter(|t| {
                                            t.hops == cl.hops
                                                && t.rung == cl.precision.rung()
                                                && t.day_bits == d
                                        })
                                        .collect()
                                })
                                .unwrap_or_default();
                            match matched.first() {
                                Some(_) => {
                                    let crossings = matched
                                        .iter()
                                        .map(|t| t.crossings as usize)
                                        .min()
                                        .unwrap_or(0);
                                    *c.winner_crossings.entry(crossings).or_default() += 1;
                                    row.argmin_compared[ci] += 1;
                                    if !tied.iter().any(|t| {
                                        t.hops == cl.hops
                                            && t.rung == cl.precision.rung()
                                            && Some(t.day_bits) == d
                                    }) {
                                        row.not_argmin[ci] += 1;
                                        if row.absent_samples.len() < 6 {
                                            row.absent_samples.push(format!(
                                                "seed {seed} {}/{} subj {:?} holder {:?}: \
                                                 shipped hops={} rung={} | the set's argmin \
                                                 hops={} rung={} (same day: {}) — argmin \
                                                 width_bits {} vs the shipped telling's {}",
                                                arm.label(),
                                                rule.label(),
                                                e.subject,
                                                holder,
                                                cl.hops,
                                                cl.precision.rung(),
                                                set[0].hops,
                                                set[0].rung,
                                                Some(set[0].day_bits) == d,
                                                set[0].width_bits,
                                                matched[0].width_bits,
                                            ));
                                        }
                                    }
                                    shipped_day = d;
                                }
                                None => row.shipped_absent += 1,
                            }
                        }
                        None => row.shipped_absent += 1,
                    }
                    let Some(shipped_day) = shipped_day else {
                        continue; // already counted as a control failure
                    };

                    let mut chosen: BTreeSet<u64> = BTreeSet::new();
                    picks[0].insert(*holder, shipped_day);
                    chosen.insert(shipped_day);
                    for (si, sel) in Selection::ALL.iter().enumerate().skip(1) {
                        let t = select(*sel, set);
                        chosen.insert(t.day_bits);
                        picks[si].insert(*holder, t.day_bits);
                    }
                    if chosen.len() > 1 {
                        c.rules_disagree += 1;
                    }
                    for (slot, p) in c.differs_from_shipped.iter_mut().zip(picks.iter()) {
                        if p[holder] != shipped_day {
                            *slot += 1;
                        }
                    }
                }

                // ---- S1 ----
                if foreign {
                    let ap = e
                        .attacker_people
                        .as_ref()
                        .expect("a foreign ending names an attacker people");
                    for (si, pick) in picks.iter().enumerate() {
                        let sets = by_people(read, pick);
                        let (Some(v), Some(r)) = (sets.get(&e.people), sets.get(ap)) else {
                            continue;
                        };
                        let v_only = v.difference(r).count();
                        let r_only = r.difference(v).count();
                        verdict[ci][ri][si] = Some(if v_only > 0 && r_only > 0 {
                            0
                        } else if v_only > 0 || r_only > 0 {
                            1
                        } else {
                            2
                        });
                    }
                }
            }
        }

        // Commit this ending's §6.5 verdicts. The FULL population goes to
        // `div`, which is what the published control is pinned against; the
        // MATCHED population — both arms, only where the contact enumeration
        // completed — goes to `div_matched`, which is the only pair a ratio
        // may be taken over.
        if contact_capped.iter().any(|x| *x) {
            row.unmatched_endings += 1;
        }
        for (ci, per_rule) in verdict.iter().enumerate() {
            for (ri, per_sel) in per_rule.iter().enumerate() {
                for (si, code) in per_sel.iter().enumerate() {
                    let Some(code) = code else { continue };
                    let bump = |d: &mut DivRow| {
                        d.compared += 1;
                        match code {
                            0 => d.mutually_exclusive += 1,
                            1 => d.one_sided += 1,
                            _ => d.identical += 1,
                        }
                    };
                    bump(&mut row.div[ci][ri][si]);
                    if !contact_capped[ri] {
                        bump(&mut row.div_matched[ci][ri][si]);
                    }
                }
            }
        }
    }
    row
}

/// Print a `label -> count` distribution compactly, capped so a long tail
/// cannot swamp the line.
fn show_hist(hist: &BTreeMap<usize, usize>) -> String {
    if hist.is_empty() {
        return "(none)".to_string();
    }
    let mut parts: Vec<String> = hist
        .iter()
        .take(12)
        .map(|(k, v)| format!("{k}:{v}"))
        .collect();
    if hist.len() > 12 {
        parts.push(format!("(+{} more buckets)", hist.len() - 12));
    }
    parts.join(" ")
}

/// Sum one `usize` field over the panel.
fn sum<F: Fn(&SeedRow) -> usize>(rows: &[SeedRow], f: F) -> usize {
    rows.iter().map(f).sum()
}

/// Fold one [`ChoiceRow`] vertex over the panel.
fn fold_choice(rows: &[SeedRow], ci: usize, ri: usize) -> ChoiceRow {
    let mut out = ChoiceRow::default();
    for r in rows {
        let s = &r.choice[ci][ri];
        out.holders += s.holders;
        out.routes += s.routes;
        out.tellings += s.tellings;
        out.multi_telling += s.multi_telling;
        out.multi_value += s.multi_value;
        out.foreign_holders += s.foreign_holders;
        out.foreign_multi_value += s.foreign_multi_value;
        out.rules_disagree += s.rules_disagree;
        out.capped_endings += s.capped_endings;
        out.key_ties += s.key_ties;
        out.max_tellings = out.max_tellings.max(s.max_tellings);
        for (k, v) in &s.winner_crossings {
            *out.winner_crossings.entry(*k).or_default() += v;
        }
        out.max_expansions = out.max_expansions.max(s.max_expansions);
        for (k, v) in &s.value_hist {
            *out.value_hist.entry(*k).or_default() += v;
        }
        for si in 0..Selection::ALL.len() {
            out.differs_from_shipped[si] += s.differs_from_shipped[si];
        }
    }
    out
}

/// Fold one [`DivRow`] vertex over the panel. `matched` selects
/// [`SeedRow::div_matched`] (the like-for-like population) over
/// [`SeedRow::div`] (the full one).
fn fold_div(rows: &[SeedRow], matched: bool, ci: usize, ri: usize, si: usize) -> DivRow {
    let mut out = DivRow::default();
    for r in rows {
        let s = if matched {
            &r.div_matched[ci][ri][si]
        } else {
            &r.div[ci][ri][si]
        };
        out.compared += s.compared;
        out.mutually_exclusive += s.mutually_exclusive;
        out.one_sided += s.one_sided;
        out.identical += s.identical;
    }
    out
}

// ===========================================================================
// S0 / S1 — THE PROBE.
// ===========================================================================

/// S0 and S1 over the 12-seed panel: how often a holder has a choice at all,
/// and whether The Parley's pooling survives four other people-blind
/// tie-breaks.
///
/// claim: structural(seed: panel) — false-positive seed-loop flag; the loop
/// binds a census-panel prefix, not a search over seeds.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn whether_the_tiebreak_or_the_contact_pooled_the_accounts() {
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

    println!("\n============ THE UNDERTOW TIE-BREAK PROBE ============");
    println!(
        "panel                : {} seeds (census seeds 0-11), {} measured, skipped {skipped:?}",
        PANEL.len(),
        rows.len()
    );
    println!("predicate            : {PREDICATE}");
    println!(
        "baseline             : {} — the clock is Off throughout, exactly as The Parley's \
         contact arms were, so nothing here varies two things at once",
        Transmission::AS_SHIPPED.label()
    );
    println!(
        "method               : the candidate set reaching each holder is ENUMERATED over \
         SIMPLE PATHS on the shipped graph, and each selection rule is applied to it. The \
         contact arm is enumerated on §6.5's own population (foreign endings) and the descent \
         arm on every ending — see the module doc's three bounds. No arm is implemented; \
         nothing in windows/hearsay/src changes."
    );
    println!(
        "cost                 : {PILOT_SEEDS}-seed pilot {PILOT_TEST_SECONDS:.2} s of nextest \
         time; full {}-seed panel {FULL_RUN_TEST_SECONDS:.2} s.",
        PANEL.len()
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
         {BASELINE_FOREIGN_12} foreign / {BASELINE_MUTUALLY_EXCLUSIVE_12} mutually-exclusive \
         (descent x multiplicative) on this panel."
    );

    // PER SEED, so a panel total is never read as if it were tight.
    let mult_index = Accumulation::ALL
        .iter()
        .position(|r| *r == Accumulation::Multiplicative)
        .expect("Accumulation::ALL carries Multiplicative");
    println!(
        "\n  {:<6} {:>9} {:>9} {:>7} {:>11} {:>11} {:>12} {:>12}",
        "seed", "endings", "foreign", "edges", "d:all", "d:foreign", "c:foreign", "c:>=2 value"
    );
    for r in &rows {
        println!(
            "  {:<6} {:>9} {:>9} {:>7} {:>11} {:>11} {:>12} {:>12}",
            r.seed,
            r.endings,
            r.foreign,
            r.seam_edges,
            r.choice[0][mult_index].holders,
            r.choice[0][mult_index].foreign_holders,
            r.choice[1][mult_index].holders,
            r.choice[1][mult_index].multi_value,
        );
    }

    // =====================================================================
    // S0 — IS THERE A CHOICE AT ALL? Printed FIRST: if holders receive one
    // value, every rule agrees trivially and S1 measures nothing.
    // =====================================================================
    println!("\n=== S0 — IS THERE A CHOICE AT ALL? (the anti-vacuity measurement) ===");
    println!(
        "  A holder can only be affected by a tie-break if two or more DISTINCT REMEMBERED \
         VALUES reached it. `tellings` are distinct arriving (width, hops, witness, day, rung) \
         tuples; `routes` counts distinct simple paths, which is larger wherever the seam \
         gives one witness several ways in."
    );
    println!(
        "  **THE TWO ARMS COVER DIFFERENT POPULATIONS AND THE `foreign` COLUMNS ARE THE \
         LIKE-FOR-LIKE PAIR.** Descent is enumerated on every ending because it is free; \
         contact only on §6.5's own population, the endings whose attacker is of another \
         people. Read `holders` down the descent rows and across the `f:` columns, never \
         across the two arms' `holders`."
    );
    println!(
        "\n  {:<9} {:<15} {:>10} {:>11} {:>10} {:>10} {:>10} {:>9} {:>10} {:>11}",
        "arm",
        "rule",
        "holders",
        "routes",
        "tellings",
        ">=2 tell",
        ">=2 VALUE",
        "val%",
        "f:holders",
        "f:>=2 VALUE"
    );
    let mut multi_value_total = 0usize;
    let mut capped_total = 0usize;
    for (ci, arm) in Contact::ALL.iter().enumerate() {
        for (ri, rule) in Accumulation::ALL.iter().enumerate() {
            let c = fold_choice(&rows, ci, ri);
            multi_value_total += c.multi_value;
            capped_total += c.capped_endings;
            println!(
                "  {:<9} {:<15} {:>10} {:>11} {:>10} {:>10} {:>10} {:>8.2}% {:>10} {:>10} \
                 ({:.2}%)",
                arm.label(),
                rule.label(),
                c.holders,
                c.routes,
                c.tellings,
                c.multi_telling,
                c.multi_value,
                pct(c.multi_value, c.holders),
                c.foreign_holders,
                c.foreign_multi_value,
                pct(c.foreign_multi_value, c.foreign_holders),
            );
        }
    }
    println!("\n  distinct remembered values per holder, and how far the rules diverge on them:");
    for (ci, arm) in Contact::ALL.iter().enumerate() {
        for (ri, rule) in Accumulation::ALL.iter().enumerate() {
            let c = fold_choice(&rows, ci, ri);
            println!("\n  --- {} x {} ---", arm.label(), rule.label());
            println!(
                "    values-per-holder histogram : {}",
                show_hist(&c.value_hist)
            );
            println!(
                "    holders where the 5 rules DISAGREE on the value: {} ({:.4}% of {} holders)",
                c.rules_disagree,
                pct(c.rules_disagree, c.holders),
                c.holders
            );
            for (si, sel) in Selection::ALL.iter().enumerate() {
                println!(
                    "      {:<14} differs from least-damage on {:>8} holders ({:.4}%)",
                    sel.label(),
                    c.differs_from_shipped[si],
                    pct(c.differs_from_shipped[si], c.holders)
                );
            }
            println!(
                "    seam crossings on the SHIPPED winner's route: {} — unbounded; a cap \
                 at 2 was tried and rejected (see the constants block)",
                show_hist(&c.winner_crossings)
            );
            println!(
                "    holders whose MINIMUM KEY is attained by two or more different \
                 remembered days: {} ({:.4}%) — at these the SHIPPED model's answer is \
                 decided by relaxation order, not by its key",
                c.key_ties,
                pct(c.key_ties, c.holders)
            );
            println!(
                "    enumeration: max {} tellings at one holder, max {} expansions on one \
                 ending, {} capped endings",
                c.max_tellings, c.max_expansions, c.capped_endings
            );
        }
    }

    // =====================================================================
    // S1 — THE PARLEY'S §6.5 QUANTITIES, PER SELECTION RULE.
    // =====================================================================
    // =====================================================================
    // S0b — AN UNPLANNED FINDING THE INSTRUMENT WALKED INTO, REPORTED WHERE
    // IT WAS FOUND RATHER THAN TIDIED AWAY.
    // =====================================================================
    println!("\n=== S0b — IS THE SHIPPED WALK ITS OWN ARGMIN? ===");
    println!(
        "  `variants_about_accumulating`'s doc specifies its answer as the argmin of \
         `(width.to_bits(), hops, witness)`. The enumeration below contains the route the \
         shipped walk took (that is the asserted control) — so it can also be asked whether \
         that route is the MINIMUM of the same key over the same graph."
    );
    println!(
        "\n  {:<9} {:>12} {:>14} {:>9}",
        "arm", "compared", "not the argmin", "share"
    );
    for (ci, arm) in Contact::ALL.iter().enumerate() {
        let n = sum(&rows, |r| r.not_argmin[ci]);
        let d = sum(&rows, |r| r.argmin_compared[ci]);
        println!("  {:<9} {d:>12} {n:>14} {:>8.4}%", arm.label(), pct(n, d));
    }
    let argmin_samples: Vec<&String> = rows.iter().flat_map(|r| r.absent_samples.iter()).collect();
    for line in argmin_samples.iter().take(6) {
        println!("    {line}");
    }
    println!(
        "  READ THIS CAREFULLY BEFORE CARRYING IT ANYWHERE. The descent row is an EXACT \
         equivalence control and is asserted to zero: on the founding tree every holder \
         receives exactly one telling, so `least-damage` there is the shipped answer by \
         construction and the per-step arithmetic in this file is provably the arithmetic in \
         derive.rs. The CONTACT row is a REPORT. A non-zero there says the shipped route is \
         not the minimum of the shipped key over the SIMPLE PATHS of the seam-augmented \
         graph; adding a cycle cannot lower a non-decreasing width, so the two ought to \
         agree. This probe does not resolve it — it is stated as a discrepancy with its \
         evidence, for a campaign that means to look at derive.rs rather than at the \
         Parley's numbers. **NOTE WHY THE PREDECESSOR PROBE COULD NOT SEE IT**: \
         `probe_seam_direction.rs` reproduced the shipped walk holder-for-holder with zero \
         mismatches, but its copy was itself a RELAXATION, so it would inherit this \
         behaviour rather than detect it. An instrument held to another instrument agrees \
         with it about the thing they share."
    );

    println!("\n=== S1 — THE PARLEY'S §6.5 QUANTITIES, PER SELECTION RULE ===");
    println!(
        "  Population: cross-people endings where BOTH the victim's people and the raider's \
         people hold the account. `identical` is the pooling signal; `mutex` is §6.5's frozen \
         divergence measure. THE PARLEY'S HEADLINE IS THE mutex CONTACT/DESCENT RATIO."
    );
    let unmatched = sum(&rows, |r| r.unmatched_endings);
    println!(
        "  EVERY ROW BELOW IS THE MATCHED POPULATION: the foreign endings whose CONTACT \
         enumeration ran to completion, scored on both arms. {unmatched} foreign ending(s) \
         exhausted MAX_EXPANSIONS and are dropped from BOTH arms, so the ratio stays \
         like-for-like. Over the FULL foreign population the descent arm scores {} \
         mutually-exclusive x multiplicative, which is the figure parley_readout.rs pins \
         and the control below asserts.",
        fold_div(
            &rows,
            false,
            0,
            Accumulation::ALL
                .iter()
                .position(|r| *r == Accumulation::Multiplicative)
                .expect("Accumulation::ALL carries Multiplicative"),
            0
        )
        .mutually_exclusive
    );
    // ratio[ri][si] — kept so the verdict table below reads them rather than
    // re-deriving them in prose.
    let mut mutex_ratio = [[f64::NAN; 5]; 3];
    let mut ident_ratio = [[f64::NAN; 5]; 3];
    for (ri, rule) in Accumulation::ALL.iter().enumerate() {
        println!("\n  === ACCUMULATION: {} ===", rule.label());
        println!(
            "    {:<14} {:<9} {:>9} {:>7} {:>10} {:>10} {:>9} {:>9}",
            "selection", "arm", "compared", "mutex", "one-sided", "identical", "mutex%", "ident%"
        );
        for (si, sel) in Selection::ALL.iter().enumerate() {
            let d = fold_div(&rows, true, 0, ri, si);
            let c = fold_div(&rows, true, 1, ri, si);
            for (arm, x) in [(Contact::Descent, d), (Contact::WithRaidSeam, c)] {
                println!(
                    "    {:<14} {:<9} {:>9} {:>7} {:>10} {:>10} {:>8.2}% {:>8.2}%",
                    if arm == Contact::Descent {
                        sel.label()
                    } else {
                        ""
                    },
                    arm.label(),
                    x.compared,
                    x.mutually_exclusive,
                    x.one_sided,
                    x.identical,
                    pct(x.mutually_exclusive, x.compared),
                    pct(x.identical, x.compared),
                );
            }
            mutex_ratio[ri][si] = c.mutually_exclusive as f64 / d.mutually_exclusive.max(1) as f64;
            ident_ratio[ri][si] = c.identical as f64 / d.identical.max(1) as f64;
            println!(
                "    {:<14} {:<9} contact/descent: mutex {}->{} = {:.2}x  |  identical \
                 {}->{} = {:.2}x",
                "",
                "RATIO",
                d.mutually_exclusive,
                c.mutually_exclusive,
                mutex_ratio[ri][si],
                d.identical,
                c.identical,
                ident_ratio[ri][si],
            );
        }
        println!(
            "    The Parley published {:.2}x for this rule on its 40-SEED panel, measured \
             before The Underworld moved settlement placement; re-derived on the merge \
             product the same 40-seed quantity is {:.2}x. This panel is the first twelve \
             seeds of both, so the `least-damage` row above is the comparable quantity and \
             the two 40-seed figures are printed only for orientation.",
            PARLEY_MUTEX_RATIO_40[ri], UNDERTOW_MUTEX_RATIO_40[ri]
        );
    }

    // =====================================================================
    // THE VERDICT TABLE. Written as the brief asks it to be read: does
    // pooling persist under all the rules, or only some?
    // =====================================================================
    println!("\n=== VERDICT — DOES POOLING SURVIVE THE TIE-BREAK? ===");
    println!(
        "  POOLS = contact lowered the mutually-exclusive count AND raised the identical \
         count, which is exactly what The Parley reported. **A VERTEX IS ONLY EVIDENCE ABOUT \
         THE TIE-BREAK WHERE THE BASELINE ITSELF POOLS** — where it does not, no alternative \
         can discriminate and the vertex is marked `n/a`, not `no`."
    );
    let mut baseline_pools = [false; 3];
    for (ri, flag) in baseline_pools.iter_mut().enumerate() {
        let d = fold_div(&rows, true, 0, ri, 0);
        let c = fold_div(&rows, true, 1, ri, 0);
        *flag = c.mutually_exclusive < d.mutually_exclusive && c.identical > d.identical;
    }
    println!(
        "\n  {:<15} {:<20} {:<20} {:<20}",
        "selection", "additive", "quadrature", "multiplicative"
    );
    let mut breaks = 0usize;
    let mut discriminating = 0usize;
    let mut worst_mutex_gap = 0i64;
    for (si, sel) in Selection::ALL.iter().enumerate() {
        let mut vertices: Vec<String> = Vec::new();
        for (ri, base_pools) in baseline_pools.iter().enumerate() {
            let d = fold_div(&rows, true, 0, ri, si);
            let c = fold_div(&rows, true, 1, ri, si);
            let pools = c.mutually_exclusive < d.mutually_exclusive && c.identical > d.identical;
            let base_c = fold_div(&rows, true, 1, ri, 0);
            let gap = c.mutually_exclusive as i64 - base_c.mutually_exclusive as i64;
            worst_mutex_gap = worst_mutex_gap.max(gap.abs());
            if *base_pools && si > 0 {
                discriminating += 1;
                if !pools {
                    breaks += 1;
                }
            }
            vertices.push(format!(
                "{} {:.2}x {:+}",
                if !*base_pools {
                    "n/a  "
                } else if pools {
                    "POOLS"
                } else {
                    "BREAK"
                },
                c.mutually_exclusive as f64 / d.mutually_exclusive.max(1) as f64,
                gap
            ));
        }
        println!(
            "  {:<15} {:<20} {:<20} {:<20}",
            sel.label(),
            vertices[0],
            vertices[1],
            vertices[2]
        );
    }
    println!(
        "  (the signed column is this rule's mutually-exclusive COUNT under contact minus the \
         baseline's, over the same {} compared endings — the absolute size of the effect, \
         which a ratio hides)",
        fold_div(&rows, true, 1, 0, 0).compared
    );
    println!(
        "\n  VERDICT: of the {discriminating} (selection x accumulation) vertices where the \
         BASELINE pools and an alternative could therefore disagree, {breaks} did. The \
         largest gap between any rule's mutually-exclusive count and the baseline's, on any \
         accumulation rule, is {worst_mutex_gap} event(s) out of {}.",
        fold_div(&rows, true, 1, 0, 0).compared
    );
    println!(
        "  READING: {}",
        if breaks == 0 {
            "POOLING PERSISTS UNDER EVERY PEOPLE-BLIND TIE-BREAK TESTED, wherever the \
             baseline pools at all. THE TIE-BREAK IS EXONERATED: The Parley's headline is a \
             statement about contact, not about the argmin."
        } else {
            "at least one alternative rule fails to pool where the baseline does — the \
             tie-break is IMPLICATED, and the BREAK vertices say which rules and which \
             accumulation rules."
        }
    );
    println!(
        "  AND THE REASON IT CANNOT BE THE TIE-BREAK IS STRONGER THAN THE TABLE. Under \
         DESCENT — the ratio's DENOMINATOR — every one of the {} holders on this panel \
         received exactly ONE telling. A tie-break breaks ties; on the founding tree there \
         are none. Half of The Parley's ratio is tie-break-independent by construction, not \
         by measurement.",
        sum(&rows, |r| r.choice[0][0].holders)
    );
    println!(
        "  ANTI-VACUITY: {multi_value_total} holder-observations across the whole matrix had \
         two or more distinct remembered values to choose from, and the rules moved as many \
         as {:.1}% of holders apart on a single vertex (contact x multiplicative). SO THE \
         MATRIX IS NOT VACUOUS — the tie-break is doing a great deal of work at the level of \
         the individual community, and almost none at the level of the §6.5 aggregate.",
        Accumulation::ALL
            .iter()
            .enumerate()
            .map(|(ri, _)| {
                let c = fold_choice(&rows, 1, ri);
                pct(c.rules_disagree, c.holders)
            })
            .fold(0.0f64, f64::max)
    );
    println!(
        "  RECENCY AND FREQUENCY ARE NOT SHIPPABLE AS WRITTEN. `recency` maximises hops, \
         which is unbounded around a seam cycle — it is defined here only because this probe \
         restricted routes to simple paths. `frequency` and `frequency-w` need the whole \
         arriving multiset, which a best-first relaxation discards. `primacy` is the only \
         alternative that is an argmin of a monotone key and could ship unchanged."
    );

    // =====================================================================
    // POSITIVE CONTROLS ONLY. No assertion below is about an outcome.
    // =====================================================================
    assert!(!rows.is_empty(), "control: the panel produced no seeds");
    assert!(
        skipped.len() < PANEL.len(),
        "control: every panel seed was skipped for want of a year rung"
    );

    // (1) THE INSTRUMENT IS THE SHIPPED INSTRUMENT. Asserted on every ending,
    // every accumulation rule and both arms, not sampled — a reimplementation
    // that drifted would make every number above unattributable.
    let shipped_absent = sum(&rows, |r| r.shipped_absent);
    assert_eq!(
        shipped_absent, 0,
        "control: the shipped walk's own answer (hops, rung, remembered day) must appear \
         SOMEWHERE in the enumerated candidate set, on every holder, under BOTH Contact arms \
         and all three accumulation rules — that is what proves the enumeration is a superset \
         of the route the shipped walk took; it was absent on {shipped_absent} holders"
    );
    let descent_not_argmin = sum(&rows, |r| r.not_argmin[0]);
    assert_eq!(
        descent_not_argmin, 0,
        "control: on the founding tree every holder receives exactly one telling, so the \
         enumeration's argmin IS the shipped answer there — this is the exact equivalence \
         control, and it is what establishes that this file's per-step arithmetic is \
         derive.rs's; it failed on {descent_not_argmin} holders"
    );
    let holder_set_mismatch = sum(&rows, |r| r.holder_set_mismatch);
    assert_eq!(
        holder_set_mismatch, 0,
        "control: the enumeration must reach exactly the shipped walk's holder set — a \
         selection rule reorders tellings and can never change WHO holds; \
         {holder_set_mismatch} endings differed"
    );

    // (2) THE TWO ARMS WERE SCORED ON THE SAME ENDINGS. A foreign ending
    // whose CONTACT enumeration exhausts `MAX_EXPANSIONS` contributes to
    // neither arm of `div_matched`, so the ratio the verdict table divides is
    // like-for-like by construction — and this asserts that construction
    // rather than trusting it. (`capped_total` is REPORTED above; it is not
    // asserted to zero, because dropping those endings from both arms is the
    // designed handling, not a failure.)
    for (ri, rule) in Accumulation::ALL.iter().enumerate() {
        for (si, sel) in Selection::ALL.iter().enumerate() {
            let d = fold_div(&rows, true, 0, ri, si).compared;
            let c = fold_div(&rows, true, 1, ri, si).compared;
            assert_eq!(
                d,
                c,
                "control: the matched population must score both arms on the same endings, \
                 or the {} x {} ratio is not like-for-like: descent {d}, contact {c}",
                sel.label(),
                rule.label()
            );
        }
    }
    assert!(
        capped_total > 0 || sum(&rows, |r| r.unmatched_endings) == 0,
        "control: an unmatched ending can only come from a capped enumeration"
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
    let descent_mutex = fold_div(&rows, false, 0, multiplicative, 0).mutually_exclusive;
    assert_eq!(
        descent_mutex, BASELINE_MUTUALLY_EXCLUSIVE_12,
        "control: descent x multiplicative x least-damage must re-derive \
         parley_readout.rs's pinned mutually-exclusive count exactly"
    );
    assert!(
        seam_edges > 0,
        "control: the panel must carry seam edges, or the contact arm is the descent arm"
    );
    let compared = fold_div(&rows, true, 1, multiplicative, 0).compared;
    assert!(
        compared > 0,
        "control: the panel must carry cross-people endings where both sides hold"
    );

    // (4) THE ENUMERATOR FOUND MORE THAN ONE ROUTE TO SOMEBODY. This is an
    // INSTRUMENT control, not the S0 finding: it proves the candidate-set
    // machinery fired at all. Whether those extra routes carry different
    // VALUES is exactly what S0 reports and is deliberately not asserted.
    let contact_multiplicative = fold_choice(&rows, 1, multiplicative);
    assert!(
        contact_multiplicative.routes > contact_multiplicative.holders,
        "control: the enumerator must find more arriving routes than holders under contact, \
         or it never enumerated an alternative at all: {} routes for {} holders",
        contact_multiplicative.routes,
        contact_multiplicative.holders
    );
    assert!(
        contact_multiplicative.multi_telling > 0,
        "control: some holder must be reached by two or more DISTINCT tellings, or the \
         selection rules were never handed a set with more than one member"
    );
}
