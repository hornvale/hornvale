# The Touchstone — design

**Program:** Myth, campaign 6 · **Drafted:** 2026-08-19 · **Status:** spec, at G3

A touchstone is the assayer's stone: rub a metal against it and the streak
tells true gold from fool's gold. This campaign builds the myth thread's
touchstone — an instrument that can tell a **working** transmission mechanism
from an **inert** one, which the divergence aggregate provably cannot.

## 1. The problem: the aggregate is nearly blind, for three compounding reasons

The myth thread (`windows/hearsay/`) derives, from committed facts alone, what
every holder believes about each event: a `Claim { holder, subject, predicate,
object, grade, hops, precision }`. `object` is the remembered day, `precision`
the rung it is remembered at, and the account is *selected* per holder by a
best-first relaxation whose ordering key is `(width, hops, witness)`
(`derive.rs:472`).

The thread reports its results through one panel-level number — the
**divergence aggregate**, `DivRow::mutually_exclusive`
(`undertow_readout.rs:624`): a count of cross-people endings on which the
victim's people and the raider's people each hold a remembered day the other
holds *nowhere*, out of `compared` such endings (421 on the 40-seed panel,
~100 on the 12-seed prefix).

Three campaigns have now watched a mechanism rewrite what a large fraction of
individuals believe while this aggregate barely twitches
(`KNOW-selection-aggregate-dissociation`, idea-registry:1213):

- Swapping the **selection rule** moves the held telling at **up to 41.9% of
  holders** and the aggregate by **≤4 of 100** (`probe_tiebreak_rules.rs`;
  figures at `undertow_readout.rs:1653`).
- The **crossing penalty** changes the held telling at 0.10–0.25% of 1,198,577
  held tellings and moves the aggregate by **0, 0, +2 of 421** across the three
  accumulation rules (`undertow_readout.rs:1681`).

Seen once, that is a result about a mechanism; seen three times, it is a
property of the **instrument**. The aggregate is blind for three reasons that
compound, and the instrument this campaign builds inverts each:

| why the aggregate is blind | what the instrument does instead |
|---|---|
| **homogeneity** — one scalar per panel; a count over a population cancels the movers, who are a *tail* (the Undertow scale-probe lesson: a ratio of medians cannot see a tail) | reports a **distribution** over holders with explicit tail mass, never a single count |
| **decomposability** — it fuses everything into one cross-people day-set question | reports a **per-component belief-delta**: which of {route, remembered-day, rung, hops, width} moved |
| **materiality** — it reads only the *emitted* disagreement between two peoples' day-sets | reads **process/route-level churn** — *which witness a holder now believes through* — which changes with no change to the emitted day-set at all |

The third is the sharpest and the least visible. A holder's remembered day is
`object = ladder.apply(precision, day)`, and `ladder` is the *originating
witness's people's* ladder (`derive.rs:512`). So a change that flips which
route wins can re-floor a holder's remembered day **at an unchanged rung
index**, because two peoples' ladders assign different spans to the same rung.
Route identity is load-bearing even for the day channel — and route identity
is exactly what the shipped walk throws away.

## 2. The enabling fact: the measure is half-built, in a test file

The route is not lost information — it is *discarded* information.
`variants_about_accumulating` carries the winning witness as the third element
of its ordering key through the whole relaxation, then drops it:
`reached.into_values().map(|t| t.claim)` (`derive.rs:597`). The remembered
day, rung, hops and grade survive in the returned `Claim`; the **witness/route**
and the continuous **width** do not.

Because of that, `undertow_readout.rs` ships a **private copy of the entire
walk** (`undertow_readout.rs:16-35` says so in its own words), whose richer
record `Held { claim, width, crossings: Vec<Crossed>, seam_steps }`
(`undertow_readout.rs:263`) carries the route the library dropped. It already
computes route-churn between two arms — `ChangeRow.cross_route_changed`
(`undertow_readout.rs:1012`) — and pays a standing tax for it: a heavy-battery
control that asserts the copied walk matches the shipped walk holder-for-holder,
forever.

So the myth thread's needed measure is **half-built, buried in a test battery,
and duplicating determinism-critical code**. This campaign promotes it into the
library as first-class, unit-tested code, and preregisters the demonstration
that it does what the aggregate cannot.

## 3. The instrument: a per-holder belief-delta between two arms

The touchstone takes a world, an event population, and two transmission
configurations — a **baseline** arm A and a **changed** arm B — and reports,
for every (holder, event) reached under both, which components of the held
telling changed:

```
BeliefDelta per (holder, event):
  route_changed   : bool   -- winning witness / crossing sequence differs
  day_changed     : bool   -- remembered day (object) differs, bit-exact
  rung_changed    : bool   -- precision index differs
  hops_changed    : bool   -- depth differs
  width_changed   : bool   -- accumulated damage width differs, bit-exact
```

These roll up into a **distribution over holders**, reported with:

- **tail mass** — the count and fraction of holders for whom *each* component
  changed (the movers are the population of interest; there is no mean to hide
  them behind);
- a **people-pair cut** — the same counts keyed on the pair of peoples the
  holder's account spans, because a myth effect concentrates on a handful of
  people-pairs and cutting on the *event* population would report their
  behaviour as the world's (the Undertow ruling; `the-undertow.md`).

The registry row's candidate observable — *day-moved exceeds rung-moved (227
vs 214)* — is one cell of this: `day_changed` fires more often than
`rung_changed` because a re-floored day at an unchanged rung is exactly the
route-driven ladder effect of §1. The instrument reports the whole vector, not
that one cell.

### 3.1 What lands in the library

- A **traced walk** in `windows/hearsay/src/` returning `Vec<HeldTelling>`,
  where `HeldTelling { claim: Claim, witness: EntityId, width: f64,
  crossings: Vec<Crossed> }` carries the route and width the shipped walk
  drops. It shares the relaxation logic; the shipped `variants_about_accumulating`
  is left **byte-identical** (§6).
- A **belief-delta** module computing `BeliefDelta` and its roll-ups
  (tail counts, people-pair cut) from two arms' `Vec<HeldTelling>`.
- Unit tests for each, proven by **mutation** (§5), not by assertion alone.

## 4. Preregistration: the discrimination the touchstone must pass

Frozen here, before the measurement code exists (decision 0016; the study JSON
has no hypothesis field, so the freeze lives in this spec). The touchstone is
**valid iff it separates a change that rewrites beliefs from one that does
not**, on the same panel where the aggregate reads ~0 for both.

Define, for a pair of arms, **`changed_tail`** = the fraction of (holder,
event) pairs reached under both arms for which **any** tracked component
{route, day, rung, hops, width} differs.

**Both controls are FROZEN below, with reachability evidence re-derived on
this tree** (Task 1, `windows/hearsay/tests/touchstone_controls_probe.rs`, the
12-seed census-prefix panel; run
`cargo nextest run -p hornvale-hearsay --run-ignored all -E
'test(touchstone_controls_probe)'`). No instrument exists yet: the probe
hand-rolls a `Claim`-inequality diff, so nothing here was tuned by the thing it
will judge.

**Positive control — a working mechanism the aggregate misses.** The
**selection-rule (ordering-key) swap** of `probe_tiebreak_rules.rs`, under
`(Contact::WithRaidSeam, Accumulation::Multiplicative)` — the aggregate's own
arm and rule:

- **Arm A (baseline)** — today's shipped selection: smallest final width →
  fewest hops → witness, i.e. the shipped walk `variants_about_accumulating`
  itself (`derive.rs:472`; the ordering key `least_damage_key`,
  `probe_tiebreak_rules.rs:347`).
- **Arm B (changed)** — the **`Selection::Recency`** rule (most hops, then
  least damage; `probe_tiebreak_rules.rs:289`), applied to the candidate set
  the seam delivers to each holder (`enumerate`, `probe_tiebreak_rules.rs:509`;
  `select`, `:362`).

Re-derived on this tree over 3,177 (holder, foreign-ending) pairs: arm B
**rewrites the held telling (`Claim`-diff: remembered day, rung, or hops
differs) at 62.64% of holders** (1,990 / 3,177), while the divergence aggregate
`mutually_exclusive` moves **from 10 to 14 — a delta of +4 of ~102**. The
day-only value-change rate is 38.37% (1,219). Prediction: **`positive_tail ≥
20%`** — a floor the 62.64% held-telling churn clears by 3×, an order of
magnitude above the aggregate's ≤4/~100 ≈ 4%. The touchstone will legitimately
report **more** than the `Claim`-diff rate, because `route` and `width` live
*outside* the `Claim`; that is the instrument seeing what a `Claim`-diff cannot,
not over-counting.

Two findings recorded with the freeze, neither a rescue (no floor was
lowered):

1. **The exact prior 41.9% *value*-change signature does not reproduce on this
   tree** — the strongest value-change here is recency's 38.37%, under 40%. The
   substrate has moved (settlement placement, per the Undertow/Underworld) since
   that figure was measured. The dissociation **property** — a selection swap
   rewriting a large fraction of held tellings while the aggregate barely
   twitches — reproduces robustly; the exact magnitude does not.
2. **The positive control requires an *enumeration-based* selection rule.**
   `Selection::Primacy` — the one alternative expressible as a shipped-shape
   relaxation — rewrites only 19.86% of `Claim`s (below the 20% floor), so it is
   not a viable positive control. Recency and frequency (31.35%) clear the floor
   but need the whole arriving candidate set, so **arm B is produced by the
   enumeration, exactly as `probe_tiebreak_rules.rs` does**, not by a pure
   relaxation. Tasks 3–4 must feed arm B that way.

**Negative control — a genuinely inert change, provably zero by a theorem.**
`(Contact::Descent, Crossing::Free)` vs `(Contact::Descent,
Crossing::ContactWeighted)`, on the **sub-population of holders whose entire
ancestry shares one people** ("people-homogeneous-ancestry" holders).

- **The theorem.** Under `Contact::Descent` the only route is descent (the seam
  is never consulted). For a holder whose whole ancestry chain is one people,
  every descent step has `people_of(teller) == people_of(hearer)`, so
  `crossing_penalty` returns 0 on every step — its `from == to` early-return
  guard (`derive.rs:124`) — under **both** `Crossing::Free` and
  `Crossing::ContactWeighted`. Identical width ⇒ identical rung, remembered day,
  hops, and route ⇒ the two configs produce **bit-identical `Claim`s** for every
  such holder. This is a code theorem, not a measurement.
- **Non-vacuous.** On the panel's 102 foreign endings, **2,014 holders have
  people-homogeneous ancestry**, and the probe asserts their claims bit-identical
  across the two `Crossing` arms (churn = 0). The count is > 0, so the control is
  reachable.
- **Mutation-proven.** Deleting the `from == to` guard reddens the theorem
  fixture (`touchstone_controls_probe_negative_theorem`): `ContactWeighted` then
  charges a full finest rung on every same-people step
  (`edges_between(p, p) == 0`, so the penalty is `span(FINEST) / (1 + 0)`),
  moving the rung and remembered day. Verified by running the mutation; see the
  Task 1 report.

Prediction: **`negative_tail ≤ 1%`** — exactly 0 on the provable sub-population,
the ≤1% being slack for the measurement framing. The identity change (arm A =
arm B) gives zero by *construction* and is retained only as a mutation-floor
sanity check, **not** as this scientific negative. The negative control is what
supplies the ceiling the positive floor needs: an instrument wired to fire on
everything would redden the negative, so a passing negative is the evidence that
a high positive is signal rather than a constant.

**Success criterion (the headline result).** The touchstone passes iff
`positive_tail ≥ 20%` **and** `negative_tail ≤ 1%`, giving a separation margin
of at least 20× where the divergence aggregate separates the two by ≤4/~100 in
absolute count — i.e. does not separate them at all. A falsified prediction is
a finding, not a failure: if the positive control does **not** move the
touchstone, the thread's dissociation is deeper than a missing decomposition
and that is the headline instead. Nothing is retuned to rescue the prediction
(verified by commit ordering at close).

## 5. Validation discipline (the retros' standing lessons, made concrete)

Every one of these has cost a prior Myth campaign turns; each is a task-level
requirement here.

- **Every measure is proven by a mutation, not an assertion.** A test that
  neutralises the component-change flag must redden; a counter wired to a
  constant still prints as a finding (the Parley deferral). Assert the target
  text exists before mutating it.
- **The positive control must be reachable; the negative must be non-vacuous.**
  See the pre-freeze task in §4.
- **The traced walk must agree with the shipped walk holder-for-holder**, as a
  library test — the guard the readout pays today, moved into the library
  where it belongs (§6).
- **Re-derive, never transcribe; re-measure on the merge product.** Every
  published figure is produced by an assertion failing on this tree, not copied
  from `main`. Absorb `main` at every plan-stage boundary — the Undertow ran
  137 commits without absorbing and a textually-clean merge hid a moved
  substrate that invalidated every number it published.
- **Imperatives hide assertions.** Plan steps state the *property* a mutation
  must demonstrate and let the implementer find one; they do not prescribe a
  specific mutation from outside the code, and they write decision-rule branch
  tables, not predicted outcomes.

## 6. Layering, cost, and what this campaign does *not* touch

- **Window-only.** Everything new lives under `windows/hearsay/`. Gate cost is
  the windows tier, not the 470.8 s kernel tier.
- **No epoch, no save-format contract.** `Claim` is `#[derive(Clone, Debug,
  PartialEq)]` and explicitly never `Serialize` (decision 0100 rule 5;
  `kernel/src/claim.rs:12`). `HeldTelling` is a new *window* record, not a
  kernel change, and this crate draws nothing and commits nothing. No stream
  label, no stream-order slot, no epoch suffix.
- **The pinned walk stays byte-identical.** The traced walk is a sibling; the
  shipped `variants_about_accumulating` keeps its exact behaviour, guarded by
  the agreement test and by every existing campaign baseline. **Full
  unification** (making the shipped walk a projection of the traced one) is a
  cleaner end state but refactors determinism-critical pinned code, and
  bundling that into a measurement campaign is the substrate-moved scope-creep
  the retros warn against — it is **deferred** to a `TOOL-` row.
- **Not a census metric.** Registering a lab metric runs it on ~2,000 census
  worlds forever (nine studies declare `"metrics": "all"`), which four prior
  campaigns' authors learned the hard way. The touchstone ships as a **heavy
  battery** with re-derivable asserted numbers, like its predecessors — a
  40-seed panel (~2.3 s/seed ≈ 90–115 s, matching the Undertow/Parley
  batteries), `#[ignore]`d into the heavy tier, off the commit gate.
- **No generated artifact should drift.** Believed, and to be *verified* at
  implementation time by Parley's call-site method: one grep for this crate's
  per-holder output under `windows/lab/` and the census extractors, reading
  down the call chain, before claiming an empty drift.

## 7. Non-goals (what this campaign is *not*)

- **Not a mechanism campaign.** It builds the measure; it does not add a new
  transmission arm. The thread keeps building mechanisms its own measure cannot
  see — this campaign fixes the measure so the *next* mechanism campaign can be
  believed.
- **Not `KNOW-derived-vs-constant-penalty`.** That question (does a derived
  crossing magnitude do anything a constant would not) presupposes an
  instrument that can see the penalty at all — which is what §1 says the
  aggregate cannot. It is the natural *next* campaign, and the touchstone is
  its precondition.
- **Not `KNOW-misattribution-drift`.** The thread's eventual destination
  (0021's richest use), and another mechanism — downstream of a working measure.
- **Not the unit erratum.** Left frozen for a session that has not read the
  Palimpsest exploratory column; this session has not opened those files.

## 8. Decisions taken in this spec (promote to the durable record at merge)

1. The measure is **promoted into the library**, not added as more test-file
   scaffolding — "studies are data, metrics are code" (decision 0011).
2. A **traced sibling walk**, not walk unification; unification deferred.
3. Controls **preregistered and FROZEN** in §4 with a numeric success
   criterion, both re-derived on this tree (Task 1). The negative control's
   pre-freeze task is **discharged**: `(Descent, Free)` vs
   `(Descent, ContactWeighted)` on people-homogeneous-ancestry holders, provably
   zero by the `crossing_penalty` `from == to` theorem, non-empty (2,014
   holders), mutation-proven. The positive control is the `Recency` selection
   swap; its exact prior 41.9% *value* magnitude did not reproduce (substrate
   drift, a recorded finding), while the dissociation property did.
4. Delivery as a **heavy battery**, explicitly not a census metric; no epoch,
   no kernel edit, window-only.

## 9. Open questions for review

- **Delivery form.** Heavy battery only (recommended), or additionally a
  committed, drift-checked readout artifact? The latter is the standing wish of
  the last three campaigns (numbers in prose rather than re-derived), but its
  authoring path is unverified (the Parley deferral). Recommendation: heavy
  battery for this campaign; bank the artifact as a followup.
- **Instrument population.** All holders (recommended, matching the 41.9%
  dissociation which is over all holders), with the people-pair cut as a
  secondary view — versus restricting to the cross-people population the
  aggregate itself measures. Recommendation: all holders + people-pair cut.
- **Negative control identity** — **RESOLVED in §4** (the pre-freeze task is
  discharged): `(Descent, Free)` vs `(Descent, ContactWeighted)` on
  people-homogeneous-ancestry holders, provably zero and non-vacuous (2,014
  holders on the panel). Kept in this list only as the record that it was the
  single most expensive mistake available (Parley/Undertow both made it) and was
  closed before the freeze rather than after.
