# Decision ledger — The Cadastre (2026-09-12)

Campaign: widen `technologies/asimov-1989` from a 41-item cluster sample of
the invention.cards catalogue to a census of all 1,484 items, with the
catalogue's own `Built on` lattice.

Autopilot engaged (Nathan, this session). G3 and G6 are hard stops.

## Entries

**#1 [G1] — Grow the corpus in place, stand up a second one, or leave it?**

*Question.* No corpus in any of the six families has ever grown after its
freeze (`git log -S'items.len(), '` over `cli/tests/suite/` returns only the
commits that first authored each assertion; `henrich-2004-extended` was born
extended, not widened). So there is no precedent either way, and the three
candidates diverge materially.

*Decision.* **Widen `asimov-1989` in place to the full catalogue**, discharge
the cross-corpus obligation the widening creates against
`henrich-2004-extended` as one written class refusal, and report a rate rather
than a raw `absent` count.

*Why.* Two measured constraints decided it, neither of which was in the
pre-pass recommendation.

1. *The cross-corpus rule makes a THIRD corpus the most expensive option, not
   the cleanest.* `cross_corpus_ruling_gaps` (`cli/src/technologies.rs:1255`)
   fires on `registry:` anchors: a row cited by one corpus must be mentioned by
   every sibling. Verified rather than reasoned — one synthetic `deferred` item
   citing `registry:TECH-4` was added to `asimov-1989` and the SIBLING was
   audited:

   ```
   error: technology coverage audit found 1 finding(s) for `henrich-2004-extended`:
   registry:TECH-4 is cited by asimov-1989's item inv-probe-xyz but is never
   mentioned anywhere in henrich-2004-extended ...
   ```

   A new corpus makes *both* existing corpora owe rulings on every row it
   cites. Widening in place makes only `henrich-2004-extended` owe them.

2. *0136 clause 2 forbids an instrument silently switching what it measures.*
   Growing the id from 41 to 1,484 items does exactly that unless the original
   sample stays individually recoverable. So the 41 arc items keep their ids
   and their `source` arc attribution inside the widened corpus, and the
   campaign reports both series.

*Alternatives discarded.*
- *Grow in place with no sample-preservation* — rejected: silently switches
  what the id measures, and destroys the comparability of the committed
  35/6/0 reading.
- *A new corpus alongside* — rejected on constraint 1 above; it is the worst
  case for the sibling-ruling cost, and the two corpora would share one source
  and one declared bias, so the matrix gains no column of its own.
- *A frozen source manifest carrying the enumeration and lattice with no
  verdicts* — genuinely attractive and rejected only on Nathan's explicit
  direction to expand the corpus. Recorded because it is the cheap fallback if
  the scoring cost overruns: acts 1 and 2 of the decomposition below are blind
  and carry no ruling obligation at all.

*Ideonomy passes / overturns.* One pass (abstraction-lift + tree-finding,
rendered as a matrix, on polarity/scope/purpose). **One partial overturn:** the
pass produced the sample-vs-census lift, which is what surfaced constraint 2
and the reporting discipline, and the four-act decomposition below, which is
what made the fallback in the last bullet nameable. The sibling-ruling cost
(constraint 1) was found by reading the resolver during the pass, not by the
pass itself.

*The decomposition the pass produced,* because later tasks are keyed to it:
widening is four separable acts with different freeze status — **enumerate**
(blind, cheap), **import the lattice** (blind, mechanical), **score** (NOT
blind; where every hazard lives), **band a criterion** (forbidden outright —
family law disqualifies any session that has read the distribution, and every
session now alive has).

*Capture actions.* Spec §3. The manifest-only fallback wants an idea-registry
row if this campaign does not land it.

---

**#2 [Q] — Does the catalogue's own lattice contain a cycle?**

*Question.* `parse` rejects a cycle in `presupposes` as a parse error. If the
real `Built on` graph has one, the corpus cannot be authored from it without a
cycle-breaking rule — and a cycle-breaking rule is a selection decision, which
must be blind and stated before it is applied.

*Decision.* **Proceed on the measurement, and make full acyclicity a Task-1
gate with a named branch if it fails.**

*Why.* Measured on a real connected closure rather than assumed: a BFS from 8
random seeds exhausted at **152 nodes / 201 edges with 0 cycles**, roots
`biped` and `law-of-octaves`. A 40-item random sample independently showed
`Built on` present on **40/40** items, mean **1.40** edges/item, max 3 — so the
lattice is dense and near-rootless, and the projected full graph is ~2,080
edges.

*What this does NOT establish.* 152 of 1,484 nodes is 10%; zero cycles there is
evidence, not proof. Task 1 must run the acyclicity check over the whole graph
before authoring, and the branch table is in the spec rather than a prediction
here: cycle found -> STOP and report, do not invent a tie-break mid-task.

*Ideonomy passes / overturns.* None; this is an empirical feasibility check
with one right answer, not a design choice.

*Capture actions.* Spec §5, Task 1's gate.

---

**#3 [Q] — `absent` or `inapplicable` for the ~1,200 post-Classical items?**

*Question.* Hornvale's `TechHorizon` has four values topping out at
`Classical`. Most of the catalogue is 17th–20th century science. Is the
100-inch telescope `absent` ("nobody's yet") or `inapplicable` ("the world
deliberately lacks a precondition")?

*Decision.* **`absent`. This corpus scores no `inapplicable` at all, and
`provenance` says so and why.**

*Why.* Three reasons, in strength order.

1. *Observed sibling practice.* The Kiln's spec §4.1 explicitly nominated
   `inapplicable` for exactly this ("Asimov is not owed a world"), and the
   corpus it produced scores **zero** of them — 35 `absent`, 6 `deferred`. The
   family has never once used the value. Precedent is what the corpus did, not
   what its spec anticipated.
2. *`inapplicable` asserts a design intent that does not exist.* It means the
   world **deliberately** lacks a precondition. No ratified decision says
   Hornvale has no industrial era; the four-token model is an unfinished model,
   not a declared ceiling. Scoring 1,200 items `inapplicable` would manufacture
   a deliberate choice out of an unbuilt one.
3. *It is the one verdict with no guard.* 0136 deliberately does not ratchet
   the `inapplicable` tally. Routing the bulk of a 1,484-item corpus through
   the single unratcheted verdict puts the campaign's whole mass where nothing
   watches it.

*The polarity trap, stated because it is the reason this entry exists.* Ledger
#12 established that a high `absent` count is **this family's flattering
result**. `inapplicable` is flattering in the opposite direction — it converts
"we cannot do this" into "we needn't." **Both available verdicts are
self-serving, in opposite directions**, so neither can be chosen on which one
looks honest. It is decided on the three reasons above instead, and it is the
lead flagged item at G3 because it is the call most worth Nathan overruling.

*Alternatives discarded.*
- *A date-keyed mechanical rule (attested after the Classical ceiling ->
  `inapplicable`)* — rejected: it equates Asimov's Earth chronology with
  Hornvale world-years, which is a category error, and it would dress a
  judgement as a derivation.

*Ideonomy passes / overturns.* One (polarity substitution — the flip is what
produced "both directions are self-serving", which is the entry's actual
finding). No overturn.

*Capture actions.* Spec §4.2; G3 flagged items.

## Follow-ups

- **The manifest-only fallback** (#1) wants an idea-registry row if this
  campaign does not land: the enumeration plus the lattice, frozen, carrying no
  verdicts and therefore owing no sibling rulings.
