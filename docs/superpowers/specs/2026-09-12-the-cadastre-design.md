# The Cadastre — design

**Status:** draft, awaiting G3 · **Date:** 2026-09-12 ·
**Ledger:** `docs/superpowers/ledgers/2026-09-12-the-cadastre.md`

Widen `technologies/asimov-1989` from a 41-item cluster sample of the
invention.cards catalogue to a **census of all 1,484 items**, carrying the
catalogue's own `Built on` lattice, and report the result as a rate rather
than a count.

## 1. What is actually being changed, in one sentence

A cluster sample becomes a census of the same population.

That framing is the spec's organising idea and it is not decoration. The 41
items are the union of the three story arcs invention.cards publishes; 1,484
is every item it publishes. Every hazard below is a hazard some survey
discipline has already met at exactly this transition, and the answers
transfer: **when surveillance widens, the raw count jumps and the rate does
not.**

## 2. Why now — the measured state

The Kiln froze `asimov-1989` at 41 items and scored it **35 `absent`, 6
`deferred`, 0 `present`**. Its spec §4.1 capped the corpus at 40–80 items with
an explicit argument: *"A 1,500-item column against a four-token model
produces ~1,496 identical cells — an expensive way to learn we have a clock."*

That argument was about cost, and the cost was never measured. It has been
now. A synthetic 1,484-item corpus was built from the real slug list, put in
place of `asimov-1989`, run through the real resolver, and removed (tree
restored byte-identical, `check` back to rc=0):

```
                        41 items      1,484 items     factor
report                  0.025 s       0.105 s           --
check (anchors vs repo) ~0.03 s       0.19-0.26 s       --
corpus JSON             113 KB        491 KB           4.3x
generated audit         692 lines     3,578 lines      5.2x
```

Compute is nil. The 5.2x on the artifact is not the 36x a per-item
extrapolation predicts, because `note`, `disclosure`, `statistic`, `criterion`
and `contested` are all `Option` in the deserializer — the mandatory set is
`id`, `title`, `introduces`, `presupposes`, `verdict`, `anchor`, and `anchor`
is the empty string for `absent`. Every one of those is mechanical from the
slug plus its item page.

**The spec's cap was therefore right about the clock and wrong about the
price.** §2 of this document supersedes The Kiln's §4.1 size clause for this
corpus only; it does not touch the clock finding, which stands and is this
campaign's whole reason for expecting a flat column.

### 2.1 The catalogue is stable across three independent fetches

The Kiln recorded two fetches on 2026-09-11 (150,326 bytes; 1,484 distinct
slugs after discarding the page's self-link; three arcs at 16/10/15). A third,
issued 2026-09-12 for this spec, returned **150,326 bytes and the identical
1,484 slugs** — `diff` against The Kiln's saved list is empty but for the
self-link this fetch had not yet discarded.

The third fetch also re-confirmed the arcs independently: `knights` 16,
`republic-of-letters` 10, `steam-diffusion` 15, union **41**, and `diff`
against The Kiln's saved arc union is empty. So every count that campaign
recorded reproduces a day later from a separate session.

Three fetches over two days is not a stability guarantee, and Task 1 re-fetches
under its own provenance discipline regardless. It is enough to proceed.

## 3. Scope — four acts, and this campaign runs three

Widening decomposes into four separable acts with different freeze status.
Later tasks are keyed to this decomposition.

```
                        | blind? | in scope | why
------------------------+--------+----------+---------------------------
1. enumerate            | yes    | YES      | selection-free by construction
   (1,484 slugs)        |        |          |
2. import the lattice   | yes    | YES      | the source's own edges
   (Built on)           |        |          |
3. score                | NO     | YES      | where every hazard lives
   (verdicts)           |        |          |
4. band a criterion     | NO     | NO       | FORBIDDEN -- see below
```

**Act 4 is closed to this campaign and to every future one.** Family law:
*"Any session that has read the distribution a criterion bands is disqualified
from re-banding that criterion… Band quality can only be improved before the
first measurement."* Every session now alive has read `occ-tech`'s
distribution. This campaign authors **no `criterion` and no `statistic` on any
new item**, which is consistent anyway: those fields are `None` wherever an
item carries no measurable claim, and an `absent` item carries none.

## 4. Selection and the verdict vocabulary

### 4.1 The selection rule — the whole catalogue, stated before selecting

> Every item invention.cards publishes at `/browse/`, with no filter of any
> kind. Nothing is added; nothing is removed; no item is judged for fitness
> before it is admitted.

This is **strictly more blind than the rule it replaces**. The arc rule
admitted three curated sequences someone at invention.cards chose; a census
admits the population. It also repairs a fragility The Kiln recorded and could
not fix: 41 sat one item above its own 40-item floor, and the widen branch was
provably dead because three arcs is all the catalogue publishes, so a single
arc edited upstream would have put the corpus below its floor with no
mechanical remedy.

**"Appropriate to our scenario" is the one rule forbidden here.** Filtering to
what a world of goblins, kobolds and drow plausibly does is selection keyed to
Hornvale's own code, which makes the selection the measurement. The mechanism
for *"Asimov is not owed a world"* operates per item after admission, never at
the door.

### 4.2 No item is scored `inapplicable`

Most of the catalogue is 17th–20th century science and Hornvale's
`TechHorizon` tops out at `Classical`. The tempting verdict for those items is
`inapplicable` ("the world deliberately lacks a precondition"). **This corpus
scores none, and `provenance` states the rule and its reason.**

Ledger #3 carries the full argument; the three loads it bears:

1. **Observed sibling practice.** The Kiln's spec nominated `inapplicable` for
   exactly this case and the corpus it produced scores **zero**. Precedent is
   what a corpus did, not what its spec anticipated.
2. **It asserts a design intent that does not exist.** No ratified decision
   says Hornvale has no industrial era. The four-token model is an *unbuilt*
   model, not a *declared* ceiling, and `inapplicable` would manufacture
   deliberateness out of incompleteness.
3. **It is the one unratcheted verdict.** 0136 deliberately does not ratchet
   the `inapplicable` tally. Routing 1,200 items through the single verdict
   nothing watches puts the campaign's whole mass where nothing watches it.

**Both available verdicts are self-serving, in opposite directions, and that
is why this needed deciding rather than defaulting.** Ledger #12 established
that a high `absent` count is *this family's* flattering result — the thesis
is that the model is impoverished. `inapplicable` flatters in the opposite
direction, converting "we cannot" into "we needn't." Neither can be chosen for
looking honest.

## 5. The lattice, and the gate that must pass before authoring

`presupposes` edges are the catalogue's own `Built on` links, imported whole.
Measured on the real site:

- 40-item random sample: `Built on` present on **40/40**, mean **1.40**
  edges/item, max 3. Projected full graph ~2,080 edges against the **40** the
  corpus carries today.
- BFS from 8 random seeds exhausted at **152 nodes / 201 edges, 0 cycles**,
  roots `biped` and `law-of-octaves`.

**`parse` rejects a cycle in `presupposes` as a parse error**, so acyclicity
is a precondition of the corpus existing at all, and 152 of 1,484 nodes is
evidence rather than proof.

**Task 1 gate — a branch table, not a prediction:**

- Full graph is acyclic → proceed; record node and edge counts in `provenance`.
- **A cycle is found → STOP and report.** Do not invent a tie-break mid-task.
  Breaking a cycle is a selection decision; it must be stated before it is
  applied, which means a spec amendment and a ledger entry, not an
  implementer's call.
- An item page is unreachable or has no `Built on` nav → record the item as a
  root with the fetch failure noted; do not infer an edge.

`Led to` links are **not** imported, and the reason is measured rather than
assumed. Over the 179 item pages fetched for this spec, `Led to` is the
**exact** inverse of `Built on`: 210 comparable `Built on` edges, 210 inverted
`Led to` edges, **zero** edges present in one direction and absent in the
other. So importing both would double every edge and leave the closure
unchanged. Task 1 still re-checks the two directions over the full graph and
records any disagreement it finds — a site inconsistency silently unioned into
our data would launder the source's error into ours — but imports `Built on`
only.

## 6. The cross-corpus obligation, and how it is discharged

The family's rule: *a row cited by one corpus must be ruled on by every
corpus.* Verified rather than reasoned — one synthetic `deferred` item citing
`registry:TECH-4` was added to `asimov-1989`, and auditing the **sibling**
produced:

```
error: technology coverage audit found 1 finding(s) for `henrich-2004-extended`:
registry:TECH-4 is cited by asimov-1989's item inv-probe-xyz but is never
mentioned anywhere in henrich-2004-extended ...
```

So the cost that scales is **not item count** — it is the number of distinct
`registry:` rows the widened corpus newly cites, and each one reddens a frozen
sibling until that sibling rules on it in writing.

**Discharge: one written class refusal in `henrich-2004-extended`'s
`provenance`,** naming every newly-cited row and the single demand none of
them discharges — that corpus scores loss *trajectory*, and an invention-side
reach row cannot discharge a trajectory demand. The rule's own text permits
exactly this: *"refuse it in writing — in provenance or an item's note —
naming the demand it does not discharge,"* and the check describes itself as
*"a grep-level floor, not the judgement itself."*

**This edits a frozen corpus, and that is the campaign's most contestable
act.** The defence: the re-freeze prohibition is written about *criteria and
bands* ("disqualified from re-banding"), and a written refusal that moves no
verdict is not a band. Nothing in `henrich-2004-extended`'s items, verdicts,
anchors, statistics or criteria changes. It wants a decision record of its own
(§8) because it is the first time any corpus in any family has been edited
after its freeze.

**If that defence is rejected at G3**, the fallback is act-1-and-2 only: a
frozen source manifest carrying the enumeration and the lattice with no
verdicts, which owes no sibling ruling because it cites no registry row. It is
the cheap half of this campaign and it is recorded in the ledger's follow-ups.

## 7. Reporting — the rate, and the sample kept recoverable

Two rules, both from §1's framing.

**The 41 arc items keep their ids and their arc attribution** inside the
widened corpus. 0136 clause 2 forbids an instrument silently switching what it
measures; the original 35/6/0 reading stays a comparable series precisely
because its population is still individually identifiable. The report renders
both: the census, and the arc sample within it.

**The headline is a rate, never the raw `absent` count.** `absent` will rise
from 35 to something near 1,450, and that number is 36x more impressive while
being exactly as informative — it is a denominator change. The `absent`
*fraction* is the comparable quantity, and the finding worth reporting is
whether it **moved** between sample and census. It is a real question with a
real answer either way:

- fraction unchanged → the arcs were representative; the clock finding
  generalises from 41 to the whole catalogue.
- fraction **rose** → the arcs were the catalogue's Hornvale-friendliest
  corner (they are: knights, letters, steam), and the sample flattered us.
- fraction **fell** → the arcs were unrepresentatively harsh; worth knowing
  before the successor campaign uses either column as a falsifier.

## 8. What moves in the repository

- `technologies/asimov-1989.technology.json` — 41 → 1,484 items; `provenance`
  and `frozen` restated for the census; the arc items unchanged in substance.
- `technologies/henrich-2004-extended.technology.json` — `provenance` only,
  the class refusal of §6. No item changes.
- `cli/src/technologies.rs` — `novelty_baseline("asimov-1989")` moves off 35.
  The baseline is a hardcoded function, deliberately, so moving it is a
  reviewable act in source rather than a data edit.
- `cli/tests/suite/technology_corpus.rs` — the two item-count assertions
  (currently `41`).
- `docs/audits/technology-coverage-asimov-1989.md` — regenerated; already
  declared in `docs/generated-paths.txt`.
- `technologies/CLAUDE.md` — the census selection rule and the
  no-`inapplicable` rule become family law.
- A decision record for the post-freeze sibling edit (§6).
- Chronicle entry, retrospective, freshness sweep, per project Definition of
  Done.

**This campaign is not prose-only** — it touches Rust, so it pays the full
phase ladder at merge rather than skipping `clients` and `heavy`.

## 9. Success criteria

1. `hornvale technologies check asimov-1989` and `… check
   henrich-2004-extended` both exit 0.
2. The corpus parses with 1,484 items and an acyclic lattice; the item-count
   assertions pin 1,484.
3. Every one of the 41 arc items is still present, by id, with its arc named
   in `source`.
4. The report renders both series and states the `absent` fraction for each.
5. No item carries `inapplicable`, `criterion`, or `statistic` that did not
   carry it before.
6. `make gate-commit` green; the merge passes the full ladder.

## 10. What would falsify the campaign's premise

The premise is that a census of this catalogue measures the same flat column
the sample did, more defensibly. It is falsified if the `absent` fraction
moves materially between sample and census — and **that is a finding, not a
failure.** Per project practice several campaigns ship the null as the
headline; this one should ship the *movement* as the headline if there is any.

The premise is also falsified, in a way that stops the campaign rather than
reporting it, if the full graph contains a cycle (§5).

## 11. Out of scope

- **Replacing `tech_for` with an acquisition model** — `TECH-acquisition-model`
  in the idea registry. That is the successor campaign; this one only makes
  its falsifier bigger.
- **Any criterion or band** (§3, act 4).
- **A second source.** The census is of one catalogue.
- **Reading the column as a roadmap.** Decision 0095: the corpus is an
  instrument and cannot be a backlog. A 1,484-item artifact reads much more
  like a backlog than a 41-item one, and `provenance` says so explicitly.
