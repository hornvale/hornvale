# The Kiln — a technology corpus, frozen before the model that would move it

**Status:** approved at G3 (Nathan, 2026-09-11) · **Date:** 2026-09-11 · **Campaign:** The Kiln
· **Ledger:** [`docs/superpowers/ledgers/2026-09-11-the-kiln.md`](../ledgers/2026-09-11-the-kiln.md)

**Decision block: 0986–0995** (reserved 2026-09-11; main ceiling was 0958).
Minted: 0986. Declared here late, at Task 8, because this spec originally
declared none: the campaign was about to mint **0937** on the strength of the
highest record visible in its own branch (0936), and 0937 belongs to
**the-seedbed**'s reserved 0936–0945. `decision_blocks_do_not_overlap_across_campaigns`
is blind to a record minted with no declaration at all — its own doc says so —
so nothing would have objected.

## 1. Why

Hornvale ships a technology model. It was never ratified, it is a clock, and
it cannot express loss. Seven findings, each a command-and-output pair taken
on 2026-09-11 and recorded in full in the ledger:

| # | finding |
|---|---|
| F1 | No decision record covers the four-rung horizon. Its entire rationale is §5.3 of [the Living Community spec](2026-07-20-the-living-community-design.md) — one paragraph, fact #7 of 12, in a campaign about *ruins*. |
| F2 | `tech_for(year)` (`windows/worldgen/src/history_bake.rs:2900`) is a clock: Neolithic < 400, Bronze < 900, Iron < 1400, else Classical. Documented "monotone in `year`, so tech only ever rises" — **loss is inexpressible by construction**. |
| F3 | §5.3 justified *committing* the horizon because "a people's trajectory is globally dependent (contact, displacement)". `tech_offset` is drawn once at genesis (`history_bake.rs:6577`), inherited down the lineage, and **never mutated**. The spec argued for a mechanism nobody built. |
| F4 | Rungs at 400/900/1400, offsets uniform on [0,300], `end_year: 2000.0` ⇒ every survivor is Classical. Committed gallery: **10 `classical`, 1 `bronze-working`**. The horizon dates the dead. |
| F5 | `tech_weight` = 1.0/1.5/2.25/3.0, multiplied into raiding strength — the "progress scalar … a civilisation with a *level*" that `book/src/frontier/frontier.md:1915` rejects by name. |
| F6 | `occ-tech` is load-bearing: raiding strength, `delve_depth_m`, residue structures, almanac and `windows/lot` prose, four census metrics. Changing its semantics is an **epoch**. |
| F7 | There are **five** corpus families, not the four `CLAUDE.md` lists; `repertory/` is missing from the directory guides. |

The gap between F5 and the frontier essay is the campaign's real subject: the
doctrine says *capability thresholds, never a tech tree*, and the code is a
tech tree's scalar with none of a graph's structure.

## 2. Scope

**This campaign ships corpora and a resolver. It does not touch
`tech_for`.** The model replacement is a successor campaign (ledger #1).

The order is not convenience. Decision 0016 makes a freeze meaningful only
if it precedes the code that would move it, and decision 0936 made that
freeze *structural* by authoring its corpus in a task that ran before any
evaluation code existed. F2–F4 guarantee the first score is a genuine
preregistered miss rather than a retrofit.

**The corpus is an instrument, never a roadmap.** Decision 0095 is explicit,
and The Repertoire's own review history is the warning: its one Critical
finding was an artifact that "listed seven capabilities the world already had
under a heading reading *missing*." The report must not be readable as a
backlog. Deciding which technologies Hornvale implements remains a human act
performed *on* the column, not an output of it.

## 3. Family

Decision 0135 opens a family when the **resolution basis** differs. The
existing five:

| family | asks | resolves against |
|---|---|---|
| `tropes/` | can a world *represent* a situation? | the concept registry |
| `systems/` | does the *program* implement a capability? | repository facts |
| `sentences/` | can the *grammar* produce an utterance? | `domains/language` |
| `regularities/` | does the world *grow* a macro-regularity? | measurement over the census |
| `repertory/` | can the world *play* a scene? | a real `possess` snapshot |

`technologies/` asks whether **a people acquires, holds, and loses** a
capability. The basis is the per-people capability trajectory over the
census: neither static reach nor a macro-statistic.

**The honest counterargument, stated because it is strong.** This is closest
to `regularities/`, and shares its verdict machinery wholesale (§5). What it
adds is (a) a **unit** that is a capability held by a people rather than a
statistic over a population, (b) **demands derived by closure over a
prerequisite lattice**, which regularities has no notion of, and (c) the value
`lost`. A reviewer who concludes this is regularities with extra fields is
making a defensible argument, and it was put to Nathan at G3 as the
campaign's lead flagged item.

**Ratified at G3 (Nathan, 2026-09-11): `technologies/` is a sixth family.**
This campaign therefore mints a decision record opening it, as 0135 did for
`systems/` and 0936 for `regularities/`. The counterargument above stays in
this spec deliberately — a ratification that erases the case against it leaves
the next reader unable to tell a decision from an assumption.

## 4. Two corpora, not one

An invention catalogue is a **progress narrative** — ordered by date of first
attestation, organised on the assumption that technology accumulates. That is
the same assumption F2 found in `tech_for`. **A corpus drawn from it is
structurally blind to loss.** It will never carry an item for Roman concrete
forgotten, Linear B lost, or Polynesian deep-water navigation abandoned.

So The Kiln freezes two columns (ledger #5). Decision 0095 already rules that
the output is a matrix over corpora and that *the disagreement between columns
is the finding*.

### 4.1 The invention corpus — scores reach today

- **Source.** Isaac Asimov's chronology of science and discovery, as
  presented by `invention.cards` (~1,500 illustrated items, chronological,
  with dependency chains and curated story arcs). Provenance cites Asimov as
  the catalogue and invention.cards as where it was encountered and on what
  date, with both fetches recorded.
- **Size: 40–80 items, not 1,500.** Precedent: Polti is 36,
  `wolverson-2021` is 74, `sugarscape-1996` is 45. A 1,500-item column
  against a four-token model produces ~1,496 identical cells — an expensive
  way to learn we have a clock.
- **Selection rule (ledger #7).** Stated in `provenance`, and applicable by
  someone who has never read `history_bake.rs`: items are taken from the
  catalogue's **own named story arcs and dependency chains**, not from a
  judgement about what Hornvale would score well on. A rule keyed to our own
  code would make the selection the measurement.
- **Declared bias.** A mid-20th-century Western progress narrative, ordered
  by European/Mediterranean attestation, treating discovery as cumulative and
  one-directional, under-recording non-material technique. For a world of
  goblins, kobolds and drow this is load-bearing: the catalogue will demand
  things a subterranean people would never want and omit things it would.
  *Asimov is not owed a world* — the `inapplicable(reason)` verdict exists
  for exactly this (0095).

### 4.2 The collapse corpus — scores trajectory, `unmeasured` until the successor

Technologies documented as held and then lost. Every sibling corpus is a
**named external source with a year** (`polti`, `tvtropes-2012`,
`wolverson-2021`, `sugarscape-1996`), and this one must be too.

**Anchor: Henrich (2004)**, "Demography and Cultural Evolution: How Adaptive
Cultural Processes Can Produce Maladaptive Losses — the Tasmanian Case,"
*American Antiquity* 69(2) — the canonical treatment of technology loss.
Accepted at G3, **with latitude to range more speculatively** (Nathan,
2026-09-11).

**What that latitude costs, and how it is paid.** Every sibling corpus is a
single named source, which is what lets `provenance` state one bias. A corpus
ranging past Henrich is **not** one source, so it declares a different
provenance character: Henrich supplies the *theory of loss* (a skill
distribution drifting below a self-sustaining threshold), and each item beyond
his cases **carries its own citation**, with contested cases — Roman concrete,
Greek fire, Antikythera-class gearing, Polynesian deep-water voyaging — marked
contested in the item rather than silently promoted to fact. This is a
deliberate departure from sibling convention and is named as one, because an
authored corpus that reads like a sourced one is the failure 0095 exists to
prevent. It remains externally sourced per item; what it is not is
single-sourced.

**The two corpora carry incompatible theories of what a technology is, and
that must be declared.** Asimov's chronology frames a technology as an
*individual invention* — who made it, when. Henrich frames it as a
*distribution of skill in a population* that can drift below a
self-sustaining threshold. They are not reconcilable, and under 0095 that is
the point: the disagreement is the finding. Worth stating plainly, because the
population framing is **much closer to the project's own doctrine** — TECH-1's
"capability threshold the world crosses when a computed bar is cleared" is
Henrich's shape, not Asimov's.

**This column is mostly `absent`, with `unmeasured` reserved for the items that
land the finding** — and an earlier draft of this paragraph said "entirely
`unmeasured` on freeze", which contradicted §5.2.

The contradiction was inherited: that sentence was written while the design
carried **two** verdict fields (a reach verdict and a trajectory verdict), so
the trajectory field would indeed have read `unmeasured` on every row. Ledger
#6 withdrew the two-field shape after reading `sugarscape-1996`'s actual data
and collapsed both into one **pipelined** field, where a measured value is only
reachable once reach has succeeded. §4.2 was not updated with it.

Under the ratified pipeline the collapse column scores like any other:

- A documented loss whose technology Hornvale **cannot model at all** — Roman
  concrete, Linear B literacy, Polynesian deep-water voyaging — fails reach and
  scores `absent`. Most items are here.
- A documented loss whose technology Hornvale **does** model scores
  `unmeasured`: reach passes, and the loss cannot be measured because the model
  is monotone. **`TechHorizon::Bronze` exists**, so "bronze-working, lost" is
  exactly such an item.

That second class is the campaign's sharpest statement, and the one-field
pipeline is what makes it sayable: *of N documented losses, Hornvale can
represent K of the technologies at all, and of those K it can represent the
loss of exactly zero.* "Entirely `unmeasured`" would have flattened the two
classes together and thrown away the K. That looks like nothing and is the
preregistration working; §7 requires the report to say so where a reader meets
the column, not only in this spec.

## 5. The data model

### 5.1 Shape

Following the sibling corpora exactly: `technologies/<name>.technology.json`,
carrying `corpus`, `unit`, `ordered`, `provenance`, `frozen`, `items`.

An item derives its demands rather than declaring them (**decision 0386**,
ledger #3). Each item names the one demand it `introduces` and the items it
`presupposes`; the demand set is the transitive closure, computed on read and
**never written into the file**. 0386's own evidence is the argument: a
hand-written demand list under-described itself three times in a twelve-entry
corpus, and "a derived set cannot under-describe, because no human restates
it." Decision 0261 supplies the rest — materialising the closure would state
one fact twice and need an agreement test whose cheapest repair is deletion.

0386 forbids both shapes in one file, and says a third shape **reopens that
record rather than extending it**. This corpus takes the derived side, wholly.

### 5.2 The verdict vocabulary — one field

`regularities/sugarscape-1996.regularity.json` carries a **single** `verdict`
field whose observed values are `absent`(24) `deferred`(9) `inapplicable`(6)
`grown`(3) `refused`(2) `flat`(1) — 0136's five and 0936's additions
together. The vocabulary is an implicit **pipeline**: a measured value is only
reachable if reach already succeeded, so one field loses no information
(ledger #6).

| verdict | meaning | anchor |
|---|---|---|
| `present` | every derived demand is met | mechanism (`test:` / `path:`) |
| `refused` | a ratified decision declines it | decision, checked in-force |
| `deferred` | planned, unbuilt | registry row, not `shipped` |
| `absent` | nobody's yet | none — the honest red |
| `inapplicable` | the world deliberately lacks a precondition | `reason:` prose |
| `grown` | measured: a people acquires and keeps it | `doc:` |
| `flat` | measured: never acquired | `doc:` |
| **`lost`** | **measured: acquired, then given up** | `doc:` |
| `unmeasured` | frozen, not yet scored | **mechanism** (`test:`/`path:`) for the reach half (ledger #18); a lifecycle state, tallied separately |

**`lost` names a scope, and must say which.** A technology can be given up by
one community, by a whole people, or by every people in the world — three
different claims. Leaving it unsaid is how `systems/` acquired the defect
0136 clause 2 was written to fix: an instrument that silently switches subject
across its own corpus, with the choice tracking whichever produced the nicer
verdict, invisible to five reviews because every anchor resolved. **`lost` in
this corpus means: a people that held the capability no longer holds it**, and
the resolver's doc comment states that scope in the direction it enforces.

**`lost` is the one new value and owes its own justification**, exactly as
0936 justified its two. A sibling family can express only degrees of absence
and, in regularities' case, a measured miss. Neither can express a capability
that was held and released — which is the single thing this campaign exists
to make expressible, and the axis on which Hornvale's model is provably
silent (F2).

### 5.2a The criterion — why a bare verdict is not enough

A bare `grown`/`flat` is **blind to the pathology that motivated this
campaign.** F4's finding is not that Hornvale fails to acquire technologies —
it is that *every surviving community acquires all of them*. Under a boolean
verdict, "every people has bronze" and "half the peoples have bronze" both
score `grown`, so the instrument could not see the defect it exists to detect.

The fix is precedented and already in the sibling: `sugarscape-1996` items
carry a `statistic` and a `criterion`
(`{"kind": "median-in-band", "lo": -1.2, "hi": -0.8}`), and the verdict is
whether the criterion is met. A technology item therefore carries a statistic
over the **distribution across peoples**, not a boolean over the world. Under
such a criterion today's world scores `flat` — the holding fraction is 1.0,
outside any band that expresses divergence — which is the honest reading, and
a bare boolean would have reported `grown`.

An item's reach verdict is **authored** as its weakest demand. That is 0136
clause 2's discipline over **homogeneous** components — every demand asks the
one same question, "is this demand met?" Clause 2 governs two halves asking
the same question in two places and **never contemplates heterogeneous
components**; an earlier draft claimed otherwise and is withdrawn (ledger #2).
Note that `cli/src/systems.rs` computes no ordering at all: "weakest" is an
authoring discipline there, and stays one here.

### 5.3 Non-blind items

Any item not authored blind carries a `disclosure` field, per the mechanism
sugarscape already uses verbatim for `sug-wealth-skew`. We author these
corpora knowing the model is a clock; §4.1's selection rule is the structural
mitigation, and `disclosure` is the per-item escape where it does not hold.

## 6. The resolver

Code, not data (**decision 0011**). `cli/tests/suite/technology_corpus.rs`,
beside its five siblings. Nothing in `domains/*` or `windows/*` reads a corpus
file.

Guards, each inherited rather than invented:

- **The freeze** — the item count is asserted, as
  `regularity_corpus.rs:60` asserts 45. Changing a corpus becomes a
  deliberate act (0016).
- **Anchor resolution** — 0136's four conditions: DANGLING, STALE-DEFERRED,
  UNJUSTIFIED, NOVELTY (the `absent` count ratchets). Resolution must not
  shell out to `cargo`.
- **`doc:` admissibility** — 0936's rule: admissible only against a path
  `docs/generated-paths.txt` gives a generator. Hand-written prose is refused.
- **Closure integrity** — `presupposes` names an item in this corpus; the
  lattice is acyclic; ids unique.
- **The two-way guard** (0936) — once the successor campaign makes
  trajectory measurable, an authored verdict and a computed verdict must agree
  in **both** directions. An implementation reddening only one direction has
  built half a guard. Not exercised by The Kiln, because every trajectory
  verdict is `unmeasured`; the guard ships anyway, with a test proving it
  reddens both ways against constructed input rather than against the corpus.

**Diagnosability is part of the design** (0136): a DANGLING or STALE-DEFERRED
failure names the anchor, the item citing it, the kind of change that caused
it, and the two legitimate repairs.

## 7. The report

`docs/audits/technology-coverage-<corpus>.md`, one per corpus, drift-checked
via `docs/generated-paths.txt` — which must also gain the corpus directory
itself, `git add`-ed in the same commit (`git diff --exit-code` against an
untracked path is silently vacuous).

The report prints, **before any number**: the provenance, the declared bias,
the selection rule, and the statement that `present` is the verdict the
instrument is least entitled to. The `unmeasured` tally is reported
**separately from coverage**, never folded into a percentage, with the reason
beside it.

**The demand set is a first-class output, not a by-product.** Against a clock,
the verdict column is largely predictable in advance; the derived demands —
what a world would need in order to grow each technology — are the part that
carries information on day one, and they are the specification the successor
campaign is built against.

## 8. Deliverables

1. `technologies/` with two frozen corpora and a directory `CLAUDE.md`.
2. `cli/tests/suite/technology_corpus.rs` — the resolver and its guards.
3. Two committed reports; `docs/generated-paths.txt` updated.
4. A decision record opening the family (or ruling it an extension of
   `regularities/`, per §3).
5. Idea-registry rows: the successor model campaign, and both corpora. **No
   existing row proposes a technology corpus** — checked; 45 rows mention
   "corpus", none for technology, invention or craft.
6. Chronicle entry, retrospective, Confidence Gradient re-score if a bet moves.
7. `CLAUDE.md` directory-guides fix for F7 (`repertory/` unlisted).

## 9. Accepted costs

- A column whose `unmeasured` rows stay `unmeasured` until a later campaign —
  the rows that matter most, and the ones that look emptiest.
- A sixth loader, resolver and report that will never share code with its
  siblings — 0936 accepted exactly this for the fourth.
- Two corpora authored by people who know the model, mitigated by §4.1 and
  §5.3 but not eliminated.
- The corpora bind future registry rows: a `deferred` verdict makes a row's
  ID and status load-bearing for a committed artifact outside the registry's
  own drift check (0136's accepted cost, inherited).

## 10. Out of scope

Replacing `tech_for`; touching `occ-tech`, `tech_weight` or `delve_depth_m`;
any epoch. All belong to the successor campaign, which this corpus exists to
give something to falsify against.
