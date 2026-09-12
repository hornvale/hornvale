# 0986. A technology corpus is a sixth family, and a capability can be lost

**Status:** Accepted (2026-09-11) · **Decider:** Nathan · **Relates:**
[0011](0011-studies-are-data-metrics-are-code.md),
[0016](0016-studies-preregister-hypotheses.md),
[0095](0095-a-corpus-is-an-instrument-never-a-standard.md),
[0135](0135-a-capability-corpus-is-a-sibling-to-a-trope-corpus.md),
[0136](0136-a-coverage-verdict-cites-a-checked-anchor.md),
[0261](0261-a-rule-duplicated-on-purpose-carries-a-two-way-agreement-test.md),
[0386](0386-a-corpus-declares-its-demands-or-derives-them-never-both.md),
[0936](0936-a-grown-regularity-corpus-is-a-fourth-family-and-its-verdict-can-decay.md);
[The Kiln](../../book/src/chronicle/the-kiln.md)

In the context of scoring Hornvale against external catalogues, facing a
question none of the five existing families can put — not whether the world can
*represent*, *implement*, *parse* or *play* something, and not whether it
*produces* a macro-statistic, but whether **a people acquires, holds, and
loses** a capability — we decided that **`technologies/` opens a sixth corpus
family resolving against the per-people capability trajectory over the
committed census**, and that **its vocabulary gains one new value, `lost`,
whose scope is named in the direction the resolver enforces**, accepting a
sixth loader, resolver and report that will never share code with its
siblings.

**Ratified by Nathan at G3 on 2026-09-11**, over the counterargument below,
which was the campaign's lead flagged item at that gate.

## Why a family, not an extension

Decision 0135 opens a family when the **resolution basis** differs. `tropes/`
resolves against the concept registry, `systems/` against repository facts,
`sentences/` against the grammar, `repertory/` against a real `possess`
snapshot, `regularities/` against a measurement over the committed census. This
family resolves against the **per-people capability trajectory** over that same
census: neither static reach nor a macro-statistic over a population.

Three things separate it from its nearest neighbour, and the first is the one
0135's test is about:

1. **The unit is a capability held by a people**, not a statistic over a
   population. A regularity is true of a world; a technology is held by some
   peoples and not others, and that difference *is* the measurement.
2. **Demands derive by transitive closure over a prerequisite lattice.**
   `regularities/` has no notion of one. Each item names the single demand it
   `introduces` and the items it `presupposes`; the demand set is computed on
   read and never written into the file (decision 0386, whose derived side this
   family takes wholly — a third shape reopens that record rather than
   extending it). Decision 0261 supplies the rest: materialising the closure
   would state one fact twice.
3. **The value `lost`**, below.

## The counterargument, preserved

This family is closest to `regularities/` and shares its verdict machinery
wholesale. **A reviewer who concludes this is regularities with extra fields is
making a defensible argument.** It was put to Nathan at G3 as the campaign's
lead flagged item and the sixth family was ratified over it.

That case is recorded here, and in the spec, and in `technologies/CLAUDE.md`,
deliberately. A ratification that erases the case against itself leaves the next
reader unable to tell a decision from an assumption — and this project has
already measured what a record that outlives its own premise does to readers
acting in good faith.

## What the family adds to the vocabulary: one value, in one field

**One `verdict` field, not two.** An earlier design carried a reach verdict and
a trajectory verdict separately; reading
`regularities/sugarscape-1996.regularity.json` withdrew it — that corpus carries
0136's five values and 0936's three in a **single** field. The vocabulary is an
implicit **pipeline**: a measured value is reachable only once reach has
succeeded, so one field loses no information and adds no structure (campaign
ledger #6).

**`lost` — measured: acquired, then given up.** The sibling families can express
degrees of *absence*, and `regularities/` a measured *miss*. None can express a
capability that was **held and released**, which is the single thing this family
exists to make expressible and the axis on which Hornvale's model is provably
silent: `tech_for` is documented *monotone in `year`, so tech only ever rises*.

**`lost` names a scope, and must say which.** A capability can be given up by
one community, by a whole people, or by every people in the world — three
different claims. **`lost` in this family means: a people that held the
capability no longer holds it**, and the resolver states that scope in the
direction it enforces. Leaving it unsaid is how `systems/` acquired the defect
0136 clause 2 was written to fix: an instrument that silently switches subject
across its own corpus, with the choice tracking whichever produced the nicer
verdict, invisible to five reviews because every anchor resolved.

**`unmeasured` carries a mechanism anchor here** (`test:` or `path:`), which
neither 0136 nor 0936 contemplated. Under the pipeline, an `unmeasured` item's
*reach* has already succeeded — *this world models this capability* — and that
half is a positive claim exactly as checkable as a `present` verdict's; only the
trajectory is unmeasured. It matters beyond two cells because `unmeasured`
carries the sharpest statement this instrument makes — *of N documented losses
Hornvale can represent K of the technologies at all, and of those K it can
represent the loss of exactly zero* — and **K is the figure a reader will
quote.** An unanchored K would be the one number in the whole instrument resting
on nobody's word but the author's, while reading as the most informative
(campaign ledger #18).

## The guards

**The two-way guard ships unexercised, and deliberately.** Once the successor
campaign makes trajectory measurable, an authored verdict and a computed verdict
must agree in **both** directions, as 0936 requires: an implementation
reddening only one direction has built half a guard, and the half it skips is
the one that lets a corpus quietly under-report the world. No trajectory verdict
in either frozen corpus is measured today, so the guard is proven against
constructed input rather than against the corpora.

**NOVELTY watches the `absent` count alone, and must pass on a fall.** This
follows 0136, whose own words are that `NOVELTY` "ratchets only the `absent`
count, not `inapplicable`'s, so this instrument's one falsification-by-count
guard still watches a single number." Both verdicts are unfalsifiable by
construction; ratcheting both would not add a second check, it would blur the
one. The baselines are the counts measured off the committed files — 35 for
`asimov-1989`, 31 for `henrich-2004-extended` — and a rise fires while a fall
passes, because every correction this campaign made moved the count **down**. A
ratchet that objected to a fall would punish exactly the repairs it exists to
invite.

The omission that produced this clause is worth recording: the campaign's plan
specified three of 0136's four conditions and its own checklist would have read
complete without the fourth. It was surfaced by an implementer declining to
build something its brief had not asked for and **reporting the gap** instead of
silently building or silently skipping it (campaign ledger #26).

**`doc:` anchors keep 0936's admissibility rule** — admissible only against a
path `docs/generated-paths.txt` gives a generator. Hand-written prose asserting
a capability is refused.

## The expressive gap this vocabulary has, and how to reopen it

**The one-field vocabulary cannot say "loss is planned but unbuilt."** This was
found by trying to use it. `MEM-8` — the artifact channel — plans precisely the
loss mechanism, in its own words: *"the knowledge does not decay — the capacity
to make more does."* It was refused as an anchor on a ground that is decisive
rather than stylistic: **`deferred` is a reach-stage verdict under the pipeline,
and `MEM-8` plans a trajectory mechanism.** A row planning the loss half cannot
discharge a demand whose *reach* has already failed — and reach has failed for
all 31 `absent` items in the collapse column, so `MEM-8` discharges none of
them.

**The gap is real, and the rule for a reader who disagrees is stated here so it
is not discovered by acting:** open a decision record extending the vocabulary.
**Do not re-anchor the column.** Anchoring a mass of `absent` items to one
generic registry row is what family law already names as turning a measurement
into a wish list, and it would do it to most of one corpus on the strength of a
single `raw` row. The instrument is not a roadmap (0095); deciding which
technologies Hornvale implements stays a human act performed *on* the column.

## A mention is not a ruling

A registry row cited as an anchor by **any** corpus in this family must be
explicitly ruled on — cited, or refused in writing — in **every other** corpus,
because decision 0095's matrix is read *across* and a row that discharges a
demand in one column while being unexamined in the other makes the comparison
between them meaningless.

The obvious implementation is a text search for the row id, and **it is wrong.**
A corpus here "named" a sibling's anchor only while *reporting what the sibling
did with it*: a grep is satisfied and nothing has been ruled on. So the law
carries a second clause — **a row is ruled on when the corpus states its own
verdict about that row for a named item, not when the row's identifier
appears** — and the mechanical check is a **floor**, with the judgement still
owed by the author (campaign ledger #21).

The general shape, which is why this is in a decision record rather than a test
comment: when a rule is about whether something was *considered*, the cheap
implementation tests whether it was *named*, and the two differ exactly where
the rule matters.

## Where this family sits in the order, and one discrepancy left uncorrected

Measured from `git log --diff-filter=A` over each directory:

```text
tropes         2026-07-31
systems        2026-08-15
sentences      2026-08-24
repertory      2026-08-30
regularities   2026-09-07
technologies   2026-09-11
```

`technologies/` is the **sixth**. `repertory/` is the fourth and carries **no
decision record opening it** — every other family has one, and that absence is
the root cause of what follows. `regularities/` is the **fifth**, and decision
0936 calls it the fourth.

**0936 was false on the day it was ratified**, not merely overtaken: it is
stamped `Accepted (2026-09-08)`, and `repertory/` had existed for nine days
under a merged chronicle whose own words are *"This campaign founds a fourth
corpus family, `repertory/`"*. **It stands uncorrected.** `docs/CLAUDE.md`'s
append-only boundary is `main`, and binds explicitly including "a correction of
something that was false the day it was written." Nothing in this campaign edits
it. The ordering is stated here instead, so a reader meeting 0936's *fourth*
beside this record's *sixth* does not find a hole where the fifth should be.

**The generalisation is the durable part.** An ordinal is a claim about a
population, asserted by a document that cannot see the population change. 0936
counted correctly among the families its author had in mind and wrongly among
the families that existed. Prefer **"a sibling family, founded by X"** to "the
Nth sibling": the first cannot rot, the second must.

## Consequence

A corpus is data and a resolver is code (0011), frozen before measurement
(0016) — made structural here, as 0936 made it, by authoring both corpora in
tasks that ran before any evaluation code existed — an instrument with declared
bias and never a standard (0095), every verdict citing an anchor the resolver
re-checks (0136).

**The cost accepted is real and permanent**, and it is the cost 0936 accepted
for the fourth: a sixth loader, a sixth resolver and a sixth report that will
never share code with their siblings, plus a standing obligation on any future
reader not to "unify" them. This family adds one cost of its own — every corpus
joining it owes a ruling on every registry row any sibling cites, and that cost
grows with the family.

Two columns were frozen rather than one, because an invention catalogue is a
progress narrative and is **structurally blind to loss** — the same assumption
the shipped model carries. Under 0095 the disagreement between the two columns
is the finding. `present` is the verdict this family is least entitled to.

**See also.** `technologies/CLAUDE.md` (family law);
`docs/audits/technology-coverage-asimov-1989.md` and
`docs/audits/technology-coverage-henrich-2004-extended.md` (the first two
columns); [the campaign
ledger](../superpowers/ledgers/2026-09-11-the-kiln.md); root `CLAUDE.md`'s
`technologies/` directory guide.
