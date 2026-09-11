# CLAUDE.md — working in `technologies/`

The **sixth** corpus family. Read the root `CLAUDE.md`'s directory-guide block
for the other five first; this is the technology-specific map.

## The family's question

`tropes/` asks whether a world can **represent** a situation, `systems/`
whether the program **implements** a capability, `sentences/` whether the
grammar can **produce** an utterance, `regularities/` whether the world
**grows** a macro-regularity, `repertory/` whether the world can **play** a
scene. This one asks:

> **Does a *people* acquire, hold, and lose this capability?**

The resolution basis is the per-people capability trajectory over the census —
neither static reach (the first three) nor a macro-statistic over a population
(the fourth). That difference in basis is what decision 0135 says opens a
family, and it is why this is a sixth directory rather than a field on
`regularities/`.

**The honest counterargument is kept, not erased.** This family is closest to
`regularities/` and shares its verdict machinery wholesale. What it adds is a
**unit** that is a capability held by a people rather than a statistic over a
population, **demands derived by closure over a prerequisite lattice**, which
regularities has no notion of, and the value **`lost`**. A reviewer who
concludes this is regularities with extra fields is making a defensible
argument; it was the campaign's lead flagged item at G3 and Nathan ratified the
sixth family over it. A ratification that deletes the case against itself
leaves the next reader unable to tell a decision from an assumption.

## The data/code split (decision 0011)

**The corpus is data; the resolver is code.** A corpus is
`technologies/<name>.technology.json`. The resolver is
`cli/tests/suite/technology_corpus.rs`, beside its five siblings. **Nothing in
`domains/*` or `windows/*` reads a corpus file** — the same rule that holds for
`tropes/`, `systems/`, `sentences/` and `regularities/`.

## Demands are DERIVED, never declared (decision 0386)

Each item names the **one** demand it `introduces` and the items it
`presupposes`. The demand set is the **transitive closure** over `presupposes`,
computed on read and **never written into the file**.

0386's own evidence is the argument: a hand-written demand list
under-described itself three times in a twelve-entry corpus, and a derived set
cannot under-describe because no human restates it. Decision 0261 supplies the
rest — materialising the closure would state one fact twice and need an
agreement test whose cheapest repair is deletion.

**0386 forbids both shapes in one file, and a third shape reopens that record
rather than extending it.** This family takes the derived side wholly. If you
find yourself wanting a declared demand list here, that is a decision record to
write, not an edit to make.

**`presupposes` names an item in this corpus and nothing else.** A real-world
prerequisite that is not one of the corpus's own items is dropped from the
lattice and named in the item's `note`. Do not invent an item to satisfy an
edge and do not point outside the file — the resolver reds on a dangling
reference.

## The freeze (decision 0016), made structural

A corpus is frozen **before the code that would move it exists**, which is the
discipline decision 0936 established for `regularities/` by authoring its
corpus in a task that ran before any evaluation code did. `asimov-1989` was
authored the same way: no resolver, no closure computation, no anchor
resolution and no report existed in the repository when it was committed, and
the git history is the proof rather than the file's own word.

The item count is **asserted by the resolver**, so changing a corpus is a
deliberate act.

**Re-freezing is not something a later session may do.** Any session that has
read the distribution a criterion bands is disqualified from re-banding that
criterion — and for this family the disqualifying reads are ordinary ones
(`occ-tech`'s distribution is in the census, the almanac, `windows/lot`'s prose
and the campaign spec's own findings). Band quality can only be improved
*before* the first measurement. Each corpus states its own disqualification
scope in `frozen`.

## `lost` names a scope, and must say which

`lost` is this family's one new verdict value and it owes its own
justification, exactly as 0936's two did: a sibling family can express degrees
of absence and, in regularities' case, a measured miss, but none can express a
capability that was **held and released** — the single thing this family exists
to make expressible, and the axis on which Hornvale's model is provably silent
(`tech_for` is documented "monotone in `year`, so tech only ever rises").

A capability can be given up by one community, by a whole people, or by every
people in the world. **Three different claims.** Leaving it unsaid is how
`systems/` acquired the defect 0136 clause 2 was written to fix: an instrument
that silently switches subject across its own corpus, with the choice tracking
whichever produced the nicer verdict, invisible to five reviews because every
anchor resolved.

**`lost` in this family means: a people that held the capability no longer
holds it.** The resolver's doc comment states that scope in the direction it
enforces.

## The criterion — a bare boolean is blind to divergence

**Every item carries a `statistic` and a `criterion`, and this is not optional
polish.** The pathology that opened this family is not that Hornvale fails to
acquire technologies; it is that *every surviving community acquires all of
them*. Under a bare boolean verdict, "every people has bronze" and "half the
peoples have bronze" both score `grown` — so the instrument could not see the
defect it exists to detect.

So an item's statistic is over the **distribution across peoples**, never a
boolean over the world, and the verdict is whether the criterion is met. The
precedent is already in the sibling: `sugarscape-1996` items carry a
`statistic` and a `criterion` (`{"kind": "median-in-band", ...}`) and score
against it.

**An item's reach verdict is authored as its weakest demand.** That is 0136
clause 2's discipline over *homogeneous* components — every demand asks the one
same question, "is this demand met?" Clause 2 governs two halves asking the same
question in two places and never contemplates heterogeneous components.
"Weakest" is an **authoring discipline**, not a computation: `cli/src/systems.rs`
computes no ordering either.

## The verdict vocabulary — one field

One `verdict` field, not two. The vocabulary is an implicit **pipeline**: a
measured value is reachable only if reach already succeeded, so one field loses
no information.

| verdict | meaning | anchor |
| --- | --- | --- |
| `present` | every derived demand is met | mechanism (`test:` / `path:`) |
| `refused` | a ratified decision declines it | `decision:NNNN`, checked in-force |
| `deferred` | planned, unbuilt | `registry:<row-id>`, not `shipped` |
| `absent` | nobody's yet | none — the honest red |
| `inapplicable` | the world deliberately lacks a precondition | `reason:` prose |
| `grown` | measured: a people acquires and keeps it | `doc:` |
| `flat` | measured: never acquired | `doc:` |
| `lost` | measured: acquired, then given up | `doc:` |
| `unmeasured` | frozen, not yet scored | none; tallied separately |

**A consequence of the pipeline that surprises people:** a corpus none of whose
items reaches `present` carries **zero** `unmeasured` as well as zero measured
values. `unmeasured` is for an item whose reach *passes* and whose trajectory is
not yet scored. Authoring one to make a column look complete asserts a reach
success that does not exist.

`doc:` anchors are admissible **only against a path
`docs/generated-paths.txt` gives a generator** (0936's rule). Hand-written
prose is refused.

## Non-blind items — `disclosure` marks PER-ITEM non-blindness

**Ratified by campaign ledger #12, which rejected the opposite rule; read that
entry before changing this section.** `disclosure` means the same thing here as
in `regularities/sugarscape-1996`, where the field marks the one item whose
statistic had been looked at before authoring. So:

> **An item whose verdict turned on having read the model owes a
> `disclosure` — `absent` included.**

**The rejected rule, recorded because the next author will reach for it.** The
first corpus in this family shipped with "`disclosure` is carried by every item
whose verdict is not `absent`", on the reasoning that `absent` is the default
that cites nothing, so model knowledge cannot have manufactured it. That
rationale assumes `absent` is the *conservative* direction. **For this family it
is the flattering one**: the thesis these corpora test is that Hornvale's
technology model is impoverished, so a high `absent` count is the result that
confirms the author's expectation, and an item that is really `deferred` but
scored `absent` is the self-serving error. A rule exempting that direction from
disclosure exempts the only direction that needed it — and it produced an
instance immediately (a root scored `absent` while an idea-registry row named its
demand, with no disclosure inviting anyone to audit the search).

**Corpus-level non-blindness goes in `provenance`, not onto every item.** That is
a fact about the whole instrument, and copying it onto N items states one fact N
times, which decision 0261 forbids. The two levels are different instruments for
different facts; keep both.

**The workable cut, and it is enforceable:** a verdict is either **chosen** or
**inherited**. Chosen verdicts carry a disclosure — every **root** (no
`presupposes` edge, so nothing upstream forces it), plus every non-root where a
specific candidate anchor or a non-`absent` verdict was **considered and
refused**. A non-root whose verdict simply follows the weakest-demand rule from
an `absent` prerequisite carries none, because the lattice decided it and no
Hornvale fact did. **Roots are the class the original defect came out of, so
assert that every root carries one.**

## Two rules for scoring `deferred`, both learned the hard way

**1. Sweep the whole `absent` column against every candidate row, not only the
rows you happened to find.** `absent` is the cheap verdict and it is the one
that needs the search. Checking `BIO-animal-domestication` while never checking
`BIO-8` — whose first two words are "Sharpens [[BIO-8]]" — is how one root sat
`absent` with a row naming its demand.

**2. A row that plans a PREREQUISITE of an item's demand does not make the item
`deferred`.** The row must name the capability the item's own `introduces` token
names. Demands are derived by closure, so an item whose prerequisite becomes
planned still has its own demand unplanned, and its verdict is its weakest
demand. A row naming "metallurgy yields tools" does not discharge an item whose
demand is a load-bearing fitting and whose metal is a *dropped* prerequisite.

**Breadth is not a refusal.** A row broader than the item's demand can still
name it; if you accept one broad row you may not refuse another for breadth
alone. And a row that names a capability as an **input it assumes** genuinely
does not plan it — but test that reading before using it: if one of the row's
other enumerated elements has already *shipped* as a deliverable, the row
enumerates deliverables and the refusal fails.

## A frozen band over an undefined statistic freezes nothing

The band is the easy half. **A corpus must define, in `provenance`, every degree
of freedom the statistic leaves open**, or the session that scores it later holds
exactly the discretion the freeze exists to remove. For a per-people statistic
that is at least:

- **Aggregation.** `tech` and `tech_offset` are fields on a **community**, not a
  people, and the census reports them per **occupation**. A people's communities
  can sit on different rungs, so "this people holds it" needs a rule —
  any / all / latest / at-closure — and the rule must be chosen for what it does
  to `lost`, not for convenience.
- **Evaluation instant.** `peoples-placed` and `peoples-alive-at-bake-end` are
  both census columns and give **different denominators**.
- **The N the band's stated meaning depends on.** "At least one holds it and at
  least one lacks it" is what a `[lo, hi]` fraction band means only while
  `1/N >= lo`. State the N, measured, or express the criterion as a count —
  otherwise the claim becomes false silently as worlds grow.

**And keep the unit straight in the argument as well as in the field.** Evidence
gathered per *community* does not establish a per-*peoples* fraction without an
explicit step; write the step down.

## The corpus is an instrument, never a roadmap (decision 0095)

**The report must not be readable as a backlog.** The Repertoire's one Critical
finding was an artifact that "listed seven capabilities the world already had
under a heading reading *missing*", and the failure mode here is the mirror of
it: anchoring a mass of `absent` items to one generic idea-registry row turns a
measurement into a wish list. Deciding which technologies Hornvale implements
remains a human act performed **on** the column, never an output of it.

`present` is the verdict this family is least entitled to.
