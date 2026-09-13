# CLAUDE.md — working in `technologies/`

This family is opened by **decision 0986** (*a technology corpus is a sixth
family, and a capability can be lost*), ratified by Nathan at G3, 2026-09-11.
Every rule below that cites 0011, 0016, 0095, 0135, 0136, 0386 or 0936 inherits
it through that record; 0986 is where this family's own additions are decided.

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
`technologies/<name>.technology.json`. The resolver **will be**
`cli/tests/suite/technology_corpus.rs`, to sit beside its five siblings — Task 4
writes it, and **it does not exist while a corpus is being authored** (see the
freeze, below). Every statement about the resolver in this file is therefore a
**requirement on that task**, never a description of the tree. **Nothing in
`domains/*` or `windows/*` may read a corpus file** — the same rule that holds
for `tropes/`, `systems/`, `sentences/` and `regularities/`.

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
edge and do not point outside the file — **Task 4's resolver must red** on a
dangling reference.

## A corpus is closed under its source's dependency relation (decision 0987)

**The consequence of the previous section, stated as a rule so it cannot be
rediscovered as a bug.** `presupposes` naming only in-corpus items means any
corpus authored from a *subset* of a source that itself carries a dependency
relation loses every edge crossing the subset's boundary, silently, at
authoring time — the field that would carry the missing edge does not exist
to carry it. The Kiln's own words on the resulting damage: "the derived
demand set UNDER-DESCRIBES every such item's real prerequisites." A corpus
that admits `inv-printing-press` without admitting steel says it is built on
nothing, when the source it was drawn from says otherwise.

**The rule:** a corpus drawn from a source that carries its own dependency
relation is closed under that relation, in the direction that terminates. A
corpus that cannot close states the truncation and its size in `provenance`.
For this family, that direction is `Built on` (prerequisites terminate; the
source's own `Led to` — consequences — does not, and closing under it would
reconstruct the connected catalogue rather than select a corpus from it).

**This is family law inherited from a decision that binds every corpus
family whose source is graph-structured**, not a `technologies/`-only rule —
see decision 0987. `asimov-1989` is its first application: closing the
three-arc-plus-pre-1700 seed under `Built on` grew it from 41 items / 40
edges to **301 items / 401 edges**, terminating at one root (`biped`), zero
cycles. A corpus authored here that admits a proper subset of a
graph-structured source without running this closure is not selecting a
sample — it is silently truncating a relation the corpus exists to expose,
and must instead close first or declare the truncation's size in
`provenance`.

## The freeze (decision 0016), made structural

A corpus is frozen **before the code that would move it exists**, which is the
discipline decision 0936 established for `regularities/` by authoring its
corpus in a task that ran before any evaluation code did. `asimov-1989` was
authored the same way: no resolver, no closure computation, no anchor
resolution and no report existed in the repository when it was committed, and
the git history is the proof rather than the file's own word.

**Task 4's resolver must assert the item count**, so that changing a corpus
becomes a deliberate act. Until it does, nothing holds that invariant.

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
holds it.** **Task 4's resolver must state that scope in its doc comment**, in
the direction it enforces.

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
| `unmeasured` | frozen, not yet scored: reach PASSED, trajectory unscored | mechanism (`test:` / `path:`), for the reach half; tallied separately |

**`unmeasured` CARRIES A MECHANISM ANCHOR (campaign ledger #18), AND THIS LINE
SAID THE OPPOSITE FOR A WHOLE FIX ROUND WHILE BOTH CORPORA CARRIED ONE.** The
reach half of an `unmeasured` verdict is a positive, checkable claim — *this
world models this capability* — and it is exactly as checkable as a `present`
verdict's, because under the pipeline reach has already succeeded. Only the
trajectory is unmeasured. `technologies/henrich-2004-extended` anchors both of
its `unmeasured` items at `path:domains/history/src/record.rs`, where
`TechHorizon::Bronze` and `::Iron` are defined; **two items may cite one path**,
and a resolver must accept that rather than require anchors to be distinct.
Line numbers belong in the `note`, never in the anchor, so the anchor survives
an edit above it — the sibling families' `path:` anchors carry none either.

**Why the stale row is recorded rather than quietly replaced.** Ruling #18's
capture actions named the rule's *consumers* (a decision record, a resolver) and
**no contradictor**, so the one document every author of this family is required
to read went on stating the rule's opposite. A Task 4 or Task 5 author would have
implemented "no anchor" and reddened the corpus the rule exists to score, and
would have been right to, because the law said so. **A ruling's capture action
must name every place that states what it overturns**, not only the places that
will consume it: "who needs this?" is the question that gets asked, and "who
currently says the opposite?" is the one that does not (campaign ledger #19).

**A consequence of the pipeline that surprises people:** a corpus none of whose
items reaches `present` carries **zero** `unmeasured` as well as zero measured
values. `unmeasured` is for an item whose reach *passes* and whose trajectory is
not yet scored. Authoring one to make a column look complete asserts a reach
success that does not exist.

`doc:` anchors are admissible **only against a path
`docs/generated-paths.txt` gives a generator** (0936's rule). Hand-written
prose is refused.

## No `inapplicable` without a ratified decision to cite

The table above lists `inapplicable`'s anchor as `reason:` prose. **That prose
must name a ratified decision, or the verdict is not available at all.**
`inapplicable` asserts that the world *deliberately* lacks a precondition —
and Hornvale has no living-world technology model to have made that choice
about. `TechHorizon`'s four values date **discovered ruins**
(`OccupationRecord.tech`, vestige placement, occupation fleshing — every
consumer is occupation- or vestige-side, never a living community's own
capability state), so there is no ceiling anywhere in the tree that a ratified
decision has drawn, and nothing about the model's incompleteness was decided
on purpose. An `inapplicable` verdict here would manufacture a deliberate
choice out of an unbuilt one.

**The second reason is independent of the first and would hold even if a
ceiling existed:** decision 0136 deliberately leaves `inapplicable`'s tally
unratcheted — it is the one verdict in the whole family's vocabulary that no
guard watches. Routing mass through it is routing mass to the place nothing
checks.

**Both live corpora score zero `inapplicable` items**, and this is a
precedent, not a coincidence: The Cadastre's spec explicitly nominated the
value for the ~1,200 items the shipped model has no ceiling for, and refused
it for exactly these two reasons (spec §5, §6). A future item that looks like
a candidate for `inapplicable` is `absent` — the honest red — unless and
until a decision record actually draws the ceiling it would assert.

## A row cited by ONE corpus must be ruled on by EVERY corpus

**SILENCE IN ONE COLUMN, NOT DISAGREEMENT BETWEEN TWO, IS THE DETECTABLE
SIGNAL.** A registry row cited as an anchor by **any** corpus in this family must
be explicitly ruled on — cited, or refused in writing — in **every other**
corpus. Task 5's resolver enforces it (plan Step 4b); until it does, nothing
holds it.

**The obvious check is the wrong one, and it was written and reported before it
was tested.** Fix round 1 added a harness asserting that *no registry row is
scored two ways across the matrix*. That check **cannot fire**: only `deferred`
may carry a `registry:` anchor, every per-corpus validator already asserts that,
so two columns citing one row always agree by construction. Worse, it could not
have caught the defect that motivated it: before the amendment, `MAP-8` was cited
by `henrich-2004-extended` and **absent from `asimov-1989` entirely**, so there
was no shared anchor for any agreement test to compare. The failure mode is a row
one column has never looked at, which a cross-column *comparison* is structurally
blind to — it needs two citations and the defect is one.

**A MENTION IS NOT A RULING, and a text search cannot tell them apart.** Naming a
sibling's anchor while reporting what the sibling did — "`inv-library` on `MEM-4`"
— satisfies a grep and rules on nothing. The obligation is a ruling about THIS
corpus's own demands: this row names item X's demand and is cited, or it names no
demand here and is refused with the reason. A resolver that greps for the row id
will pass on a mention, so the check is a floor and the author still owes the
judgement. **Writing this section immediately exposed two violations in the two
corpora it governs** — `asimov-1989` was silent on `MAP-18` and
`henrich-2004-extended` on `BIO-animal-domestication`, each cited by the other —
both closed in the same commit, which is the only reason this paragraph is not a
third instance of a law contradicting its data.

**Why this is family law rather than one resolver's test.** The two columns of
decision 0095's matrix are read ACROSS; a row that discharges a demand in one
column and is unexamined in the other makes the comparison between them
meaningless, and neither column can show that alone (0136 clause 2's "an
instrument that silently switches what it is measuring", across corpora instead
of within one). The cost is real and is accepted: every corpus added to this
family owes a ruling on every row any sibling cites, and that cost grows with
the family.

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

**The cut, stated exactly because a proxy for it has already failed once:** a
verdict is either **chosen** or **inherited**.

> **An item's verdict is CHOSEN when no prerequisite anywhere in its derived
> closure is `absent`.** Nothing upstream forces it, so it rests on a search of
> the repository. Otherwise it is **inherited**: the weakest-demand rule reads it
> off an `absent` prerequisite and no Hornvale fact decided it.

**Chosen items carry a `disclosure`. Inherited items do not** — marking them
would restore the noise decision 0261 and ledger #12 both refuse.

**Do not key this on roots.** An item with no `presupposes` edge at all is a
strict *subset* of chosen, and keying the check on it under-covers by exactly the
items whose prerequisites are all non-`absent`.

**Distrust a root-keyed check ALWAYS, from the first authoring — not after some
event.** An earlier draft of this section said "a re-score moves items into the
chosen set, which is precisely when the proxy diverges", and that is false. It
was checked: the chosen set was recomputed from `asimov-1989`'s own freeze
commit and it is **the identical set**, symmetric difference empty, so the
re-score that prompted ledger #13 created **zero** chosen items. The two chosen
non-roots had been chosen since the corpus was first authored, because one
`deferred` verdict was authored at the freeze and their prerequisite is it. **A
root-keyed check under-covers the moment the rule is written**, over whatever
items already have all-non-`absent` prerequisites, and needs no trigger to do
it.

The distinction is the whole value of this paragraph: a **permanent structural**
hazard stated as an **event** tells a future author to start checking *after*
something happens, which is exactly when it is too late, and lets them believe a
fresh corpus is safe. Nothing makes a root-keyed check safe.

**A refusal that could not have changed the verdict is not a choice.** An item
with an `absent` prerequisite scores `absent` whether or not a candidate anchor
is accepted, so recording "considered and refused" in its note is evidence, not
non-blindness. Put the argument in the `note`; leave the `disclosure` off.

**The resolver enforces the chosen rule**, two-directionally: every chosen item
carries a `disclosure`, and no inherited item does.
`technologies::disclosure_gaps` (`cli/src/technologies.rs`) is the check, reached
through `audit_family` — which is what `hornvale technologies check` calls — and
its four constructed fixtures are in `cli/tests/suite/technology_coverage.rs`.
The check computes "chosen" from the closure, never from roots, and
`a_chosen_non_root_is_a_finding_where_a_root_keyed_check_would_pass` is the test
that holds that distinction: a root-keyed implementation passes every other test
in the file and fails that one.

**This paragraph read "Task 4's resolver MUST enforce" for the whole campaign,
and for the whole campaign nothing did** (ledger #38, found by the final
whole-branch review; built in the pre-merge fix wave). The rule was ratified at
ledger #13, written here, and published as a **MUST** in
`henrich-2004-extended`'s `provenance`, which the committed report prints
verbatim — and `grep -c disclosure` over both test files returned 0 and 0. It
held in the data by coincidence the entire time (16 of 41 chosen and 16
disclosed in one corpus, 32 and 32 in the other, zero violations), so nothing was
ever wrong and nothing ever noticed. **The failure mode was the one this section
predicts:** a successor moves one item off `absent`, items downstream become
chosen, the report prints their empty `disclosure` cells — which read as
*authored blind* — and no test reds, because the regenerated report matches the
edited corpus. The durable lesson is narrower than "implement your rules": a
capture action routed to a later task is a promise recorded in a document that
does not execute, so **it must be verified at that task's review, not at the
moment it is written.**

## Do not claim an enforcement the tree does not hold

**A corpus in this family is authored before its resolver exists**, so during its
authoring task there is nothing to enforce anything, and the validation its author
runs is a throwaway script in a session scratchpad. `provenance` may describe that
validation **only in the past tense, naming what was run and that it is not
committed.**

`asimov-1989` shipped for one round claiming "the build refuses to write this
file unless every root carries a disclosure." No such harness was committed — and
the same file's `frozen` field asserts that no evaluation code exists, so two
fields of one frozen artifact contradicted each other (ledger #13).

**Committing the harness is not the repair.** It would make `frozen`'s claim
false and break the Global Constraint that no evaluation code exists until the
corpora land — the single property that makes decision 0016's freeze *structural*
here rather than promised. **Correct the prose instead**, and let the rule pass to
the resolver as a requirement.

**Why this is worse than stating nothing:** a cited mitigation that does not
exist removes the reason to build it. A resolver author reading "enforced" has no
cause to re-implement the check, so the invariant ends up believed by everyone
and held by nothing.

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
