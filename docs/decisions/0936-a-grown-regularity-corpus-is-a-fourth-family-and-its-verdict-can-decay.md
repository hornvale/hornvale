# 0936. A grown-regularity corpus is a fourth family, and its verdict can decay

**Status:** Accepted (2026-09-08) · **Decider:** Nathan · **Relates:**
[0011](0011-studies-are-data-metrics-are-code.md),
[0016](0016-studies-preregister-hypotheses.md),
[0095](0095-a-corpus-is-an-instrument-never-a-standard.md),
[0135](0135-a-capability-corpus-is-a-sibling-to-a-trope-corpus.md),
[0136](0136-a-coverage-verdict-cites-a-checked-anchor.md),
[0261](0261-a-rule-duplicated-on-purpose-carries-a-two-way-agreement-test.md);
[The Seedbed](../../book/src/chronicle/the-seedbed.md)

In the context of scoring Hornvale against external catalogues, facing a
question none of the three existing families can put — not whether the world
can *represent*, *implement* or *parse* something, but whether it **produces**
it — we decided that **`regularities/` opens a fourth corpus family resolving
against measurement over the committed census**, and that **because a grown
regularity can be destroyed by an ordinary retune, its verdict is held by a
two-way guard rather than a ratchet**, accepting a fourth loader, resolver and
report that will never share code with its siblings.

## Why a family, not an extension

Decision 0135 opens a family when the **resolution basis** differs. `tropes/`
resolves against the concept registry, `systems/` against repository facts,
`sentences/` against the grammar. A macro-regularity resolves against a
measurement over 1,000 committed worlds, which is a fourth basis.

The near neighbour is not a sibling corpus but the Domesday, and the
distinction is exactly provenance. `studies/expectations.json` holds claims
Hornvale makes **about itself**, checked for internal coherence. A regularity
corpus imports a claim **from outside** and asks whether the world reproduces
it. Under 0095 those are opposite objects, and folding one into the other would
give `expectations.json` a provenance field it has no business carrying.

## What the family adds to 0136's vocabulary

Two values, each for a reason its siblings do not have.

**`flat` — measured, and the criterion is not met.** The sibling families can
express only degrees of *absence*, because a grammar either parses a sentence
or does not. A generative test's most valuable output is the measured **miss**,
and collapsing it into `absent` discards the finding.

**`unmeasured` — frozen but not yet scored.** The criterion is the prediction
and the verdict is the record; authoring both at freeze time makes one of them
a guess. This is a lifecycle state, tallied separately and never a coverage
verdict, and it is what allows a corpus to be authored in a task that runs
**before any evaluation code exists** — which is what makes 0016's freeze
structural rather than promised.

## The guard, and why it is two-way

`tropes/`, `systems/` and `sentences/` ratchet: a built capability stays built.
A **grown** regularity does not — it is emergent, and any retune of the history
bake can destroy it while every other gate stays green. So the authored verdict
and the computed verdict must agree in **both** directions:

- authored `grown`, computed flat → RED. A regularity was lost.
- authored `flat`, computed grown → RED. Stale pessimism; a real gain is
  claimed deliberately, in a commit that says so.

An implementation reddening only the first has built half a guard, and the half
it skipped is the one that lets a corpus quietly under-report the world.

The same reasoning extends to deferral. A `deferred` item whose enabling
registry row ships does not merely misreport — it **withholds the item from
measurement**, since the corpus goes on claiming it cannot measure what it now
can. That is a stronger consequence than the `systems` family's stale deferral,
and it is checked.

## The anchor is generated documentation

Every earlier mechanism for backing a coverage verdict failed the same way —
a registry token (0577), a hand-maintained list (0330), an anchor into code, an
anchor into a test — each defeated because the program was certifying itself to
itself. This family's terminal anchor is a **`doc:` anchor**, admissible only
against a path `docs/generated-paths.txt` gives a generator. Hand-written prose
is refused: prose asserting a capability is 0330's failure in new clothes.

Generated prose cannot overclaim, because it is a pure function of committed
data — but only if it is genuinely derived. The anchor therefore checks that
the cited page **states the item's claim**, and the rendered claim line carries
the criterion, the measurement and the verdict, all computed at render time.

## Consequence

A corpus is data and a resolver is code (0011), frozen before measurement
(0016), an instrument with declared bias and never a standard (0095). One
column is one reading; the matrix wants a second, and Axelrod (1984) is the
natural candidate. Where a window must read a corpus it may not depend on the
resolver for, the duplication is purposeful and carries 0261's two-way
agreement test in full.
