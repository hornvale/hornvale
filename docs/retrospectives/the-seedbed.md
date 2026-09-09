# The Seedbed — retrospective

Process, not product. The campaign built `regularities/`; what it *learned*
was mostly about how its own checks fail.

## Six vacuous guards, one campaign, and the sixth was inside the fix for the fifth

Every one was found by mutation, never by reading:

1. A criterion **satisfied by its own negation** — a world where climate
   displaced nobody returns `Number(0.0)`, which is *present*, clearing a
   `present-on-fraction` bar.
2. A criterion measuring **the same statistic twice** across two species and
   calling the second "speciation".
3. A test whose fixture **could not discriminate** its own subject:
   `FractionInBandAtLeast` divided by the present slice while its doc said
   "worlds", and the test passed four values against a population of four.
4. A guard that **recomputed its fix's data** instead of reading production's
   output — its comment claimed a positive control it did not have.
5. A guard whose **population emptied when its task succeeded**: `measure`
   loops over `unmeasured` items, and the task that test belonged to measured
   all of them.
6. **The `doc:` anchor was never checked against the page it cited** — the
   family's headline mechanism. Repointing an anchor at a generated page with
   no claim on it left 186 tests green.

And after fixing (6), the *title half* of the new check turned out to be
untested too, because the negative control used a page with no marker at all.

The pattern is not carelessness. In every case the guard's **subject was not
quite the thing under test** — adjacent to it, derived from it, or selected by
a population that later changed. Two rules earned their place and are now in
memory: *a guard authored alongside its fix asserts the fix's data, not its
effect*, and *a guard whose population is defined by a lifecycle state expires
when the lifecycle advances*. The working prior for the next campaign is that
one more exists and mutation is the only cheap way to find it.

## Nine defects originated in the controller's own text

The plan is the one artifact nothing compiles and nothing runs, and it showed:
a non-compiling assertion, a wrong claim about a file's format, a miscited
decision, a miscounted grep, a test that could not distinguish a one-way guard
from a two-way one, an undercount of my own classification rule, a
data-dependent guard fixture, a negative assertion checking the wrong string,
and a factually inverted claim about what `architecture.rs` examines.

None reached `main`; implementers or reviewers caught all nine. But the shape
is consistent and worth naming: **not one was a logic error.** Every one was an
unexamined premise — a field's type, a file's shape, a decision's actual
content, what a grep was matching against, a number that was true when written.

The sharpest instance: I asserted `architecture.rs` "does not examine
dev-dependencies", having grepped for the literal string `dev-dependencies` and
got zero hits. The behaviour lives in an unfiltered `collect` that never spells
the word. The reviewer added the edge and got a RED in one command. **Searching
for a word is not testing a behaviour**, and the decisive experiment was always
cheaper than the reasoning that replaced it.

## Two process failures of mine, both caught by reviewers

**Rulings went to scratch.** Ledger entries #1–#5 were committed; every ruling
from Tasks 1–7 went into `.superpowers/sdd/…/progress.md`, which is git-ignored
and dies with the worktree. `campaign-autopilot` states the split explicitly and
even names a prior campaign that got it wrong in one sitting. Task 7's reviewer
found it; all of it was promoted (ledger #6–#8). It should have been written
durably as each ruling occurred.

**I committed a reviewer's live mutation.** While verifying the new
stale-deferral guard's negative control, a reviewer flipped a registry row to
`shipped`; I was concurrently committing capture rows to that same file and my
`git add` took the flip with it, shipping a false status that reddened the tip.
The standing rule — *explicit paths, never `add -A` in a shared worktree* — was
followed and did not help, because the collision was on the file I meant to
commit. **The control is temporal, not path-based:** do not commit a tracked
file while a review licensed to mutate it is in flight.

## The absorption cadence was missed

This merge was the branch's **first meeting with main** since the campaign
began, against CLAUDE.md's stage-boundary cadence. It cost nothing here — the
three conflicts were all in regenerator-owned artifacts and resolving by
regeneration produced the correct value — but the campaign ran long, `main`
moved twice during it, and the outcome was luck rather than discipline.

One artifact of the miss is worth carrying: `git merge-base main HEAD` used the
**stale local `main`** (145 commits behind), producing a review package of 196
commits and 1.7 MB. The real campaign was 51 commits. A reviewer handed the
first package would have spent its budget on other campaigns' terrain work.

## What the measurement taught about authoring criteria blind

Of four measured items, three passed and two of those three had **no reachable
failing side** on this population — one had a maximum of 0.4229 against a
ceiling of 0.5. The single failure was the only band taken from an outside
empirical law with a number in it.

The tempting lesson — *prefer outside laws* — is wrong, and a reviewer
demolished it with a counterexample inside the campaign's own data: another
passing item *also* took both poles from the source. Band widths were nearly
identical; what differed was **where the band sat relative to the data's mass**,
which is precisely what a blind author cannot know.

So the rule the next corpus inherits is not about provenance. It is a
**preregistered reachability check**, authorable while blind: *state the value
the statistic must take to redden, and argue from the mechanism that a
plausible world produces it.* The corpus already states each item's falsifying
world; the missing half was why this bake can produce it. That one sentence
would have caught all six demoted criteria before the freeze.

## Deferred minors, and where each landed

| minor | outcome |
| --- | --- |
| T1 — `Verdict` fixture exercises only `unmeasured`, so kebab-case is untestable | carried; no multi-word variant exists |
| T1 — `Item::statistic` unasserted under `serde(default)` | **closed by design** in T2's freeze test |
| T2 — `serde(default)` makes an omitted `emergence_type` indistinguishable from an authored `null` | carried; note `roadmap_instrument` later got the raw-JSON freeze test this lacks |
| T2 — three frozen-note wording nits (over-general `SETTLED`; grain slip; provenance silent on declining items) | carried; frozen text, unfixable without a re-freeze |
| T3 — `attest.rs`'s parser and `generated_paths.rs`'s `declared()` are a deliberate pair with no agreement test | carried; pre-existing, out of scope |
| T6 — header counts had no enforcement | **fixed at merge**; the counts were deleted in favour of the re-derivation commands |
| T6 — `table_text` pipe escaping unexercised; `--corpus` with no value defaults silently | carried; matches `systems` |
| T6 — `MOSTLY` scan case-sensitive; black-box test re-derives the two-sided classification | carried; the re-derivation is independent and does discriminate |
| T8 — the frozen `frozen` prose says the disclosure lives "in its own note" | carried for whoever re-freezes; still true, now incomplete |
| Final — the title half of the anchor check is untested | parked with a ruling; one fixture closes it |
| Final — two wrong counts in `measured_reading`'s doc comment (35/10, actually 41/4) | parked; doc-only, in the commit whose headline fix deleted counts |

## What went right, and is worth repeating

The freeze held, and it held **structurally**: the corpus was authored in a task
that ran before any evaluation code existed, so the author could not have
peeked. A reviewer verified it by extracting `(id, verdict, statistic,
criterion)` at all six commits touching the corpus and diffing pairwise — and
found that **all six demotions cost the campaign apparent successes.** Had they
stayed, the tally would read 9 grown / 1 flat instead of 3 / 1. The instrument
was tightened against its own interest at every step, which is the one property
no amount of prose could have established.
