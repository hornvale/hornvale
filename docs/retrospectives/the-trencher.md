# Retrospective: The Trencher

**Status:** SHIPPED 2026-09-13; the merge gate passed. Stages 0–3 (the supply half); Stage 4 (the biota) cut by
Nathan mid-campaign and deferred to a successor.

Ledger: `docs/superpowers/ledgers/2026-09-11-the-trencher.md`, 34 entries.

## The one that cost the most: a number outliving its frame

Three times, on three different axes, I carried a **true number into a frame
where it meant something else**. Verification was never the failure — I had run
the command every time.

| stated | actually true of |
|---|---|
| "`CHEMOSYNTHATE` is no longer an aggregate" | this branch only; `main` is ids 0–6 |
| "your §2 says `ResourceVector` is not sufficient" | the *spec*'s §2 — a different author's document |
| "`pub const HYDROGEN` on main is 0" | a grep **count**, not an id |

The first went into a **board post**, headlined *"ResourceAxis IDS 0-10 ARE
TAKEN NOW"* — false of the tree every campaign branches from. A peer campaign
repeated it back to me, and I had to redact and repost. My error propagated one
hop before anyone caught it.

**The fix is not "check harder."** Every one of these was checked. The fix is
to **carry the frame in the sentence** — which tree, which document, which
quantity — because a bare number invites the reader to supply a frame, and they
will supply a plausible one.

## A correct instruction can rest on a rotted reason

I wrote *"never re-pin `survivorship_probe`"* and attached a measured z to it.
The instruction stayed right. The measurement died two world-moves later, and I
restated the pair **four times** — three ledger entries and a dispatch brief —
without re-running it. A peer's unrelated message finally prompted the run: it
passes.

The pair is more dangerous than a wrong instruction, because **nothing about it
looks wrong**, and whoever acts on it inherits the *reason*, not the rule. The
fourth restatement was sitting in a live subagent's brief; I corrected it
mid-flight.

## Two gate holes composed, and diligence made it worse

The branch sat red for several commits and nothing caught it:

1. A `--no-ff` merge fires `pre-merge-commit` only, and `scripts/hooks/` holds
   no such hook — the documented ungated shape.
2. Every commit after it was **docs-only**, so the hook ran the prose tests:
   *"no Rust-relevant paths staged."*

Neither is a defect alone. Composed, the subfloor tier had not run on a commit
since before the census — and **a campaign that ledgers diligently is more
exposed**, because every ledger commit is docs-only. **Run `make gate-commit`
by hand after a world-moving merge.**

## The census is a world-mover, not only a fix

I ruled "repair once, after the last thing that moves the world," then treated
the census as the *remedy* for two red tests rather than as a mover of 116
golden files. It was both: it fixed two and broke twelve. Then main landed an
astronomy campaign with its own census, and the same lesson arrived a third
time — a second census was owed, and its ordering constraint is circular
(census needs a pushed SHA, SHA needs a green merge, merge needs pins the
census will move), so two re-pin rounds were structurally unavoidable.

## An inventory from one run is a sample, not a census

The red count went **23 → 12 → 14 → 25 → 13 → 11** across six passes. Each was
an honest reading of one run. Twice, an implementer found reds my sweep had not
taken.

Worse and more specific: **asserts are sequential, so twelve failures were
hiding twenty-six drifted values** across twenty-four sites — including three
of four species in one table whose own note records that exact mistake being
made before. Reading one failure per run would have repeated a documented error
fourfold.

## Verifying the brief before dispatch paid every single time

Never once did it come back empty:

- Task 3 — four defects; the expensive one **silent**: the brief never named
  `v1_basis()`, and its own test passed whether or not the axes were appended.
- Task 4 — five; including that the term it moved had been justified by a
  prediction the measurement then falsified.
- Stage 3 — the plan said "10 sites"; it was **9, of which 2 were production**.
  Its inert guard already existed, stronger than the one specified.

Three minutes of grep, once per task, immediately before dispatch.

## Two techniques worth stealing

**Prove a classification, don't infer it.** Faced with twelve reds I had called
"literal drift," an implementer softened *only the pin assertions* to
non-fatal prints, left every invariant and precondition **armed**, and re-ran.
All twelve passed — proving the invariants were reached and held. That is a
positive control for a *classification*, and it is stronger than any amount of
reading.

**Do not let the fix's author write its guard.** Task 8 went to a different
implementer deliberately. It reproduced the gate author's central claim
independently, widened it past the buffer — and found a comment the gate commit
had added claiming coverage its test **cannot provide**, demonstrated by
mutation rather than argument.

## What the peer exchange was worth

Two live campaigns caught, between them: my board scope error, my
document-conflation, my count-as-value phrasing, and a stale measurement I had
repeated four times. I caught, in theirs: a remedy that would have removed the
guard it repaired, and a stale axis id heading into a spec revision.

**Not one of these was found by the side that made it.** That is a fact about
what self-review structurally cannot do, not about carelessness.

## Deferred minors and what nearly evaporated

The folded-in campaign's two deferred minors lived **only in git-ignored
scratch belonging to a campaign that never merged** — outside the committed
ledger's guarantee entirely. Both turned out moot (the code they referenced is
not in this tree), but they would have vanished silently at the next worktree
recycle. The Cartulary's fix covers the campaign that *writes* a ledger; a
**folded-in** campaign's scratch is still uncovered.

Also found by reading the ledger end to end at close, which is the step's whole
point: **two pairs of duplicate entry numbers**, the duplicate-`TOOL-24` shape
in the campaign's own record. Resolved by suffix rather than renumber, because
two live citations pointed at one of them.

## The thing to say plainly

This campaign landed **a producer with no consumer** — and produced that same
defect three times in its own work (the metabolite axes, the dim methane axis,
the metaphysics gate with no production reader) after spending its first stage
learning to name it in other people's code.

Naming a defect class does not protect the next thing you write from it. It
only makes it faster to recognise afterwards.

---

## The second half of the campaign, and what it cost to learn

Tasks 15–17 ran after the passage above was written. It predicted them.

## A falsifier that could not fire, and fired open

The yield-form change was preregistered with five predictions, one of them
labelled **"THE FALSIFIER FOR THE WHOLE APPROACH"**: *the median leader margin
stays ≥ 0.02*. The quantity it named already existed and returns a **ratio of
leader to runner-up**, which after a descending sort is bounded below by `1.0`.
The threshold was satisfied at every reading that can exist, under every arm,
however degenerate.

The direction is the part worth keeping. That clause existed to stop a
compressing transform from shipping; as written it would have returned HELD for
a transform that flattened all four axes into a tie. **It failed open, toward
the answer argued for two paragraphs earlier in the same entry.** The
implementer caught it and adjudicated against an absolute difference instead.

The narrow rule: **a preregistered threshold must name the quantity and its
unit, not just a number.** "Margin" named two quantities that differ by whether
`0.02` is trivial or decisive, and nothing said which.

## A probe over a field is not a probe over a world

The four-metabolite probe sampled 104,845 readings and could not have seen that
the shipping change cost seed 42 forty-four percent of its facts, because
population is downstream of a history bake the probe never runs. Field
measurements and world measurements answer different questions and the first
does not bound the second.

Its companion: **one world is an anecdote.** That forty-four percent was
reported as an effect size; across five seeds the effect is about ten percent
and seed 42 is a three-to-fourfold outlier. The campaign had already written
this lesson down before making the mistake.

## Three statistics, each wrong in a different way

Writing the band guard falsified three of the author's own choices in
succession — and each was wrong for a *different* reason, which is why none of
them was caught by being careful about the previous one:

- **wrong normalisation** — ratio spread where absolute was the question. Every
  axis's `p90/p10` compressed hard, which read as a flattened world; in
  absolute terms, which is what a threshold uses, methane's range *grew* 35%.
- **wrong order statistic** — the modal band reported one of four axes moving
  with depth. A mode moves only when the bulk crosses a cut and is blind to a
  distribution sliding underneath it. The share-above-a-cut shows four of four.
- **degenerate on the shape** — a plain median, on an axis where 54.6% of
  readings are exactly zero, *is* exactly zero. It cannot report drift at all
  until the absent share crosses one half, then jumps.

**Writing the guard is what exposed each.** None was visible from the design.

## Diagnosing a class confers no immunity — now with dates

The passage above says naming a defect class does not protect the next thing
you write. Tasks 15–17 supplied two more instances, both committed **after**
writing the paragraph explaining why they were wrong, in the file whose entire
purpose is guarding an unfalsifiable surface:

- a declared frequency (`per_100k`) that **nothing read** — a seventh instance
  of the prose-claims-no-assertion-checks row this campaign itself minted;
- **one tolerance for four differently-shaped axes**, which is the exact
  argument the per-axis band cuts are built on, one level up. A tolerance is a
  threshold. The bands got per-axis cuts after measuring; their guard got a
  single shared number, four hundred times looser than the claim it protected.

## Guards can be defeated by syntax, not only by omission

`source_phrase` — the only place a metabolite's identity reaches a human — was
registered as a seam and the registration was **inert**. The scanner found no
call sites, because every call sat inside a `format!` and the parser does not
descend into macro token streams. A registration the tool cannot probe is worse
than none: the committed roster reads healthy while nothing is checked.

Check that a newly registered guard can **see** its subject before trusting its
green. Hoisting the calls into bindings made it real, and the verdict then came
back GUARDED at both sites.

## Two operational facts learned expensively

- **Do not wrap `seam-guard run` in a timeout.** Two call sites at a full
  scoped test run each exceed fifteen minutes, and the kill leaves its mutation
  in the working tree. It surfaced only because the next run refused on a dirty
  tree and the diff was read rather than assumed. A `git add -A` would have
  committed a neutralised seam whose guard is the test it defeats.
- **Pay for the attribution build.** When an absorb produces new failures they
  have three possible owners — main landed red, the branch was already red, or
  the merge product alone — and each implies a different response. A detached
  worktree at `origin/main` running exactly the new failures answered it in one
  run: all eleven passed there, so it was a collision, and the repair was ours.
  Guessing would have been free and wrong.

## Two findings that are about evidence, not code

- **A stale claim that later turns out true does not retroactively become
  evidence.** A statistic this campaign was corrected for restating without
  re-deriving has since genuinely stopped holding. The comfortable reading is
  that the correction was unnecessary; the correction was about provenance, not
  about the conclusion, and provenance is a property of *when* a thing was
  asserted.
- **When a control moves alongside its subject, diff by identity, not
  position.** A control golden moved 75% and looked like a recipe change; the
  population itself had shifted (240 facets out, 221 in), and diffing by vertex
  id showed the true control unchanged at **0 of 10,978**. A line-ordered diff
  would have reported a regression that did not happen.
