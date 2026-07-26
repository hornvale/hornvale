# The Purview — Retrospective

Process lessons only; the product is in
[the chronicle](../../book/src/chronicle/the-purview.md).

## What worked

**Running the ideonomy pass on the settled question.** The approach was
never really in doubt — build the protocol half first — and the pass came
back "yes, and also" rather than reversing anything. But two of its
enrichments changed the design's shape before a line was written: the
hierarchicalness axis showed that a coarse chart is the same builder at a
shallower depth, deleting a planned aggregation layer, and the homogeneity
axis relocated the two-grains thesis out of "two renderers" and into one
cell schema whose detail varies by epistemic state. Both would have been
expensive to discover during implementation. The overlay's claim that the
settledness of an answer is a *reason* to run a pass, not a reason to skip
one, earned itself here.

**Verifying a claim at drafting time rather than review time.** The spec
first argued that the fog rule was physically honest because "a room is
~1.7 km across, so a radius-4 neighbourhood is ~12 km." Checking it before
it shipped found the sim **defines no planetary radius at all** — the mesh
is on the unit sphere and elevation is the only metric length in the model.
The claim was replaced with the measured angular figure, and the schema now
forbids asserting metres per cell. This is the failure mode named PROC-18/19,
caught by the drafting session for once instead of by a reviewer.

**Looking at the output with human eyes, as a required step.** The chart
passed every test while rendering a sheared parallelogram. No assertion the
plan could plausibly have contained would have caught it; the instruction to
render a real neighbourhood and *look* did, and the implementer correctly
reported it rather than silently "improving" the contract.

**Mutation-testing the deliverable tests.** The final reviews did not merely
read the thesis tests, they tried to break them — and found two clauses that
could not fail. This is the single highest-value thing any reviewer did in
the campaign, and it should be the default ask whenever a campaign's point
*is* a test rather than a feature.

## What to do differently

**A killed subagent leaves the controller as author and reviewer.** Task 4's
implementer was cut off mid-run by a spend limit; the controller finished it
inline to conserve budget, which meant its author and its only reviewer were
the same party. It was flagged in the progress ledger and routed explicitly
to the whole-branch review, which then found two real defects in it — a
reference page that asserted "field order is contract" and never stated the
order, and a factual overclaim about a wasm export that does not exist. The
handling was right; the lesson is that *finishing it inline* should
automatically owe an independent review of that commit, named at the time,
not remembered later.

**Stage-boundary absorption did not happen.** CLAUDE.md asks campaign
branches to absorb main at every plan-stage boundary. This branch met main
exactly once, at close, and absorbed **78 commits** across six campaigns in a
single merge. It resolved cleanly enough — three conflicts, two of them
generated files — but the semantic collision it hid was real and had to be
checked by hand rather than assumed: The Snapshot had rewritten the
Casement's transcript to render from a session snapshot while this campaign
had changed the same `splitResponse` path to class chart lines as monospace.
They compose, but only inspection established that. A 78-commit absorption is
exactly the "surfacing drift at a 105-commit merge" the cadence exists to
prevent.

**A hand-written merge resolution needs a compile check before it is
committed.** Resolving `session.rs` with a script dropped a closing brace at
the conflict boundary. The gate passed anyway — because cargo reads the
working tree, and the fix was in the working tree while the merge commit had
captured the broken version. History would have contained a non-compiling
commit. `git stash` plus a build of the *committed* state is the check;
"the gate is green" is not, when the gate ran before the commit.

## Inherited and outbound debt

**Cleared by the absorption.** This branch ran red against 32 `hornvale-lab`
census-fixture failures for its whole life, attributable to The Vestige's
four added metrics. Main re-pinned the goldens (`946d1b51`) before close, so
the merged result is green. The attribution work was still worth doing — it
is what made it safe to keep shipping against a red suite.

**Outbound, not this campaign's to fix.** `make vessel-check` is already red
on main (seed 43 has no settlement; `new --seed 43` is byte-identical at
merge-base and tip, so world generation did not move), likely from
`a5640639`. And main now **tracks `.superpowers/sdd/decision-ledger.md`**
even though `.gitignore` ignores `.superpowers/` — The Hearth's close
committed its scratch ledger by mistake, and the merge overwrote this
campaign's ledger with it. Nothing load-bearing was lost (the material
entries were already promoted into the spec and the commit messages), but
gitignored scratch is now tracked, and it is worth deleting deliberately
rather than letting the next campaign inherit it again.

## Follow-ups

- The gallery artifact is a weak showcase: all 31 cells are
  river/shelf/tropical-seasonal-forest with zero seams — a uniform field of
  `+`. A second observer would show terrain and a seam.
- The biome reaches the player as two examinable nouns — the chart's slug and
  the prose's spaced name — so the thesis's sharpest clause never fires on it.
- `scene-surrounds-seed-42.json` is strictly drift-checked while its siblings
  are excluded for carrying libm-thresholded classifications, which it also
  carries. Low risk at 31 cells; the inclusion should be a recorded decision.
- `examine` builds a full purview scene (~1.5 ms) even for prose-only nouns.
- The lat/lon → unit-sphere conversion is now hand-copied five times.
- Deferred by design: per-species sense radius (EXP-3), memory that is wrong
  rather than stale (MEM-1), the anti-map (RENDER-7), an NPC's own purview,
  and the graphical tilemap client.
