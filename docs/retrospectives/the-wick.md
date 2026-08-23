# The Wick — retrospective

**In flight** (merge pending). Process lessons only. The product story is in
[the chronicle](../../book/src/chronicle/the-wick.md).

## The 600-second subagent ceiling vs a workspace crate's suite

Three of four dispatches hit the harness's 600 s timeout mid-task — not
because the work was wrong but because `cargo test -p hornvale-vessel`
(424+ tests, ~100 s) plus gate-commit (~23 s) plus reading time does not
always fit one turn. The recovery pattern from The Chroma held each time:
preserve the partial diff to scratch, dispatch a successor with CONTINUE
not restart. But two of three successors *also* timed out, and both times
the work was verifiably green when the controller finally checked — the
bottleneck was the turn budget, not the agent. For small workspace
campaigns, consider: (a) brief subagents to commit green increments early
rather than at task end, or (b) running single-task campaigns like this
one inline under per-step verification instead of dispatching.

## A scratch probe test almost shipped

A `tmp_probe_skyglow_candidates` test — the tool that gathered the ambient-
level evidence — was still in the tree at review time and tripped clippy
(`map_identity`). The evidence it produced was already recorded in
`SKYGLOW_SCALE`'s doc comment, so the probe deleted cleanly. Lesson: an
evidence-gathering probe should carry its own deletion in the task brief,
or be written as a documented example, not a `#[test]`.

## The rebaseline prediction discipline worked exactly as advertised

The prediction (chamber fixture moves; walk band, transcripts, almanacs do
not) was written before regeneration, and the diff matched it cell for
cell — including the *non*-movement of `session-seed-42-turn-0.json`,
which is the assertion that Task 2 did not leak outdoors. Cheap to write,
and it converted "regen done" into "regen done *and* understood."

## What carried over well from The Chroma

Per-task brief verification (one task ahead, grepped against the tree),
the read-do preamble, and challenge-response on return all ran without a
single wrong-tree or unverified-claim incident across the whole campaign.
