# The Forebay — retrospective

**Merged:** pending. Process lessons only; what shipped is in
[the chronicle](../../book/src/chronicle/the-forebay.md).

## Pre-dispatch verification found a defect in the plan's own text before every single task ran

Three separate defects, none of which a re-read of the plan against the spec
would have caught — only opening the code did:

- The plan's own bash-apostrophe line set named three offending lines;
  `:138` sits outside every `$(...)` substitution and was never actually
  parsed as a comment inside one, so only two of the three were real.
- Three sketched tests used `RoomAddr::new(0, &[0, 0, 0])`. There is no such
  constructor — `RoomAddr` is a two-field public struct with no
  constructor function at all, so the sketch would not have compiled.
  `RoomAddr { face, path: vec![...] }` is the actual construction, matching
  the file's own `all_addrs` helper.
- Every line citation in Task 3 was stale before Task 3 even started: Task 1
  added roughly 119 lines to `room.rs` ahead of it. The original test list
  also miscounted — "six" tests where the real count is eight, having
  counted two `neighbors_memo` tests as `corner_weights` ones.

Each was corrected in the plan text before dispatch, with the command and
output that established the correction left inline rather than asserted.
None of the three would have surfaced from re-reading the spec more
carefully — the spec and the plan agreed with each other and were both
wrong about the code.

## A split-brain brief produces exactly the defect it predicts

Working out decision 0208's reframe — the guard was not "total," it was
retired because the key that made it necessary was completed — happened
mid-session, and the plan's Task 4 *content* instructions were updated to
say so. Its Task 4 *file list* was not: the filename stayed
`0208-corner-weights-level-guard-is-total.md`, the exact framing the reframe
existed to reject. The implementer followed the file list rather than
inferring the correction from the prose around it, and the record landed
titled "Completing the key retired the level guard" in a file whose name
asserted the opposite of its own title. An implementer treats the
checklist, not the surrounding argument, as authoritative — which means a
correction made in one place and not the other produces exactly the defect
that place predicts, not a diluted version of it. Fixed with `git mv` and
two referrer updates once caught; the general lesson is that a plan
correction is not done until every place the same fact is named agrees.

## Bash 3.2 is a defect *class*, and writing the check found instances the fix-hunting never did

Task 0 arrived already knowing about two dead guards on macOS's bash 3.2 —
a parse failure from an apostrophe inside a `$(...)` comment, and a runtime
failure from `mapfile` (bash 4+). Those were the fourth and fifth instances
of a class two other project files already document defending against in
prose (`census-canonical-host.sh`, `subfloor-run-chunked.sh`) — diagnosed
correctly, twice, and still not enforced anywhere.

Writing `check-bash32.sh` itself — not hunting for more instances by eye,
just building the detector — turned up a **third variant** of the same
class in the check's own first draft: a `case…esac` placed directly inside
a `$(...)` command substitution also fails to parse under bash 3.2, reduced
to a five-word repro. And the same class struck the campaign's own git
tooling twice more during this session: a `git commit -m "$(cat <<'MSG'
…)"` heredoc-inside-`$(...)` died at a parenthesis mid-message and produced
a commit whose body was silently truncated — caught only by reading the
landed commit back, not by any exit code, since the shell that mis-parsed it
did not fail loudly. Every commit after that point used `git commit -F` with
the message in a file instead.

The generalization is not "watch for apostrophes" or "watch for `mapfile`" —
it is that a lint pass over a fixed list of known-bad constructs will always
be behind the actual defect surface, because bash 3.2's parser has more
failure modes than any one session enumerates by inspection. The check that
shipped is a two-halved detector (`bash -n` for parse failures, a construct
grep for runtime-only ones) precisely because neither half alone would have
caught both classes, and it is scoped to the roster the scan actually found
rather than asserted to be complete.

## `shellcheck` passing is not evidence a script runs

`shellcheck scripts/test-worktree-freshness.sh` exits 0, completely clean,
against a file `/bin/bash -n` cannot parse at all. A shell linter reasons
about the script's *contents*; it does not run the interpreter it is
written for, so a construct that only the real parser rejects is invisible
to it. This is the reason `check-bash32.sh` shells out to `/bin/bash -n`
directly rather than trusting a lint's exit code — the false green was
verified, not assumed, before the check was designed around it.

## The wire failed silently, and the board carried the question to close with no answer

The natural readout site for the memo's hit-rate measurement was inside
`windows/vessel/examples/agent_scaling.rs`, held off by a concurrent
campaign (`campaign/the-hand`). A cross-session message to that campaign's
live session was sent and **expired unapproved** — it never reached the
other session's Claude at all, and nothing in that failure mode raises an
error the sender can see. The durable channel — a board `ask` post,
`thread=forebay-memo-hitrate-readout` — took over, synced to `origin` so the
other host would see it at its next session start, and it went unanswered
through the end of this campaign.

The asymmetry worth keeping: a wire message that is never approved decays
to nothing, silently, and the sender has no signal that it happened. A
board post accumulates regardless — it sits there, visible, whether or not
anyone replies. For a question whose answer is worth keeping across a
session boundary, the durable channel should be tried first, not held as
the fallback after a wire attempt that can fail without telling you it
failed. This campaign's spec names the fallback branch it actually took
(a synthetic kernel-side probe, explicitly weaker than the bench reading it
could not get) and states the open coordination plainly rather than
guessing at an answer the readout never supplied.

## Two guards in the repository caught real things, and are worth naming as wins rather than friction

- `cli/tests`' `docs_consistency::decision_cites_in_sources_resolve` refused
  a source comment citing decision `0206` before that decision was actually
  minted — a real ordering bug in how the campaign was assembling its own
  evidence, caught mechanically rather than by review.
- A wrapper refused a plan step that invoked the test suite twice to ask two
  independent questions in one command — "run once, inspect many" is a
  standing rule in this project's CLAUDE.md, and here it was enforced rather
  than merely stated, against a plan step that would have paid for the
  suite twice to save one grep.

Both refusals were correct, and both caught a flaw in *this campaign's own
process* rather than in the code it was writing — which is the more
valuable half of what a gate can do, and the harder half to notice when it
works.

## `gate-commit`'s documented cost is wrong for this host by about 10x

CLAUDE.md says a `gate-commit` run with no source change costs 10-16 s. On
`ambrose` the measured **warm floor is 110-155 s**, and the first run in a
freshly-taken or freshly-absorbed worktree costs 450-700 s. A Task-0 implementer
flagged a 547 s run it could not explain and honestly declined to guess a cause;
re-running on the identical tree gave 116 s, which isolated it to cold-cache
cost rather than to the change under test.

Aggregating `docs/timings.md` (738 `gate-commit` rows) showed the cold/warm pair
is reproducible across three branches on this host:

```
  the-scour    454.1  ->  110.3
  the-leat     239.1  ->  155.6
  the-forebay  547.7  ->  115.1
```

Per-host means: ambrose 140.6 s, MacBookPro 96.2 s, lefford 57.5 s.

**`cpu_ratio` says what the warm floor actually is.** Cold runs sit near 2.85 —
a parallel build. Warm runs sit at **0.71-0.98**, i.e. serial work, and the
sub-floor tier itself ran 483 tests in 12.0 s inside a 115 s wall. So roughly
100 s of a warm `gate-commit` here is single-threaded work *ahead of* the tests,
which no amount of test-scoping can touch — the same reason `gate-fast`'s
test-scoping only ever bought ~10%.

Two lessons, one of them a trap. First: budget from the ledger, per host, not
from prose — CLAUDE.md's own guidance about census cost teaches exactly that for
a different label, and the same discipline was needed here. Second, for whoever
re-derives it: **`wall_s` is field 4 of `docs/timings.md`, not field 3.** Field 3
is the label, so a naive `awk -F'|' '{print $3}'` gives every row `0.0` and a
mean of zero, which looks like a broken ledger rather than a mis-indexed column.

Posted to the board as a `technique` with the aggregation, since the number will
decay and a board post is where a corrected one can land.

## The merge went in without a stage gate first, deliberately

Nathan chose to submit the merge rather than wait for the queued stage gate to
report. Recording why that is not a shortcut: a **merge** request runs all six
chamber phases — including the whole-workspace suite a stage gate runs — and
pushes only on green, holding the request and leaving `main` untouched
otherwise. The stage gate's distinct value is that a red costs *milliseconds at
the mouth* rather than the box; going straight to merge trades that cheapness
for one queue slot instead of two. With `gate-commit` green, both byte-identity
goldens passing, a clean post-absorption regeneration and a conflict-free
`merge-tree` against a current `main`, the residual risk was a phase this branch
has no plausible way to redden.

What made that judgement worth recording is that the **whole-workspace suite had
not run anywhere** — `gate-commit` covers only the sub-floor tier, and the local
wrapper correctly refuses a workspace-wide test invocation, naming the sluice as
the thing that runs it. So the merge submission is not skipping the full suite;
it is the first and only place it runs.

## Deferred, with homes

Every deferred item from this campaign is registered in the idea registry
(Task 5, five rows, each checked against a duplicate ID before adding — a
duplicate `TOOL-24` once slipped through this exact step in an earlier
campaign):

- `TOOL-log-bounding-divergence-restoration` and
  `TOOL-log-bounding-epoch-fact-lifetime` — stage 7's two independent
  halves, both gated on `campaign/the-hand` freeing `windows/vessel/`.
- `TOOL-scan-at-prefill-or-faster-scan` — the honest follow-up if the
  residual 13.4% is worth addressing: a faster `scan_at`, or a
  reachable-set prefill sized by distinct-rooms-per-tick, not by a hit
  rate.
- `TOOL-ticksystem-step-no-cache-hook` — `TickSystem::step` still has
  nowhere to hang a cache; `step_with_occupancy` is a local widening of one
  call site, not the kernel-trait fix.
- `TOOL-agent-scaling-memo-clone-per-tick` — `agent_scaling.rs` clones the
  whole memo every tick to satisfy a borrow, and `malloc`/`memcpy` is 33.3%
  of that bench; a property of the harness, not necessarily the sim,
  unmeasured which.
