# sweep-followups

Three follow-ups from the `sweep-scope` landing (merge `4860c7c7f`).

## Stage 1: cargo-sweep is a declared prerequisite of `outboard`
**Goal**: The dependency `test-sweep-roots.sh` created is written down where a
person provisioning a chamber host will read it, beside nextest (0040) and
cargo-sweep (0848).
**Success Criteria**: CLAUDE.md states that the `outboard` set requires
cargo-sweep on the host; a fresh host learns it before the red, not from it.
**Tests**: n/a (documentation); `docs_consistency` / `lane_sets` stay green.
**Status**: Not Started

## Stage 2: the worktree-enumeration ratchet — PREMISE FAILED, SEE BELOW
**Goal (as specified)**: default-deny source scan enforcing "any tool that
enumerates this repo's worktrees derives them from `git worktree list`, never
from a path prefix", allowlist read site by site.
**Status**: Blocked — reporting, not building. Two findings from reading the
11 files / 33 lines the scan would cover:

  1. **The invariant is already satisfied everywhere, with zero violations.**
     Every enumerator already derives from `git worktree list`
     (`worktree-take.sh:39`, `census-run.sh`, `heavy-run.sh`, `sluice-run.sh`,
     `nightly-census.sh`, `test-census-path.sh`, `sweep-roots.sh`). Nobody
     enumerates by `find` or by prefix. Of the 33 matched lines, 21 are
     comments, 11 are test fixtures constructing a fake pool under `mktemp -d`,
     and 1 is `worktree-take.sh:40` creating the pool it owns. The allowlist
     would be the whole population and the violation set empty.

  2. **The ratchet would not have caught the bug that motivates it.** The
     defect was `cargo sweep -r .` — a recursion root that reached neither
     pool. It hardcoded no pool path, so a grep for `.claude/worktrees` is
     blind to it. What catches that class is an assertion that the scope covers
     every worktree `git worktree list` reports, which is
     `scripts/test-sweep-roots.sh` and already landed.

**The hazard that IS unguarded, and it is not this one**: nothing recycles or
reaps the second pool. `worktree-take.sh` enumerates correctly and then filters
to `$POOL` (`case "$wt" in "$POOL"/*`), so the 29 worktrees under
`~/.config/superpowers/worktrees/` — 11 on merged branches, ~161 GB — are
outside any recycling or reaping mechanism. The Sexton's rationale (73 branches
against 3 live worktrees, unrecorded cold builds) applies to them exactly and
does not reach them. A ratchet over the enumeration invariant would sit green
while that accumulates. Proposal is in the session summary; Nathan's call.

## Stage 3: the SKIP message names the host and the remedy
**Goal**: A missing cargo-sweep reads as a missing dev tool on THIS host, not
as a code regression.
**Success Criteria**: The failure names the hostname and prints the install
line; it stays a FAILURE, never a skip.
**Tests**: drive the absent-tool path with a stubbed PATH and assert on the
message.
**Status**: Not Started
