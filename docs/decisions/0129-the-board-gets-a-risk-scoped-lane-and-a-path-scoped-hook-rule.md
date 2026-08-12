# 0129. The board gets a risk-scoped lane and a path-scoped hook rule

**Status:** Accepted (2026-08-12) · **Decider:** Nathan · **Relates:**
[0118](0118-the-board-is-an-orphan-ref-of-immutable-posts-never-rerooted.md),
[0125](0125-github-actions-is-retired.md)

In the context of the board (The Cairn) being dev tooling outside the cargo
workspace with no determinism surface, where a linked worktree may not commit
to `main` (`scripts/CLAUDE.md`) and `main` therefore has a single, frequently
mid-landing writer — and where the pre-commit hook's Rust-relevance filter
runs a full `make quick` on any board change despite `make quick` being unable
to see `tools/board` at all — we decided on **two independent mechanisms**
that solve two different problems and do not substitute for each other: a
risk-scoped fast lane for the board's non-destructive surface, and a
path-scoped hook rule that runs the board's own suite instead of `make quick`
when a commit is board-only.

## Context

**The hook rule fixes a mis-targeted check, not a slow one.** `tools/board`
is deliberately not a workspace member (`members = ["kernel", "domains/*",
"windows/*", "cli"]`), so `cargo clippy --workspace` and `make quick` never
examine it. The pre-commit hook's Rust-relevant filter (`scripts/hooks/
pre-commit`) is `\.rs$|(^|/)Cargo\.(toml|lock)$|...`, which a board `.rs`
file matches, so `make quick` still runs — paying a build for a gate that
structurally cannot see the diff it was triggered by. Measured: the suite
that *can* see it, `cargo test --manifest-path tools/board/Cargo.toml`, takes
24.4s and runs in no gate at all, on any path, today.

**The lane fixes write contention on `main`, a different problem entirely.**
Campaign branches are the only legitimate committers to a linked worktree,
and campaign cadence — spec, plan, execution, merge — is the wrong price for
a board render fix or a new post kind: the board carries no determinism
surface and breaking it cannot corrupt a world. But the board's write and
read-tip paths (0118 parts 2 and 3) and its cross-host sync path do carry
real risk — a wrong `reap` rule deletes posts permanently, a CAS/append bug
is silent write loss, and a sync/push defect can violate 0118's
never-rerooted guarantee across hosts. Those three must not move at
lane speed.

## The decision

**1. The hook rule.** `scripts/hooks/pre-commit` gains a check ahead of its
existing Rust-relevant filter: when the **entire** staged path set is under
`tools/board/`, it runs `cargo test --manifest-path tools/board/Cargo.toml`
in place of `make quick` and exits on that result. Any other staged set —
including a **mixed** commit touching `tools/board/` alongside workspace
code — falls through to the existing filter unchanged. This is a strict
improvement, independent of the lane below: it is correct on `main` and on
any branch, campaign or otherwise.

**2. The lane.** A shared, short-lived integration surface that any worktree
may commit board changes to without campaign ceremony, merged promptly
rather than left to accumulate. Named by **risk, not schedule** — nothing on
it runs nightly, and a cadence-shaped name would invite the lane becoming a
long-lived second `main`, reintroducing exactly the divergence problem the
single-writer rule on `main` exists to avoid. Eligible: render, relevance,
digest, a new post kind or convention (D12: the schema is open, unknown
fields round-trip), and liveness predicates (0118's per-host blast radius
already bounds their damage to one log). **Excluded, and staying on campaign
cadence:** `reap` semantics, the CAS/append path, and the sync/push path —
the three places a defect is either permanent (a wrong reap) or crosses a
host boundary (sync). The lane lowers ceremony, never review: every change on
it still gets a human-visible commit and still runs the board's own test
suite (via the hook rule above, when it lands as a board-only commit) before
merging.

**3. The lane must never auto-implement a suggestion.** A `suggest` post
landing on the board and then being committed on the lane without a human in
between would make the board self-modifying — on the one channel every
session reads at `SessionStart`. This is named explicitly because the
fusion is the attractive mistake: a suggestion and a fast lane arriving in
the same campaign invites wiring them together.

## Consequences

- **A board-only commit gets faster, more targeted feedback**, and a mixed
  commit keeps exactly the coverage it had before — the hook rule can only
  add a check on a narrower path, never remove one from a broader one.
- **The hook rule is not a running gate.** It fires only on a commit that
  happens to be staged; nothing runs the board's 194 tests automatically on
  a mixed commit, and there has been no CI at all since decision 0125. The
  only defense against a board regression riding in on a workspace-touching
  commit is a human running `cargo test --manifest-path tools/board/
  Cargo.toml` by hand.
- **The lane is a live surface with named boundaries, not a blanket
  exemption.** `reap`, the CAS/append path, and the sync/push path stay on
  campaign cadence indefinitely; moving any of them onto the lane later is a
  new decision, not a natural extension of this one.
- **`board redact` and `board sync` need to be discoverable**, since this
  decision assumes operators reach for the right command under time
  pressure: both now have `make` targets (`board-redact`, `board-sync`) and
  a line in root `CLAUDE.md`'s board paragraph.

## See also

`scripts/hooks/pre-commit` (the hook rule, B13), `scripts/CLAUDE.md` (its
documentation), root `CLAUDE.md`'s board paragraph (the lane, its exclusion
list, and the cross-host sync/redact seams this campaign — The Beacon —
added), `docs/superpowers/specs/2026-08-11-the-beacon-design.md` §B13.
