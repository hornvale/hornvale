# 0146. The census worktree path is resolved, not passed

**Status:** Accepted (2026-08-19) · **Decider:** Nathan · **Amends:**
[0079](0079-census-goldens-are-authored-on-one-enforced-host.md)'s
`HV_CENSUS_WORKTREE=canonical` clause · **Relates:**
[0063](0063-census-regen-is-local-again.md),
[0139](0139-main-advances-only-through-the-lock.md)

In the context of `scripts/census-run.sh` stating, in its own comment, that its
worktree path is "outside the repo to keep `git status` clean here", while the
project's documented invocation passed `HV_CENSUS_WORKTREE=canonical` — a bare
relative name, resolved against a cwd that same command sets to the repo root —
we decided that **the census worktree path is resolved by the script, and an
override, if given, must be absolute**, because a raw caller-supplied path is
the mechanism that put the worktree inside the repository and kept it there for
two months.

## Context

`~/Projects/hornvale/canonical` was a registered git worktree living inside the
main checkout: untracked, un-ignored, and reported by `git status` as `??` on
every run. A `git clean -fdx` in the main checkout would have deleted a
registered worktree out from under git.

**It was not a stray.** The value appeared in four live places — `CLAUDE.md`,
`scripts/census-canonical-host.sh`, the compiled refusal message in
`windows/lab/src/census_guard.rs`, and 0079's own Consequences section, which
names it as one of "the two traps that have each cost a wasted run". The
convention was deliberate, documented, and wrong in exactly the way its own
documentation described.

**0079 had already solved this, for a different field.** It resolved the
canonical *host* "unconditionally from the declaration file, not defaulted from
the environment", so that "changing the canonical box means editing one line in
version control, visible in review". The host got that treatment; the worktree
path kept an unanchored environment override. This decision applies 0079's own
reasoning to the field 0079 left behind.

**Why the fix is not "document a different value".** Repointing the override to
the sibling default would change what everyone is told to type without stopping
the next caller typing a relative path. The trap is the raw override, not the
particular value that fell into it.

## The ruling

1. `census-run.sh` resolves the default itself, anchored to the **main
   worktree** — via `git worktree list --porcelain`, whose first entry is the
   main worktree by git's own ordering. Anchoring to `$repo_root` was a second,
   quieter instance of the same bug: invoked from a linked worktree it resolved
   inside `.claude/worktrees/`, a different directory per campaign, each one a
   fresh cold build.
2. `HV_CENSUS_WORKTREE`, when set, **must be absolute**, and a relative value is
   refused. The refusal fires *before* the census lock is taken, following the
   canal-lock rule the merge queue's mouth already applies: turn a vessel away
   at the gate, never inside the chamber.
3. The override survives, absolute only, as a test seam.
4. `census-run.sh worktree` prints the resolved path under no lock, from any
   box. It exists so a test can assert the resolution without paying for a
   census — the alternative was asserting against a copy of the expression,
   which is the shape that drifts from its original silently.

## Consequences

- The refusal names **absoluteness**, the bound it actually enforces — not
  "outside the repo", which it does not test. An absolute path inside the
  repository is still accepted. A refusal that named a bound it did not enforce
  would be a false claim in an error message, a failure this project has
  shipped three times.
- `scripts/scheduled/nightly-census.sh` no longer computes the path; it asks
  `census-run.sh worktree`, so the two cannot drift.
- **This decision was ratified without a confirming census run.** The
  resolution, the refusal, the gate-ordering and the mutation are pinned by
  `scripts/test-census-path.sh`, which runs no census by construction. What is
  *not* proven is that the default path drives a full census to completion —
  that is a path-only change touching no compute, but it is unverified, and the
  next census refresh is the first real exercise of it. Recorded here rather
  than left as an assumption someone would have to reconstruct.
