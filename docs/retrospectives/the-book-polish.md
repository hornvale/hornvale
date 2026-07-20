# Retrospective — The Book Polish

One page of process, not product. The product is chronicled; this is what
the close learned that the code does not record.

## What went well

- **Deliberately skipping the subagent-dispatch ceremony for a two-string
  fix was the right call.** Both defects were closed-string surface
  changes confined to one file (`windows/book/src/lib.rs`) plus their
  exact-string test pins; a full brainstorm → spec → worktree → per-task
  implementer/reviewer dispatch pipeline would have spent more turns on
  process than the fix itself took to write and re-pin. The spec and
  ledger still existed (G3 stayed a hard stop), but execution ran inline
  with ordinary TDD — write the failing re-pin, fix the string, watch it
  go green. Size-matching the process to the change, not defaulting to the
  heaviest available ceremony, is worth repeating for future single-file
  polish work.
- **Verifying a hand-merged conflict by running the actual conflicted
  test, rather than reasoning about what the merge "should" produce,
  caught nothing wrong — because it was checked, not assumed.** Absorbing
  22 commits of `origin/main` (including a parallel campaign's physics
  recalibration touching the same test file this campaign edited) produced
  a real three-way conflict. The combined hand-merge was confirmed correct
  by compiling and running `the_reckoning_renders_the_epoch_pair` against
  the merged tree before trusting it, not by inspecting the diff and
  declaring it plausible.

## What was awkward

- **A generated chronicle file was briefly written to the wrong
  checkout.** Mid-close, the chronicle entry landed in the main checkout
  (`/Users/nathan/Projects/hornvale/hornvale`) instead of the campaign
  worktree — harmless only because the main checkout's own dirty state
  (another session's in-progress work) had nothing staged, so the stray
  untracked file could be moved without touching anyone else's changes.
  Had that other session's tree been mid-`git add`, the same slip could
  have collided. Durable DoD artifacts belong on the branch, in the
  worktree, from the first `Write` call — a smaller version of the
  scratch-ledger discipline, extended to every file the close produces,
  not just the git-ignored scratch ledger itself.
- **A `git commit` heredoc intermittently failed to parse** (`unexpected
  EOF while looking for matching` a quote), silently dropping the
  preceding `&&`-chained `git add` with it. Writing the commit message to
  a scratch file and using `git commit -F <file>` sidestepped it; the
  root cause was not chased further since this campaign's blast radius did
  not warrant it, but it is the same class of bash-chain fragility other
  campaigns have hit.

## Follow-ups

None — both defects this campaign set out to fix are closed, and no new
mechanism or registry-worthy idea surfaced. LANG-43 (irregularity) and
LANG-44 (numeracy) remain queued as their own, larger campaigns.
