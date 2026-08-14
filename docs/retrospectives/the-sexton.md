# The Sexton — retrospective

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-sexton.md).

## 1. The premise was wrong, and one line of `awk` said so

The campaign was commissioned to attack the census. `docs/timings.md` had 368
`gate` rows, 247 `rebaseline`, 34 `census` — a month of data nobody had
aggregated. Summing it put the census third at 17.5% behind the gate at 57%.

**The lesson is not "measure first."** It is that a *ledger that is never
aggregated is not a measurement*, and this repo had one for a month while
every cost conversation ran on intuition. The rows existed; the sum did not.
Ask what is missing from a label list as readily as what is largest in it —
`prewarm` had zero rows against seventy-three branches, which is how a whole
cost class stays out of every decision about cost.

## 2. Verify the brief against the code one task ahead, never in a batch

`dispatching-hornvale-subagents` says to do this immediately before each
dispatch, and the timing is the whole reason it works. Verifying one task's
claims with the tree in the state the implementer will actually find it is a
different and much easier job than verifying five tasks at plan-writing time.

It paid on nearly every task. Two examples where batching would have failed
outright:

- **Task 4's insertion anchor was inverted by Task 3's own restructure.** The
  brief said "insert after `alarm_status=$$?;`"; Ruling 6 had moved that line
  inside a green-only branch, so following the brief literally would have run
  the defect ledger *only on green gates*. Only checkable after Task 3 landed.
- **Task 8's board design was invalidated by a fact about `git clone`.** A
  fresh clone does not fetch `refs/hornvale/*`, so the scheduled job's board
  posts would have been silently rejected forever. Checkable only by reading
  the board's sync code with the scheduler's shape in hand.

## 3. Implementers overriding the plan was right every time it happened

Five times an implementer declined to follow the brief and said so. Every one
was correct:

| What the brief said | Why it was wrong |
|---|---|
| `git checkout -- docs/generated-paths.txt` to restore | the file is untracked at that point; the command errors and restores nothing |
| `assert!(false, …)` as a deliberate-red probe | `clippy::assertions_on_constants` denies it, so the probe reddens clippy, not nextest |
| amend commit `<sha>` | the controller's own commits had landed on top; amending would rebase a commit the implementer did not own |
| `2> >(tee …)` to restore gate output | a bashism; this Makefile sets no `SHELL`, so under lefford's `dash` it reports `tee`'s always-zero status as **the gate's verdict** |
| a Step-4 test asserting the scratch sweep worked | its unrepaired `mv` broke the registry lookup, so the script fell to the cold path and the assertion passed **without entering the code under test** |

The fourth would have shipped a silently always-green gate on the canonical
box. Keep "verify rather than comply" in every dispatch, and keep asking for
the report to name contradictions with the brief — that is where these came
from.

## 4. Fixing a finding is where the next finding comes from

Three times a fix introduced a defect the original did not have:

- Task 8's cleanup fix captured the whole `git status`, so modified *tracked*
  files — the normal output of a drift night — triggered a "cruft removed"
  notice that misdescribed reverted files as removed.
- The same fix interpolated filenames into `make board-post NOTE=`, which a
  quote can split into extra argv words.
- The final review's I4 fix moved nextest's status into a file, which reports
  **green** when the file is empty or stale — the exact failure class it was
  defending against, moved rather than removed.

**A change that adds an announcement or a capture path adds everything that
path can get wrong, and it is not covered by the tests that covered the thing
being announced.** Scope a re-review to the fix diff *specifically* for this;
it caught all three.

## 5. The instrument was the broken thing four times

A concurrency sampler counted its own `grep`. A `grep` for a landed fix
returned a false negative through the author's quoting. A planted "shellcheck
violation" was not a violation. A ledger lookup failed because the shell's cwd
had silently reset to `/`.

Every one produced confident, specific, wrong output, and every one was caught
by the same move: **run the positive control.** A negative result means
nothing unless the check fires on a known-positive. This is `campaign-autopilot`'s
"a mutation test must prove it mutated," applied one level up to the measuring
apparatus rather than the code.

## 6. A measurement can decay inside one working day

The spec's headline — world generation is 5% of census cost, extraction 95% —
was measured in the morning and was **6× wrong by that evening**, after
absorbing another campaign. The conclusion it supported ("worldgen optimisation
aims at the wrong 5%") became false.

`PROC-floors-erode-unseen` says a floor whose measurement is never re-taken
decays into a catastrophe alarm. It fired here inside a single day, on this
campaign's own spec, and was caught only because an implementer reported a cost
that contradicted its brief instead of quietly matching it.

Two practices follow. **Re-measure a load-bearing figure at every absorption**,
not at the close. And **state corrections as corrections**: the spec now carries
both measurements, dated, with the cause and the profile caveat, rather than
corrected digits. A document that silently changes a number teaches nothing;
one that records how the number was wrong teaches a reader to distrust the
method that produced it.

The corollary bit too: a first re-measurement in `--release` against an
original in `dev` would have conflated the campaign's effect with a profile
change. CLAUDE.md's Whetstone rule — check which profile a path uses before
comparing — is what made the number interpretable.

## 7. Process deviations, recorded

- **DoD artifacts landed after the merge, not before it.** `closing-a-campaign`
  says chronicle and retrospective go on the branch before merging; the owner
  authorised the merge at G6 and the artifacts followed. Harmless here (the
  branch still existed and fast-forwarded again) but the order is the rule for
  a reason: a merged campaign with no chronicle is one interruption away from
  never having one.
- **The `hv-guard-bash.sh` guard false-positived three times** — on a command
  that merely *quoted* two test invocations while patching a plan, on two
  *different* scoped targets in one call, and on ledger prose describing the
  guard. It counts occurrences in command text rather than detecting a repeated
  target. The guard is right to be conservative; this is a narrowing suggestion,
  and its own design note explains why a too-broad guard dies by having its
  override exported into a shell profile.
- **A `rebaseline` failure was lost** because the run was piped to `tail -2`.
  CLAUDE.md's own rule is "run once, inspect many"; the one run where it
  mattered was the one that violated it. The real finding was what the loss
  exposed — the parallelised script could not name which job failed — now fixed.

## 8. Deferred, with homes

| Item | Home |
|---|---|
| `defect-ledger.sh` blank branch column on detached HEAD | this file; cosmetic |
| `worktree-take.sh` `ROOT` empty-string shape | this file; fails loud (`git -C ''` → 128), not destructive |
| `hv-guard-bash.sh` command-text false positives | §7 above |
| `load_rows` raw `{:?}` header-mismatch dump | this file; pre-existing, legibly distinct from value drift |
| `lab claim-status` exit-code contract (0 free / 1 held) | registry row `TOOL-claim-status-exit-code` — would remove every shell caller's prose dependency |
| `make board-post NOTE=` macro expansion executes `$(shell …)` | registry row `PROC-board-post-note-is-macro-expanded`, board lane |
| `gate-fast` mixes scoped and full runs under one label | this file; introduced by this campaign |
| The I7 doc test is brittle to prose rewrap | this file; introduced by this campaign |
| Nightly cleanup removes a human's untracked files | this file; deliberate and commented, but narrower than the guard reads |
| The scheduler is written, not installed | CLAUDE.md, in conditional language |
