# The Stylus — retrospective

**Merged:** 2026-08-20. Process lessons only. The result is in
[the chronicle](../../book/src/chronicle/the-stylus.md). This campaign also
lands [The Portolan part I](the-portolan.md) — absorbed as Task 0 because the
spec was written against a tree this branch did not have — which gets its own
retrospective for the same reason it gets its own chronicle entry.

## A correct artifact with absent coupling, four times

The campaign's strongest pattern, and three of the four instances are mine:

| instance | the artifact was right | nothing forced its use |
|---|---|---|
| Task 0's commit message | the merge itself was correct | the message described the approach I had *replaced* before dispatch |
| Task 2's Step 8 mutation probe | the mutation was valid | it targeted `render_with` while the prescribed test called `entry::draw`, which never calls it — the probe could not reach the test |
| Task 3's empty-submit guard | the guard returned before every mutation | the test asserted `snapshot()` and `released`, both of which `Session::handle("")`'s own no-op masks — it could not fail |
| the sluice's own `kind` fix (Lefford, cross-session) | the right kind was derived from the queue | derived *into a file* nothing read back; the operator hand-typed `merge` anyway |

The lesson is not "be careful." It is that a derived value a human may still
override is a suggestion, not a fix, and a check not *coupled* to the thing
it constrains will pass while that thing goes its own way.

## The spec was written against a tree its own branch did not have

Three spec claims were unrunnable as written: §0 quoted `action_for` from a
branch this one did not contain; §4 superseded an absent look mode; Task 4
re-pointed an absent sweep. Found by running `git merge-base --is-ancestor`,
not by re-reading — the check is mechanical: before planning, verify the
spec's quoted code exists in the branch's tree. Cost one added task (Task 0).

## A green test can assert the wrong property

Part I's keyspace sweep passed throughout the period when typing `look` sent
`go e` and dropped `ook`. Task 1 captured that green *deliberately* before
implementing, as evidence. A test asserting the wrong property is not a weak
test — it is a confident one.

## `make gate-commit` compiles none of `clients/`

Most of this campaign lived outside the cargo workspace; `make game-check` is
the gate that covers it. A green commit gate said nothing about the client,
every commit.

## PROC-12's merge driver emitted a confidently-wrong artifact, and the merge still succeeded

Full detail is in the idea-registry row and on the board as a `technique`.
Headline: absorbing `main` produced a conflict-free merge whose committed
`docs/audits/type-audit-report.md` read byte-identical to the pre-merge side,
while `origin/main` held different counts — and the correct merged value
existed on neither side. The driver ran (replaying with the driver disabled
conflicts on exactly that path), exited zero, and regenerated from a tree
that did not yet carry `main`'s source changes. Nothing flags a zero-exit
merge, and `git diff --exit-code` compares worktree to HEAD, both stale, and
reports clean. Merging without rebaselining would have regressed a sibling
campaign's committed artifact. Not this campaign's process control to fix;
registered as `PROC-merge-driver-regenerates-from-unmerged-tree`.

## Board hold-offs render as live long after they stop being true

All six board hold-off notices visible at submission were stale — every
authoring branch was already an ancestor of `origin/main`. Verified by
ancestry, not believed. One ("origin/main IS WRONG RIGHT NOW — DO NOT PULL")
had been rendering as live for four days. Two argued over decision numbers
0134-0136 while `main` stood at 0158.

## A stage gate pushed to main

`req-a7f00ebe4859`, submitted `kind=stage`, ended `landed` and moved `main
ffc247f3a..5ed9b5f21` — an incomplete campaign sat on `main` between Stage 1
and Task 3, with a client that had no keyboard route to end a session. The
plumbing was sound (`sluice-run.sh:728` honours `kind`); the failure was a
hand-typed `merge` at the exec line, the second occurrence that day. Nathan
ruled no revert. The operational lesson: **verify a queue row's claim by
ancestry rather than by reading its state column** — the row itself was
wrong, not the code that read it. Registered as
`PROC-stage-request-should-read-its-own-queue-row`.

## `git checkout -- <file>` cost a task its uncommitted work, mid-mutation

It reverts the whole file to `HEAD`, not just the mutation — a recurrence of
a lesson already recorded from [The Axes](../../book/src/chronicle/the-axes.md),
which means the lesson did not reach the agent doing the work. The fix must
therefore be mechanical, not mnemonic: registered as
`TOOL-mutate-restore-is-unsafe-by-default`, proposing `scripts/mutate.py`
grow a backup and a `--restore` path so the safe route is the default one.

## An unguarded `cd` reset the shell to the main checkout, on `main`

Happened mid-boundary, after removing a throwaway worktree. Caught only
because the next command printed `pwd && git branch` before doing anything
else. Nothing was committed to `main`.

## Deferred minors, with homes

- `input.rs:1-2`'s module-doc opener overstated `action_for`'s reach ("on its
  way to a verb line" — only `Action::Submit` becomes one). Fixed in this
  close, not deferred, since it is new prose this campaign introduced in a
  crate whose docs are load-bearing.
- `Driver::apply` briefly carried eight identical empty match arms — a shape
  the plan mandated for a task boundary, dead on arrival until a later task
  gave each arm a real body. Self-resolved, but worth keeping as a process
  observation: **a plan can mandate a shape that is dead on arrival for two
  tasks**, and nothing caught that gap until the arms filled in.
- One boundary case is hand-traced correct but unpinned by a test: a line
  exactly as wide as the pane.
- ~~A multi-byte glyph preceding the caret in the render path~~ — STRUCK on
  the final review. Every index in the path was traced: `write_command_line`
  builds a `Vec<char>` and does all window arithmetic in char counts,
  `write_line` is `line.chars().enumerate()`, `wrap` measures with
  `word.chars().count()`, and the echo block is `text.chars().collect()`.
  There is no byte offset anywhere in `entry.rs`, so a multi-byte-but-
  single-column glyph cannot misalign anything — carrying this as debt would
  only invite someone to "fix" code that is already correct.
- The echo-clipping logic counts `char`s, not display *width* — that is a
  real gap, but it is not a single-function bug: `wrap`, `write_line`,
  `write_command_line`, `strip::draw`, and the echo block are ALL
  char-counted, so a wide (e.g. CJK) glyph would misalign every one of them
  the same way, and fixing the echo alone would make one function disagree
  with the four around it. Too broad, and too crate-spanning, for a
  retrospective bullet to usefully carry — moved to
  [[CLIENT-display-width-not-char-count]], where a reader looking for open
  work will actually find it.
- The windowing rule is documented on the private `write_command_line`, not
  summarised on the public `draw`.
