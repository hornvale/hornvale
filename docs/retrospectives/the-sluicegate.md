# The Sluicegate — retrospective

Process lessons. The product is described in
[the chronicle](../../book/src/chronicle/the-sluicegate.md).

## The tally, and where it came from

Five tasks, four task-level fix rounds, one final fix wave. **Every defect that
reached a reviewer came from the plan's prose or the controller's own test
code. None came from an implementer's code.** The count is roughly fifteen:

- a `Cargo.toml` missing the empty `[workspace]` table every sibling tool
  carries — I had read one of those siblings during the pre-dispatch check,
  saw the table and its comment, and did not carry it into the plan;
- `Store::lock()` moved into Task 2 in the Interfaces line and nowhere else,
  so the implementer would have called a method with no code to write;
- Task 2 titled for two verbs and specifying one;
- a `claim` snippet using a nested `if let` that fails the lint the plan itself
  mandates;
- `Row::parse` specified to drop a short line, where the shell it replaces pads
  and keeps it — silent request loss;
- and its mirror, missed at the same time: a line with more than seven fields
  truncated permanently on the next write;
- `set_state` specified without the lock the shell version takes;
- Task 4 gating only the crate's tests, on a crate nothing else lints;
- the census path omitted from Task 5's file list, which would have left every
  census running unbookkept;
- a `sha="${2:?…}"` ordering that fires before the branch that would have made
  it optional;
- a test grepping captured output for a line the script prints *after*
  reassigning stdout to a log file;
- a race test at 1000 concurrent threads, which would have exhausted file
  descriptors on any macOS checkout;
- two expected-count off-by-ones;
- a verification step written as `grep … | grep -v '^scripts/CLAUDE.md'`,
  excluding the single occurrence that would have failed it;
- a spec success criterion relaxed by 97% without amending the criterion.

## The three lessons worth carrying

**1. A test can encode the defect as expected behaviour, and then defend it.**
This is the campaign's central finding and it is stronger than the lesson it
extends. The final review found that a run resolved from a request identifier
proceeded without holding its row. That was not merely untested: restoring the
pre-fix test file against the fixed scripts yields *221 passed, 1 failed* —
the failing test being one I wrote, which seeded its row `queued` and asserted
that the run **proceeded**. Every green run of that suite was evidence for the
bug. The remedy is not "test more"; it is that a test asserting a *positive*
outcome must be read for what it permits, and that a guard needs its negative
control written at the same time.

**2. The same bug class recurred three times, each time through a new surface.**
An exported variable inherited by a nested process; a build on the hot path
making a failure indistinguishable from success; and an identifier that
inherited the assumption the variable had encoded without asserting it. The
reviewer's phrasing is the durable form: *the campaign fixed the variable leak
and inherited the assumption the variable encoded, without ever asserting it.*
When you remove a mechanism, name what it was asserting and assert it
explicitly, or the assumption survives without its enforcement.

**3. Nothing was found by re-reading.** Every defect above died to a command —
a `grep` of the directory being edited, a reproduction of a race, a differential
run of old against new, checking out an old test file and running it. The
controller's own three wrong diagnoses this session followed the same pattern:
each was plausible, confidently held, and settled by one command. The
pre-dispatch brief check (three minutes of grep against the live tree, one task
ahead) caught five plan defects before they cost a fix round; it earned its
place every time it ran.

## Process observations

- **The controller skipped Task 4's task review** and went straight to a fix
  round on its own reading. The reading found a real problem; it is not the
  same instrument, and the overdue review then found a Critical. Recovered by
  reviewing the full range afterwards.
- **An over-absolute instruction produced a bad fix.** "Fix the Rust, not the
  test; do not edit the test file" is right for behavioural regressions, and
  this failure was a test-fixture artifact — so the implementer, with no other
  route, added a `$HOME` cache to a production script. The instruction caused
  it; the fix round said so.
- **`make gate-commit` does not lint an excluded crate.** Task 1 committed
  with a live `-D warnings` failure and a formatting diff through a green gate.
  Nothing in the repository would have caught it; Task 4 added the gate lines.
- **A repo guard blocked a Bash call because the commit message contained a
  phrase resembling a forbidden command.** The commit succeeded via a message
  file; the ledger append in the same call never ran and had to be redone.
- **`rustup` resolves its toolchain override from the working directory, not
  from `--manifest-path`.** A build invoked from an unrelated directory
  silently uses the default toolchain. This bit twice — once as a wrong
  measurement in the spec's own dependency claim, once as a production bug in
  a build path the queue invoked.

## Deferred minors and where they landed

| Item | Outcome |
| --- | --- |
| `Cargo.toml` lacks `license`/`publish`/`[[bin]]` | carried forward; registry row below |
| `Row::parse` returned a vestigial `Option` | fixed in the final wave |
| `claim` refusals lost the 12-char sha | fixed in the final wave |
| Error-text prefix drift `sluice-queue:` → `sluice:` | accepted as-is |
| `HV_SLUICE_BIN` export never unset in the harness | accepted; production path covered by the ~1,200 lines preceding it |
| Unconditional release build in the harness | accepted; sits after the host-skip guard |
| `scripts/CLAUDE.md` describing the script as pure bash | fixed in the final wave |
| `sluice-census.sh`'s own `exit 3` colliding numerically with the shim's | carried forward; registry row below |

## What the close owes elsewhere

The queue now depends on a git-ignored binary that nothing builds
automatically, so `claim`, `set-state` and `list` all refuse the moment
lefford's main checkout fast-forwards — while `make sluice-status` keeps
working, so from another machine the queue looks alive. That is a procedure
change, recorded in `scripts/CLAUDE.md` and worth a human's attention at the
next fast-forward.
