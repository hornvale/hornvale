# The Sluicegate — the queue's state machine becomes a Rust tool

**Status:** draft, awaiting G3 · **Branch:** `campaign/the-sluicegate` ·
**Registry:** `TOOL-sluice-queue-in-rust`, and it subsumes
`PROC-stage-request-should-read-its-own-queue-row`

## 1. The problem, as measured rather than as felt

On 2026-09-04/05 a single defect in the queue's dispatch interlock took **four
chamber runs** to land. The defect itself was one bug; **every failure after
the first was in the tests, and two of the three were ambient-environment
failures peculiar to shell**:

| run | outcome | cause |
| --- | --- | --- |
| 1 | reported `LANDED`, **did not land** | a test overrode `HV_SLUICE_REPO_ROOT` to the real repo; the nested `sluice-run.sh` reset the chamber's own worktree and discarded the merge product. main took the pre-merge tree and the chamber reported rc=0. |
| 2 | rc=11 `outboard` | two shellcheck findings (SC2317 on a trap-only function, SC2034 on an unused capture) |
| 3 | rc=11 `outboard` | `sluice-drain.sh` `export`s `HV_SLUICE_CLAIMED`; a nested `sluice-run.sh` inherited it, believed its row was already claimed, and **skipped the interlock** |
| 4 | landed | — |

Run 3 is the one that matters most: the leaked variable **reopened the exact
duplicate-execution hole the campaign existed to close**, one process level
down. It was not caught by review, by 218 passing tests, or by mutation-testing
every assertion — because every assertion was correct. The defect lived in the
environment they ran in.

The original defect was of the same family: `sluice-queue.sh next` selected a
row and a *separate* `set-state running` marked it, with the flock released
between them because each invocation is its own process. Two dispatchers, or a
dispatcher and a direct run, both saw an unclaimed row. Observed: one merge ran
twice, pids 1240741/1253175, two ~800 KB logs for `48aa9373b6f2`, both rc=0.

**The through-line is not "bash is bad", and the honest version of this argument
is narrower than the one this spec first made.** Lift the domain away and the
shape is: *a shared mutable resource coordinated by short-lived processes that
communicate through ambient inherited state, where the invariant spans more than
one process lifetime.* That shape recurs in database transactions, PID files,
cron jobs sharing `/tmp`, and CI runners, and its library answer is to **make
the transaction boundary and the process boundary coincide** — which is
language-agnostic.

So: **neither defect strictly required Rust to fix.** The TOCTOU fix is one
`claim` verb doing read-and-write under a single lock, and that shipped in bash
on 2026-09-05. The leak's real fix is passing a request ID rather than
exporting one, which is `PROC-stage-request-should-read-its-own-queue-row`, also
achievable in bash. A spec that claimed the language caused these bugs would be
overstating, and this one did before an ideonomy pass caught it.

The defensible claim is about **cost, not capability**: this component's
invariants are (a) a read-modify-write that must not straddle a process
boundary and (b) coordination state that must not be ambient. Shell makes both
of those cheap to violate and expensive to test — every verb is a fresh process
whose lock dies with it, and every variable is inherited by descendants nobody
enumerated. We have now paid that cost four times inside one campaign. The
argument for Rust is **prevention and testability**, not repair: a `claim()`
holding its lock across the read-modify-write, and a parameter that cannot leak
into a grandchild, make the bad shapes hard to *express* rather than merely
fixed once.

## 2. Scope

**Moves to `tools/sluice` (Rust):**

- `scripts/sluice-queue.sh` (454 lines) — the state machine: `add`, `claim`,
  `set-state`, `next`, `list`, ancestry coalescing, note sanitising, the closed
  `state`/`kind` vocabularies.
- `scripts/sluice-mouth.sh` (187 lines) — the admit/refuse predicates. Second
  wave; it is mostly `git merge-tree` plumbing and its five-valued exit is a
  contract several callers read.

**Stays in bash:**

- `scripts/sluice-run.sh` (987 lines) — the chamber. Its hard parts are signal
  handling (INT/TERM/HUP with an escalation deadline), `flock` acquisition,
  worktree lifecycle, subprocess supervision, and a phase loop that commits
  drift. That is shell's actual job, it was hard-won, and **no failure this
  campaign was in that logic**. Rewriting 987 lines of working process
  supervision to fix bugs that lived elsewhere trades three known defects for
  an unknown number.
- `scripts/sluice-drain.sh` (185 lines) — thin; becomes thinner still (§4).

### 2.1 One alternative worth naming, and why it loses

**Extract only `claim` as a small binary, leaving the rest of the state machine
in shell.** It targets exactly the invariant that needs the transaction and the
process to coincide, and it is the smallest possible change. It loses because it
splits one state machine across two languages: `add`'s ancestry coalescing and
`set-state`'s refusal-on-no-match would keep their shell implementations while
the row format and locking discipline they share moved, so a future change would
have to be made correctly in both. Smaller than the proposal, and worse than
either neighbour on that axis.

## 3. Design

### 3.1 One binary, verbs matching today's subcommands

`tools/sluice` exposes the same verbs `sluice-queue.sh` does, so callers change
their invocation and nothing else. The single behavioural change is that
`claim` is a *transaction* rather than a read: it selects and marks under one
lock, in one process, and returns the claimed row.

```
sluice add <branch> <sha> [kind]     -> request id
sluice claim [--sha <sha>] [note]    -> the claimed row, or nothing
sluice set-state <id> <state> [note]
sluice list
```

Exit codes `claim` already establishes and callers depend on: **4** = a row
exists but is not queued (somebody else holds it — refuse); **5** = no row at
all (an ad hoc run, which is allowed).

### 3.2 The request ID replaces the leaked variable

`PROC-stage-request-should-read-its-own-queue-row` proposed that
`sluice-run.sh` take a **request ID** and read branch/sha/kind out of the row,
so no operator-supplied value can disagree with the row that authorised the
run. That row was filed after an operator hand-typed `merge` for a `kind=stage`
request and landed it on main.

It is also the correct fix for run 3 above. `HV_SLUICE_CLAIMED` exists only
because the runner is told *what* to run rather than *which row authorised it*;
given an ID, there is no ambient variable to export, inherit, or unset. The
`unset` shipped on 2026-09-05 is a patch on a design this row already named.

So: `sluice-run.sh <request-id>` becomes the supported form. Branch/sha/kind
are read from the row. The positional form stays for ad hoc runs with no row.

### 3.3 The on-disk format does not change

`queue.tsv` stays a TSV with the same seven columns. `scripts/queue-watch.sh`,
`make sluice-status`, `sluice-drain.sh` and any operator's `awk` keep working
untouched **throughout** the migration and after it. Replacing the logic and
the format at once would make the migration irreversible and would couple a
correctness fix to a data migration; they are separate concerns and only one of
them is urgent.

### 3.4 No new dependencies — measured, not assumed

`std::fs::File::lock()` is available on the repo's pinned toolchain and needs
no crate:

```
$ rustc --version                       # inside the repo
rustc 1.96.1 (31fca3adb 2026-06-26)
$ ./locktest
std File::lock() WORKS — no dependency needed
```

And it is **the same lock** `flock(1)` takes, which is what makes a phased
migration safe while some callers are still shell:

```
rust: holding the lock
flock(1) while rust holds it: REFUSED — good, they share the same lock
flock(1) after release: ACQUIRED — good
```

This corrects a claim made twice during the proposal ("one flock dep,
`libc`/`rustix`"). It was wrong because the first test ran outside the repo and
picked up the 1.77.1 default rather than the pinned 1.96.1.

## 4. Testing

The point of the migration is testability, so the criterion is specific rather
than "unit tests":

- **Every verb is a library function over an injected state directory**, with
  no ambient environment read below the CLI layer. `HV_SLUICE_DIR` is parsed
  once at `main()` and passed down. A test constructs a `TempDir` and calls the
  function; it cannot accidentally address the real queue, which is the class
  of failure that discarded a merge product in run 1.
- **Concurrency is tested in-process** — N threads racing `claim` on one queued
  row, asserting exactly one winner. Today's shell equivalent spawns 12
  processes and takes seconds; the Rust one is milliseconds and can run
  thousands of iterations.
- **The existing shell suite stays** and keeps running in `outboard`. It is the
  integration check that the binary and its callers agree; the Rust tests are
  the unit check. Deleting it during migration would remove the only evidence
  the two agree.
- `tools/sluice`'s `cargo test` joins `lane-outboard.sh` beside the five tools
  already gated there (`board`, `digest`, `type-audit`, `placement-audit`,
  `seam-guard`).

## 5. Migration and rollback

1. `tools/sluice` implements the verbs; `scripts/sluice-queue.sh` becomes a
   thin shim that `exec`s the binary, so every existing caller is unchanged and
   the shell suite exercises the Rust implementation immediately.
2. Callers move to the binary directly; the shim stays until none reference it.
3. `sluice-run.sh` gains the request-ID form; `HV_SLUICE_CLAIMED` is deleted.
4. `sluice-mouth.sh` follows, or does not — it is separable and its value is
   lower.

**Rollback** is `git revert` of the shim commit at any point in 1–2: the TSV is
unchanged, so a reverted tree reads the same queue.

## 6. Success criteria

- Every verb has unit tests over an injected state dir; **no test can address
  the real queue or the real repo**, and the blast-radius guard shipped on
  2026-09-05 stays in the shell suite as the backstop.
- The atomicity property is tested in-process at >=1000 iterations without a
  double-claim.
- `sluice-run.sh` takes a request ID and no `HV_SLUICE_CLAIMED` exists in the
  tree.
- `scripts/queue-watch.sh` and `make sluice-status` are **unmodified** by the
  campaign — the proof that the format contract held.
- `tools/sluice` is gated in `lane-outboard.sh`.

## 7. Non-goals

- Rewriting `sluice-run.sh`'s process supervision.
- Changing the on-disk format, the queue's semantics, the phase lists, or
  anything about what a gate runs.
- A daemon, a scheduler, or any automation of what the operator decides. The
  queue orders and mechanises; **vetting stays human** (0139), and nothing here
  changes that.

## 8. Risks

- **A rewrite reintroduces solved bugs.** `sluice-queue.sh` encodes hard-won
  behaviour — ancestry coalescing that must never supersede a `running` row,
  three-valued `--is-ancestor` handling where 128 means "cannot resolve", note
  sanitising that strips tabs and newlines because one would corrupt every
  later read, and a `set-state` that refuses an unmatched id rather than
  silently succeeding. Each has a comment explaining a real incident. Mitigation:
  port the comments with the code, and keep the shell suite running against the
  binary throughout.
- **The mouth's five-valued exit is a contract.** Callers branch on 1/2/3/4 and
  `sluice-run.sh` offsets them by 20. Second wave, separately.
- **Scope creep into `sluice-run.sh`.** Named as a non-goal because it is the
  most likely way this campaign becomes a month.
