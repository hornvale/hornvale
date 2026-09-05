# Decision ledger — The Sluicegate (`campaign/the-sluicegate`)

Scope: move the merge queue's state machine to a Rust tool. Spec:
[2026-09-05-the-sluicegate-design.md](../specs/2026-09-05-the-sluicegate-design.md).
No decision block reserved yet — none is owed until a decision is actually
minted, and the spec proposes none before G3.

## #1 [G1] — how much of the sluice moves

**Question**: which parts of the merge queue become a Rust tool?

**Decision**: the state machine (`sluice-queue.sh`, later `sluice-mouth.sh`).
`sluice-run.sh`'s process supervision stays in bash and is named a non-goal.

**Why**: the campaign that motivated this took four chamber runs to land one
fix, and every failure after the first was in the tests, not the fix — two of
the three ambient-environment failures (`HV_SLUICE_REPO_ROOT`,
`HV_SLUICE_CLAIMED`). None was in `sluice-run.sh`'s signal handling, flock
acquisition, worktree lifecycle or phase loop. Rewriting 987 lines of working
process supervision to fix defects that lived elsewhere trades three known bugs
for an unknown number.

**Alternatives discarded**:

- **Everything, including `sluice-run.sh`** — the non-goal above; also the most
  likely way this becomes a month rather than a campaign.
- **Extract only `claim` as a binary** — surfaced by the ideonomy pass and NOT
  considered before it. Smallest possible change and it targets the exact
  invariant, but it splits one state machine across two languages, so a future
  change to the row format or the locking discipline must be made correctly in
  both. Worse than either neighbour on the scale.
- **Change the on-disk format too (e.g. SQLite for free transactions)** —
  rejected: it couples a correctness fix to a data migration, breaks
  `queue-watch.sh`/`sluice-status`/drain simultaneously, and makes rollback
  impossible. The TSV stays.

**ideonomy passes / overturns**: 1 pass (negation + abstraction-lift, organon
scale, prompts rate/connectivity/purpose). **0 overturns of the decision, 1
material correction to its justification** — see #2, which is the pass's real
product.

**Capture actions**: registry row `TOOL-sluice-queue-in-rust`; the spec; this
ledger; the reconciliation row the new spec owes.

## #2 [Q] — what the migration may honestly claim

**Question**: did bash *cause* the defects this campaign is reacting to?

**The spec's first draft said yes, in effect** — that a read-modify-write
spanning a process boundary and coordination through exported environment "are
the two things a process-per-verb shell script cannot express". The
abstraction-lift falsified it. Strip the domain and the shape is a shared
mutable resource coordinated by short-lived processes through ambient inherited
state, with an invariant spanning more than one process lifetime; the standard
answer is to make the transaction boundary and the process boundary coincide,
and that is **language-agnostic**.

**Decision**: the spec argues **cost, not capability**. Neither defect required
Rust to fix — the TOCTOU fix shipped in bash the same night, and the leak's
real fix (a request ID, `PROC-stage-request-should-read-its-own-queue-row`) is
also achievable in bash. What Rust buys is that the bad shapes become hard to
*express* and cheap to *test*, and we have paid the cost of their being easy
four times inside one campaign.

**Why it matters rather than being a nicety**: a spec that overstates its
motivation invites a reviewer to accept a migration on a premise that will not
survive the first person who asks "so why did you fix it in bash last night?".
This campaign has already had one decision (0766) approved on a false premise
and then withdrawn; the cheap moment to catch the second one is now.

**ideonomy passes / overturns**: this entry IS the product of #1's pass; no
separate pass was run for it.

**Capture actions**: spec §1 rewritten; spec §2.1 added naming the discarded
`claim`-only alternative.

## #3 [Q] — the flock dependency, twice claimed and twice wrong

**Question**: does a Rust implementation need a new crate for file locking?

**Answered by running it, after asserting the opposite twice.** I told Nathan
in conversation, and wrote into the registry row, that `std` has no file
locking and a `libc`/`rustix` dependency would be required. `std::fs::File::lock`
stabilised in 1.89 and the repo pins **1.96.1** via `rust-toolchain.toml`:

```
$ rustc --version        # inside the repo
rustc 1.96.1 (31fca3adb 2026-06-26)
$ ./locktest
std File::lock() WORKS — no dependency needed
```

The first test said otherwise because it ran in `/tmp`, outside the repo, and
picked up the 1.77.1 default toolchain. **A toolchain claim is only true where
`rust-toolchain.toml` applies**, and a scratch directory is exactly where it
does not.

**Also verified, because a phased migration depends on it**: Rust's lock and
`flock(1)` are the same lock, so bash and Rust callers can coexist mid-migration.

```
rust: holding the lock
flock(1) while rust holds it: REFUSED — good, they share the same lock
flock(1) after release: ACQUIRED — good
```

**ideonomy passes / overturns**: 0 — a factual question with a command that
settles it, stated rather than left blank.

**Capture actions**: registry row corrected; spec §3.4 quotes both runs.

## Follow-ups

- `sluice-mouth.sh`'s five-valued exit is a contract (`sluice-run.sh` offsets it
  by 20). Second wave, and it needs its own compatibility check.
- The two stale `held` rows belonging to other campaigns (`the-hallmark`
  2026-09-02, `the-plat` 2026-09-04) are resolved in substance but are not this
  operator's rows to retire.
