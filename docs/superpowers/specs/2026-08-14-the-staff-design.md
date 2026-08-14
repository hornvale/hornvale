# The Staff — three gates by purpose, one lane on the canonical box

**Campaign:** The Staff · **Date:** 2026-08-14 · **Status:** Draft for review

On a single-line railway section, only the train holding the *staff* may enter.
There is one lefford. This campaign gives it one staff — and, first, stops
sending it work that never needed to leave the Mac.

Continues the metaphor of The Siding (decision
[0086](../../decisions/0086-the-heavy-tier-runs-on-the-canonical-box.md)), which
this spec amends. Resolves `PROC-gate-purpose-split`, which the idea registry
records as Nathan's call rather than a work item.

---

## 1. The problem

### 1.1 The commit gate is priced like a merge gate

This is the campaign's central finding and it reframed everything after it.
Summed from `docs/timings.md`:

```
  gate        n=417    49.0 h of Mac time      avg 423 s
  quick       n=15      0.1 h                  avg  16 s

  the same 417 runs as `quick` instead:   49.0 h  ->  1.8 h
```

**Roughly 47 hours a month of saturating Mac work exists because a per-commit
instrument runs a merge-gate workload.** A single `make gate` reports
`cpu_ratio` **8.25–8.50 on ten cores** — nextest already owns the machine — so
two concurrent gates cost about double each and both look hung. Every
consequence the project has been treating as a separate problem descends from
this one mispricing: the two-to-three campaign ceiling, the human-staggering
rule, loadavg 42–63 during The Timekeeper, and the anxiety about the gate
drifting 234 s → 934 s.

`make gate-fast`, documented as "ITERATION ONLY", measures **381 s against the
full gate's 423 s** — 10% cheaper (n=4, which is all the evidence there is). It
does not deliver its stated purpose and the purpose split leaves it no job.

### 1.2 The timing baseline has already forked three ways

CLAUDE.md names this as an open blind spot — the baseline is keyed on
`hostname -s`, so a new host takes a silent free pass on its first run and
thereafter measures only against itself. It is no longer hypothetical:

```
docs/timings/test-baseline-ambrose.tsv
docs/timings/test-baseline-MacBookPro.tsv
docs/timings/test-baseline-lefford.tsv
```

Three files, three claims about what the suite costs, none comparable. The
Whetstone already ranked the suite off the wrong host's file and named the wrong
hot crate for it.

### 1.3 A dozen suites exist and three are dispatchable

`make heavy-remote` covers the heavy tier; the census is a raw `ssh` line
memorised from CLAUDE.md; everything else runs wherever the session is sitting.
Three suites run **nowhere**: `tools/board` (~198 `#[test]` functions),
`tools/digest`, and `clients/atlas` — the last named by decision
[0125](../../decisions/0125-github-actions-is-retired.md) as having "no gate at
all".

## 2. The ruling

**Three gates, named for what they gate. One lane for the two that cost
minutes.**

```
  commit gate    LOCAL, seconds        every commit
                 lints + tripwires + the sub-floor test tier

  merge gate     LANE, minutes         branch integration, plan-stage boundaries
                 gate + artifacts + outboard + clients

  close gate     LANE, tens of minutes pre-merge
                 merge + heavy + census
```

**The Macs run the commit gate and nothing above it.** `make gate`,
`gate-full`, `rebaseline`, the client checks, `census-check` and `seam-guard`
**refuse** on a Mac and print the dispatch line. `make gate-fast` is retired.

**The lane is one strictly serial queue on lefford.** Every job in it takes one
shared claim, first-come-first-served, no exceptions and no priority tiers.

This amends 0086's placement table, reverses 0081's advisory carve-out for the
gates, and resolves `PROC-gate-purpose-split`. It does not touch 0079 or 0063,
both of which it strengthens.

### 2.1 The costs, stated rather than engineered around

**A merge gate can queue behind an hour of work.** One lane means a ~7-minute
merge gate may wait behind a ~46-minute heavy tier or a census that has ranged
from 949 s to 19,207 s. This is precisely the arithmetic decision 0081 declined.
It is accepted because §5 makes dispatch asynchronous — the wait costs queue
position, not attention — and because §1.1 removes the *frequent* caller from the
lane entirely. The reconciliation that would have removed the wait (a timer
enqueuing the census overnight, via the written-but-never-installed
`scripts/scheduled/`) was offered and declined; the census stays a deliberate
human act.

**lefford unreachable means no merge gate anywhere.** The guard fails closed.
There is deliberately no `HV_LANE_FORCE=1`: an override that exists is an
override used under deadline, and one comparable result set is the whole point.
The recovery is to fix lefford or change one line in the roster, in a reviewable
commit — the posture `scripts/census-canonical-host.sh` already takes.

## 3. The set taxonomy

Sets are named for **the guarantee they provide** and grouped by **the direction
that guarantee moves in over time**, which is what determines when in the
workflow it must run. A guarantee that *decays* needs a clock; one that *drifts*
needs the boundary that moves it; one that *accumulates* needs every change.
Cost is deliberately not the organising axis — cost is a property of a set, not
its identity.

```
                                                             authors
  set        direction      scope          guarantee          artifacts  cost*
  --------------------------------------------------------------------------
  style      steady         workspace      policy / format       no      ~16 s
  subfloor   accumulating   workspace      fast behaviour        no        ?
  gate       accumulating   workspace      full behaviour        no      ~7 min
  artifacts  drifting       repo           committed output     YES      ~4 min
  outboard   steady         repo           the guards work      YES        ?
  clients    accumulating   CROSS-REPO     the ABI surface      YES        ?
  heavy      accumulating   CROSS-PLATFORM deep behaviour       YES     ~46 min
  census     decaying       CROSS-PLATFORM the instrument       YES  16 min - 5 h
```

`*` Cost is a **hint, not a claim.** The figures shown come from
`docs/timings.md` rows. The three marked `?` have never been measured as sets
and are left blank rather than guessed — `outboard` carries `make seam-guard`,
whose cost scales with its roster (measured: **2 seams over 8 call sites**, each
costing a full scoped test run); `clients` gains a step nothing has timed; and
`subfloor`'s first measurement is §4.2, which came in **12.8× over its
estimate**. Measuring all three is task 1 of the plan.

`style` and `subfloor` are the two sets that stay **local**: together they are
the commit gate.

Composition is kept as a **separate relation** from the taxonomy, because mixing
"kinds of guarantee" with "contains" is exactly today's tangle — `gate-full` is
`gate` + `heavy` (part-of) while `heavy:` versus `probe:` is sub-type, and both
live under `#[ignore]`.

### 3.1 What each set contains

- **style** — `cargo fmt --check`, `cargo clippy -D warnings`, `type-audit
  check`, the type-audit report freshness check, the conflict-marker and
  stream-manifest tripwires, `shellcheck`.
- **subfloor** — every test whose recorded duration is below
  `BASELINE_FLOOR_SECS` (1.0 s). See §4.
- **gate** — `cargo nextest run --workspace` + doctests + the duration alarm +
  the baseline rewrite. Today's `make gate`, unchanged in content.
- **artifacts** — `scripts/regenerate-artifacts.sh` (census-skipped) and the
  `docs/generated-paths.txt` drift check. Today's `make rebaseline`.
- **outboard** — the suites guarding the repo from outside the cargo workspace:
  `tools/board` (~198 tests, run by nobody today), `tools/digest`,
  `tools/type-audit`, and `make seam-guard`. **New set; closes a 0125 gap.**
- **clients** — `vessel-check`, `world-check`, `game-check`, and `clients/atlas`
  (`deno fmt --check`, `deno lint`, `deno task check`, `deno task test`, plus
  `deno task build` and the `atlas.js` bundle diff). **atlas is new.**

  **A latent vacuous drift check, found while checking this table.**
  `book/src/gallery/atlas.js` is committed and `book/src/gallery/` is declared
  in `docs/generated-paths.txt`, so it *looks* drift-checked. But **nothing
  regenerates it** — `grep -n atlas scripts/regenerate-artifacts.sh Makefile`
  returns nothing. `make rebaseline` never writes it, so the drift check that
  follows always reports clean whatever `clients/atlas/src/` did. It is in sync
  today only by luck: source and bundle last moved in the same commit,
  `eb25fe52`, 2026-07-09. Latent, not live — but it is a green check that cannot
  go red, the shape this repo has now written down four times. Adding
  `deno task build` closes it; the plan must verify the closure by mutating the
  source and confirming the diff goes **red**.
- **heavy** — `scripts/gate-full-heavy.sh`, unchanged.
- **census** — `scripts/census-run.sh`, unchanged.

### 3.2 The roster is one file with several readers

`scripts/lane-sets.tsv` defines every set: name, rung, where it may run, whether
it authors artifacts, a cost hint, and its command. The Makefile, the
dispatcher, the host guard and the enforcement test all read that one file.

This follows `scripts/census-canonical-host.txt` (one file, a shell reader and a
Rust reader, so the canonical hostname is never written twice) and
`docs/generated-paths.txt` (the single source of truth no guide restates,
enforced by `cli/tests/generated_paths.rs`). TSV rather than JSON because
`/bin/sh` on lefford is `dash` and must parse it without a tool.

## 4. The commit gate

### 4.1 The tier already exists

The Timekeeper's baseline machinery already computes and commits the partition
this needs. `windows/lab/src/timings.rs`:

```rust
pub const BASELINE_FLOOR_SECS: f64 = 1.0;
```

and `docs/timings/test-baseline-lefford.tsv` carries the fold:

```
<below-floor>    58.211 s    2073 tests
604 tracked tests            6879 s
```

**77% of the suite's tests cost 58 CPU-seconds combined; the other 23% cost
6,879.** The tier is derived from committed data and rewritten by every green
merge gate, so it is self-maintaining rather than hand-kept — which matters,
because `cli/tests/heavy_tier.rs` already carries two guards whose whole job is
catching hand-maintained rosters drifting, and its own module doc calls one of
them "a FLOOR, NOT A CENSUS".

### 4.2 The measurement, which was 12.8× over the estimate

Estimated ~14 s wall. Measured, on a warm Mac worktree, selecting by excluding
the 604 tracked ids from `test-baseline-lefford.tsv`:

```
  178.6 s wall     2,961 tests     cpu_ratio 4.35
```

Two causes, one fatal and one that this campaign removes:

**The baseline is 1,670 commits stale.** Recorded at `b7ce5941`; it knows 2,677
tests against today's ~3,565. **~888 tests (25%) are absent from it** and
defaulted to *included*, several of them slow. This is the project's own
documented failure — a committed baseline is a claim with a date — at a larger
multiple than the last time it was recorded.

**It describes lefford and was run on a Mac.** §2's single-host ruling removes
this confound by construction.

### 4.3 Unknown tests default to EXCLUDED

A test with no baseline row is **not** in the commit gate. It enters on the next
green merge gate, which measures it and rewrites the baseline.

This inverts the repo's usual default-deny instinct (type-audit, `tropes check`,
the seam-guard verdict) and the inversion is deliberate: **the commit gate is a
speed tier, not a coverage guarantee.** Coverage is the merge gate's job, and it
runs the whole suite regardless. Defaulting to *included* is what produced the
178.6 s above. Defaulting to *excluded* makes the tier self-healing and bounds
its cost by construction.

The residual risk is honest and must be stated in the plan: **a test written and
committed but never merge-gated never runs in the commit gate.** The mitigation
is not a mechanism, it is the merge gate's cadence — CLAUDE.md already requires
absorbing main at every plan-stage boundary.

## 5. Dispatch

```bash
make lane SET=gate REF=<full-sha>        # enqueue; prints a job id; RETURNS
make lane RUNG=merge REF=<full-sha>      # enqueue the rung's sets, in order
make lane-status                         # who holds the staff, who is waiting
make lane-log [JOB=<id>]                 # read a finished job back
make lane-wait JOB=<id>                  # opt-in blocking, never the default
```

**`REF` is a full SHA, never a branch name.** `heavy-remote` and `census-run.sh`
both learned this: the ref feeds `reset --hard`, which can land on a stale local
branch of that name on lefford. Push first.

**Dispatch detaches.** `scripts/lane-run.sh` forks a `setsid` child, prints the
job id and exits, so a dropped ssh costs nothing. This is the residual problem
after §6's measurement — ordering is already solved; `flock -w` pinning its
caller is not. The Siding paid for this once already (a run launched under
`| tail -40` buffered, died 60 s in, and looked alive for an hour).

**Evidence by construction.** Every job writes `<job-id>.log` and appends an
outcome row — enqueued, started, finished, rc, wall, waited, sha, set — on every
exit path including signals, generalising `heavy-run.sh`'s `runs.tsv` and its
EXIT/INT/TERM/HUP traps.

**Warm worktrees, keyed by branch** — subject to §7, which is the reason this
sentence is not as safe as it sounds.

## 6. What measurement changed about this design

Three claims were checked with a command rather than reasoned about. Two were
wrong, and the spec records them because a deleted component is invisible in the
final design.

**`flock` orders correctly — the ticket-spool runner is deleted.** I was about
to specify one, believing flock gives no ordering guarantee and therefore could
not deliver "strict serial order". Two trials on lefford — six spaced waiters,
then eight **simultaneous** waiters against a held lock — granted strictly in
arrival order, 8/8. The existing claim in `census-run.sh` and
`gate-full-heavy.sh` **already is** a strict serial lane.

**The sub-floor tier cost 178.6 s, not ~14 s** (§4.2).

**A recycled worktree serves stale binaries** (§7).

## 7. The worktree pool serves stale binaries — absorbed into this campaign

`make worktree-take` recycles a pool member by `git switch -c` and then, at
`scripts/worktree-take.sh:132`, `mv "$recycled" "$DEST"` — it **renames the
directory** to the campaign name and **preserves `target/`**, which is the whole
point (a cold build is a measured 771 s).

But `env!("CARGO_MANIFEST_DIR")` and `CARGO_TARGET_TMPDIR` are **compile-time
constants**, and cargo does not treat a renamed worktree as a reason to rebuild:
`cargo build --workspace --all-targets` reported everything fresh. So every
cached test binary still points at the previous campaign's path. **31 files**
across `kernel/ domains/ windows/ cli/` use one of the two macros — **29 under
`tests/`, 2 under `src/`**, which is what makes the fix cheap.

Taking `the-axes` → `the-staff` produced six failures that read exactly like a
red main:

```
hornvale-alchemy::draws_nothing            domain_draws_nothing
hornvale-worldgen::proto_goblinoid_golden  (x2)
hornvale-terrain::channel_golden           the_channel_network_is_pinned
hornvale-vessel::session_snapshot          (x2)
```

The panic names the old path outright:
`reading /Users/.../worktrees/the-axes/domains/alchemy/src: No such file or
directory`. **Positive control**, same commit and tree: cached binary FAIL;
`touch domains/alchemy/tests/draws_nothing.rs` and it PASSES.

**Why this is in scope rather than adjacent.** §5 proposes warm per-branch
worktrees on lefford — the same mechanism, where a spurious red is far more
expensive because the lane's verdict is the one everybody trusts.

**The guarantee to implement, not the mechanism.** A worktree handed out by the
pool, or created by the lane, must never serve a binary compiled under a
different path. The plan names the property and lets the implementer find the
mechanism after reading — a plan author does not know which invalidation cargo
honours, and both prescribed-mutation guesses in The Quire were nulls. Candidate
directions, in increasing order of blast radius: touch the 31 grep-derived files
after the `mv`; `cargo clean -p` the ten affected crates; stop renaming
altogether and carry campaign identity in the branch rather than the path.

**The acceptance test is the positive control above**, generalised: take a pool
worktree, run the six tests, and require them GREEN — with a companion arm
proving the test can go red, since a check that only ever passes proves nothing.

## 8. The guard

`scripts/census-canonical-host.sh` already fails closed on hostname for two job
kinds. It generalises to `require_canonical_host <set>`, reading the roster, and
is wired into `gate`, `gate-full`, `rebaseline`, the three client checks,
`census-check` and `seam-guard` — and **not** into the commit gate.

The refusal names the exact dispatch line for the set the caller attempted.

### 8.1 The workflow this produces

```
  edit  ->  commit gate (local, seconds)  ->  commit  ->  push
        ->  make lane RUNG=merge REF=<sha>  ->  read back  ->  amend if red
```

The merge gate runs against a **pushed SHA**. Campaign branches are disposable
and rewritable, so amending a red commit is cheap.

## 9. What this fixes for free

- **One timing baseline.** Every merge gate runs on one host, so the three
  forked `test-baseline-*.tsv` files collapse to one and CLAUDE.md's blind
  spot (2) stops existing.
- **The duration alarm becomes trustworthy.** Blind spot (1) is that the guard
  asks only whether a *census claim* is held, so parallel sessions are invisible
  and it enforces against contended timings. Under strict serialisation nothing
  else is running, so it finally asks the question that matters.
- **Three suites gain a gate** — `tools/board`, `tools/digest`, `clients/atlas`.
- **The atlas drift check stops being vacuous** (§3.1).
- **Recycled worktrees stop emitting spurious reds** (§7).

## 10. Testing

Each asserts a *direction*, named in its own doc comment, because a guard that
states only "it works" is silently mistaken for total.

1. **Exclusion, not merely a lock file** — a second job waits while the first
   holds. Extends `scripts/test-heavy-lock.sh`.
2. **Order** — N simultaneous enqueues complete in arrival order, pinning §6's
   measurement as a property rather than leaving it prose.
3. **Detachment** — kill the dispatching ssh; the job still completes and still
   records its outcome.
4. **The guard refuses** — a *positive control*. That it passes on lefford
   proves nothing; the test must show it goes red on a non-canonical host.
5. **Roster completeness** (`cli/tests/lane_sets.rs`) — every set's command
   exists; every dispatchable check belongs to exactly one set; CLAUDE.md names
   the roster file and does not restate it. Carries `generated_paths.rs`'s
   hazard: `git diff --exit-code` against an untracked path is silently vacuous.
6. **The commit gate's budget** — the sub-floor tier is asserted under a wall
   ceiling, so §4.2 cannot silently recur.
7. **Worktree freshness** — §7's acceptance test, with its red arm.
8. **wasm-opt acceptance** — lefford's `wasm-opt` output must be byte-identical
   to the Mac's before `world-check`'s size gate is trusted there. binaryen is
   not installed on lefford today (`command -v wasm-opt` returns nothing), which
   is why the size gate would otherwise measure an unoptimized binary.

## 11. Risks

| Risk | Disposition |
|---|---|
| A merge gate queues behind an hour of heavy/census work | Accepted, §2.1. Dispatch is asynchronous; §1.1 removes the frequent caller from the lane. |
| lefford down = no merge gate anywhere | Accepted, §2.1. No force override, deliberately. |
| A test never merge-gated never enters the commit gate | Accepted, §4.3. Bounded by merge-gate cadence, which CLAUDE.md already requires. |
| Cold worktree build (771 s) dominates a lane job | Mitigated by warm per-branch worktrees; **must be measured**, and §7 must land first or the mitigation carries the bug. |
| lefford's `wasm-opt` differs from the Mac's | Acceptance step, §10.8. Blocks moving `world-check` until proven. |
| An adopted orphan suite is red right now | Expected. A red `tools/board` or `atlas` is a real finding for the chronicle, not a topology problem. |
| Retiring `gate-fast` removes a tool someone relies on | n=4 says it is 10% cheaper than the full gate. If that is wrong, the evidence is one measurement away and the retirement is one revert away. |

## 12. Out of scope

- Installing `scripts/scheduled/` (declined; the registry row stands).
- Any lane split, priority tier, or preemption. 0086 rejected ranking a
  4-minute job against a 40-minute one, and one lane was chosen deliberately.
- `PROC-risk-weighted-test-schedule`, `TOOL-inverted-seam-guard`,
  `TOOL-suite-fixture-split` — all reduce what the lane *carries*, a different
  campaign from where it runs and when.
