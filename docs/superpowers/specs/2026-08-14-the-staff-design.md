# The Staff — one lane on the canonical box, and a taxonomy for what runs in it

**Campaign:** The Staff · **Date:** 2026-08-14 · **Status:** Draft for review

On a single-line railway section, only the train holding the *staff* may enter.
There is one lefford. This campaign gives it one staff.

Continues the metaphor of The Siding (decision
[0086](../../decisions/0086-the-heavy-tier-runs-on-the-canonical-box.md)), which
this spec amends.

---

## 1. The problem, in measured terms

Decision 0086 split the machines by placement: heavy tier and censuses on
lefford, campaigns and the commit gate on the Mac. Three things have since made
the Mac half of that ruling expensive.

**The Mac saturates and lefford does not.** A single `make gate` reports
`cpu_ratio` **8.25–8.50 on ten cores** — nextest already has the whole machine.
The same gate on lefford reports **18.6–32.2 on forty** (17 rows in
`docs/timings.md`, 251–593 s wall). So two concurrent Mac gates cost roughly
double each and both look hung, which is why CLAUDE.md documents a working
ceiling of two to three campaigns and a *human*-staggering rule rather than a
lock. During The Timekeeper that rule was observed to fail: loadavg 42–63.

**The timing baseline has already forked three ways.** CLAUDE.md names this as
an open blind spot — the baseline is keyed on `hostname -s`, so a new host takes
a silent free pass on its first run and then measures against itself. It is no
longer hypothetical:

```
docs/timings/test-baseline-ambrose.tsv
docs/timings/test-baseline-MacBookPro.tsv
docs/timings/test-baseline-lefford.tsv
```

Three files, three claims about what the suite costs, none comparable to the
others. The Whetstone already ranked the suite off the wrong host's file and
named the wrong hot crate for it.

**A dozen suites exist and three are dispatchable.** `make heavy-remote` covers
the heavy tier; the census is a raw `ssh` line memorised from CLAUDE.md;
everything else runs wherever the session happens to be sitting. Three suites
run *nowhere*: `tools/board` (~198 `#[test]` functions; CLAUDE.md says 194 and
that nothing runs them), `tools/digest`, and `clients/atlas` — the last named
by decision [0125](../../decisions/0125-github-actions-is-retired.md) as having
"no gate at all".

## 2. The ruling this spec asks for

**Every nontrivial check runs on lefford, in one strictly serial lane. The
Macs refuse to run them.**

| Machine | Runs |
|---|---|
| MacBookPro, ambrose | editing, review, `make quick` (~16 s), `cargo test -p <crate>`, `cargo check` |
| lefford | one lane: `gate`, `gate-fast`, `artifacts`, `outboard`, `clients`, `heavy`, `census` |

This amends 0086's placement table and reverses 0081's advisory carve-out for
the gates. It does not touch 0079 (goldens authored on one enforced host) or
0063 (the canonical platform), both of which it strengthens.

### 2.1 The costs, stated rather than engineered around

Two consequences follow directly from the ruling. Both were presented with
their arithmetic and accepted deliberately; neither is mitigated in this
campaign, and neither should be discovered later as a surprise.

**A gate can queue behind an hour of work.** One lane means a ~7-minute gate
may wait behind a ~46-minute heavy tier or a census that has historically
ranged from 949 s to 19,207 s. This is precisely the arithmetic decision 0081
declined ("waiting twelve minutes to start a four-minute gate is worse than the
contention"). It is accepted here because the lane is *leavable*: §4 makes
dispatch asynchronous, so the wait costs queue position, not attention. The
reconciliation that would have removed the wait — letting a timer enqueue the
census overnight via the already-written, never-installed `scripts/scheduled/`
— was offered and declined; the census stays a deliberate human act.

**lefford unreachable means no gate anywhere.** The guard fails closed. There
is deliberately no `HV_LANE_FORCE=1` escape: an override that exists is an
override that gets used under deadline, and the whole value of a single lane is
that every recorded result is comparable. The documented recovery is to fix
lefford or to change one line in the roster file, in a reviewable commit — the
same posture `scripts/census-canonical-host.sh` already takes.

## 3. The set taxonomy

Sets are named by **the guarantee they provide**, and grouped by **the direction
that guarantee moves in over time** — which is what determines when in the
workflow it must run. A guarantee that *decays* needs a clock; one that *drifts*
needs the boundary that moves it; one that *accumulates* needs every change.
Cost is deliberately not the organising axis; cost is a property of a set, not
its identity.

```
                                                             authors
  set        direction      scope          guarantee          artifacts   cost*
  --------------------------------------------------------------------------
  style      steady         workspace      policy / format       no       ~16 s
  gate       accumulating   workspace      behaviour             no       ~7 min
  artifacts  drifting       repo           committed output     YES       ~4 min
  outboard   steady         repo           the guards work      YES         ?
  clients    accumulating   CROSS-REPO     the ABI surface      YES         ?
  heavy      accumulating   CROSS-PLATFORM deep behaviour       YES      ~46 min
  census     decaying       CROSS-PLATFORM the instrument       YES   16 min - 5 h
```

`*` Cost is a **hint, not a claim**. The four figures shown come from
`docs/timings.md` rows. The two marked `?` have never been measured as a set and
are deliberately left blank rather than guessed: `outboard` carries
`make seam-guard`, whose cost scales with its roster — currently 2 seams over
**8 call sites**, each costing a full scoped test run — and `clients` gains a
step nothing has ever timed. Measuring both is task 1 of the plan. A committed
cost figure is a claim with a date, and this project has been wrong by 2.2x and
by 6.7x reading one that nobody re-derived.

`style` is the one set that stays **local**: it is under the one-minute line, it
is what the pre-commit hook already runs, and a lane round-trip for a formatting
check would be pure ceremony.

Composition is kept as a **separate relation** from the taxonomy, because mixing
"kinds of guarantee" with "contains" is exactly today's tangle — `gate-full` is
`gate` + `heavy` (part-of) while `heavy:` versus `probe:` is sub-type, and both
live under `#[ignore]`:

```
  commit     = style (local)  ->  gate
  integrate  = gate + artifacts + outboard + clients
  close      = integrate + heavy + census
```

### 3.1 What each set contains

- **style** — `cargo fmt --check`, `cargo clippy -D warnings`, `type-audit
  check`, `type-audit` report freshness, `shellcheck`. Today's `make quick` plus
  shellcheck, which currently runs when someone remembers.
- **gate** — `cargo nextest run --workspace` + doctests + the duration alarm +
  `ci-record`. Today's `make gate`, unchanged in content.
- **artifacts** — `scripts/regenerate-artifacts.sh` (census-skipped) and the
  `docs/generated-paths.txt` drift check. Today's `make rebaseline`.
- **outboard** — the suites that guard the repo from outside the cargo
  workspace: `tools/board` (~198 tests, run by nobody today), `tools/digest`,
  `tools/type-audit`, and `make seam-guard`. **New set; closes a 0125 gap.**
- **clients** — `vessel-check`, `world-check`, `game-check`, and `clients/atlas`
  (`deno fmt --check`, `deno lint`, `deno task check`, `deno task test`, plus
  `deno task build` and the `atlas.js` bundle diff). **atlas is new; the other
  three move.**

  **A latent vacuous drift check, found while checking this table.**
  `book/src/gallery/atlas.js` is committed and `book/src/gallery/` is declared
  in `docs/generated-paths.txt` — so it *looks* drift-checked. But **nothing
  regenerates it**: `grep -n atlas scripts/regenerate-artifacts.sh Makefile`
  returns nothing. `make rebaseline` never writes the file, so the drift check
  that follows always reports clean regardless of whether `clients/atlas/src/`
  moved. It is in sync today only by luck — source and bundle were last touched
  in the same commit, `eb25fe52`, 2026-07-09 — so this is a latent hazard rather
  than a live defect, and it is the same shape CLAUDE.md warns about for the
  digest's `render` subcommands ("running one of these bare regenerates nothing,
  so the drift check reports an empty diff and reads as 'no drift' when in fact
  nothing was rebuilt"). Including `deno task build` in this set is what closes
  it. The plan must verify the closure the way CLAUDE.md prescribes: mutate the
  source, confirm the diff goes **red**. An empty diff needs a positive control.
- **heavy** — `scripts/gate-full-heavy.sh`, unchanged.
- **census** — `scripts/census-run.sh`, unchanged.

### 3.2 The roster is one file with several readers

`scripts/lane-sets.tsv` defines every set: name, rung, where it may run,
whether it authors artifacts, a cost hint, and its command. The Makefile, the
dispatcher, the host guard, and the enforcement test all read that one file.

This follows `scripts/census-canonical-host.txt` (one file, a shell reader and a
Rust reader, so the canonical hostname is never written twice) and
`docs/generated-paths.txt` (the single source of truth that no guide restates,
enforced by `cli/tests/generated_paths.rs`). TSV rather than JSON because
`/bin/sh` on lefford is `dash` and must parse it without a tool.

## 4. Dispatch

```bash
make lane SET=gate REF=<full-sha>        # enqueue; prints a job id; RETURNS
make lane RUNG=integrate REF=<full-sha>  # enqueue the rung's sets, in order
make lane-status                         # who holds the staff, who is waiting
make lane-log [JOB=<id>]                 # read a finished job back
make lane-wait JOB=<id>                  # opt-in blocking, never the default
```

**`REF` is a full SHA, never a branch name.** `heavy-remote` and `census-run.sh`
both learned this the hard way: the ref feeds `reset --hard`, which can land on a
stale local branch of that name on lefford. Push first.

**Dispatch detaches.** `scripts/lane-run.sh` forks a `setsid` child, prints the
job id, and exits — so a dropped ssh costs nothing. This is the residual problem
after §5's measurement: ordering is already solved, but `flock -w` pins its
caller, and with waits now measured in tens of minutes a pinned caller is a lost
run. The Siding already paid for this once (a run launched under `| tail -40`
buffered, died 60 s in, and looked alive for an hour).

**Evidence by construction.** Every job writes `<job-id>.log` and appends an
outcome row — enqueued, started, finished, rc, wall, waited, sha, set — to a
durable jobs ledger, on every exit path including signals. This generalises
`heavy-run.sh`'s existing `runs.tsv` and its EXIT/INT/TERM/HUP traps, which
exist because observing an expensive run through the *caller's* plumbing means
any surprise costs the whole run again.

**Warm worktrees, keyed by branch.** A cold build in a fresh worktree is a
measured 771 s and would dominate a 420 s gate. Lane worktrees persist per
branch under an LRU cap, the `make prewarm` precedent that
`PROC-gate-on-the-canonical-box` already names.

## 5. What measurement changed about this design

I was about to specify a ticket-spool runner, on the belief that `flock` gives
no ordering guarantee and therefore could not deliver "strict serial order".
Two trials on lefford — six spaced waiters, then **eight simultaneous** waiters
against a held lock — granted strictly in arrival order, 8/8. The existing claim
in `census-run.sh` and `gate-full-heavy.sh` **already is** a strict serial lane.
The spool component is deleted.

This is the campaign-autopilot rule about verifying tool-behaviour claims paying
off at drafting time rather than at review, and the spec records it because the
deleted component is invisible in the final design.

## 6. The guard

`scripts/census-canonical-host.sh` already fails closed on hostname for two job
kinds. It generalises to `require_canonical_host <set>`, reading the roster, and
is wired into `gate`, `gate-fast`, `gate-full`, `rebaseline`, the three client
checks, `census-check`, and `seam-guard` — and **not** into `quick` or
`cargo test -p`.

The refusal names the exact dispatch line for the set the caller tried to run.

### 6.1 The workflow this changes

The gate today runs against a dirty working tree before a commit. Under the
hard lock it runs against a **pushed SHA**, so the loop becomes:

```
  edit  ->  make quick (hook, local, ~16 s)  ->  commit  ->  push
        ->  make lane SET=gate REF=<sha>  ->  read back  ->  amend if red
```

**This makes the gate a post-commit, pre-merge instrument rather than a
pre-commit one.** That is a real reversal of the standing rule "`make gate`
still gates commits", and it is the same question the idea registry carries as
`PROC-gate-purpose-split` — flagged there as Nathan's call. It is not being
smuggled in as a side effect: it is a direct and unavoidable consequence of the
hard lock, and it is flagged for the G3 review on that basis. Campaign branches
are already disposable and rewritable, so amending a red commit is cheap.

## 7. What this fixes for free

- **One timing baseline.** Every gate runs on one host, so the three forked
  `test-baseline-*.tsv` files collapse to one and CLAUDE.md's blind spot (2) —
  a renamed host silently taking a free pass — stops existing.
- **The alarm becomes trustworthy.** Blind spot (1) is that the duration guard
  asks only whether a *census claim* is held, so parallel sessions are invisible
  and it will enforce against thoroughly contended timings. Under strict
  serialisation nothing else is running, so the question it asks is finally the
  question that matters.
- **Three suites gain a gate.** `tools/board`, `tools/digest` and
  `clients/atlas` go from "run by nobody, ever" to a rung.

## 8. Testing

Each of these asserts a *direction*, named in its own doc comment, because a
guard that states only "it works" is silently mistaken for total.

1. **Exclusion, not merely a lock file** — a second job waits while the first
   holds. Extends `scripts/test-heavy-lock.sh`, which already proves this shape.
2. **Order** — N simultaneous enqueues complete in arrival order. This pins §5's
   measurement as a property rather than leaving it as a claim in prose.
3. **Detachment** — kill the dispatching ssh; the job still completes and still
   records its outcome.
4. **The guard refuses** — a *positive control*. Asserting that the guard passes
   on lefford proves nothing; the test must show it goes red on a non-canonical
   host, driven by an env override of the roster's host field.
5. **Roster completeness** (`cli/tests/lane_sets.rs`) — every set's command
   exists; every dispatchable check belongs to exactly one set; CLAUDE.md still
   names the roster file and does not restate it. The `generated_paths.rs`
   precedent, including its hazard: a `git diff --exit-code` against an
   untracked path is silently vacuous.
6. **wasm-opt acceptance** — lefford's `wasm-opt` output must be byte-identical
   to the Mac's before `world-check`'s size gate is trusted there. binaryen is
   not currently installed on lefford (`command -v wasm-opt` returns nothing),
   which is why the size gate would otherwise measure an unoptimized binary.

## 9. Risks

| Risk | Disposition |
|---|---|
| A gate queues behind an hour of heavy/census work | Accepted, §2.1. Dispatch is asynchronous so the cost is position, not attention. |
| lefford down = no gate anywhere | Accepted, §2.1. No force override, deliberately. |
| Cold worktree build (771 s) dominates a 420 s gate | Mitigated by per-branch warm worktrees; **must be measured**, not assumed. |
| lefford's `wasm-opt` differs from the Mac's | Acceptance step, §8.6. Blocks moving `world-check` until proven. |
| An adopted orphan suite is red right now | Expected. A red `tools/board` or `atlas` is a real finding for the chronicle, not a topology problem. |
| The gate becomes post-commit | Flagged at G3 as the lead item, §6.1. Reverses a standing rule. |

## 10. Out of scope

- Installing `scripts/scheduled/` (declined at #5; the registry row stands).
- Any lane split, priority tier, or preemption. Decision 0086 rejected ranking a
  4-minute job against a 40-minute one, and one lane was chosen deliberately.
- `PROC-risk-weighted-test-schedule`, `TOOL-inverted-seam-guard`,
  `TOOL-suite-fixture-split` — all reduce what the lane *carries*, which is a
  different campaign from where it runs.
