# The Sexton — design

**Campaign:** The Sexton. **Base:** `main` @ `02ccf86a`.

A sexton rings the bells on schedule, keeps the yard swept, and buries the
dead. This campaign does the first two and lays the groundwork for the third:
it restores automatic scheduling to a project that deleted its scheduler, makes
visible a category of cost no instrument currently records, and starts the
dataset that every future retirement decision will need.

---

## §1 Why this campaign exists

`docs/timings.md` has been accumulating for a month and nobody had summed it.

```
  label         runs   total_wall_h   mean_wall_s   share
  -----------  -----  -------------  ------------  ------
  gate           368          43.89         429.4   57.0%
  census          34          13.50        1429.3   17.5%
  rebaseline     247          12.22         178.1   15.9%
  heavy            4           5.77        5195.3    7.5%
  ci               9           1.66         664.3    2.2%
  TOTAL                       77.04
```

Seventy-seven hours of measured human waiting between 2026-07-13 and
2026-08-13. Three facts fall out of that table and its per-host companion, and
each one contradicts something the project currently believes.

### §1.1 The fast box is idle

```
  host            runs   wall_h   cores
  MacBookPro       368    34.18      10
  ambrose          190    15.77      12
  lefford           70    22.86      40   <- 3.1% of the 744-hour window
  Greyjoy           27     3.23       ?
```

**Fifty hours of waiting happened on 10–12 core laptops while a 40-core machine
sat idle 97% of the time.** Decision 0086 put gates on the Mac and heavy work on
lefford because lefford's other jobs are long; the ledger says lefford's other
jobs occupy it for under an hour a day. This campaign does not move the gate —
that is S11, deferred — but it removes the reason the premise held.

### §1.2 The census is scheduled at the worst possible moment

The two most recent census refreshes:

```
  61442eda  "the authorized refresh"   3 columns ADDED    / 205   (1719 s wall)
  cc576f08  "the Rill's refresh"       3 columns MODIFIED / 205  (19208 s wall)
```

The Rill spent **five hours and twenty minutes to move three columns.** The
other 202 were recomputed to confirm they had not moved — which is the
determinism guard doing its job, at the dataset's price, on the critical path,
at campaign close, when a human is most eager to finish. It is the one job in
the repo with no reason to block anybody.

And the cost split inside it is not where anyone assumed. Measured on `ambrose`,
`main`, 2026-08-13:

```
  cargo run -p hornvale -- new --seed 42                    1.60 CPU-s
  lab run, 4 worlds x 1 Full-rung metric        6.55 CPU-s -> 1.64 CPU-s/world
  lab run, 4 worlds x all 205 metrics         125.37 CPU-s -> 31.34 CPU-s/world
```

**World generation is 5% of census cost; metric extraction is 95%.** Every
worldgen optimisation campaign is aiming at the wrong 5% where the census is
concerned.

### §1.3 The ledger cannot see its own largest blind spot

```
  live worktrees:                      3    target/ across them:  43 GB
  distinct branches in the ledger:    73    compilation cache:    none
  rows labelled `prewarm`:             0    shared target dir:    none
```

`docs/timings.md` records gate, rebaseline, census, ci and heavy — and nothing
else. Every cold worktree build is invisible to it. CLAUDE.md's own measured
figure for a full workspace test build is 771 s; at 73 branches in a month,
even assuming only half got a fresh worktree, that is **roughly eight hours of
entirely unrecorded waiting** — comparable to the census line, and absent from
every decision ever made about cost.

This project has been bitten by exactly this before. Decision 0086's amendment
records that `docs/timings.md` carried *zero* `gate` rows while the gate crept
from 234 s to 934 s: "the ledger built to catch a suite creeping '65s -> 43.5
min' was never wired to the most-run expensive command in the repo." The lesson
was applied to the gate and never generalised.

### §1.4 The systemic cause: 0125 deleted a scheduler, not a runner

Every verification activity in the repo sits at the fully-controlled end of the
autonomy axis, and CLAUDE.md documents each one as a thing a human must
remember:

| activity | evidence in CLAUDE.md |
|---|---|
| `make rebaseline` | "docs/audits/ … omitting it is a common miss" |
| the drift check | "nothing runs it for you" |
| board sync | "Nothing syncs the board for you" |
| board binary rebuild | "nothing rebuilds its binary for you" |
| board tests | "Nothing automatically runs the board's tests" |
| census refresh | push a SHA, ssh, commit there, pull back |

Decision 0125 deleted `.github/workflows/` because runner minutes are metered
on a private repo. **But compute was never the scarce resource** — §1.1 shows a
40-core box idle 97% of the time. What was actually lost was *something
remembering to run things*, and the replacement has been six paragraphs of
prose asking humans to remember instead.

Stated at the level it belongs: *a scheduling function was bundled inside a
metered execution service; deleting the service deleted the scheduler, and the
scheduler was the half with no substitute.*

This campaign builds the substitute, on hardware the project already owns, at
zero marginal cost.

---

## §2 Scope

Nine moves. All are small, mutually independent, and touch no ratified
doctrine. Five are **enabling**: they create datasets that later, larger
decisions need in order to be evidence-based rather than guesses.

| # | Move | Limb |
|---|---|---|
| S13 | A test that every generated directory has an index entry | sweep |
| S14 | Ledger every command that makes a human wait | sweep |
| S15 | Recycle worktrees instead of destroying them | sweep |
| S16 | A systemd-timer scheduler on lefford | bells |
| S1 | A three-world census sentinel in the commit gate | bells |
| S2 | The census runs nightly, not at campaign close | bells |
| S3 | `regenerate-artifacts.sh` runs as a DAG, not a list | sweep |
| S4 | Fold `make ci`'s instrumentation into `make gate` | bells |
| S5 | A committed defect ledger | burial (groundwork) |

### §2.1 Explicitly out of scope, and why

| Deferred | Reason |
|---|---|
| S6 serialize Mac gates | Reverses decision 0081. Wants its own record and fresh `cpu_ratio` rows, which S14 supplies. |
| S7 gate / gate-fast purpose split | Reverses standing doctrine ("`make gate` gates commits"; "gate-fast is ITERATION ONLY"). Nathan's call, not a work item. |
| S8 content-addressed verdict cache | Its value rests on an unmeasured hit rate. S5 supplies the data; build it after. |
| S9 inverted seam-guard ("which tests guard nothing") | Depends on S5's catch-rate data to be defensible rather than a guess. |
| S10 census guard / dataset split | The larger census restructuring. S2 captures most of its human-wait value first, at a fraction of the effort. |
| S11 gate execution on lefford | Its blocker is 0086's premise, which S2 expires. Sequence after, with S14's data. |
| S12 seed count 1000 → 300 | **Fidelity carve-out.** Flagged in §6, decided by Nathan only. |
| S17 preregistered census-column diff | Wants S10's column store. The highest-value coverage idea; next campaign. |
| S18 name the suite's life cycle as policy | A decision record, not work. |
| sccache | Admissible under 0040's dev-tool precedent, but see §3.3 — the low-risk move dominates it. |

---

## §3 The moves

### §3.1 S13 — a test that every generated directory is indexed

**Goal.** Close a known vacuity permanently.

**Mechanism.** CLAUDE.md documents the hazard in its own words: "`git diff
--exit-code <path>` is silently **VACUOUS** against a path with no index entry,
so the FIRST commit that introduces a new generated directory must `git add` it
before the check can ever fail. Nothing in regenerate-artifacts.sh guards
that." Add a workspace test that reads the drift-check path list and asserts
each has at least one tracked file.

**Why it leads.** The artifact drift check is the load-bearing verification in
this repo — simplest system that would still work: one command produces
artifacts, `git diff --exit-code` compares them. It is also the one with no CI,
no schedule, and a documented history of being forgotten. This is the cheapest
item in the campaign and it protects the most.

**Direction the check enforces**, stated in its own doc comment per the
campaign-autopilot rule: it asserts *every declared path is tracked*. It is
structurally blind to a generated directory nobody declared.

### §3.2 S14 — ledger every command that makes a human wait

**Goal.** Make §1.3's eight invisible hours visible.

**Mechanism.** `scripts/timed.sh` already wraps `gate`, `ci`, `rebaseline` and
`census`. Extend it to `prewarm`, `preflight`, `gate-fast`, `quick`, and the
client checks (`vessel-check`, `world-check`, `game-check`). Same row format,
same ledger.

**Risk.** Row-count growth. `docs/timings.md` is already at 782 lines and
CLAUDE.md warns gates "will dominate the ledger by row count." Mitigation: this
campaign adds no new reader requirement — `scripts/timed.sh report <label>`
already filters — but the follow-up register should carry the eventual need to
compact or shard the ledger.

### §3.3 S15 — recycle worktrees instead of destroying them

**Goal.** Remove most of the cold-build cost with zero new tooling.

**Mechanism.** A small pool of long-lived worktrees under
`.claude/worktrees/`, reassigned with `git switch` rather than created and
destroyed per campaign. Documented in CLAUDE.md's process section; a
`make worktree-take NAME=<campaign>` convenience target.

**Why this and not a compiler cache.** This was an ideonomy overturn and it is
worth recording. A shared `RUSTC_WRAPPER` is *admissible* — decision 0004 binds
"the **workspace** depends on serde/serde_json only", and a dev tool is exactly
the category decision 0040 used to admit nextest. But the cost is caused by
**destroying worktrees**, not by lacking a cache: 73 branches against 3 live
worktrees. Recycling captures most of the saving with no new determinism
surface, and **a compiler cache that ever returns a wrong object file is a
silent determinism violation** — the worst bug class in this repo, and a poor
first instrument for a saving a scheduling change already gets. sccache goes to
the registry with that risk stated, not into this campaign.

**Interaction to respect.** `.superpowers/sdd/` is per-worktree and git-ignored;
recycling a worktree must sweep it, or a campaign inherits the previous
campaign's ledger. CLAUDE.md already warns that a shared regeneration worktree
on lefford needs its HEAD verified before reuse — same discipline, now local.

### §3.4 S16 — a scheduler on lefford

**Goal.** Restore what 0125 deleted, on hardware the project owns.

**Mechanism.** systemd timers. Verified present on the box:
`systemd 252 (252.39-1~deb12u2)`, with cron also available. Timers are chosen
for `journalctl` (a failed run leaves a durable, queryable record) and
`Persistent=true` (survives a reboot). A nightly job whose failure is invisible
would reproduce the exact pathology this campaign exists to fix.

**The constraint that makes this safe.** **A scheduled job never commits, and
never touches `main`.** It produces artifacts, writes its result, and posts to
the board; a human commits. Direct precedent: decision 0129's lane rule — "the
lane must never be wired to auto-implement a suggestion… that would make the
board self-modifying with no human in the loop, on the one channel every session
reads at `SessionStart`." Same argument, same channel. The concrete hazard this
forecloses is a nightly job committing while a session is mid-landing, which
`make preflight` warns about and cannot prevent.

**Constitutional note.** The wall-clock ban governs the *sim*, not the
infrastructure. Scheduling sits outside the determinism boundary, exactly as
`clients/` does.

**Initial timer set.** Nightly census on `main` (§3.6); nightly uncached full
gate on `main`; nightly drift sweep (`make rebaseline` + the `git diff
--exit-code` list, reporting only); `make board-sync`; orphan-worktree report.

### §3.5 S1 — a three-world census sentinel in the commit gate

**Goal.** Catch census drift at the commit that caused it, not five hours into
a campaign close.

**Mechanism.** A gate test runs the full metric roster over three seeds and
compares against **the first three rows of the committed
`book/src/laboratory/generated/the-census/rows.csv`.** No new artifact: the
sentinel's expectation is derived from the census's own committed rows, so it is
automatically correct the moment a census lands and adds nothing to
`regenerate-artifacts.sh`. Comparison goes through the existing
`canonicalize_row` in `windows/lab/src/runner.rs`, whose doc comment states it
exists to "canonicalize a row for comparison with fixture-loaded rows".

**Cost.** Measured: 31.34 CPU-s/world all-metric on ambrose, so three seeds
≈ 94 CPU-s ≈ 8 s wall on twelve cores. The reference band it must be judged
against, same host, from the ledger: **`make gate` on `ambrose` runs
489–805 s** (the spread is contention, not work — see §2.1's S6 row). Eight
seconds is ~1.6% of the fastest of those.

**Feasibility, and why it is not blocked by 0079.** Decision 0079 enforces that
census goldens are *authored* on one host because the machines disagreed on
~0.1% of discrete-count metrics. **Decision 0090 refines that and the audit came
back clean:** the divergence was traced to `f64::floor()` dispatching to
per-host glibc on the default `x86-64` codegen baseline, fixed by the baseline
pin at `3a7092c3`, and a 40-world all-metric probe is byte-identical between
x86_64/Linux and aarch64/Darwin. The sentinel only *reads*; 0079's write-path
enforcement is untouched.

**The bonus, and the risk, are the same thing.** The sentinel turns 0090's
one-time audit into a continuous one, over metrics that did not exist when that
audit ran. If a newly-added metric is host-divergent, the sentinel reddens on
the Mac — which is precisely the failure 0079 feared and could not detect. That
needs a **declared-waiver escape hatch on the `tropes check` / seam-guard
ratchet pattern** (a waiver must carry a reason; a reasonless one is a parse
error), never a disabled test.

### §3.6 S2 — the census runs nightly

**Goal.** Take the census off the campaign-close critical path.

**Mechanism.** A systemd timer on lefford runs the census against `main` nightly,
writes `rows.csv` and the summary into a scratch location, runs `make lab-diff`
against the committed copy, and **posts the diff to the board**. It does not
commit. A campaign close then reads the morning's result instead of waiting five
hours for one.

**Consequence for CLAUDE.md.** The "census re-pin at close" ritual becomes "read
last night's diff; if it is empty, you are done." The commit of a moved column
remains a deliberate human act on the canonical box, unchanged by this campaign.

### §3.7 S3 — `regenerate-artifacts.sh` as a DAG

**Goal.** Recover most of the 12.2-hour rebaseline line.

**Mechanism.** The script is 62 sequential `cargo run` invocations. Measured
`cpu_ratio` across recent rebaseline rows is **0.72–2.11 on 10–12 core boxes** —
it is effectively serial, and nobody decided that; it is an artifact of the
script being a shell list. Most invocations are independent; several share the
same seed-42 world and re-derive it in separate processes. Restructure into
declared stages with explicit dependencies and run each stage's members
concurrently.

**Determinism.** Outputs are distinct files, so ordering does not affect bytes.
The success criterion is `make rebaseline` leaving every generated artifact
byte-unchanged — the same falsifier The Whetstone used for its profile change.

### §3.8 S4 — fold `make ci` into `make gate`

**Goal.** Stop discarding 368 measurements a month.

**Mechanism.** `make gate` emits the `libtest-json-plus` stream and feeds the
existing timings alarm and `ci-record`. The ordering constraint the Makefile
already documents at length is preserved: the alarm compares against the
baseline still on disk, *then* the recorder overwrites it, and a red run never
becomes a baseline.

**Why.** `make ci` has run **9 times against `make gate`'s 368.** The Timekeeper
built a per-test duration alarm to watch a gate that crept from 234 s to 934 s,
and the instrument runs at 2.4% of the frequency of the thing it watches — while
every gate already computes the durations it needs and throws them away.

**Blind spot this also improves.** The Timekeeper's open follow-up #1 is that
the contention guard "cannot see ordinary load." With 368 samples a month
instead of 9, the load-aware suppression it wants becomes calibratable.

### §3.9 S5 — a committed defect ledger

**Goal.** Start the only dataset that can make S7, S9 and S12 evidence-based.

**Mechanism.** On a red gate, append the failing test ids and the changed-crate
set to `docs/timings/defects-<host>.tsv`. Committed and per-host, matching
`test-baseline-<host>.tsv`, for the reason CLAUDE.md gives for that file: "`git
log -p` on it is the archaeology of how the suite's cost moved over time."

**Why it matters more than it looks.** Every scheduling or retirement idea in
the deferred list needs one number nobody records: **has this test ever caught
anything?** `docs/timings.md` has six RED rows and each records only *that* it
was red. Without this, "retire that test" and "run this one every tenth gate"
are opinions. With a few months of it, they are actuarial.

---

## §4 Success criteria

1. `make gate` includes the sentinel and its wall-clock addition on `ambrose`
   is `≤ 2%` of the 489 s floor recorded in §3.5 — i.e. `≤ ~10 s`, measured on
   a quiet box against a same-host before-arm, never against another host's
   baseline.
2. `make rebaseline` is byte-identical to today's output, at a measured
   `cpu_ratio > 4` on a twelve-core box.
3. A red gate leaves a row in `docs/timings/defects-<host>.tsv`; a green gate
   leaves none.
4. Every command in §3.2's list leaves a `docs/timings.md` row.
5. A new generated directory with no index entry reddens the gate.
6. The lefford timers run for seven consecutive nights, and a deliberately
   introduced census drift is reported on the board without any human
   invocation and without any commit.
7. `make ci` and `make gate` produce the same baseline effects; running both
   is no longer necessary.

---

## §5 Interaction with live campaigns

**The Glasshouse** holds a board `hold-off` on `domains/astronomy/`,
`domains/climate/`, `domains/terrain/` and intends an epoch that will move
census values. The Sexton touches none of those paths. The sentinel's golden is
census-*derived* rather than pinned, so a Glasshouse census refresh updates the
sentinel's expectation for free — the coupling is by construction, not by hand.

**The Holdfast** is optimising worldgen byte-identically by declaration, so it
cannot interact with any assertion here.

A board `notice` is posted for `scripts/`, `Makefile`, `docs/timings.md`,
`docs/timings/`, `windows/lab/` and `cli/tests/`.

---

## §6 Flagged for review

- **§3.5's escape hatch is determinism-contract-adjacent.** A waiver mechanism
  on a cross-host reproducibility check is the kind of thing that, mis-specified,
  lets exactly decision 0079's silent failure back in. It should be reviewed as
  a contract, not as a convenience.
- **S12 (1000 seeds → 300) is a fidelity carve-out and is NOT in this
  campaign.** Recording the arithmetic so the option stays visible: a proportion
  carries a ±3.1% confidence half-width at n=1000 and ±5.7% at n=300, for a 3.3×
  cut. It should not be touched until the precision requirement is stated per
  metric class, and it is Nathan's call alone.
- **S15's worktree recycling changes a documented process rule** (CLAUDE.md's
  "Campaigns run in git worktrees under `.claude/worktrees/<campaign>/`"). Low
  stakes, but it is a process change rather than a tooling one.
- **Low-confidence assumption:** the ~8 hours of unrecorded prewarm in §1.3 is
  an *estimate* (73 branches × CLAUDE.md's measured 771 s, halved for reuse),
  not a measurement. S14 exists precisely to replace it with one, and no
  decision in this campaign depends on its magnitude.

---

## §7 Decisions this campaign will propose

None are minted here. Candidates for ratification at close, subject to the
number-collision hazard registry row `PROC-decision-number-collision` (check
`origin/main` before minting):

- *A scheduled job never commits.* (§3.4's constraint — the durable form of the
  0129 lane argument, generalised from the board to all automation.)
- *Every command that makes a human wait leaves a ledger row.* (The
  generalisation of 0086's amendment that was never made.)
