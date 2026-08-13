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
jobs occupy it for under an hour a day, so that premise is gone. **Idle is not
the same as fast, though** — §1.5 measures what this box actually is, and the
answer reshapes S11 from "move the gate" into "split it" (§3.11).

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

## §1.5 A correction this spec is built on

An earlier draft of this campaign argued that moving the gate to lefford would
floor it near 116 s, reasoning `4628 CPU-s / 40 cores`. **That is wrong, and
the ledger says so.** Every `gate` and `ci` row ever recorded on each host:

```
  make gate ON lefford (40 cores)   wall 243-594 s    cpu  6,945-12,090 CPU-s
  make gate ON the Macs (10-12)     wall 322-805 s    cpu  2,485- 3,873 CPU-s
```

**lefford burns 2-3x the CPU-seconds for the same suite** — its cores are much
slower individually, so core count buys throughput, never latency. Best-case
lefford is 243 s against the Macs' 322 s: a ~25-35% win, not 3x.

The error was dividing *one host's* CPU-seconds by *another host's* core count.
The repo already carries the rule that would have caught it — "a duration is
meaningless without its host", which is why `docs/timings/` keys baselines on
`hostname -s` — but it is stated about **wall time** and was applied only
there. **CPU-seconds are not portable across hosts either**, and this spec
states that as the generalisation.

Consequences, both load-bearing below: S11 shrinks from "move the gate" to
"split it" (§3.11), and no cross-host projection in this document rests on
arithmetic rather than on a recorded row.

---

## §2 Scope

Thirteen moves in four stages. The staging is not administrative — it is the
argument. **Stages 1-3 are cheap, doctrine-free instrumentation and scheduling;
Stage 4 is where the four hard decisions get made, and it is deliberately last
because three of the four rest on premises the earlier stages either supply
evidence for or destroy.**

| Stage | Name | Moves |
|---|---|---|
| 1 | **See** | S14 ledger every waiting command · S5 defect ledger · S13 index-entry test · S4 fold `ci` into `gate` |
| 2 | **Sweep** | S3 parallel `regenerate-artifacts.sh` · S15 recycle worktrees |
| 3 | **Bells** | S16 scheduler on lefford · S2 nightly census · S1 census sentinel |
| 4 | **Decide** | S7 gate purpose split · S18 suite life cycle · S11 split gate across hosts · S12 census sampling |

### §2.1 The honest limit of Stage 1

S5 and S14 are instruments, and an instrument installed today has no readings
today. **This campaign cannot close the loop on them.** It lands them, states
what each is for, and hands the readings to a later campaign. Saying so here
prevents the failure this project has already recorded twice — a bound cited
from a measurement nobody re-took (`PROC-floors-erode-unseen`), and an
instrument shipped and then never watched (`make ci`, 9 runs against 368).

### §2.2 Still out of scope

| Deferred | Reason |
|---|---|
| S6 serialize Mac gates | Reverses decision 0081, whose reopening criterion is fresh `cpu_ratio` rows. S14 supplies them; the decision belongs to the campaign that reads them. |
| S8 verdict cache | Value is entirely a hit rate, obtainable by replaying the ledger's 368 gate SHAs. Measure in Stage 1, build later. |
| S9 inverted seam-guard | The death organ of S18. Needs S5's catch-rate data to be defensible; S18 ratifies the *policy* here, S9 builds the tool after. |
| S10 census column store | The larger census restructuring. S2 captures its human-wait value first at a fraction of the effort; S17 and S12's better form both want it. |
| S17 preregistered column diff | Wants S10's column store. Highest-value coverage idea in the backlog. |

---

## §3 The moves

### Stage 1 — See

#### §3.1 S13 — a test that every generated directory is indexed

**Goal.** Close a known vacuity permanently.

**Mechanism.** CLAUDE.md documents the hazard in its own words: "`git diff
--exit-code <path>` is silently **VACUOUS** against a path with no index entry,
so the FIRST commit that introduces a new generated directory must `git add` it
before the check can ever fail. Nothing in regenerate-artifacts.sh guards
that." Add a workspace test that reads the drift-check path list and asserts
each has at least one tracked file.

**Why it leads.** The artifact drift check is the load-bearing verification in
this repo — the simplest system that would still work is *one command produces
artifacts, `git diff --exit-code` compares them*. It is also the one with no CI,
no schedule, and a documented history of being forgotten.

**Direction the check enforces**, in its own doc comment per the
campaign-autopilot rule: it asserts *every declared path is tracked*. It is
structurally blind to a generated directory nobody declared.

#### §3.2 S14 — ledger every command that makes a human wait

**Goal.** Make §1.3's eight invisible hours visible.

**Mechanism.** `scripts/timed.sh` already wraps `gate`, `ci`, `rebaseline` and
`census`. Extend it to `prewarm`, `preflight`, `gate-fast`, `quick`, and the
client checks (`vessel-check`, `world-check`, `game-check`). Same row format,
same ledger.

**Risk.** Row-count growth; `docs/timings.md` is already 782 lines. No new
reader requirement is added (`scripts/timed.sh report <label>` filters), but
eventual compaction goes to the follow-up register.

#### §3.3 S5 — a committed defect ledger

**Goal.** Start the only dataset that can make S7, S9 and S12 evidence-based.

**Mechanism.** On a red gate, append the failing test ids and the changed-crate
set to `docs/timings/defects-<host>.tsv`. Committed and per-host, matching
`test-baseline-<host>.tsv`, for the reason CLAUDE.md gives for that file:
"`git log -p` on it is the archaeology of how the suite's cost moved over time."

**Why it matters more than it looks.** Every scheduling or retirement idea in
this document needs one number nobody records: **has this test ever caught
anything?** `docs/timings.md` has six RED rows and each records only *that* it
was red. Without this, "retire that test" is an opinion; with a few months of
it, it is actuarial. Cross-domain original: claim history in insurance,
time-since-last-finding in aviation, sentinel surveillance in epidemiology.

#### §3.4 S4 — fold `make ci` into `make gate`

**Goal.** Stop discarding 368 measurements a month.

**Mechanism.** `make gate` emits the `libtest-json-plus` stream and feeds the
existing timings alarm and `ci-record`. The ordering constraint the Makefile
already documents at length is preserved: the alarm compares against the
baseline still on disk, *then* the recorder overwrites it, and a red run never
becomes a baseline.

**Why.** `make ci` has run **9 times against `make gate`'s 368** — an
instrument built to watch a gate that crept 234 s → 934 s, running at 2.4% of
the frequency of the thing it watches, while every gate already computes the
durations it needs and throws them away.

**Blind spot this also improves.** The Timekeeper's open follow-up #1 is that
the contention guard "cannot see ordinary load." With 368 samples a month
instead of 9, the load-aware suppression it wants becomes calibratable.

### Stage 2 — Sweep

#### §3.5 S3 — `regenerate-artifacts.sh` as a DAG

**Goal.** Recover most of the 12.2-hour rebaseline line.

**Mechanism.** The script is 62 sequential `cargo run` invocations. Measured
`cpu_ratio` across recent rebaseline rows is **0.72-2.11 on 10-12 core boxes** —
effectively serial, and nobody decided that; it is an artifact of the script
being a shell list. Most invocations are independent; several share the same
seed-42 world and re-derive it in separate processes. Restructure into declared
stages with explicit dependencies, running each stage's members concurrently.

**Determinism.** Outputs are distinct files, so ordering cannot affect bytes.
The success criterion is `make rebaseline` leaving every generated artifact
byte-unchanged — the same falsifier The Whetstone used for its profile change.

#### §3.6 S15 — recycle worktrees instead of destroying them

**Goal.** Remove most of the cold-build cost with zero new tooling.

**Mechanism.** A small pool of long-lived worktrees under `.claude/worktrees/`,
reassigned with `git switch` rather than created and destroyed per campaign,
plus a `make worktree-take NAME=<campaign>` convenience target.

**Why this and not a compiler cache.** A shared `RUSTC_WRAPPER` is *admissible*
— decision 0004 binds "the **workspace** depends on serde/serde_json only", and
a dev tool is exactly the category decision 0040 used to admit nextest. But the
cost is caused by **destroying worktrees**, not by lacking a cache: 73 branches
against 3 live worktrees, 43 GB of `target/`. Recycling captures most of the
saving with no new determinism surface, and **a compiler cache that ever
returns a wrong object file is a silent determinism violation** — the worst bug
class in this repo, and a poor first instrument for a saving a scheduling change
already gets. `TOOL-sccache` carries the option with that risk stated.

**Interaction to respect.** `.superpowers/sdd/` is per-worktree and git-ignored;
recycling must sweep it, or a campaign inherits the previous campaign's ledger.
CLAUDE.md already requires verifying HEAD before reusing lefford's shared
regeneration worktree — the same discipline, now local.

### Stage 3 — Bells

#### §3.7 S16 — a scheduler on lefford

**Goal.** Restore what 0125 deleted, on hardware the project owns.

**Mechanism.** systemd timers. Verified present: `systemd 252 (252.39-1~deb12u2)`,
with cron also available. Timers chosen for `journalctl` (a failed run leaves a
durable, queryable record) and `Persistent=true` (survives a reboot). A nightly
job whose failure is invisible would reproduce the exact pathology this campaign
exists to fix.

**The constraint that makes this safe.** **A scheduled job never commits, and
never touches `main`.** It produces artifacts, writes its result, and posts to
the board; a human commits. Direct precedent: decision 0129's lane rule — "the
lane must never be wired to auto-implement a suggestion… that would make the
board self-modifying with no human in the loop, on the one channel every session
reads at `SessionStart`." Same argument, same channel. The concrete hazard
foreclosed is a nightly job committing while a session is mid-landing, which
`make preflight` warns about and cannot prevent.

**Constitutional note.** The wall-clock ban governs the *sim*, not the
infrastructure. Scheduling sits outside the determinism boundary, exactly as
`clients/` does.

**Initial timer set.** Nightly census on `main` (§3.8); nightly uncached full
gate on `main`; nightly drift sweep (`make rebaseline` plus the `git diff
--exit-code` list, reporting only); `make board-sync`; orphan-worktree report.

#### §3.8 S2 — the census runs nightly

**Goal.** Take the census off the campaign-close critical path.

**Mechanism.** A timer runs the census against `main` nightly, writes `rows.csv`
and the summary to a scratch location, runs `make lab-diff` against the
committed copy, and **posts the diff to the board**. It does not commit. A
campaign close then reads the morning's result instead of waiting five hours.

**Consequence for CLAUDE.md.** The "census re-pin at close" ritual becomes "read
last night's diff; if it is empty, you are done." Committing a moved column
remains a deliberate human act on the canonical box.

#### §3.9 S1 — a three-world census sentinel in the commit gate

**Goal.** Catch census drift at the commit that caused it, not five hours into a
campaign close.

**Mechanism.** A gate test runs the full metric roster over three seeds and
compares against **the first three rows of the committed
`book/src/laboratory/generated/the-census/rows.csv`.** No new artifact: the
expectation is derived from the census's own committed rows, so it is
automatically correct the moment a census lands and adds nothing to
`regenerate-artifacts.sh`. Comparison goes through the existing
`canonicalize_row` in `windows/lab/src/runner.rs`, whose doc comment states it
exists to "canonicalize a row for comparison with fixture-loaded rows".

**Cost.** Measured: 31.34 CPU-s/world all-metric on ambrose, so three seeds
≈ 94 CPU-s ≈ 8 s wall on twelve cores. The reference band, same host, from the
ledger: **`make gate` on `ambrose` runs 489-805 s** (the spread is contention,
not work — §2.2's S6 row). Eight seconds is ~1.6% of the fastest of those.

**Feasibility, and why 0079 does not block it.** Decision 0079 enforces that
census goldens are *authored* on one host because the machines disagreed on
~0.1% of discrete-count metrics. **Decision 0090 refines that and the audit came
back clean:** the divergence was traced to `f64::floor()` dispatching to
per-host glibc on the default `x86-64` codegen baseline, fixed by the baseline
pin at `3a7092c3`, and a 40-world all-metric probe is byte-identical between
x86_64/Linux and aarch64/Darwin. The sentinel only *reads*; 0079's write-path
enforcement is untouched.

**The bonus and the risk are the same thing.** The sentinel turns 0090's
one-time audit into a continuous one, over metrics that did not exist when that
audit ran. If a newly-added metric is host-divergent the sentinel reddens on the
Mac — precisely the failure 0079 feared and could not detect. That needs a
**declared-waiver escape hatch on the `tropes check` / seam-guard ratchet
pattern** (a waiver carries a reason; a reasonless one is a parse error), never
a disabled test.

### Stage 4 — Decide

Every move in this stage reverses or reopens something settled. They are last
because Stages 1-3 change what is true about their premises.

#### §3.10 S7 — the gate serves two ends; give it two instruments

**The claim.** `make gate` serves (i) *protect main from a broken commit* and
(ii) *give the author confidence to keep going*. (i) wants complete-and-once at
the merge boundary; (ii) wants fast-and-partial and continuous. One instrument
priced at (i) is being run at (ii)'s frequency — **368 times a month**.

**What changes.** `gate-fast` becomes the per-commit instrument and `make gate`
moves to the merge boundary, where `make preflight` already lives. Both already
exist; only doctrine forbids using them this way ("`make gate` still gates
commits"; "ITERATION ONLY").

**Why it survived the pass that killed S11's premise.** The obvious dissolution
— *make the gate cheap enough that nobody needs a split* — is exactly what
§1.5 refutes. The gate is ~4-8 minutes wherever it runs, on either host, at
either core count. **No infrastructure move in this campaign makes S7
unnecessary**, which is why it is a decision rather than a deferral.

**The pass did reshape it.** Negating "the gate is one instrument" yields
*three*, not two: protection, confidence, and **measurement** — and S4 makes the
gate a measurement instrument in Stage 1. So the split must assign the timing
alarm deliberately to one of the three, not leave it riding on whichever
happens to run. Recommendation: measurement rides with *protection*, because a
duration measured under a partial run is not comparable to a baseline.

**Ratification.** A decision record. Number chosen **after** the final
absorption, checked against `origin/main` per `PROC-decision-number-collision`.

#### §3.11 S11 — split the gate across hosts; do not move it

**Corrected by §1.5.** "Move the gate to lefford" is not worth its machinery:
243 s best-case there against 322 s best-case on a Mac, for push + ssh + a
remote worktree.

**What the negation gives instead.** lefford's shape is *many slow cores* — it
is good at embarrassingly parallel long tails and bad at latency. The Macs are
the reverse. So assign by shape rather than by box: **the latency-critical body
of the gate stays local; the long parallel tail goes to lefford, concurrently.**
The natural tail is already identified — 703 tests carry 96% of the suite's
4,628 CPU-s, and the heavy tier plus the census sentinel are both
throughput-shaped.

**The objective function is not wall time. It is local occupancy.** Owner
correction, 2026-08-13: a 400 s gate that leaves the laptop idle beats a 322 s
one that pins twelve cores and makes everything else on the machine sluggish.
Efficiency is the wrong axis; *interruption* is the right one. So the criterion
is two-part and the occupancy half is primary.

**Preregistered success criteria**, so this cannot be declared a win by
narration, both measured on a quiet box against a same-host before-arm:

1. **Primary — local occupancy.** The share of total gate CPU landing on the
   box the human is typing on falls by at least 60%.
2. **Secondary — wall time.** The split gate's wall time on the Mac is at least
   25% below that box's current median.

If (1) holds and (2) does not, **that is still a pass** and the spec says so in
advance, because (1) is what the owner actually asked for. If neither holds, the
split reverts and the null is recorded.

**A caveat against overclaiming, since the owner's suspicion is partly about
contention.** Several rows on both sides are contended: `ambrose` gates at
`cpu_ratio` 4.23-4.49 on twelve cores are ~35% utilised, and lefford's range is
18.6-30.0 on forty. The honest comparison is best-against-best — lefford 243.3 s
at ratio 28.99 versus the Macs' 321.7 s at ratio 8.03 — and that still gives
~25%, not more. **Less contention narrows the gap on the annoyance axis, not on
the efficiency axis.** Both halves of the criterion above exist so the campaign
cannot confuse the two.

**Sequencing.** After S16, because the dispatch mechanism is the scheduler's,
and after S14, because the before-arm must come from a recorded row.

#### §3.12 S18 — name the suite's life cycle, and ratify it

**The claim.** A living suite has three organs and Hornvale has one.
*Reproduction* — a finding becomes a test, a panel test becomes a census metric
— exists as practice. *Aging* (time-since-last-catch) and *death* (retire a
test that guards nothing) do not. That is why cost ratchets: The Whetstone's
934 s → 460 s recovery came from compiler flags, a one-time win that cannot
repeat.

**What this campaign does.** Ratifies the policy and lands *aging*'s instrument
(S5). *Death*'s instrument is S9, deferred, because retiring a test on anything
less than evidence is exactly the irreversible move this campaign should not
make in a hurry — nobody re-adds a deleted test.

**The guardrail, from the reversibility pass.** A retired assertion must
**migrate, not evaporate**: it becomes a census column or a golden artifact.
Which is precisely why S18 and S10 are the same mechanism seen from two ends —
migration is only affordable if the census is incremental.

#### §3.13 S12 — the census sampling question

**A fidelity trade, and therefore treated as one.** This campaign does **not**
cut the seed count. It does the work that would make such a cut a decision
rather than a guess, and stops.

**What Stage 4 delivers:** a stated precision requirement **per metric class**
(the `SummaryKind` axis already exists), and a measurement of what actually
degrades at n=300 against the committed n=1000 rows — which is a pure read over
data already on disk, costing no census run at all.

**The arithmetic on the table.** A proportion carries a ±3.1% confidence
half-width at n=1000 and ±5.7% at n=300, for a 3.3x cut.

**What the pass added, and it is better than the original.** Negating "fewer
seeds" gives **fewer seeds *per column*, not per census**: measure each metric
on a seeded 300-world subset, a *different* subset per metric. The dataset keeps
1,000 rows, every world stays represented, and each column costs 30%. Costs:
the CSV becomes sparse, chart `n` varies by column, and the determinism guard
goes blind on unsampled cells — so it is an option to evaluate, not a
recommendation, and it wants S10's column store to be implementable at all.

**The premise most likely to dissolve.** After S2, the census costs a machine's
night, not a human's close. The human-wait justification for cutting it
**dies in Stage 3**; only "lefford's nights are finite" survives, which is a far
weaker case. Stage 4 should re-ask whether S12 still has a live premise before
spending on it.

---

## §4 Success criteria

**Stage 1.** (1) Every command in §3.2's list leaves a `docs/timings.md` row.
(2) A red gate leaves a row in `docs/timings/defects-<host>.tsv`; a green gate
leaves none. (3) A new generated directory with no index entry reddens the gate.
(4) `make ci` and `make gate` produce the same baseline effects; running both is
no longer necessary.

**Stage 2.** (5) `make rebaseline` is byte-identical to today's output at a
measured `cpu_ratio > 4` on a twelve-core box.

**Stage 3.** (6) `make gate` includes the sentinel and its wall-clock addition
on `ambrose` is `≤ 2%` of the 489 s floor in §3.9 — `≤ ~10 s`, measured on a
quiet box against a same-host before-arm. (7) The timers run seven consecutive
nights, and a deliberately introduced census drift is reported on the board with
no human invocation and no commit.

**Stage 4.** (8) S7 and S18 are ratified decision records, numbered after the
final absorption. (9) S11 either meets §3.11's preregistered 25% criterion or is
reverted with the null recorded. (10) S12 produces a per-metric-class precision
statement and a degradation table read from the committed n=1000 rows — and
**no seed-count change ships in this campaign**.

**Every stage.** `make preflight` from the branch at each stage boundary, per
CLAUDE.md; absorb main; never mid-measurement.

---

## §5 Interaction with live campaigns

**The Glasshouse** holds a board `hold-off` on `domains/astronomy/`,
`domains/climate/`, `domains/terrain/` and intends an epoch that will move
census values. The Sexton touches none of those paths. The sentinel's golden is
census-*derived* rather than pinned, so a Glasshouse census refresh updates the
sentinel's expectation for free — the coupling is by construction.

**The Holdfast** is byte-identical by declaration and cannot interact with any
assertion here.

A board `notice` is posted for `scripts/`, `Makefile`, `docs/timings.md`,
`docs/timings/`, `windows/lab/` and `cli/tests/`; it will be re-posted when
Stage 4 begins, because Stage 4 changes shared doctrine.

---

## §6 Flagged for review

- **§3.9's escape hatch is determinism-contract-adjacent.** A waiver on a
  cross-host reproducibility check is the one thing here that, mis-specified,
  lets decision 0079's silent failure back in. Review it as a contract.
- **S12 remains a fidelity carve-out.** It is in scope for *analysis* only;
  §4's criterion 10 states explicitly that no seed-count change ships. The cut
  itself stays Nathan's, in a later campaign, with the precision statement in
  hand.
- **S7 and S18 reverse or codify standing doctrine** and produce decision
  records. They are in Stage 4 so they are decided with Stages 1-3's evidence.
- **§1.5 is a correction to this document's own earlier draft.** It is stated
  in the spec rather than quietly fixed, because the generalisation — CPU-seconds
  are not portable across hosts — is the reusable part.
- **Low-confidence assumption:** the ~8 unrecorded prewarm hours in §1.3 is an
  estimate (73 branches × a measured 771 s, halved for reuse), not a
  measurement. S14 replaces it with one; nothing here depends on its magnitude.

---

## §7 Decisions this campaign will propose

Numbers chosen **after** the final absorption and checked against `origin/main`
(`PROC-decision-number-collision` has fired three times).

1. *A scheduled job never commits.* (§3.7 — the 0129 lane argument generalised
   from the board to all automation.)
2. *Every command that makes a human wait leaves a ledger row.* (The
   generalisation of 0086's amendment that was never made.)
3. *The gate is two instruments, and measurement rides with protection.* (§3.10,
   S7 — supersedes the "`make gate` gates commits" doctrine.)
4. *The test suite has a life cycle; a retired assertion migrates, never
   evaporates.* (§3.12, S18.)
5. *CPU-seconds are not portable across hosts.* (§1.5 — candidate; may be better
   as a CLAUDE.md line than a record, to be decided at close.)

---

## §8 What this campaign sets up but does not do: a second host

Recorded here because the owner named it as the destination (2026-08-13, "if we
bring back using AWS servers, we might see comparable efficiency and much less
annoyance… once we have the overall workflow ironed out"), and because the
argument for it is stronger than opinion: **decision 0063 abandoned AWS on two
stated premises, and both have since expired.**

| 0063's premise | Its status now |
|---|---|
| *Load* — "~7 minutes on a 40-core box … no longer monopolizes the machine", which is what dissolved 0046's remote round-trip | The Rill's census ran **19,207 s**. That is **46x** the figure 0063 relied on. Run 0063's own argument on today's number and it points back at remote. |
| *Bytes* — "this machine is NOT byte-identical to AWS": seed 681 `divergence-hobgoblin` reads 5 here and 6 in the AWS golden, a discrete count decided upstream of quantize-at-emit | **Decision 0090 diagnosed exactly that class of flip** — `f64::floor()` dispatching to per-host glibc on the default `x86-64` codegen baseline — and it was **fixed by the baseline pin at `3a7092c3`**, with a 40-world all-metric probe byte-identical across x86_64/Linux and aarch64/Darwin afterwards. |

**This is new information, not a fresh opinion**, which is the standard the
registry's anti-relitigation rule sets. It is still not this campaign's work:
0063 is a ratified decision and superseding it needs its own record, its own
measurement, and an owner decision.

**What this campaign contributes to it, and it is the whole difficulty.**
Qualifying a host has never been a runnable operation — 0063's finding came from
noticing a golden disagreed, after the fact. **S1 is that operation.** The census
sentinel is a continuous cross-host reproducibility check, and adding a host
becomes: run the sentinel there, read the verdict. Its declared-waiver escape
hatch (§3.9) is what keeps a genuine divergence visible instead of ignored.

So the sequence, for a later campaign: land Stages 1-3 → run the sentinel on the
candidate host → if clean, re-run 0063's own seed-681 counterexample as a
falsifier → then, and only then, a decision record superseding 0063.

**One thing that must not be assumed on the way.** §1.5's lesson applies with
full force to a machine nobody has measured: an AWS instance's CPU-seconds are
not comparable to lefford's or to a Mac's, and **no host may be adopted on
projected arithmetic.** Qualify it with the sentinel; time it with recorded rows.
