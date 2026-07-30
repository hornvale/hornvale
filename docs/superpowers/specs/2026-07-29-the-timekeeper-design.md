# The Timekeeper — Design

**Status:** drafted, awaiting G3 · **Date:** 2026-07-29 ·
**Campaign:** The Timekeeper

The suite has never watched its own clock. This campaign gives every test a
recorded duration, a committed baseline, and an alarm when it shifts — plus
one entry point (`make ci`) that persists its raw output instead of leaving it
in whoever's terminal ran it.

No domain, kernel, or window logic. No draws, no seed labels, no save-format
surface.

## 1. The problem

Decision [0040](../../decisions/0040-nextest-is-the-gate-runner.md) adopted
nextest to bring the commit gate "under 4 min", measured at **234 s** on an
M1 Max. Measured 2026-07-29 on a quiet Mac:

```
make gate, quiet Mac        934.5 s    2548 passed, 86 skipped, 24 doctest crates
make gate, same Mac, busy   940.8 s
0040's documented budget    234 s
```

**Roughly 4× over, and contention accounts for six seconds of it.** The suite
simply grew. `docs/timings.md` — the ledger whose own header says it exists
because "a whole suite creeping 65s → 43.5 min lived unnoticed until it hurt"
— carries **zero rows labelled `gate`**. The instrument was never wired to the
most-run expensive command in the repo.

This is the fifth instance The Siding found of one shape: *a check that exists
but never runs in the configuration where it would fire*
([`TOOL-paired-generate-verify`](../../../book/src/frontier/idea-registry.md)).
The others were a census stale for 139 commits, a benchmark's timings inside a
drift-checked tree nothing reconciles, and two checks written into a spec and
believed unexecuted.

**The mechanism to fix it is already installed and switched off.** There is no
`nextest.toml` anywhere in the tree, so profiles, `slow-timeout`, and
structured output are all unused. nextest 0.9.140's `libtest-json-plus`
emits, verified:

```json
{"type":"test","event":"ok","name":"…text_of_returns_text_values_only","exec_time":0.007394709}
{"type":"suite","event":"ok","passed":1,"failed":0,…,"nextest":{"crate":"hornvale-kernel","kind":"lib"}}
```

Per-test durations, as JSON. `serde_json` is already a workspace dependency,
so no XML parser and no new crate (decision 0004 holds).

## 2. Decisions

**N1 — the review loop is a failing test plus git archaeology; no dashboard.**
Nathan, asked directly: "Test failing on a duration shift is fine, or just
'wtf, it's still running?' followed by git archaeology." This kills the
Jenkins question outright and has a design consequence: **the ledger must be
committed**, or `git log` cannot reach it.

**N2 — the alarm is a floor plus a multiple, floor 5 s.** Of 2548 tests the
overwhelming majority run in single-digit milliseconds, where a 2× swing is
scheduler noise; every failure worth catching (the gate at 4×,
`the_hearths_effect…` past 540 s, `scene_api_cost` at 19.7 s) clears 5 s
comfortably.

**N3 — derive limits from recorded history, do not hand-set them.** The
statistical-process-control framing: control limits come from the process's
own history and are recomputed as it changes. Hand-set ceilings have two
failure modes and this repo has both — `scene_api_cost`'s 13,000 ms constant
false-alarmed under contention today, and 0040's "~4 min" rotted loose by 4×.

**N4 — observation and enforcement are separate knobs.** `slow-timeout`'s
`period` marks a test slow; `terminate-after` kills it. Only `period` is set.
The slow-query-log / statement-timeout distinction: conflating them means a
contention blip destroys a run instead of reporting one.

**N5 — no test may write a committed artifact without also asserting
something about it that could fail.** This is the rule that converts
generation-as-test from a hazard into the pairing fix. Without it, "green"
means "it wrote a file", which is exactly how `the-sounding` and the census
rotted.

**N6 — scope: A, B and E only.** Migrating CI logic wholesale into Rust tests
(C) and the Lefford job queue (D) are deliberately out; see §5.

## 3. The design

### 3a. `.config/nextest.toml`

Two profiles. `default` preserves today's behaviour exactly. `ci` adds
`libtest-json-plus` output, full failure capture, and `slow-timeout.period`
for marking. No `terminate-after` (N4).

### 3b. The ledger — a rolling baseline, one row per test

```
docs/timings/test-baseline-<host>.tsv
  test-id <tab> exec_time_s <tab> recorded-at-sha
```

Rewritten in place each `make ci`; ~2548 rows. Small in the tree, and
**`git log -p` on it is the archaeology** (N1). Appending every run would give
richer statistics and grow without bound; git already holds history, so the
file holds only the present.

One file **per host**, keyed on `hostname -s`. The Mac and Lefford differ by
roughly 4×; a shared baseline would alarm on every cross-machine run.

### 3c. Two alarms, not one

**Per-test:** fail when `exec_time >= 5.0s` **and** `exec_time > 2 × baseline`.

**Whole-suite:** fail when the run's total exceeds its own recorded total by
more than 25%.

> Changed during execution, with the owner's approval: the whole-suite alarm
> compares the INTERSECTION of test ids present on both sides, not each
> side's raw total. Raw totals would trip on pure test-count growth (this
> repo adds tests constantly) and could hide a real regression in the
> survivors behind removed tests (fewer rows can shrink the current total
> even while the surviving tests got slower). See `suite_shift` in
> `windows/lab/src/timings.rs`.

The second is load-bearing and easy to omit. The gate's 234 s → 934.5 s creep
was almost certainly *not* any single test doubling — it is 2548 tests each
growing a little, plus new tests arriving. **A per-test alarm cannot see death
by a thousand cuts.** Only the suite total can.

Both alarms handle: a test with no baseline (new — record, never fail), a
baseline with no test (removed — prune), and a host with no baseline file
(first run — record, never fail).

### 3d. The alarm must not fire on contention

Enforce by default; suppress only when
`hornvale_lab::census_claim::current_holder()` shows a LIVE holder on this
box — evidence that some other heavy job (a census, the heavy tier) is
running right now and timings are contended. Today's evidence:
`scene_api_cost`'s genesis step read **19,722 ms contended** and **3,818 ms
quiet**, a 5.2× swing that would false-alarm every per-test budget in the
suite. This degrades gracefully without the queue (campaign D) — it simply
suppresses more and enforces less while the box is busy.

> Changed during execution, with the owner's approval: the polarity above is
> inverted from the original draft, which read "enforce only when the run
> HOLDS the box claim." `make ci` never acquires the claim itself, so that
> reading meant an ordinary quiet machine — the overwhelmingly common case —
> would never enforce. The claim's presence, not its absence, is the signal
> that timings are untrustworthy: enforce by default, suppress on evidence
> of contention. See commit `38b28474` and `cli/tests/timings_alarm.rs`'s
> `enforcement_is_appropriate`.

### 3e. `make ci`

A thin wrapper: run the `ci` profile, persist raw output, print a summary and
the list of files holding the detail. No logic in the recipe (N6's
"make is a wrapper" principle); everything it decides lives in Rust.

### 3f. Raw-output persistence is itself tested

A test asserting that after a run the raw output file exists, is non-empty,
and contains the run's own test names. Nathan's requirement, made literal —
otherwise the persistence is one more unverified check, which is the subject
of this campaign.

## 4. Verification

- The alarm **fires**: mutate a baseline row to a tenth of its value and the
  suite alarm goes red. Asserting an alarm exists is not asserting it alarms.
- The alarm **stays quiet** on a clean re-run.
- A new test (absent from baseline) does not fail the run.
- A run with a live box claim (evidence of contention) records but does not
  enforce; an ordinary run with no claim enforces. (Changed during
  execution alongside §3d — see the note there: the original bullet had this
  backwards.)
- The raw-output test fails when the output file is deleted.
- `make ci` is green on a quiet Mac, and its summary names every file it
  wrote.
- `make gate` behaviour is unchanged — the `default` profile is untouched.

## 5. Non-goals

- **Migrating CI logic into Rust tests (C).** The large piece; deferred until
  A+B+E prove the pattern.
- **The Lefford queue (D).** Wanted, and it is what makes per-test enforcement
  trustworthy, but §3d degrades gracefully without it.
- **Jenkins, or any dashboard.** Foreclosed by N1.
- **`terminate-after`.** Out by N4; revisit only with data.
- **Re-pinning The Siding's seven red calibration pins.** Another campaign's
  measurements.

## 6. Assumptions requiring measurement

**A1 — the 2× / 25% tolerances are CHOSEN, not derived.** They should be
revisited against the baseline's own recorded spread once several runs exist,
exactly as 0081 said of its constants and as N3 demands of ceilings generally.
Shipping a hand-set constant in a campaign whose thesis is "derive the limits"
is a tension, stated rather than hidden: there is no history to derive from
until this campaign creates it.

**A2 — `libtest-json-plus` is experimental.** Verified emitting `exec_time` on
nextest 0.9.140 (2026-07-29). The format may change; the parser must fail
loudly on an unrecognised shape rather than silently recording zeros.

## 7. Definition of Done

- `.config/nextest.toml`, the ledger, both alarms, `make ci`, the
  raw-output test — all verified per §4.
- A decision record ratifying the review loop (N1) and the observation /
  enforcement split (N4).
- Root `CLAUDE.md` documents `make ci` and the baseline files.
- Chronicle entry, freshness sweep, retrospective.
- A1's tolerances recorded as a follow-up with the data that would settle them.
