# The Timekeeper — retrospective

**Closed:** 2026-07-30 · **Decision:** 0088 (the suite watches its own clock) ·
**Executed:** inline on `main`, at Nathan's direction — no branch, so no
stage-boundary absorption applied. 22 commits, `40bc4124..36c57e57`.

## What shipped

`make ci` runs the workspace under a new `ci` nextest profile, persists
`target/nextest/ci/run.json` and `run.log`, alarms on a duration shift against
a committed per-host baseline, then — only if the run was green — rewrites that
baseline. Two alarms: per-test (≥ 5 s **and** > 2× baseline) and whole-suite
(> 25 % on the id intersection). Wall time lands in `docs/timings.md` under the
label `ci`.

## The lesson worth keeping

**A check is worth only the configurations it actually runs in** — The Siding's
lesson, inherited, and this campaign was its own best evidence. Counting both
campaigns, sixteen instances. Two facts about them matter more than the count:

- **Eight were authored by this controller's own plan text.** Not inherited
  debt. Written the same week, by the session that had just finished
  explaining the pattern, into the machine built to detect it.
- **Four were inside that machine.** The recipe that ran the recorder before
  the alarm. The wrapper that exited 0 on a red suite. The suppression notice
  swallowed by `cargo test` without `--nocapture`, which then let contended
  durations become the permanent baseline. The contention gate wired backwards
  against its own written rationale.

Two practices caught what review did not, and both are cheap enough that
omitting them is never justified:

1. **A mutation step that requires the alarm go red on command.** Plan step 4
   said "divide a baseline row by ten, require RED." It came back green, and
   that is the only reason the never-firing alarm was found. The step existed
   only because it was added during the plan's own self-review — it was not in
   the first draft.
2. **A final review that runs the system instead of reading it.** Five findings
   the per-task reviews could not see, all in the shipped wrapper, which no
   individual task owned.

## Where spec-compliance review reaches its limit

The Task 6 reviewer passed the inverted contention gate as correct. It *was*
correct — it matched the plan exactly. The plan contradicted the rationale
written three lines above it in the spec. A reviewer checking implementation
against specification cannot catch a specification that disagrees with itself,
and no amount of reviewer diligence changes that. The structural answer is the
mutation step above: it asks the system to fail, which is a question the
specification cannot answer wrongly on its behalf.

Corollary, and this one is new: **the polarity of a guard must be a pure
function with its own unit tests.** The fix did not just flip a condition; it
extracted `enforcement_is_appropriate` and pinned both branches with tests that
fail on re-inversion. A boolean buried in a test's control flow is a decision
nothing is watching.

## Four consecutive type-audit defects from the plan

Tasks 2, 3, 4 and 5 each hit the same shape: a `pub fn` returning `String` or
`Result<_, String>` with no `type-audit:` tag, because the controller's brief
code omitted it. Each was caught, each cost a round trip. A fifth was
pre-empted only after the third had already landed.

The lesson is not "be more careful." It is that a defect recurring across
consecutive tasks belongs in **Global Constraints as a stated rule**, not left
to per-task vigilance. This is the second campaign to record the same finding
about tag grammar (The Consonance recorded the malformed `bare-ok(class:
field)` shape). Stating the rule once at the top of the plan is the fix that
works; noticing it four times is the fix that does not.

## Where the campaign's own thesis bit it

Three findings were the subject matter reproducing itself in the
implementation, and it is worth being explicit that this is the normal case,
not an embarrassment:

- A **NaN** duration parses cleanly and compares false against every
  threshold, silently disabling the alarm for that row. From controller-written
  brief code; caught by review, not by either implementer.
- A **malformed JSON line** among valid ones was dropped silently — `parse_run`
  errored only on total emptiness, so partial data loss was invisible. Fixed to
  alarm on a truncated or shape-changed stream.
- The **baseline diff was unreviewable**: same sha stamped on every row plus
  nanosecond precision meant 100 % churn, defeating the `git log -p`
  archaeology that is the file's entire justification. The measurement that
  found it (2,405 of 2,578 rows changing on jitter alone) is also what derived
  the fix's parameters.

One empirical habit paid off and should be copied: an implementer probed
`serde_json` directly and found NaN and Infinity have no valid JSON spelling,
so half of a defensive requirement was unreachable. It kept the check and wrote
tests asserting *actual* behaviour rather than asserting a falsehood. A
re-reviewer confirmed independently. Probing beats reasoning about a library's
behaviour, every time.

## Golden-pin discipline

`make gate` and `make ci` are both green at close. No pin drifted during this
campaign — it touched no domain, kernel or window logic, no draws, no seed
labels, no save-format surface.

**The Siding's seven red calibration pins remain red and remain unowned**
(`calibration.rs` ×4, `branches_family_calibration.rs` ×3). They encode The
Wearing's measured values and were deliberately not re-pinned here; re-pinning
another campaign's measurement through an unrelated campaign is what the
preregistration discipline exists to prevent. They are named in this campaign's
spec §5 as an explicit non-goal, which is the second time they have been
written down and the second time nothing has been made accountable for them.
The Siding's own lesson applies to its own debt: *a deferral that is recorded
but unowned is not tracked.*

## Follow-ups

1. **The contention guard is blind to ordinary load.** It checks only for a
   census claim, so parallel agent sessions are invisible to it. It already
   enforced against thoroughly contended timings during this campaign's own
   hysteresis runs (load 42–63 on ten cores), which is why churn landed at
   35–64 rows instead of the modelled 6. Candidate fix: also suppress when
   loadavg exceeds core count — cheap, and grounded in observed reality rather
   than the census-claim abstraction. **This is the item most likely to make
   the alarm distrusted, and distrust is how an alarm dies.**
2. **`PER_TEST_FLOOR_SECS = 5`, `PER_TEST_MULTIPLE = 2`, `SUITE_TOLERANCE =
   0.25` are chosen, not derived** (spec assumption A1). The hysteresis
   parameters *were* derived, from measured jitter — median 16.9 % across all
   tests, 3.8 % at ≥ 1 s, 2.9 % at ≥ 5 s. The same method applies to the other
   three now that runs are accumulating; decision 0088 explicitly does not
   ratify them.
3. **`make ci` is invoked by nothing.** CI is manual-only (decision 0042), so
   the alarm fires only when someone runs it. That is the same standing every
   other gate here has, and it is also exactly the shape this campaign is
   about.
4. **Residual baseline noise.** Even after the floor and the deadband, real
   millisecond jitter still moves a few dozen rows per run on this Mac. Worth
   revisiting once several quiet runs exist — the noise floor is measurable now
   in a way it was not before.

## Where time went

- **Executing inline on `main` rather than in a worktree** (Nathan's explicit
  direction) meant every intermediate state was on the branch other sessions
  absorb from. Nothing went wrong, but the campaign shipped four fix waves and
  each one was briefly live on `main`.
- **Three full `make ci` runs at ~16 minutes each** were needed to verify the
  fix wave, plus more for the hysteresis pass. That is the honest cost of a
  campaign whose subject is the suite's own wall time: you cannot measure the
  instrument without paying for the measurement.
