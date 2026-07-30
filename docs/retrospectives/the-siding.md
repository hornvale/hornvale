# The Siding — retrospective

**Closed:** 2026-07-29 · **Decisions:** 0086 (placement), 0087 (benchmark
timings) · **Executed:** inline on `main`, at Nathan's direction — no branch,
so no stage-boundary absorption applied.

## What the campaign was, and what it turned out to be

It began as a scheduling fix: three sessions had driven the canonical box to
load 85 on forty cores. It ended having found a census stale for 139 commits,
a benchmark misfiled as a golden, and two checks in its own spec that could
not have failed. The scheduling fix was the smallest thing in it.

## The lesson worth keeping

**A check is worth only the configurations it actually runs in.** Four
failures this session share one shape — an anchored check that never fires,
because the command that produces the artifact and the command that verifies
it are never invoked together:

| failure | why it never fired |
|---|---|
| census stale 139 commits | heavy tier `#[ignore]`d out of `make gate`; only an explicit run regenerates it |
| `the-sounding` timings inside a drift-checked tree | `rebaseline` never rewrites them; `gate-full` never diffs them |
| my zero-diff verification step | written into the spec, never executed — could never have passed |
| my `status` verification step | never run against a live holder; always answered "no" from the Mac |

Captured as `TOOL-paired-generate-verify`; re-scored into the Confidence
Gradient as a floor beneath checkability.

## Golden-pin discipline: it did not hold, and not by this campaign

**Seven** calibration pins are red at close, across two suites:

```
calibration.rs                     name_collision_rate_is_measured_and_pinned
                                   name_length_distributions_are_measured_and_pinned
                                   lexicon_is_regular_and_exposure_sound_for_both_species
                                   null_control_name_length_smd_is_pinned
branches_family_calibration.rs     homophony_count_is_measured_and_pinned
                                   divergence_magnitude_loudness_ordering_holds_in_aggregate_not_per_seed
                                   clean_outgroup_kobold_holds_on_every_swept_seed
gathering_calibration.rs           (4 passed — clean)
```

Recorded as four in the regen commit (`9855048d`) and corrected here: that
suite takes ~14 minutes and had not finished when the message was written, so
the count came from a partial run. The commit is pushed and append-only; this
file is the accurate record. **Reporting a count before the run finishes is
the same error as the rest of this campaign** — believing a check that had not
completed.

Per the closing walk, a red pin at close is a missed re-pin to chase back, not
a value to accept quietly — and the chase leads to `00ed687b merge: absorb
origin/main at the close, deferring the census regen`.

They were **left red deliberately.** They encode The Wearing's measured
values; accepting them is a claim about that campaign's physics, and
re-pinning someone else's measurement through an unrelated campaign is exactly
what the preregistration discipline exists to prevent. The regen itself was
paid off as a standalone, attributed change (`9855048d`).

The process lesson is narrower than "someone forgot": **the deferral was
written down and that made it feel handled.** A merge message is not a
tracking mechanism. Nothing consumed that sentence, nothing was accountable
for it, and it survived 139 commits. A deferred regen needs a row somewhere
that something reads.

## Where I cost time

- **Observed an expensive run through a pipe.** Launched a ~40-minute heavy
  run under `| tail -40`, which buffers until the pipeline closes. The output
  file stayed empty, the completion signal never came, and I reported the run
  as "still going" for an hour after it died. There was then no forensic trace
  at all, so the only way to learn why it exited was to pay for it again.
  Nathan's correction — build tooling that emits what we need rather than
  wrapping commands in ad-hoc capture — is now in `heavy-run.sh`, which writes
  a predictable log and appends an outcome row on every exit path including
  signals. **Absence of output is not evidence of progress.**
- **Measured with the wrong parser, twice.** Read a census diff with
  `awk -F,` when the rows contain quoted fields with embedded commas
  (`"farmer,shaman,chief"`), and reported "1000 rows changed on every column".
  With a real CSV parser the answer was 21 specific columns. Separately, an
  earlier column-symmetry check used a filter that could not have
  distinguished agreement from disagreement. A measurement is only as good as
  its parser, and both times the wrong answer looked plausible.
- **A rejected tool call executed anyway.** An `ssh` dispatched before the
  rejection registered, and the run I had been told not to start was running
  detached. Surfaced immediately rather than quietly used. Worth knowing this
  is possible.

## What went right

- **The falsifications were kept as findings.** A2 was designed to prove the
  hosts diverge; they do not, and the campaign's justification was narrowed in
  the spec rather than left overstated. Task 6 was skipped *by measurement*,
  and the spec says so in those words.
- **The contention thesis got measured, not asserted.** `scene_api_cost`'s
  genesis step read 19,722 ms contended and 3,818 ms quiet — a 5.2× false red
  from a timing-budget test on a shared box, which is the campaign's own
  premise demonstrated by accident.
- **Scope was held.** Three separate opportunities to widen — the census
  re-pin, the regen parallelisation, the stale-fixture fix — were captured as
  followups (`TOOL-parallel-regen`, `TOOL-paired-generate-verify`) or split
  into their own attributed commit instead of being absorbed here.

## Deferred, with reasons

- **A3 (the heavy tier's uncontended runtime) is unmeasured.** It cannot be
  measured until the calibration pins are resolved, since a cancelled run's
  wall time is not the tier's runtime. The wait timeout keeps the census's
  inherited 2700s, chosen and not derived, exactly as 0081 treated its own
  constants.
- **The three Lefford worktrees were not migrated.** `the-wearing` holds 39
  uncommitted files and a 50-file scratch ledger that dies with the worktree.
  0086 binds new campaigns; retro-migrating live ones buys little and risks
  much. They will pick up `origin/main` when their sessions resume.
