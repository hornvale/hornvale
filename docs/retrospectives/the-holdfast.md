# The Holdfast — retrospective

Process lessons. Product is in the chronicle and the spec's §8 readout.

## 1. Decompose wall time before reaching for a profiler

The campaign was requested as "flamegraph the suite, it has blown up
400%." A flamegraph would have been the wrong first instrument, and
`docs/timings.md` says so in its own header: `cpu_ratio` "separates *more
work* (user climbs) from *more contention* (wall climbs, ratio falls)."

Reading it apart took minutes and changed the entire diagnosis. Wall was
up 3.35×; CPU only 1.39×; parallelism down 2.40× — and `1.39 × 2.40 =
3.35` closes. Most of the complaint was a machine at load average 7 while
idle, not code.

**Rule: for any "X got slower" claim, decompose into work × parallelism
before profiling.** A profiler cannot see contention at all, so it would
have returned a confident, detailed, and largely irrelevant answer.

## 2. Do not rank a suite off another host's baseline

`docs/timings/test-baseline-<host>.tsv` is per-host, and CLAUDE.md already
warns (from The Whetstone) not to rank the suite off another host's file.
This campaign found the sharper version: the two Macs' baselines were also
taken on *opposite sides of an optimisation*, so the newer host's file
read −14.8% and looked healthy while the older box's read +8.3% and was
the alarming one. The intersection that mattered was 12 days and 1439
commits stale.

**A baseline comparison is only meaningful when both endpoints straddle
the same code changes.** Check what commit a baseline was recorded at
before drawing a trend from it.

## 3. The alarm exists; nothing runs it

`make ci` is purpose-built to catch exactly this drift, and it had run
**once** on the Mac in three days and not at all on the Linux box for
twelve. It caught a genuine regression on its first run in that window.
The instrument was fine; the cadence was absent, and nothing enforces a
cadence. Recorded as followup F4.

## 4. Ideonomy earned its keep on the answer that felt settled

The overlay's own red-flag table says the most confident-looking questions
are where a pass catches the wrong model. That is exactly what happened.
The pre-pass claim — hoist insolation and elevation — was supported by a
doc comment and felt closed. The inversion pass asked whether the *member*
read by the function was the same quantity as the *field* the comment
described. It was not: relief is era-fixed, height above sea level is not,
because sea level is what an ice age moves.

Implementing the unexamined version would have frozen glacial low-stands —
a world-generation defect, shipped as an optimisation, invisible to any
byte-comparison against the changed code because both arms would have been
wrong together.

**Generalised: a doc comment describes the field it is attached to, not
whatever a later structure derives from it.** Verify the member you
actually read.

## 5. Two optimisations on one path do not add up

The plan's second change was sized at ~48% of the path's `exp` before the
first change shipped. Afterwards it was worth ~3%, because the first
change already skipped those evaluations 73% of the time. The 48% was
never wrong — it was a *standalone* figure being reused as a *marginal*
one.

**Rule: after any optimisation lands on a path, re-derive the next one's
value as a marginal figure.** Carrying the original estimate forward is
how a campaign talks itself into a refactor that no longer pays.

The ordering that revealed this (the premise-independent change first) was
chosen for a different reason — robustness against a premise error — and
happened to also be the order that exposed the overlap. Luck, not
foresight, but the heuristic generalises: **ship the change that depends
on fewest assumptions first.**

## 6. Prove the test can fail, on behaviour

The equivalence test passed the moment it was written, because both sides
were still the same code — a vacuous green that looks identical to a real
one. It was made to fail first with a deliberate off-by-epsilon bound,
producing a *behavioural* red (`8.516e-8` against `2.905e-6`) rather than
a compile error, before the correct bound was written.

A validating constructor also removed a case the test had tried to cover:
`SeaLevelHeight::from_metres` rejects non-finite values, so the NaN row
panicked in the constructor. That is a stronger guarantee than the
assertion would have been, and it belongs in the doc as a reason the
shortcut needs no finiteness guard.

## 7. Interleave A/B measurements

The first timing run measured baseline three times then changed three
times, and the box turned out to be carrying another session's job at
3925% CPU. Sequential arms under drifting load are not comparable. Redone
as A/B/A/B/A/B, the two agreed (20.3% and 19.8%), which is what made the
number trustworthy — not the load being low, which it was not.

## 8. What the campaign did not measure, and said so

H2 (whole-suite `exp` share) and H3 (crate-level worldgen time) were
preregistered and **not measured** — the canonical box was busy with
another session's work and displacing it was not worth a percentage point.
Both are recorded as unmeasured in §8 rather than inferred from the single
test that was measured.

Resisting the inference is the point. One test moving −19.8% is not the
crate moving −19.8%, and a readout that quietly promotes the former into
the latter is how a preregistration stops meaning anything.
