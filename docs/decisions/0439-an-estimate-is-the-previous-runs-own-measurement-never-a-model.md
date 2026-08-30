# 0439. An estimate is the previous run's own measurement, never a model

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0438](0438-progress-is-named-by-phase-there-is-no-global-percentage.md),
[0001](0001-determinism-is-constitutional.md) ·
[The Overture](../../book/src/chronicle/the-overture.md)

In the context of drawing a bar for the phase a build is currently in, facing
the choice between a cost model and a recorded measurement, we decided the
bar's length comes from **the previous run's own duration for that same
phase**, read off disk, accepting that a first-ever run draws **no bar at all**.

**Context.** Determinism makes the previous run's number *exact* for a repeat
of the same seed and pins, and roughly right for a new seed on the same
hardware — which is strictly better than any model this project could author,
and it costs nothing to keep current. A model, by contrast, is a claim with a
date: the shares in [0438](0438-progress-is-named-by-phase-there-is-no-global-percentage.md)
were measured on one machine on one day and the project has repeatedly paid for
treating such a figure as durable.

**Consequence.** A first run has no baseline and therefore no bar. That is the
honest state, not a fallback, and it is the same rule
[0437](0437-a-view-shows-what-exists-and-one-that-cannot-speak-is-skipped.md)
applies to views. **Every read of the record is total**: a missing file, a
truncated line, a non-numeric duration, or a key from a future version all
reduce to "that phase has no baseline". Nothing here returns an error to a
caller, because there is no caller for whom a bad cache file is worse than no
cache file. The record is keyed by a stable phase key rather than by the
displayed caption, so rewording a caption cannot orphan a measurement.

**See also.** Spec §2; `clients/game/bin/src/overture/timings.rs`
(`overture-timings.tsv`, in the client's state directory).
