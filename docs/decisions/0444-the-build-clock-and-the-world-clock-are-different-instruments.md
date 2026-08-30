# 0444. The build clock and the world clock are different instruments

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0186](0186-an-instant-is-an-exact-tick-count.md),
[0438](0438-progress-is-named-by-phase-there-is-no-global-percentage.md) ·
[The Overture](../../book/src/chronicle/the-overture.md)

In the context of designing views that fill a wait, facing the appealing idea
of replaying the world's own history as the loading screen, we decided the
wait is paced by the **build clock** and a world's history is paced by the
**world clock**, and that these are not the same instrument — accepting that
the `chronicle` view is **held** rather than shipped.

**Context.** The build clock is wall time on this machine, in this process,
measured in milliseconds and knowable only from the previous run
([0439](0439-an-estimate-is-the-previous-runs-own-measurement-never-a-model.md)).
The world clock is `WorldTime`, an exact tick count since genesis, with no
relation to how long the machine took to derive it. A view that replays deep
time is legible only when paced by the second; a progress substrate is legible
only when paced by the first. Conflating them produces a screen that either
races through millennia or stalls on a phase.

**Consequence.** The chronicle view is deferred with its measurement rather
than dropped: deep time completes at 2,224 ms, making it the most expensive
view to feed and leaving the least wait to spend it in. Its value is highest on
a **cached** start, where the history already exists and there is a ~1.2 s tail
to fill ([0440](0440-a-cached-worlds-validity-is-seed-and-pins-the-label-diff-and-a-prefix-tripwire.md)) —
which is the campaign that should build it, and the reason this record exists
rather than a silent omission.

**See also.** Spec §4, §8; `book/src/frontier/idea-registry.md`
(`CLIENT-chronicle-view`).
