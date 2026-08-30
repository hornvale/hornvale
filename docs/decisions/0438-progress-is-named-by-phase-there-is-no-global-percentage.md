# 0438. Progress is named by phase; there is no global percentage

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0436](0436-the-startup-is-a-frame-with-pluggable-views-not-a-screen.md),
[0439](0439-an-estimate-is-the-previous-runs-own-measurement-never-a-model.md) ·
[The Overture](../../book/src/chronicle/the-overture.md)

In the context of a startup whose cost is **wildly uneven across its phases**,
facing the obvious design of one bar for the whole wait, we decided the
progress substrate **names the phases, marks the ones that are done, and bars
only the phase you are currently in**, accepting that a reader is never shown a
single number for how much of the whole remains.

**Context.** The absence of a global bar is a measurement, not a taste. Five
agreeing runs (`--release`, seed 42, M1 Max) put the build at:

```
  phase                      ms      share
  astronomy                    0.4    0.01%
  terrain (genesis)          202      6.6%
  settlements               1840     60.2%
  deep time                  181      5.9%
  WorldContext::build         830     27.2%
  possession                   13      0.4%
  ------------------------  ------  -------
  total                     3054     100%
```

A global bar would spend **60% of its life inside one phase**, crawling — and
would read as a hang exactly when the build is healthiest. Two items are 76% of
the wait.

**Consequence.** There are **five** phases and only four `BuildDepth` rungs.
The fifth, `Living`, is the post-genesis `WorldContext` build: it commits no
facts and is not a rung, but the player waits through 27.2% of the startup
inside it, so the substrate names it. A phase list is also more useful than a
percentage for the thing a reader actually wants to know — *what is it doing* —
and it degrades honestly when there is no baseline to bar against.

**See also.** Spec §1, §2; `clients/game/bin/src/overture/progress.rs`.
