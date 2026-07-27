# 0080. `chronicity` is a diagnostic; the population-health alarm is `stuck`

**Status:** Accepted (2026-07-26) · **Decider:** Nathan · **Refines:**
[0016](0016-studies-preregister-hypotheses.md),
[0011](0011-studies-are-data-metrics-are-code.md)

In the context of the population health metric's null control, facing the
question of which member of the distress family carries the bug alarm, we
decided that **`chronicity` is a *diagnostic* and the alarm is `stuck`** — a
distress run of at least `CHRONIC_TICKS` that *never ended*, evaluated **per
creature** — accepting that a long distress episode which recovers now passes
the control unremarked.

**Context.** The metric (`windows/lab/src/health.rs`) simulates each world's
derived creatures forward 40 ticks, reads each one's felt state per tick, and
reduces the per-creature affect series to a family: prevalence, chronicity,
recovery-rate, by-cause, by-species. Its baseline is a null control — The
Temperament §8's "in a resource-abundant, niche-matched world, persistent
distress ≈ 0; the metric is *deviation from that floor*." The control bounded
`chronicity` at zero.

**Why the bound moved.** §8's bullet list labels `chronicity` "the bug alarm",
but §8's own discriminator paragraph, and its evidence battery, both state the
signal as a **conjunction**:

> The temporal signature disambiguates a hard world from a broken sim: a spike
> that *recovers* (short half-life) is a novel/extreme world event (a frost, a
> drought) the creatures adapt to — legitimate; a spike that *persists* (no
> recovery, elevated chronicity) is a bug.

`chronicity` reads only the first conjunct — *long* — so a bound on it fires on
episodes that recovered, which the same sentence calls legitimate. Recovery is
the discriminator §8 names, and `stuck` is the quantity that carries both halves.
An episode has a **length** and a **fate**, and the alarm is the cell where both
are bad.

**Why per creature.** The tempting fix is to conjoin the two numbers already
published — `chronicity` high *and* `recovery_ticks == None`. It is worse than
the drift it repairs. `chronicity` is per-creature; `recovery_ticks` is a
**population mean**. One genuinely stuck creature among nine recovering ones
reads chronicity 1.0 with recovery `Some(9.0)`, so a population-scope
conjunction calls that population healthy and **masks the stuck creature** — it
trades a false positive for a false negative. A bug alarm may be noisy; it may
not be silent. `stuck` evaluates the conjunction inside one creature's trace, and
`one_stuck_creature_among_nine_recovering_ones_still_alarms` pins it.

**Why short-and-still-open does not alarm.** A short distress run still open when
the trace ends is **right-censored**: it might have recovered one tick after the
40th, which is undecidable from the trace. Only *long*-and-open alarms. The
asymmetry is deliberate, not an oversight, and
`a_short_run_still_open_at_the_trace_end_does_not_alarm` exists to stop a later
reader "fixing" it.

**What this does not change.** The run counter stays cause-agnostic — §8 defines
chronicity over affect **labels**, not causes — and `CHRONIC_TICKS` (8) is
untouched. `chronicity` is still computed and still reported per seed; it is only
no longer bounded. Nothing was loosened numerically: on all five sweep seeds
(0/1/2/7/42) `chronicity` reads 0.0000 exactly as `stuck` does, so the old bound
and the new one evaluate identically on every measured world. This is a
discriminator upgrade, not a relaxation, and it creates no observable slack.

**Precedent it extends.** *the-living-community* demoted `prevalence` from bound
to diagnostic for the same reason and citing the same spec — a varied world
carries momentary blips that recover, and loosening a number to pass would have
been the seed-shopping [0016](0016-studies-preregister-hypotheses.md) forbids, so
it anchored on
the metric's philosophy instead. That demotion left only a comment in
`windows/lab/tests/health_calibration.rs` and no decision entry. This one
finishes the move it began and reaches the quantity §8 actually named.

**Why this one earns an entry where that one did not.** That demotion was
occasioned by ordinary drift. This one was occasioned by the red control blocking
work — and a bound demotion under those circumstances is structurally
indistinguishable, from the outside, from rationalized seed-shopping. The only
defence is that the change stands entirely on its own spec and its own synthetic
scenarios, and a defence is worthless where nobody looks for it. A
process-integrity audit — "did this project ever loosen a bound to unblock a
campaign?" — reads `docs/decisions/`; it does not read a comment inside a test
file. So the record must be greppable from here, and it must record the
**discipline** as well as the metric semantics:

- justified from The Temperament §8, the metric's defining spec, and from
  nothing else;
- validated against the metric's own synthetic scenarios
  (`windows/lab/src/synthetic.rs`) — the stranded creature that never recovers
  reads `stuck 1.0`, the heat wave that breaks reads `stuck 0.0` — so the alarm's
  liveness is carried by planted traces and forced end-to-end runs rather than by
  a real seed, which is the right place for it, since no measured world has yet
  exhibited the long-but-recovered pattern at all;
- the blocked work's numbers never consulted, and its unblocking verified last,
  separately, and explicitly as a *consequence* rather than a success criterion.

Generalized: **a bound demotion occasioned by blocked work owes a decision entry;
one occasioned by ordinary drift does not.**

**Consequences.** A future reader asking "why isn't `chronicity` bounded?" has an
answer here instead of relitigating it. Re-bounding `chronicity` is a supersede,
not an edit. And the alarm now has a liveness obligation the old one did not: it
reads zero on every healthy world *by construction of the world*, so the
scenarios that make it fire must keep being maintained, because they are the only
thing proving it can.

Ratified at *The Convalescence*'s merge gate.
