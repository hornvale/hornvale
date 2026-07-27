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
**population mean**. One genuinely stuck creature among nine recovering ones (all
ten carrying a long run) reads chronicity 1.0 with recovery `Some(9.0)`, so a
population-scope conjunction calls that population healthy and **masks the stuck
creature** — it trades a false positive for a false negative. Note that the
discriminator there is `Some` against `None`, presence of any recovery at all, not
its magnitude: 9.0 is in fact *elevated* beside the 2.56–4.04 the measured worlds
read, and the conjunction still passes the population as well. A bug alarm may be
noisy; on the class of faults it is *defined over* it may not be silent — and a
population-scope conjunction is silent exactly there. (`stuck` has a residual
class of its own, outside the one it is defined over; *Consequences* below names
it.) `stuck` evaluates the conjunction inside one creature's trace, and
`one_stuck_creature_among_nine_recovering_ones_still_alarms` pins it.

That fix is not hypothetical: the precedent below already wrote it. The comment
above the failing assertion (`windows/lab/tests/health_calibration.rs:84-85`)
spells the alarm out as "`chronicity > 0` and `recovery_ticks == None`" — the
right conjunction at the wrong scope, between a per-creature fraction and a
population mean. So the prose that outran its assertion was not a correct check
merely left unwritten; transcribed literally it would have produced exactly the
masking-prone form rejected here.

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
discriminator upgrade, not a relaxation: the two readings coincide everywhere the
project can currently look, so no *observable* slack was created and nothing was
relaxed to let anything through. What changed is which of two coincident readings
the control is entitled to when they eventually part company.

**Precedent it extends.** *the-living-community* demoted `prevalence` from bound
to diagnostic for the same reason — a varied world carries momentary blips that
recover, and loosening a number to pass would have been the seed-shopping
[0016](0016-studies-preregister-hypotheses.md) forbids, so it anchored on
the metric's philosophy instead. (Its comment cites that philosophy as "The
Slumber §7/§8"; the Slumber spec carries no numbered sections, so the citation
does not resolve — but the move it names is the same one, and the substance is
0016's.) That demotion **is** in the decision log:
[0073](0073-epoch-granularity-is-declared.md) records it, as a supporting clause
under "pin invariants, not values." What it does not have is an entry of its own.
This one finishes the move it began and reaches the quantity §8 actually named.

**Why this one earns an entry of its own.** That demotion was occasioned by
ordinary drift. This one was occasioned by the red control blocking work — and a
bound demotion under those circumstances is structurally indistinguishable, from
the outside, from rationalized seed-shopping. The only defence is that the change
stands entirely on its own spec and its own synthetic scenarios, and a defence is
worth what its findability is worth. A process-integrity audit — "did this project
ever loosen a bound to unblock a campaign?" — reads `docs/decisions/`, and the
precedent is indeed there; but it is there inside an entry *titled about epoch
granularity*, reachable by content-grepping the log rather than by reading its
index. That serves an auditor who already suspects the question, and not one who
does not. A first-class entry under its own title is what this campaign owes, and
it must record the **discipline** as well as the metric semantics:

- justified from The Temperament §8, the metric's defining spec, and from
  nothing else;
- validated against the metric's own synthetic scenarios
  (`windows/lab/src/synthetic.rs`) — the stranded creature that never recovers
  reads `stuck 1.0`, the heat wave that breaks reads `stuck 0.0` — so the alarm's
  liveness is carried by planted traces and forced end-to-end runs rather than by
  a real seed, which is the right place for it, since no seed in the five-seed
  sweep exhibits the long-but-recovered pattern **under main's current physics**.
  It is not merely theoretical, though: an investigation produced it on a real
  seed — seed 42, under a one-tick phase perturbation of one creature's distress
  rhythms, welding `4 + 1 + 4` into a nine-tick run that then recovered
  (`recovery_ticks = Some(2.586)`; the campaign spec §1 records the trace). That
  is what showed the drift was *reachable* rather than hypothetical, and it is
  the reason the repair is necessary rather than tidy;
- the blocked work's numbers never consulted, and its unblocking verified last,
  separately, and explicitly as a *consequence* rather than a success criterion.

Generalized: **a bound demotion occasioned by blocked work owes a decision entry
of its own; one occasioned by ordinary drift is adequately served by a clause
wherever it comes up.** (The precedent above got the latter — recorded, but as a
clause inside 0073 — and that was proportionate to its circumstances.
Circumstances determine not just whether the record exists but how findable it
has to be.)

**Consequences.** A future reader asking "why isn't `chronicity` bounded?" has an
answer here instead of relitigating it. Re-bounding `chronicity` is a supersede,
not an edit. And the alarm now has a liveness obligation the old one did not: it
reads zero on every healthy world *by construction of the world*, so the
scenarios that make it fire must keep being maintained, because they are the only
thing proving it can.

**The residual class this choice accepts.** `stuck` reads the fate of the run that
is *open at the final sample*, so there is a shape of trace on which every
surviving bound is silent: near-total distress that recovers in the last ticks. A
creature distressed for ticks 1..=38 and Content for 39..=40 reads `stuck 0.0`
(the long run ended), with `chronicity 1.0` and `prevalence ≈ 0.95` — and both of
those are now reported-but-unbounded diagnostics, so the control passes green on a
creature that spent 95% of the span in distress. The 2×2 above reasons over one
episode's **length** and **fate** and never over its **multiplicity or duty
cycle**, which is where this class lives. That is the price of choosing *fate* as
the discriminator, and the choice is still right — fate is what §8 names, and it
is what separates a hard world from a broken sim — but the price is real and is
recorded here rather than discovered later. Seeing this class needs a *different*
family member (a longest-run or distress-duty-cycle diagnostic, registered as a
followup, not built by this campaign), not a re-bounding of `chronicity`. So the
claim above is scoped, not absolute: the alarm may not be silent on the class it
is *defined over*, and this is the class it is not defined over.

**See also.** [0073](0073-epoch-granularity-is-declared.md), whose "pin
invariants, not values" obligation cites the `prevalence` demotion as its
in-tree precedent. Its present-tense phrasing of the surviving invariant —
"chronicity stays zero; every distress run recovers" — is the reading this
decision supersedes: the invariant is now *`stuck` stays zero; every distress run
recovers*, with `chronicity` reported and unbounded. 0073 stands as written;
decisions supersede rather than get edited.

Ratified at *The Convalescence*'s merge gate.
