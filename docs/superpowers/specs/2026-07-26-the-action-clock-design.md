# The Action Clock — actions cost time, and time is shared

**Campaign:** The Action Clock — campaign 2 of The Rose Window
**Date:** 2026-07-26
**Status:** SHIPPED (2026-07-26) — all six tasks implemented (`071fb429` clock,
`37ded590` mass + day length, `6d26b0a0` the hoist byte-identical, `a2a9716e`
the charge, `45e0ca3a` interleaving), parked at G6 on a preregistered stop, and
closed after re-measurement against a re-frozen baseline. **The stop was never
re-pinned here** — the metric was repaired by its own campaign (The
Convalescence, decision 0080) and the prediction then confirmed on its original
terms anyway. Full record in §11.
**Parent:** `2026-07-25-the-rose-window-metaplan-design.md` §4.1 (four clocks)
and §6.2; **Amendment 1** (§1a) for the program as amended.
**Decisions in force:** 0021 (derive, never author per-creature), 0033
(quantize at emit only), 0040, 0016 (studies preregister), 0069/0072/0075 (the
fine layer this campaign's consumer is building).
**Registry:** `CLIENT-action-clock`, `CLIENT-four-clocks`.

## 1. The gap, corrected

The metaplan says "nothing in Hornvale schedules time at action granularity."
Run against the tree, that is too strong, and the true picture is more useful:

```
  action    time cost today              per-agent?  mechanism
  -------   --------------------------   ----------  --------------
  MoveTo    MOVE_DURATION = 0.1 days     NO          a COST
  Drink     0                            —           free
  Eat       0                            —           free
  Rest      jump to next waking          —           a SKIP
  Hold      jump to next act crossing    —           a SKIP
```

Three separate things are true. **Three of five actions are free.** The one that
costs **costs the same for every creature**. And two arms do not charge time at
all — they *skip* to the next moment something can change (`next_awake_day`,
`next_act`), which is an **event-driven scheduler in embryo**, the same
mechanism that stops a roguelike ticking through nothing.

So this campaign does not build a clock. It finishes one that is half-built.

**Why now.** Free actions have been harmless because drives bound them: once
sated, a drive switches off, so a zero-cost `Drink` buys nothing. The Hearth's
fine layer removes that protection — free actions plus free within-room steps
would let a creature do arbitrarily much in one instant. The gap becomes
load-bearing exactly as the interior does.

## 2. Three rungs, all three shipping (owner decision)

1. **Every action costs.** No free actions; the cost model becomes total.
2. **Cost varies per agent**, derived from what the creature already is —
   never authored per creature (decision 0021).
3. **Agents interleave.** The tick becomes a priority queue over
   `(next_action_time, entity)` instead of walking each creature through the
   whole interval in turn.

Rung 3 was recommended for deferral on the grounds that ordering nobody can
observe is machinery without a consumer — Hornvale has no contested resources,
no combat, no conversation. **The owner's call is to ship it now, and the
reason is sequencing:** The Threshold is landing anchor occupancy, and two
creatures at one anchor is precisely the first place order becomes observable.
Restructuring the tick now is cheaper than restructuring it inside a campaign
that must also do something else.

## 3. Cost

`cost(action, agent, ground) = base(action) × tempo(agent) × terrain(ground)`.

**`base` is authored, per action kind** — five small constants in *ticks*, one
dial each, replacing the single `MOVE_DURATION`. Drinking is quick; a meal is not; sleep
keeps its existing jump-to-waking, which is a *phase*, not a cost, and is not
touched.

**`tempo` is derived from BODY MASS, never authored.**

*Corrected while planning.* This section first derived tempo from
`metabolic_class`, on the stated grounds that body mass "is not currently a
species property." **That was wrong.** `SPECIES_MASS_KG` is a registered trait
predicate (`domains/species/src/lib.rs:34`, read at `:1361`), and `derive_npcs`
already reads `biosphere_registry()` to thread `temperature_niche` and
`metabolic_class` onto `Npc` — so mass is one more line of exactly the move
already made three times.

Mass is also the *better* driver, and decisively so: metabolic class has four
variants of which the live roster uses two, so class-derived tempo would give
the whole world roughly **two** distinct speeds. Per-agent cost that barely
varies per agent is not worth a campaign. Mass is continuous and genuinely
per-species — a mouse and a bear are both endotherms.

```
tempo(mass) = quantize( (mass_kg / REFERENCE_MASS_KG) ^ TIME_EXPONENT )
cost_ticks(action, agent) = round( base_ticks(action) × tempo(agent) )
```

`TIME_EXPONENT = 0.25`: biological *times* — stride period, heart interval,
lifespan — scale as roughly the quarter power of mass, the same allometry the
species domain already invokes for basal rate. Bigger is slower per action.

**The determinism detail that matters.** `powf` is a libm transcendental and
must route through `hornvale_kernel::math`; platform libms differ in the last
ULP, and here that float immediately crosses a **rounding boundary** into an
integer tick count, where a one-ULP difference could flip the result. So the
tempo is `quantize`d (8 significant digits, decision 0033's own helper) *before*
rounding, which makes the boundary reproducible across platforms. This is the
one place in the campaign where cross-platform identity is genuinely at risk,
and it is closed by construction.

Reserved, and deliberately not v1: **temperature-dependent ectotherm tempo** (a
cold lizard really is slower, and the thermal machinery to express it already
ships), and **metabolic class as a secondary modifier** on top of mass.

### 3.1 `terrain` — the macro cost function's other half

Every room-to-room move costs the same today whatever the ground: crossing a
mountain and crossing a plain are both `0.1` days. That is the macro cost
function's missing modifier, and `Terrain` already supplies what it needs.

```
terrain(from, to) = 1 + max(0, elevation(to) − elevation(from)) / CLIMB_SCALE_M
```

clamped to an authored band, and `1.0` whenever either elevation is non-finite
(`Terrain::elevation` returns `INFINITY` for an undescribable room, by its own
documented convention). Only **uphill** costs: a walking creature does not
descend meaningfully faster, and modelling that would be a second dial earning
nothing. The factor applies to `MoveTo` alone — drinking is not steeper in the
mountains.

### 3.2 Resolution: one unit, fine enough for the layer that is coming

A within-room step — anchor to anchor in The Hearth's interior — takes seconds.
A room-to-room move takes hours. At `1_000` ticks per standard day a tick is
~86 seconds, so **a within-room step is a fraction of one tick**: it rounds to
zero, which the totality rule forbids, or to one, which is 86 seconds to cross to
the hearth. The clock would be unable to express the layer it is about to serve.

The fix is resolution, not a second kind of tick:

```
  base        tick ≈    room move   within-room step   u64 headroom
  1e3/day     86 s      100         0 or 1  ← broken   2.5e16 days
  1e5/day     0.86 s    10,000      ~12                2.5e14 days
```

`BASE_TICKS_PER_STD_DAY = 100_000`. The exact-integer-day property (§4.1) is
untouched — `round(day × 1e5)` is exact by construction — and `u64` holds some
700 billion years.

**Why not two tick types**, macro and micro. Four independent settled answers
agree, and they agree against it. Discrete-event simulation uses one global clock
at the finest needed resolution and one queue, because two clocks mean a
conversion at every interaction and conversions are where drift enters — and this
campaign's priority queue *is* a DES. Roguelike energy systems (DCSS's 10 `aut`
per normal turn) pick a fine base precisely so fractional speeds are
representable, rather than adding a second clock. Multirate numerical integration
does use fast and slow subsystems, but keeps **one time axis** and varies the
*step size* — which is `UNI-32`'s coarse-constrains-fine, two step sizes and one
clock. And music notation is the cleanest statement: a whole note and a
sixteenth are one unit at different denominations, which is exactly why a bar can
contain both; make them different *types* and you cannot add them, though adding
them is the correct operation rather than an error.

Macro and micro are the same dimension — elapsed time. What differs is the
**cost function** (terrain modifies moving between rooms; features and path will
modify moving within one), and that is two derivations of one unit. There is a
determinism argument too: the queue must totally order a creature mid-room-
crossing against one mid-hall-walk, and one unit gives that for free where two
would need a conversion inside the ordering path.

## 4. Integer scheduling, `f64` commits, and a clock that divides the planet's day

The scheduler orders agents by *when they next act*. That ordering must be a
**total order with deterministic ties**, and `f64` days cannot supply one: it is
not `Ord`, and accumulated float addition is precisely the drift the project
bans in `astar`, whose costs "are `u64` integers specifically to avoid float
non-determinism."

So: **schedule in integer ticks; commit in `f64` days.**

```
  scheduling  Ticks(u64)     internal, exact, totally ordered, never serialized
  committing  day: f64       the existing save-format contract, unchanged
```

This is **quantize-at-emit applied to time** — the third instance of one
discipline (decision 0033 for floats, 0069 for space, this for time). The queue
key is `(Ticks, EntityId)`: a `BTreeSet` over integers, ties broken by entity, so
the order is a pure function of the frozen ledger.

### 4.1 The tick divides the planet's day (owner decision)

A fixed `TICKS_PER_DAY` would be arbitrary. The clock is derived instead:

```
ticks_per_local_day = max(1, round(day_length_std × BASE_TICKS_PER_STD_DAY))
tick duration       = day_length_std / ticks_per_local_day   ≈ 1/BASE std days
```

with `BASE_TICKS_PER_STD_DAY = 100_000` authored (§3.2) and `day_length_std` read from the
world's `Calendar` (`domains/astronomy/src/calendar.rs:542` —
`day: Option<StdDays>`, the local rotation period in standard days).

**Two properties hold at once, and both are wanted.**

- **The tick stays approximately absolute.** Base costs are authored *in ticks*,
  and a tick is within one part in `ticks_per_local_day` of `1/1000` of a
  standard day on every world. So a creature's stride does not slow because its
  planet rotates slowly — which would be wrong: a bear's gait is set by the bear,
  not by the sky. Cross-world variation in an action's absolute cost is under
  0.1%.
- **The local day is an EXACT integer number of ticks.** This is the reason to
  derive it at all. `ActivityCycle` — `is_awake`, `next_awake_day` — is the sim's
  one genuinely local-day-keyed mechanism, and under an arbitrary granularity
  every dawn and dusk rounds to the nearest tick. Over a long run (the health
  battery simulates many days) those roundings *beat* against the day cycle and a
  creature wakes a hair earlier each morning for no physical reason. Making the
  day divide exactly removes the beat by construction rather than bounding it.

**A tidally-locked world has no day** (`Calendar::day` is `Option`), and this is
not hypothetical — the rotation pin admits it. There, `ticks_per_local_day` falls
back to `BASE_TICKS_PER_STD_DAY` and a tick is exactly `1/1000` of a standard
day. Stated rather than left to a `unwrap_or`, because a world with no dawn is
exactly the world where a day-derived clock has nothing to derive from.

**The coupling this introduces, stated plainly.** The scheduler's granularity is
now downstream of astronomy: an epoch of the rotation draw changes the clock's
resolution. Nothing serialized depends on it — `Ticks` never leaves the
scheduler — so no save breaks, but the two are no longer independent, and a
future sky epoch should expect the walk to move.

## 5. The constraint that keeps interleaving safe

Interleaving changes **when** each agent acts. It must not change **what an
agent can see**.

Today each creature reads `frozen` — the pre-tick ledger — plus its own emitted
facts. If interleaving let creature B observe creature A's mid-tick move, every
cross-agent read would become order-sensitive, and that is exactly the hazard
`alarm_field` was designed around: PSY-12's determinism line is that *a field
over the frozen population is order-independent; direct agent-to-agent reads
mid-tick are not.*

**So cross-agent reads stay frozen-based.** The alarm field, the hazard
memory's roster, and the band's shared belief all continue to read the pre-tick
ledger, exactly as now. Interleaving is additive to the determinism story rather
than a threat to it — it reorders *acting*, not *perceiving*.

The consequence to state plainly: two creatures acting at the same simulated
moment do not see each other within that tick. That is the same one-tick
latency the alarm field already has, and it is what makes the wave terminate.

## 6. What the restructure actually touches

`DriveMovements::step` currently owns nine pieces of per-creature state in loop
locals — `pos`, `last_drank`, `last_rested`, `last_ate`, `believed`, `visited`,
`steps`, `mode`, and the walk's own `day`. Under interleaving those must live
across queue pops, in a `BTreeMap<EntityId, WalkState>` built once from
`frozen`.

That is the campaign's real work, and it is a *refactor with a behavioural
consequence* rather than a new subsystem. The decision loop, the arbitration,
the drives and the fact emission are untouched; what changes is who calls them
and in what order.

## 7. Determinism and drift

**No genesis change**, no new predicate, no new serialized quantity: `Ticks` is
internal to the scheduler and never leaves it.

**This campaign is not byte-identical, and cannot be.** Charging `Drink` and
`Eat` shifts when discharges land; per-agent tempo shifts every walk;
interleaving reorders emission within a tick. Seed-42 galleries and the health
battery both move.

That makes it a **scoped-drift campaign**, like The Haunt: the drift must be
*named and justified*, creature by creature where it is small enough to read,
and never regenerated over. The acceptance protocol is §8.

## 8. Acceptance

Preregistered before the first task, with signs, in the ledger (decision 0016):

- **Freeze the baseline from main's tip** — seed-42 galleries and the health
  battery, recorded with the commit SHA, *before* any code lands. A baseline
  taken mid-campaign aliases other campaigns' physics into this measurement.
- **Predictions.** Heavier creatures cover less ground per interval and reach
  water later; the lightest are least affected. The spread should be visible
  across species rather than clustered into two buckets — if it is not, the mass
  trait is not reaching `Npc`.
  Ametabolic agents are unaffected *entirely* — they carry no drives, so their
  walks must be identical to the byte, which is a sharp internal control.
- **The health null-control still holds**: chronicity `0.0` and every distress
  run recovering. Prevalence and the by-cause breakdown may move; chronicity
  moving is a stop.
- **Order is a pure function of the frozen ledger** — the same tick run twice
  yields the same emission sequence, and a shuffled `npcs` input yields the same
  result (the queue's tie-break, not the input order, decides).
- **No free action remains**: a property test that every `Action` advances the
  clock, so a future action cannot silently be added for free.
- **Re-time the health battery** — the longest sim in the suite, never the
  possession walk.

## 9. Scope

**In:** the five base costs; `tempo` from body mass; the `terrain` climb factor
on `MoveTo`; `Ticks`, the day-derived tick rate at `1e5` resolution, and the
conversion boundary; the priority queue and the hoisted `WalkState`; the
frozen-read constraint; the drift protocol.

**Out, each with a home:**

- **Micro (within-room) action costs.** Within-room movement is not live —
  The Hearth ships the anchor graph but nothing derives an interior or places a
  creature at an anchor; that is The Threshold. So a micro cost function has no
  consumer. This campaign's obligation is only to pick a base **fine enough that
  the fine layer needs no clock change when it arrives** (§3.2), and stop there.
- **Maintenance conditions for interval actions** — "she was interrupted"
  requires a condition that holds *throughout* an action, not just at entry.
  Owed to this campaign by The Hearth's §12, and deferred because v1's actions
  are instantaneous-with-a-cost rather than genuinely durative: nothing yet
  happens *during* one. It lands with the first action long enough to interrupt.
- **Allen's interval algebra** — thirteen relations, jointly exhaustive and
  pairwise disjoint, with a published composition table; the settled vocabulary
  if overlapping actions ever need reasoning about, and the exact twin of The
  Hearth's RCC-8 borrow. Not needed while actions do not overlap.
- Temperature-dependent ectotherm tempo; body mass as the tempo driver.
- Initiative as a *contested* quantity (who acts first when it matters) — that
  is a combat concern and combat is ordered after Vitality (metaplan §6.5).

## 10. Flagged for G3

1. **[drift — leads this list] This campaign cannot be byte-identical**, and
   unlike The Haunt's scoped drift it moves *every* metabolic creature rather
   than a few beasts. The galleries and the health battery both shift. The
   protocol is §8: preregister, freeze the baseline first, name the movement,
   never regenerate over it. Confirm that is the accepted cost.
2. **[scope — owner already decided] All three rungs.** Recorded here with the
   reasoning: interleaving has no consumer *today*, and its consumer (The
   Threshold's occupancy) arrives next. Restructuring the tick now is cheaper
   than doing it inside a campaign with another job.
3. **[determinism] `Ticks` as an internal integer clock.** Nothing new is
   serialized, and `f64` days remain the committed contract — but this is a new
   time representation in a project with one, so it leads the determinism
   review by the same convention that put epochs and save-format calls first.
4. **[risk] Interleaving is a refactor of the hottest loop in the sim.**
   `DriveMovements::step` is where the health battery spends its ~325 s. The
   restructure could plausibly cost or save meaningfully, and the honest
   position is that it is **unmeasured**. Time it on the health battery, not the
   possession walk.
5. **[interaction] The Threshold is in flight and touches the same loop.**
   Both campaigns edit `DriveMovements::step`. Sequencing or a deliberate merge
   plan is an owner call, not autopilot's.

## 11. The park, the stop, and the re-measurement (durable record)

This section is what the park commit (`f30a03ce`) said it was writing and did
not: its message claims "Records the full measurement table durably, since the
ledger is gitignored scratch," but its diff was the status header alone. The
table below is recovered from that scratch ledger, which survived only because
the worktree was not cleaned in the interval.

### 11.1 The stop, as it fired (retired measurement, `adafe55c`)

`chronicity` moved `0.0 → 0.1` on seed 42 — a preregistered **stop** (§8,
decision 0016). `make gate` was red on exactly two tests, both the chronicity
null control; fmt, clippy, type-audit and the other 2121 tests passed.

The investigation found no pathology. One creature of ten (the 90 kg
rust-monster) carried a 9-tick distress run: four `Helpless/Thirst`, ONE
`Frustrated/Fatigue`, four `Helpless/Thirst`. The 4-tick thirst block is seed
42's ordinary cadence — goblin, bugbear and giant-elk all show it, all
sub-threshold at 4 < `CHRONIC_TICKS` = 8. What made this one chronic is that the
tick *bridging* the two blocks was itself a distress label rather than the
`Searching`/`Eager` that breaks every other creature's run. The alarm fired on a
BRIDGED run across two causes, not on a creature blocked for 8 ticks. The
creature was not worse off — seed 42's prevalence actually *fell*.

`health_report` counted consecutive distress LABELS, cause-agnostic, so two
separately-recoverable rhythms that phase-aligned read as one chronic episode.

**Why this campaign could not fix it.** The null control's own comment defined
the alarm as chronicity > 0 AND `recovery_ticks == None` while asserting only
the first, so applying its documented philosophy would have made the gate green.
That is exactly why the campaign was not entitled to do it: a campaign may not
repair the instrument judging it. Owner decision — park, fix the metric as its
own campaign, then land this. That campaign was **The Convalescence** (decision
0080: chronicity is a diagnostic, the alarm is `stuck`).

### 11.2 The re-measurement (`0e93b7a1`, after absorbing 112 commits)

The `adafe55c` baseline was **retired, not reused**: main had moved five code
campaigns, `HealthReport` had gained a field, the species roster had changed,
and the battery's own wall-clock had fallen 446 s → 266 s on main's own work.
Baseline re-frozen from `origin/main` @ `f845283d`.

| seed | prevalence (base → new) | chronicity | stuck | recovery_ticks (base → new) |
|---|---|---|---|---|
| 0  | 0.260000 → 0.260000 (0)       | 0.0 → 0.0 | 0.0 → 0.0 | 4.000000 → 4.000000 (0) |
| 1  | 0.312500 → 0.332500 (+0.0200) | 0.0 → 0.0 | 0.0 → 0.0 | 4.047619 → 3.821429 (−0.226) |
| 2  | 0.105000 → 0.107500 (+0.0025) | 0.0 → 0.0 | 0.0 → 0.0 | 3.400000 → 3.181818 (−0.218) |
| 7  | 0.180000 → 0.170000 (−0.0100) | 0.0 → 0.0 | 0.0 → 0.0 | 2.560000 → 2.666667 (+0.107) |
| 42 | 0.140000 → 0.137500 (−0.0025) | 0.0 → 0.0 | 0.0 → 0.0 | 3.833333 → 3.615385 (−0.218) |

`danger`, `social` and `thermal` by-cause are 0.0 on every seed, both sides.
Battery wall-clock 250.65 s vs 266.36 s baseline (one run each).

**The stop cleared on its own original terms.** `chronicity` reads 0.000000 on
all five seeds including 42 — so §8's prediction 4 is satisfied as preregistered,
without leaning on decision 0080. The alarm `stuck` is also 0.0 everywhere, so
the campaign is green under both the old bound and the new one. The `HHHH F
HHHH` weld was a phase artifact of `adafe55c`'s physics; main's own changes moved
seed 42 off that knife edge before the repair applied. **Nothing in the cost
model was implicated by the investigation, and nothing in it was touched.**

### 11.3 The seven predictions, as preregistered

1. **CONFIRMED** — `a_heavier_creature_covers_less_ground_in_the_same_interval`.
2. **CONFIRMED** — `tempo` monotone in mass, unity at 70 kg by construction:
   kobold 13.6 kg → 0.66391154, xorn 55 kg → 0.94149098, hobgoblin 74.8 kg →
   1.0167189.
3. **CONFIRMED** — seed 42's derived population spans 7 species and **7 distinct
   tempi**, 0.66391154 → 1.5923137. Measured directly, because the pin that
   guards it asserts only `distinct > 1`, which two buckets would satisfy.
4. **CONFIRMED** (REFUTED at `adafe55c`) — chronicity 0.0 on all five seeds.
5. Reported, not judged — |Δ prevalence| ≤ 0.02, both signs present.
6. **CONFIRMED, sharply** — seed 42's xorn (Ametabolic) trace is 40/40
   `Content/None` and byte-identical between baseline and branch, while the
   metabolic creatures beside it moved. The metabolism gate leaves an Ametabolic
   creature's `drives` vector empty, so it never selects an action.
7. **CONFIRMED** — `danger` and `social` by-cause 0.0 on all five seeds.

Six confirmed and one refuted at the park; **all seven confirmed** on the
re-measurement.
