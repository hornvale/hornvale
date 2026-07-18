# The Quickening (The Walk · Milestone 2 · first liveness) — Design

Program: **The Walk** (metaplan `docs/superpowers/specs/2026-07-11-the-walk-metaplan-design.md`,
Milestone 2 "the living world," Campaign V "The World Moves" — first minimal
slice). Base: main `@7305dfc`. This is **not** the ECS substrate program (that
closed at c6 *The Ordination*; ECS c7 spatial-partition is reserved per
decision 0037, no measured trigger). Chronicle-name candidate: **The
Quickening** — the world's first perceptible autonomous motion after six
campaigns of a frozen world; confirmed at close.

## 1. What this is

Hornvale's world is generated at genesis and then **frozen**. Milestone 1 of
The Walk shipped a walkable frozen slice — `possess --seed N` gives a
look/go/examine loop over "a pure step function over a frozen world" (The Seam,
The Casement). The physical layer (sky, climate) already advances as a *derived
field over time* — the almanac renders the sky at any day. But the **social
layer** — settlements, populations, everything the agent layer commits at
genesis day 0 — never changes. Nothing the world's own agents *do* accumulates
over sim time.

This campaign lands the smallest real "the world moves without you": while you
possess an agent and let time pass, **settlements grow, and cross discrete tier
milestones** (hamlet → village → town) that the world *remembers*. It is the
first place the whole substrate program pays off at once:

- **c6's tick** (`TickSystem` + `tick`) — the mechanism, now actually *run*, to
  commit a dated event when time advances.
- **c5's kind-change-over-sim-time** (dated facts, latest-wins reads) — a
  settlement's tier is exactly the "kind that changes over sim time" c5 built
  for.
- **demography's carrying capacity** — the setpoint the growth closes toward.

It is deliberately tiny: one actor (settlements), one deterministic trajectory
(logistic growth toward K), one kind of committed event (a tier milestone). No
GOAP, no individual situated agents, no belief — those are later Milestone-2
campaigns.

## 2. Scope

**In:**

1. **The derived population trajectory.** `population(settlement, t)` — a pure,
   deterministic function of the settlement's genesis population `P0`, its
   cell's carrying capacity `K`, and elapsed `WorldTime`: logistic growth toward
   K (§4.1). Lazy, re-computed on query, **never committed** (it is reversible —
   the seam rule, §3).
2. **The derived tier.** `tier(settlement, t)` — a pure classification of
   `population(settlement, t)` against authored thresholds (hamlet / village /
   town). Also never committed; the *current* tier is always a derivation.
3. **The milestone `TickSystem`.** When the possess session advances time, a
   `TickSystem` (run through c6's kernel `tick`) evaluates each settlement's
   derived tier at the new time, and **commits a dated `settlement-tier` event
   for each threshold crossed** since the last evaluation — the discrete,
   irreversible record that a promotion *happened* (§3, the seam rule: derive
   the smooth trend, commit the discrete divergence). This is the campaign's use
   of c6's tick.
4. **The `wait` verb.** The possess session gains `wait <interval>` (a season /
   a year / N days): it advances the session's `WorldTime`, runs the milestone
   tick over the elapsed interval, and re-focalizes.
5. **Observation on re-query.** After `wait`, `look` (and the almanac at the new
   day) reflect each settlement's current tier — read as latest-committed-tier
   ELSE derived-from-population (§4.3). "The hamlet has become a village since
   you last passed."
6. **Provenance.** A committed milestone is a dated, provenanced fact, so the
   world *remembers*: an `explain`/history read can recount "this became a town
   on day 412 when its population passed 500."

**Out (deferred; captured in `followups.md`):**

- **Individual situated agents + movement** — needs an agent population with
  positions/needs (none exists); the next liveness campaign.
- **The GOAP motivation engine (PSY-6)** — the full planner + autonomy ladder;
  this slice honors only the setpoint/gap-closing *idea* (K is the setpoint),
  not the planner (§4.4).
- **Belief / inference (UNI-1)** — Milestone-2-later.
- **Richer demography dynamics** — stochastic shocks, age structure, migration,
  Malthusian collapse (§4.1 fidelity note).
- **Player-acts-mutate (Campaign IV)** — the player's own verbs committing
  events is a sibling slice, not this one (this slice is *the world moves
  without you*).
- **Genesis-time liveness** — no milestone is ever committed at world build; the
  world moves only inside a possess session (§3).

## 3. Determinism contract — the save-format line (leads G3)

**Genesis worlds are byte-identical; liveness is census-free. The world moves
only inside a possess session, never at build.**

- **No milestone is committed at genesis.** `build_world` commits exactly the
  facts it commits today. A genesis world — what the census, the three seed-42
  almanacs, and every gallery/laboratory artifact measure — is **byte-identical**;
  the existing drift-check is the proof. **No census regen, no epoch.**
- Milestones commit **only** when a possess session advances time. The possess
  session's `World` registers the liveness predicate (`settlement-tier`) at
  session start and commits milestone facts as `wait` runs — an *evolved* world,
  distinct from the frozen genesis world by construction.
- **Byte-deterministic session:** same seed + same sequence of `wait`/verbs →
  byte-identical committed milestones. No player RNG; the trajectory and the
  crossings are pure functions of (seed, elapsed time).
- **Lorenz-safe (§11):** logistic growth toward K is monotone and non-chaotic;
  no chaotic integrator is seeded from quantized ledger floats. The trajectory
  is re-derived from `P0`/`K`/`t` at full precision each query.
- **The existing possession-transcript gallery artifact is at day 0 (frozen) →
  unchanged.** The `wait` verb is opt-in; the committed transcript does not
  advance time, so no committed artifact drifts. A **new** over-time transcript
  may be *added* (and drift-checked), never a diff of the old one — verified at
  plan time.
- `settlement-tier` is a **game-layer predicate**, owned and registered by the
  liveness window (The Walk §11: new windows only, domains untouched). It never
  enters a genesis world's registry.

## 4. Architecture

The Walk §11: **new window, domains untouched.** The liveness logic lives in a
game-layer window (extending `windows/vessel`, or a small new `windows/liveness`
it depends on — plan's call), consuming `domains/demography` (K),
`domains/settlement` (P0, `POPULATION`), and the kernel (`tick`, the ledger,
`WorldTime`). No domain crate is modified.

### 4.1 The population trajectory (derived, logistic)

```
population(settlement, t) = K / (1 + ((K - P0) / P0) · e^(−r · (t − t0)))
```

- `P0` = the settlement's genesis `POPULATION` fact (day 0). `K` = the carrying
  capacity at the settlement's cell (`demography::carrying_capacity`, a per-cell
  `f64`). `t0` = 0 (genesis). `r` = a single authored growth-rate constant.
- Pure, deterministic, monotone toward K, bounded (Lorenz-safe). Re-computed on
  query; **never stored**.
- Transcendentals (`exp`) route through the kernel's libm wrapper
  (`kernel::math`) for cross-platform byte-identity (decision 0041), consistent
  with every other float in the sim.

**Fidelity note (Nathan's call at G3):** logistic is a coarse first model — real
demography has stochastic shocks (banned: no RNG), age structure, migration, and
Malthusian collapse. The first slice takes the smooth monotone logistic because
the *milestone* framing only needs threshold-*crossing*, so the trajectory's
exact shape is low-stakes here; richness is a followup.

### 4.2 The tier classification (derived)

Authored thresholds map population to a tier:

```
tier(p) = Hamlet   if p <  VILLAGE_MIN
          Village  if VILLAGE_MIN <= p < TOWN_MIN
          Town     if p >= TOWN_MIN
```

Three tiers for the first slice (hamlet/village/town); `City` and beyond are a
trivial threshold addition later. The current tier is always `tier(population(
settlement, t))` — a pure derivation, no committed fact required to *read* it.

### 4.3 The milestone system (committed via c6's tick)

The committed event is what makes this **liveness** (accumulating, remembered
history) rather than merely another derived field. A `TickSystem`:

```
struct SettlementMilestones { settlements: Vec<EntityId>, at_time: WorldTime };
impl TickSystem for SettlementMilestones {
    fn label(&self) -> &'static str { "settlement-milestones" }
    fn step(&self, frozen: &Ledger) -> Vec<Fact> {
        // for each settlement: derive tier(population(s, at_time)); compare to
        // the LAST committed settlement-tier (latest-wins over `frozen`) ELSE
        // the tier at the session's start-time; emit a dated settlement-tier
        // fact for each crossing (day = at_time).
    }
}
```

- `wait <interval>` advances the session time to `t_new` and runs **one** `tick`
  with `at_time = t_new` — a *jump*, not a per-day loop (the derived trajectory
  + crossing test is O(1) per settlement; demography is year-scale, so per-day
  stepping is waste, §4 crux #4).
- `settlement-tier` is **non-functional** (a settlement's tier changes over
  time — the same shape as c5's `instance-of`); the current tier is the *latest*
  committed one (reuse c5's latest-wins read), else the derived tier. A promotion
  is monotone (population only grows toward K), so tiers never regress in the
  first slice.
- The read for rendering: `settlement_tier(ledger, s, t)` = latest committed
  `settlement-tier` fact for `s` ELSE `tier(population(s, t))`.

### 4.4 How much PSY-6

**None of the planner.** The milestone rule is a single deterministic threshold
check over a derived quantity. PSY-6's setpoint/gap-closing idea is honored
*conceptually* — carrying capacity K is the setpoint, population the state,
growth closes the gap — but the GOAP planner, the autonomy ladder, and belief
are deferred. The first slice proves "the social world moves observably and is
remembered," not "agents plan."

### 4.5 The `wait` verb + observation

`windows/vessel/src/session.rs` gains a `wait` arm in its `match verb`
(alongside look/go/examine). It parses an interval, advances the session's
`WorldTime`, runs the milestone tick, and returns a focalized line noting any
change the agent would perceive ("Time passes. The hamlet below has swelled into
a village."). `look` and the almanac at the new day render each settlement's
current (possibly promoted) tier via §4.3.

## 5. Correctness — the ladder

The load-bearing new property is **the world moves only in-session, and
deterministically**.

1. **Type-level.** Reuse c6's `TickSystem`/`tick` and c5's latest-wins read; no
   `HashMap`; the trajectory is a pure `fn`.
2. **Property — SESSION DETERMINISM.** Same seed + same `wait` sequence →
   byte-identical committed milestone facts (serialize the session ledger twice,
   compare). Plus: the trajectory is monotone (population(t+dt) ≥ population(t))
   and bounded (population(t) < K for all t) — seeded-loop property tests over
   random (P0, K, r, t). Plus: **the milestone crossing is exact** — a tier read
   at `t` equals the tier derived from `population(t)` for the whole trajectory
   (no missed or spurious crossing), including a `wait` that jumps *past* a
   threshold in one step (the jump must still commit the crossing it skipped
   over).
3. **Genesis byte-identity (the master oracle).** A pinned test: a freshly built
   genesis world contains **zero** `settlement-tier` facts. Every committed
   artifact is byte-identical (the drift-check); no census regen.
4. **Observation shadow.** A possess-session integration test: possess seed 42,
   `wait` enough time to cross a threshold, `look` — the description reflects the
   new tier; `explain`/history recounts the dated promotion.

Every new test is **mutation-verified** before it counts (measure-don't-narrate):
break the crossing test (skip a threshold the jump passed), break monotonicity,
break the genesis-zero pin (commit a milestone at build) — each must redden its
test.

## 6. Perf budgets (rolled-our-own; no criterion)

- **Deterministic, gated:** the milestone facts committed by a fixed possess
  script (seed 42, a fixed `wait` sequence) are a pinned, drift-checkable set
  (a count + the dated tiers). No wall-time hot path (one O(#settlements) tick
  per `wait`; #settlements is small).
- No new unbounded structure at genesis (milestones accrue only in a live
  session, bounded by #settlements × #tiers — a handful).

## 7. Stages (each independently testable; genesis byte-identical throughout)

1. **The trajectory + tier (derived, pure).** `population(s, t)` (logistic) +
   `tier(p)` + `settlement_tier(ledger, s, t)` read (latest-else-derived). Unit
   + property tests (monotone, bounded, exact classification). No ledger writes,
   no session changes — pure functions.
2. **The milestone `TickSystem`.** `SettlementMilestones::step` emitting dated
   `settlement-tier` facts for crossings; the crossing-exactness test (incl. the
   jump-past-a-threshold case); the genesis-zero-milestones pin.
3. **The `wait` verb + session time-advance.** The `wait` arm; session
   `WorldTime` advance; run the tick; the session-determinism property; the
   possess-session observation integration test (wait → look reflects the new
   tier).
4. **Provenance + a gallery over-time transcript.** `explain`/history recounts a
   dated promotion; add a NEW drift-checked "a possession over time" transcript
   artifact (the old frozen transcript unchanged). Book (chronicle, freshness
   sweep, registry) + retrospective + close.

## 8. Risks and open items

- **The jump-past-a-threshold case** is the one subtle correctness point: a
  single `wait` that advances years may cross *multiple* thresholds at once; the
  tick must commit each crossing it jumped over (§5.2), not just the final tier.
  Property-covered and mutation-verified.
- **The over-time transcript artifact** is the only committed artifact this
  campaign adds; it must be a NEW file (the frozen day-0 transcript stays
  byte-identical), verified by the drift-check at close.
- **`type-audit` runs in `make gate`** (The Presiding): the new pub boundaries
  (the trajectory fn, the tier enum, the tick system, the `settlement-tier`
  predicate const) carry their verdict tags from the first commit.
- **Domains untouched** (The Walk §11): the growth model + tiers live in the
  game window, consuming demography/settlement — verified by the architecture
  test (`cli/tests/architecture.rs`); if the trajectory genuinely belongs in
  demography later, that's a deliberate move, not this slice.

## 9. Decisions (promoted from the campaign decision ledger #1–#7)

- **#5 — determinism/save-format (leads G3):** census-free, genesis
  byte-identical; the world moves only in a possess session; no epoch, no census
  regen; the over-time transcript is a new artifact, the frozen one unchanged.
- **#3 — the eager/lazy hybrid (leads G3 with #5):** derive the smooth trajectory
  (reversible — the seam rule), commit only the discrete milestone via c6's tick
  (irreversible); overturned "commit a daily population fact."
- **#2 — the first actor:** settlement demography → tier milestones (the smallest
  deterministic actor reusing existing substrate that yields discrete milestones).
- **#4 — the time model:** `wait` jumps to the new time and commits crossings in
  the interval (not a per-day loop); tier read is latest-committed-else-derived
  (reuse c5).
- **#6 — fidelity (flagged):** logistic growth toward K, one authored rate
  constant; richer demography deferred.
- **#7 — PSY-6:** none of the planner; the setpoint idea only.

## 10. Definition of Done

- All four stages green under `make gate` (incl. type-audit); the full
  `cargo nextest run --workspace` before any FF; census fixtures untouched
  (byte-identical — genesis commits no milestone; verified by diff against the
  merge-base).
- Session determinism + crossing-exactness + genesis-zero-milestones green;
  mutation-verified.
- Book: a chronicle entry (candidate *The Quickening*), a freshness sweep (the
  vessel/possess chapters that describe the world as frozen — now it can move on
  `wait`), the new over-time transcript artifact, and the frontier bookkeeping
  (The Walk metaplan Milestone 2 opened; the relevant registry rows — PSY-6
  setpoint idea touched, UNI-20 derived-view, MAP-27 — re-pointed).
- A one-page retrospective in `docs/retrospectives/`.
- The followup register's rows promoted into the retrospective.
