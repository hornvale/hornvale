# The Escapement — `WorldTime` becomes integer ticks

**Campaign**: The Escapement
**Date**: 2026-08-23
**Branch**: `campaign/the-escapement`
**Base**: `27312e02c` (`origin/main`)
**Decision block**: 0186–0195
**Status**: SHIPPED — G3 approved 2026-08-23, G6 approved 2026-08-24. Decisions 0186–0191 minted; 0230 superseded by 0191. Chronicle `book/src/chronicle/the-escapement.md`, retrospective `docs/retrospectives/the-escapement.md`.

An escapement is the part of a clock that converts continuous motion into
countable discrete beats. That is this campaign.

---

## 1. The problem, measured

`hornvale_kernel::WorldTime` is `#[serde(transparent)]` over `f64`
fractional standard days. Every emitted float in Hornvale passes through
`hornvale_kernel::quantize` at 8 **significant** digits (decision 0033), so
`WorldTime` does too — at `kernel/src/ledger.rs:324`:

```rust
fact.day = fact.day.map(|d| {
    crate::field::WorldTime::new(crate::quantize::quantize(d.day()))
```

Significant digits give precision proportional to **magnitude**. Every other
quantized quantity in the system — an elevation, a temperature, a mass, a
share — has bounded magnitude, so 8 significant digits buys effectively
constant absolute precision. **Time is the only unbounded quantity in the
system**, so its absolute precision decays linearly with world age, and
nothing in the tree says so.

Measured against the real `quantize` (`kernel/examples`, run and discarded):

| world age | spacing between storable instants |
|---|---|
| 100 yr | 86.4 s |
| 2,000 yr | 14.4 min |
| 20,000 yr | 2.4 h |
| 200,000 yr | **24 h** |
| 4.5 Myr | 100 days |

**A note on which quantity this is, because the spec got it wrong once.** These
are the **full spacing** between adjacent storable values — the distance at
which two instants stop being the same stored number. An earlier draft of this
table listed *half* that (43.2 s, 7.2 min, 1.2 h, 12 h, 50 days), which is the
distance you must move to change the stored value, i.e. the gap to the nearest
rounding boundary. Both are meaningful; mixing them in one document is not, and
this section did exactly that once §1.1 was added below in full-spacing units.
Everything here is now full spacing. The corrected figures make the argument
*stronger*, not weaker: at world-year 200,000 the old encoding could not
separate two instants a day apart at all.

The 200,000-year row is not hypothetical: `windows/worldgen/src/hazard.rs`
constructs a `WorldTime` at exactly that horizon. At that point **two
committed facts a full day apart can collapse to the same stored instant** —
not merely day from night, but any two moments within the same day.

The same decay reaches a cross-repo schema. `scene/eclipses/v1`
(`windows/scene/src/lib.rs`) emits `day`, `from_day` and `until_day` as bare
`f64` under `quantize_serde::f64_field`. An eclipse is a minutes-long event;
at world-year 20,000 its emitted time is good to 2.4 hours.

### The second motivation, found independently by another campaign

The deep-time argument above is real but remote. `campaign/the-hand` found a
**present-day** correctness bug with the same cause, without knowing this
campaign existed, and worked around it — which is stronger evidence than
anything in the table above.

`Ledger::commit` quantizes a fact's day, and that rounding goes **upward** as
often as down. So a fact committed at exactly `t` can fail its own `d <= t`
filter on read-back. The Hand's `latest_committed_position` hit this: a
possessed body read back as "no position committed yet" and fell to
`npc.home`, one line after the commit that made it. Verified here rather than
taken on trust:

```
quantize(0.011719999738288106) = 0.01171999999999999952   strictly greater
so `q <= raw` is FALSE — the fact fails its own filter
the same instant as ticks = 1172, and compares exactly equal to itself
```

Day 0.0117 is not deep time. It is the first hour of a world. The Hand's fix
was to quantize the query bound to match the stored value; this campaign
removes the quantization instead, so the mismatch cannot arise. **Whoever
merges second must delete the other's half of this pair** — both branches
compile and both suites pass in isolation, so nothing will raise it.

### The precision becomes CONSTANT, not uniformly better

The table above argues entirely from the deep end and is, on its own,
misleading. Measured against the real `quantize`, comparing full lattice steps
on both sides:

| world-day | old (8 sig-digits) | new (1 tick) | finer |
|---|---|---|---|
| 7.8 | 1.0e-7 day | 1.0e-5 day | **old, by 100×** |
| 50 | 1.0e-6 day | 1.0e-5 day | **old, by 10×** |
| 100 | 1.0e-5 day | 1.0e-5 day | coincide |
| 365.25 | 1.0e-5 day | 1.0e-5 day | coincide |
| 3,652.5 | 1.0e-4 day | 1.0e-5 day | new, by 10× |
| 36,525 | 1.0e-3 day | 1.0e-5 day | new, by 100× |

The old lattice step is exactly `10^(floor(log10(day)) - 7)`, so it equals a
tick for the whole decade `[100, 1000)`. **Below roughly world-day 100 the
encoding this campaign removes was finer than the one it installs** — by 100×
in a world's first weeks.

This does not weaken the change; it states it correctly. A tick is 0.864 s,
which is already the finest granularity the simulation itself resolves (it is
`windows/vessel`'s own scheduler quantum), so nothing in the sim could use the
sub-tick precision the old encoding offered early on. What the flip buys is a
resolution that **does not decay with world age** — and the campaign's second
motivation, the read-back bug below, is a shallow-end defect that the old
encoding's extra precision did nothing to prevent.

Stated plainly because an earlier draft of this section did not: this is a
trade, and the deep end is where it pays.

### What is *not* wrong

The **compute** path is fine, and this spec does not claim otherwise. `f64`
has 53 bits of mantissa; measured ULP at the deepest horizon the code builds
(200,000 yr) is 1.3 ms, and 0.02 s at 4.5 Myr. Orbital mechanics, sculpting
and noise are not suffering. **The entire defect is at the emit boundary.**
This is a serialization bug, not a physics bug, and scoping it that way is
what keeps the campaign small.

### The second, independent defect

Every float→int time boundary in the tree mishandles negative time, and
negative days are constitutionally legal (decision 0126: "a day is a *point
on an axis*, not a duration"; founders are born before the history record
begins).

| site | operation | wrong how |
|---|---|---|
| `domains/astronomy/src/calendar.rs` `local_day` | `local as u64` | float→int casts **saturate**: every negative local day collapses to 0, and the returned fraction goes negative |
| `windows/vessel/src/session.rs` | `.trunc() as u64` | truncates toward zero where `div_euclid` floors |
| `windows/vessel/src/liveness.rs` | `.round() as u64` | saturates |

Verified for the first:

```
local=  -5.3  `as u64` =>   0   local.fract()= -0.300
local=  -0.4  `as u64` =>   0   local.fract()= -0.400
local=   3.7  `as u64` =>   3   local.fract()=  0.700
```

Both `local_day` callers (`heliacal.rs`, `night_sky.rs`) compute
`t.0 - fraction * day_length` to find the local day's start, which lands
*after* `t` when the fraction is negative. **No test anywhere passes a
negative `StdDays` to a calendar method** — the only `StdDays(-` in the tree
is an unrelated eclipse guard. Integer ticks make all three sites one exact
`div_euclid`/`rem_euclid` pair.

**These are LIVE, not latent — this section itself got that wrong once, and
it is worth narrating exactly how.** An earlier draft of this spec said
"live." This section then "corrected" that to "latent" on the strength of a
reachability trace that was real but incomplete, and the incomplete trace
stood — in this spec, on the project board, and in decision 0187's rationale
— until a census refresh forced the question weeks later. Decision
[0190](../../decisions/0190-a-reachability-trace-is-not-closed-by-finding-one-funnel.md)
records the correction on the decision-log side; here is the trace, redone at
the site the first pass missed.

What the earlier trace found was real, as far as it went:

- The only `WorldTime` → `StdDays` conversion in `GeneratedSky`'s own path is
  `provider.rs`'s `fn t`, which is `StdDays(time.day().max(0.0))` — it
  **clamps**. Negative time cannot reach `local_day` through that funnel.
- `windows/worldgen/src/lib.rs`'s `night_sky_lines` does bypass the funnel,
  and it does pass a hardcoded `StdDays::new(0.0).unwrap()` — never negative.

What it missed: `domains/astronomy/src/heliacal.rs` calls
`Calendar::local_day` **directly, below the funnel, with a real (non-zero)
time**. `heliacal_events` computes `year_start = t.0 - calendar.year_phase(t)
* year` (`heliacal.rs:112`) from the already-clamped, non-negative `t` it is
given, then scans `SAMPLES` (400) points forward from `year_start`
(`heliacal.rs:137`) and calls `at_local_fraction`, which calls
`calendar.local_day(t_sample)` (`heliacal.rs:81`) on each one. `year_start` is
negative whenever `t < year_phase(t) * year`, which holds at genesis for
essentially every world with a nonzero drawn `year_phase_offset` — the early
scan samples are pre-genesis by construction, and nothing clamps them before
they reach `local_day`. The earlier trace checked whether a caller could hand
`local_day` a negative `StdDays` from *outside* astronomy; it did not check
whether a function *inside* astronomy, downstream of the clamp, could
construct one itself. It can, and does, on almost every seed.

Measured, not argued: instrumenting `local_day` to compare the old and fixed
fraction formulas while building a single seed-267 world (`hornvale new`)
found **1,293,003 fraction divergences out of 2,776,344 probe calls, every
one at `local < 0`, zero at `local >= 0`**
(`docs/audits/the-escapement-census-attribution.md` has the
full instrumentation and backtrace). The negative path was not a corner case
sitting behind an unreachable guard — it was the common case for the early
part of every year scan, on almost every world the domain has ever built.

The consequence reaches something observable. The corrected fraction changes
which heliacal risings and settings `heliacal_events` finds, for 2 of 354
seed-267 settlements and 1 of 274 in seed 831; that changes `presiding` for
those cells, which changes the settlement's drawn name. Those are the three
cells this campaign's census refresh moved — the fix's fingerprint, not a
regression. So the defect fixed here (§1's table above) was live on
essentially every world this domain has ever generated, not latent. It is
still fixed for the reason originally given — stage 2 ports this exact
function anyway and the fix is nearly free — but the claim that nothing was
currently broken does not survive: something was, and the census caught it.

**The clamp is the more interesting finding.** `StdDays(time.day().max(0.0))`
silently maps *both* negative time and NaN to genesis, so the sky at a
pre-genesis founder's birth is reported as the sky at genesis with no
signal. Under integer ticks the NaN half disappears by construction, and the
clamp becomes an explicit choice that should be recorded rather than
inherited: clamp, `None`, or error. Stage 2 must decide it and mint a
decision record from the block; it must not silently preserve a `max(0.0)`
whose only justification is a comment.

---

## 2. Non-goals

- ~~**Not** changing the compute path.~~ **STRUCK — this non-goal was false and
  the campaign found out at Task 1.** See §2.1. Astronomy, climate and terrain
  still *compute* in `f64`, but wherever an instant is STORED mid-computation,
  representing it quantizes the computation. The tick lattice therefore reaches
  the compute path, deliberately and with authorization.
- **Not** re-tuning any physical constant, threshold or calibration.
- **Not** breaking the cross-repo scene contract. See §5.
- **Not** renaming `windows/vessel`'s `Ticks(u64)`. It is an action *cost* — a
  duration, legitimately unsigned — and renaming it is scope creep.
- **Not** touching `windows/vessel` or `windows/lab/src/{synthetic,health}.rs`
  before `campaign/the-hand` lands. See §6, stage 4.

### 2.1 The tick lattice IS the time domain (amends decision 0033)

An earlier draft of §2 promised this campaign would not touch the compute path.
`windows/worldgen/src/hazard.rs` disproves it:

```
:500   event days are DRAWN continuously   block_start + stream.next_f64() * BLOCK_DAYS
:505   filtered continuously               if day >= start && day < end
:507   the survivor is STORED              WorldTime::new(day)   <- rounds to a tick
```

Take a stored day back out and use it as a window bound — which
`the_window_is_half_open_at_both_ends` does — and a raw continuous draw is now
compared against a tick-rounded bound. The half-open property breaks in
whichever direction the rounding went. That is quantization in the compute
path, which decision 0033 forbids in as many words.

**Ratified position (Nathan, 2026-08-23): an instant IS a tick.** The lattice is
not a quantization *of* the time domain, it is the time domain. Consequences,
accepted deliberately:

1. Code that draws a continuous time converts to ticks **once, at the draw**,
   and compares ticks exactly thereafter. One domain per comparison, never a
   raw draw against a round-tripped bound.
2. Which events fall inside a window changes at boundaries. Committed artifacts
   move; the census may move. That is the epoch.
3. **This is a determinism improvement, not merely a cost.** Boundary behaviour
   stops depending on `f64` ULP accidents and becomes exactly reproducible.
   `hazard.rs:493-499` already documents an analogous ULP boundary hazard it
   chose to *record rather than guard*, so the precedent for accepting jitter at
   this scale exists — this makes that jitter deterministic.
4. Decision 0033 needs an amending record from this campaign's block: quantize
   stays emit-only for *magnitudes*, while time is exact by representation and
   therefore leaves the quantize contract entirely.

**The physical magnitude is nothing** — 0.864 s on hazard events drawn across
10,000 to 200,000 years. The reason this needed ratification is not the
magnitude; it is that it amends a constitutional rule and moves committed bytes.

---

## 3. The design

### 3.1 The quantum

**100,000 ticks per standard day** — one tick = 0.864 s. The constant is
*promoted*, not invented: it is `BASE_TICKS_PER_STD_DAY` in
`windows/vessel/src/clock.rs`, already tested, already the clock vessel runs
on. Promoting it means the kernel's instant and vessel's scheduler beat at
the same rate, so no *scale* conversion sits between them.

**What promotion did not do — corrected after the fact.** An earlier draft of
this section claimed the promotion made "vessel's clock become the kernel's
clock" and that "the lossy `days_of` bridge is deleted rather than moved."
Neither happened, and the merged tree is the evidence:
`windows/vessel/src/clock.rs` still declares its own
`BASE_TICKS_PER_STD_DAY: u64 = 100_000` — a second, independent copy of the
literal, tied to `WorldTime::TICKS_PER_STD_DAY` by nothing but agreement;
`days_of` is live at eight-plus call sites; `Session::charge` still converts
`Ticks -> f64 days -> WorldTime` on every action; and the walk loop still
accumulates an `f64` `st.day` and compares it against a tick-derived bound
(`st.day > self.to.as_std_days()` at `windows/vessel/src/liveness.rs:5079`,
`:5164`, `:5223`). §3.4 below states the surviving half correctly, so this
document contradicted itself until this paragraph was added. What the
campaign actually delivered at the quantum is a kernel that keeps time in
exact ticks and a vessel that keeps its own clock at the same rate; unifying
the two is a vessel campaign, carried as a follow-up in this campaign's
retrospective, not work this one did.

- `Years::DAYS_PER_YEAR` is 365.25, so a year is exactly 36,525,000 ticks.
  No repeating fraction.
- `i64` range: ±9.22e18 ticks ≈ ±2.5e11 years.
- Ticks are **exactly representable in `f64`** below 2^53 ticks ≈ 2.47e8
  years, so the ticks→`f64` direction is lossless across every horizon the
  project has ever used.

Seconds was considered and rejected: at 86,400/day it is *coarser* than the
clock vessel already runs, and it would put a scale conversion between the
kernel's instant and vessel's scheduler, pointing the other way. A power of two (2^17) was considered for mask/shift
arithmetic and rejected: no existing precedent, and 1 tick = 0.864 s is
legible where 1/131072 day is not.

A **third** alternative was discarded and is recorded here because it answers
the first question a future reader asks — *why is this an emit-boundary change
rather than a compute-path one?* The pair **(exact anchor + small `f64`
offset)** would have kept a lossless integer anchor and carried the sub-tick
remainder as a float beside it. Rejected on YAGNI plus measurement: **an `f64`
ULP at the deepest horizon this code actually uses (200,000 years) is 1.3 ms,
and at 4.5 Myr it is 0.02 s.** Both are far below the 0.864 s a tick resolves,
so the **compute path has no precision defect to fix at all** — the entire
defect is at the quantize emit boundary, where 8 significant digits give
resolution proportional to magnitude. This is a discard, not a gap: the
anchor+offset shape is the right answer to a problem this project does not
have.

### 3.2 The types

```rust
/// An instant. i64 ticks since genesis; negative is legal.
pub struct WorldTime { ticks: i64 }        // was { day: f64 }

/// A signed difference between two instants.
pub struct TickSpan(i64);                  // new
```

`WorldTime` gains `Ord`, `Eq` and `Hash`-eligibility, which it cannot have
today. Consequences, all deletions:

- Four `.day().to_bits()` memo keys in `windows/vessel/src/liveness.rs`
  collapse to the value itself.
- `total_cmp`-plus-artificial-tie-break (`domains/astronomy/src/eclipses.rs`,
  `windows/worldgen/src/hazard.rs`) becomes `sort_by_key`.
- `WorldTime` becomes a legal `BTreeMap` key.
- `kernel/src/ledger.rs:324`'s quantize call **deletes outright** — an integer
  is exactly representable in JSON, so time leaves the quantization contract
  surface entirely.
- The accumulation guard at `windows/vessel/src/session.rs` (reachable live
  from `possess` stdin via two `wait 1e308`s, per its own comment) becomes
  unreachable; integer addition cannot reach infinity.
  **Corrected after the fact (The Escapement, final-review Minor 10): this
  prediction was wrong.** The guard did not become unreachable — `8d2ab4d35`
  retargeted it at the tick-range overflow instead, and
  `advanced_by_checks_the_increment_and_the_sum_separately`
  (`windows/vessel/src/session.rs`) reaches **both** of its arms: an
  unrepresentable increment and a representable increment whose sum overflows
  `i64`. Integer addition cannot reach infinity, but it can overflow, and the
  guard now names that instead. Left in place with this note rather than
  rewritten, because a design document that quietly loses its falsified
  predictions teaches nothing.

The accessor is **`ticks() -> i64`**, not `day() -> i64`. Decision 0126 exists
*because* a year stamped into a day-typed slot made `person-died`
uncommittable in every world; a method named `day` returning a tick count is
that identical defect at that identical site.

### 3.3 The continuous-time hatch

It lives in the **kernel**, not in `domains/astronomy`. Astronomy is not the
only continuous consumer:

- `domains/climate` samples continuously, reached through an *untyped* `f64`
  parameter — `is_frozen_at(cell, f64)`, `temperature_at(c, f64)`, called from
  `windows/locale/src/{surface,lib}.rs`.
- `windows/lab/src/metrics.rs` needs `f64` for percentile statistics.
- `windows/scene` takes bare `f64 day` parameters throughout.

`kernel → domains` layering forbids astronomy serving climate, so the hatch
cannot live in a domain. Hosting it in the kernel matches precedent: the
kernel already owns the shared quantity newtypes (`units.rs`) and the
emit-boundary discipline (`quantize.rs`), and decision 0033's shape is
exactly a narrow, named, documented boundary crossing that domains invoke.

The hatch is **world-independent** — one constant, no state, no parameter —
and its two directions are named asymmetrically because they *are*
asymmetric: ticks→`f64` is lossless below ~2.5e8 years, `f64`→ticks always
rounds and the rounding rule is declared at the call.

### 3.4 `astronomy::Calendar` sits above the hatch, unchanged

`domains/astronomy/src/calendar.rs`'s `Calendar` is already the
world-local-time object: derived once from the star system, holding
`day: Option<StdDays>` (`None` = tidally locked), `year`, `moon_periods`,
`forcing`, `retrograde`, and exposing `local_day`, `year_phase`,
`season_phase`, `moon_phase`, `synodic_month`, `months_per_year`,
`is_daylight`, `sky_band` and the solar geometry. It is consumed by worldgen,
lab and vessel.

**Its signatures do not change.** It keeps taking `StdDays`. The split:

```
  kernel      WorldTime(i64 ticks)  <->  f64 standard days
              world-INDEPENDENT: one constant, no parameter
  astronomy   Calendar: standard days -> local day / year / season / moon
              world-DEPENDENT: already holds day_length, year, moons, forcing
  vessel      Ticks(u64): action COST, a duration, legitimately unsigned
```

This is why the hatch needs no world parameter: `Calendar` is where the
per-world day length already lives. Note that vessel's
`ticks_per_local_day` rounds `day_length_std * BASE`, so a *local* day
remains a rounded derived count of kernel ticks — unchanged behaviour, stated
here so it is not mistaken for a new approximation.

The one change inside `Calendar` is the defect fix: `local_day` returns
`(i64, f64)` via `div_euclid`/`rem_euclid`, correct for negative time.

---

## 4. Save-format epoch

This is an epoch, deliberately and with authorization. Per the save-format
contract discipline, deliberate regeneration takes an epoch suffix rather
than a rename.

**Internal (moves):** `World`/`Ledger` JSON, the three seed-42 almanacs, the
committed vessel/session/v2 and world-seed-42 fixtures, the lab study CSVs,
the Domesday survey (a pure read over the census), and the census goldens.

**Cross-repo (does *not* move):** see §5.

No seed-derivation label changes, no new draw, and **no change to stream
consumption order** — all three verified directly, index by index, not
merely argued: `World.derived_under`, the concept registry, the fact count,
and the `(subject, predicate, provenance)` sequence are identical between a
pre- and post-campaign world at the same seed
(`docs/audits/the-escapement-census-attribution.md`). Only
`object.Text` values differ, at the same indices — nothing inserted, dropped,
or reordered.

**The clause this section used to draw from those three facts — "this
campaign alters representation, not generation" — is too strong as
originally written, and needs restating precisely.** Generation *did*
change, once: at `3bc4fd871` (Phase A Task 5), fixing `Calendar::local_day`'s
negative-time defect (§1) changed which heliacal risings and settings a
scan finds on some worlds, which changes a settlement's `presiding` concept
and, downstream, its drawn name. That is a changed **value**, produced by a
bugfix, not a changed seed label or stream order — the ledger stays
index-aligned because naming draws are salted per settlement, so a changed
concept list at one settlement cannot shift another's draws. Measured
precisely: the representation flip itself (`9ad5911a3`, Phase B) moved
**zero** census cells and zero non-`day` ledger bytes; the `local_day` fix
(`3bc4fd871`, Phase A, landed *before* the flip) moved **three**. Stage 1's
exit criterion (§7) still holds for what it actually tests — the tick
encoding's own byte-identity — which is a separate claim from "nothing in
this campaign changed a generated value," and that separate, stronger claim
is the one this section retracts.

---

## 5. The cross-repo schemas stay at v1, additively

The scene structs carry **bare `f64` day fields**, not `WorldTime`, each
already tagged `type-audit: pending(wave-2: day)` and serialized through
`quantize_serde::f64_field`. So they will not break at compile time, and we
get a choice.

**Decision: keep `f64` standard days on the wire, and *add* integer tick
fields beside them.**

- Changing `day: f64` to `day: i64` in place is forbidden — scene schemas are
  additive-or-versioned-only, a cross-repo contract the external Orrery
  consumes from the released catalog.
- Bumping to `scene/*/v2` for a precision fix breaks every existing consumer
  to buy exactness most of them do not need. Decision 0055 puts the client
  outside determinism; decisions 0022/0023 leave what it does with the output
  unconstrained.
- An **additive** `*_ticks: i64` field is permitted at v1, breaks nothing,
  and fixes the defect exactly where it is visible: `scene/eclipses/v1`'s
  eclipse timing.

So the epoch is **internal only**. The cross-repo contract does not move,
which materially shrinks stage 5 and removes the campaign's only
irreversible external commitment.

---

## 6. Stages

**Every commit compiles and gates green.** An earlier draft of this section said
the tree does not compile between stages 1 and 4 and that they would "land as
one commit series" — that was not achievable and the campaign discovered it at
Task 1. `scripts/hooks/pre-commit` runs `make gate-commit`, whose lint step is
an *unscoped* workspace clippy with `-D warnings`, so the whole workspace must
compile for any commit to land at all, and `--no-verify` is forbidden. Stage 1
therefore keeps `WorldTime::new` and `WorldTime::day` as **migration shims**
over the new API, and the last porting stage deletes them behind a grep that
must return zero. The "332 sites break loudly" property is not lost, only
collected into one controlled moment instead of blocking nine tasks' worth of
commits.

"Gated" in stage 4 means *do not write it* until The Hand's shape is known —
not that it merges separately.

**Stage 1 — the kernel.** `WorldTime { ticks: i64 }`, `TickSpan`, the named
hatch, `GENESIS`, the validating constructor (range, not finiteness). Delete
`ledger.rs:324`'s quantize call. Exit: the stream-order and pin-isolation
property tests are untouched and green; a seed-42 world's *generation* draws
are byte-identical to base modulo the day encoding.

**Stage 2 — astronomy.** Port the single funnel
(`provider.rs`'s `fn t(&self, time: WorldTime) -> StdDays`) onto the hatch.
Fix `local_day` to `(i64, f64)` with `div_euclid`/`rem_euclid`, **with a test
that passes a negative `StdDays`** — the coverage that does not exist today.
Decide and record the `fn t` clamp question (clamp / `None` / error).
Exit: **no eclipse or heliacal golden moves.** An earlier draft of this exit
criterion argued that a moved golden would prove the port changed
*reachable* behaviour, on the premise that the negative path was unreachable
before this stage. §1 now records that premise as wrong — the path is live,
through `heliacal.rs`, on almost every world — so a moved golden would not
by itself indict the port; the campaign's own census refresh moved three
cells *because* the fix changes reachable behaviour, correctly. What this
exit criterion actually checks is narrower and still holds: the specific
committed seed-42 astronomy fixtures it names were not sensitive to the fix
(their heliacal salience ranking did not flip), which is a fact about those
two seeds, not evidence that nothing downstream could move.

**Stage 3 — the remaining domains and the unblocked windows.** climate,
locale, person, terrain, species, historiography, cli, and
`windows/lab/src/metrics.rs`. Exit: workspace compiles except vessel.

**Stage 4 — vessel (gated on `campaign/the-hand`).** 176 sites in
`liveness.rs` and `session.rs`, plus `lab/src/{synthetic,health}.rs`. Purely
mechanical once the type is settled. If The Hand's `Body` refactor moves
where time lives in those files, spec against its shape rather than today's
tree — that converts a conflict into a port.

**Stage 5 — the epoch.** Regenerate artifacts, rebaseline the hand-authored
goldens, add the additive `*_ticks` scene fields, mint the decision records,
book chronicle entry + freshness sweep, retrospective.

---

## 7. Verification — decision rules, not predictions

Artifact regeneration is stated as branch tables because a prediction can be
wrong and a branch table cannot.

**After stage 1, on `make rebaseline` + the drift check:**

- Only `docs/audits/` moved → expected (the pub boundary changed); regenerate
  and commit in the same commit.
- `docs/digest/` moved → expected once decisions are minted; regenerate via
  the redirect, never by hand.
- A **lab study CSV** or **census** column moved → **STOP.** A measured value
  changed. Attribute it before proceeding — to the representation flip, or to
  something else — rather than assuming which.
- `book/src/gallery/` moved → **STOP**, epoch event, escalate.

**On the census:** a refresh is a carve-out requiring explicit authorization
and runs only on lefford. It is stage 5 work, not stage 1, and every golden it
moves must be attributed, not assumed. **This rule caught exactly the case it
exists for, late.** The census refresh that closed this campaign moved three
cells, and the closing narrative first assumed they were attributable to the
day encoding (the representation flip). They were not: attribution
(`docs/audits/the-escapement-census-attribution.md`) traced
them to `3bc4fd871`'s `local_day` bugfix, landed a stage earlier — a real
attribution, just not the first guess. "STOP and attribute" was the right
instinct; the miss was treating the first plausible cause as the attribution
itself instead of testing it.

**The claim that must be tested, not asserted:** that no seed-derivation
label, no draw and no stream consumption order changed. The pin-isolation
tests in `domains/astronomy/tests/genesis_properties.rs` and
`domains/terrain/tests/tectonic_properties.rs` are the instruments.

**On negative time:** every fixed float→int site gets a test passing a
negative value. A RED from a compile error proves nothing; capture the
behavioural red on the live surface first where the surface still exists.

---

## 8. Risks

- **The Hand collision.** 176 of 333 sites are in files another live campaign
  is restructuring, and its branch is unpushed and unreadable (no `the-hand`
  ref on origin; 92 remote branches, none matching). Mitigated by stage
  ordering and a board notice; the wire attempt was held pending approval.
- **`quantize` reads as a global rule.** Removing time from it is correct but
  invites "why is this one different?" The answer — unbounded magnitude — goes
  in the type's doc, not only here.
- **A rebaseline taken before absorbing main** encodes the change on the old
  world and disagrees with both sides. Absorb, then rebaseline, in that order.
- **`i64` overflow is now the failure mode** where infinity used to be. The
  constructor validates range; `TickSpan` arithmetic uses checked ops at the
  boundaries that used to check finiteness.
