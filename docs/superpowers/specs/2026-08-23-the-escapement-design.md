# The Escapement — `WorldTime` becomes integer ticks

**Campaign**: The Escapement
**Date**: 2026-08-23
**Branch**: `campaign/the-escapement`
**Base**: `27312e02c` (`origin/main`)
**Decision block**: 0186–0195
**Status**: spec, awaiting G3

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

| world age | committed time resolution |
|---|---|
| 100 yr | 43.2 s |
| 2,000 yr | 7.2 min |
| 20,000 yr | 1.2 h |
| 200,000 yr | **12 h** |
| 4.5 Myr | 50 days |

The 200,000-year row is not hypothetical: `windows/worldgen/src/hazard.rs`
constructs a `WorldTime` at exactly that horizon. At that point **a committed
fact cannot distinguish day from night.**

The same decay reaches a cross-repo schema. `scene/eclipses/v1`
(`windows/scene/src/lib.rs`) emits `day`, `from_day` and `until_day` as bare
`f64` under `quantize_serde::f64_field`. An eclipse is a minutes-long event;
at world-year 20,000 its emitted time is good to 1.2 hours.

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

---

## 2. Non-goals

- **Not** changing the compute path. Astronomy, climate and terrain keep
  computing in `f64`; only the representation of an *instant* changes.
- **Not** re-tuning any physical constant, threshold or calibration.
- **Not** breaking the cross-repo scene contract. See §5.
- **Not** renaming `windows/vessel`'s `Ticks(u64)`. It is an action *cost* — a
  duration, legitimately unsigned — and renaming it is scope creep.
- **Not** touching `windows/vessel` or `windows/lab/src/{synthetic,health}.rs`
  before `campaign/the-hand` lands. See §6, stage 4.

---

## 3. The design

### 3.1 The quantum

**100,000 ticks per standard day** — one tick = 0.864 s. The constant is
*promoted*, not invented: it is `BASE_TICKS_PER_STD_DAY` in
`windows/vessel/src/clock.rs`, already tested, already the clock vessel runs
on. Promoting it means vessel's clock becomes the kernel's clock and the
lossy `days_of` bridge is deleted rather than moved.

- `Years::DAYS_PER_YEAR` is 365.25, so a year is exactly 36,525,000 ticks.
  No repeating fraction.
- `i64` range: ±9.22e18 ticks ≈ ±2.5e11 years.
- Ticks are **exactly representable in `f64`** below 2^53 ticks ≈ 2.47e8
  years, so the ticks→`f64` direction is lossless across every horizon the
  project has ever used.

Seconds was considered and rejected: at 86,400/day it is *coarser* than the
clock vessel already runs, and it would leave the lossy bridge in place
pointing the other way. A power of two (2^17) was considered for mask/shift
arithmetic and rejected: no existing precedent, and 1 tick = 0.864 s is
legible where 1/131072 day is not.

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
consumption order** — this campaign alters representation, not generation.
That is the load-bearing claim of the whole epoch and stage 1's exit
criterion tests it directly (§7).

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

The tree does not compile between stages 1 and 4, so 1–4 land as one commit
series on the branch. "Gated" in stage 4 means *do not write it* until The
Hand's shape is known — not that it merges separately.

**Stage 1 — the kernel.** `WorldTime { ticks: i64 }`, `TickSpan`, the named
hatch, `GENESIS`, the validating constructor (range, not finiteness). Delete
`ledger.rs:324`'s quantize call. Exit: the stream-order and pin-isolation
property tests are untouched and green; a seed-42 world's *generation* draws
are byte-identical to base modulo the day encoding.

**Stage 2 — astronomy.** Port the single funnel
(`provider.rs`'s `fn t(&self, time: WorldTime) -> StdDays`) onto the hatch.
Fix `local_day` to `(i64, f64)` with `div_euclid`/`rem_euclid`, **with a test
that passes a negative `StdDays`** — the coverage that does not exist today.
Exit: eclipse and heliacal goldens move only where the negative-time fix
makes them move, and each such move is explained in the commit.

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
- A **lab study CSV** or **census** column moved → **STOP.** Representation
  changed a *measured value*, which contradicts §4's load-bearing claim.
  Attribute it before proceeding.
- `book/src/gallery/` moved → **STOP**, epoch event, escalate.

**On the census:** a refresh is a carve-out requiring explicit authorization
and runs only on lefford. It is stage 5 work, not stage 1, and the goldens it
moves must be attributable to the day encoding alone.

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
