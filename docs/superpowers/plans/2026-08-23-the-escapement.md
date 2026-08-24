# The Escapement Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Retype `hornvale_kernel::WorldTime` from `f64` fractional standard days to an exact `i64` count of ticks (100,000 per standard day), removing time from the `quantize` emit-boundary contract entirely.

**Architecture:** `WorldTime` becomes a private-field newtype over `i64` with `Ord`/`Eq`, plus a signed `TickSpan` difference type. A named, asymmetric conversion pair in the kernel is the *only* route between exact ticks and continuous `f64` standard days — lossless outward (below 2^53 ticks), explicitly rounding inward. `astronomy::Calendar` keeps its `StdDays` signatures and sits above that hatch; it is not modified except to fix a negative-time defect in `local_day`. The change is representation-only: no seed label, no draw, and no stream consumption order moves.

**Tech Stack:** Rust edition 2024, `serde`/`serde_json`/`libm` only, `cargo-nextest`, `tools/type-audit`.

**Spec:** `docs/superpowers/specs/2026-08-23-the-escapement-design.md`

## Global Constraints

- **Dependencies:** `serde`, `serde_json`, `libm` only. No new crates. The allowlist is `ALLOWED_EXTERNAL` in `cli/tests/architecture.rs` (decision 0004/0041).
- **Layering:** `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain depends on `hornvale-kernel` and **never** a sibling domain. Enforced by `cli/tests/architecture.rs`.
- **No `HashMap`/`HashSet`/`SystemTime`/`Instant`** — `clippy.toml` `disallowed-types`, `-D warnings`.
- **Transcendentals route through `hornvale_kernel::math`** (`clippy.toml` `disallowed-methods`). `round`, `floor`, `ceil`, `abs`, `sqrt`, `mul_add`, `powi` and arithmetic are IEEE-exact and **remain allowed as inherent methods** — `kernel/src/math.rs` says so in its module doc. This plan uses `.round()` freely and adds nothing to `math.rs`.
- **Every crate sets `#![warn(missing_docs)]`.** Every public item, field and variant gets a one-line doc comment.
- **Every primitive at a `pub` boundary carries a `type-audit:` tag** (`bare-ok(<class>)` / `waiver(<reason>)` / `pending(wave-N)`). Verify with `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`.
- **Integration tests are consolidated.** Each crate has ONE `tests/suite.rs` binary plus `tests/suite/<name>.rs` modules. `cli/tests/suite/test_binary_ratchet.rs` freezes the top-level roster against `cli/tests/fixtures/top-level-test-binaries.txt` — **never add a new top-level `tests/*.rs` file.** New tests go in `tests/suite/<name>.rs` and get a `mod` line in `tests/suite.rs`.
- **Run a single suite module with a filter, not `--test <name>`:** `cargo test -p hornvale-astronomy --test suite -- genesis_properties`.
- **`cargo fmt` is the final step before every commit.** fmt-gate skips are the most common review finding.
- **Ticks per standard day is `100_000`** — one tick = 0.864 s. `Years::DAYS_PER_YEAR` is `365.25` (`kernel/src/units.rs:337`), so one year is exactly `36_525_000` ticks.
- **Do not touch `windows/vessel/` or `windows/lab/src/{synthetic,health}.rs` before Task 11's gate clears** (`campaign/the-hand` hold-off).

---

## Execution phasing — READ THIS BEFORE ANY TASK (Ruling 8, supersedes the stage numbering below)

The stage structure below was written before two facts were known, and it is
wrong about sequencing. It is kept because its task CONTENT is still correct;
this section overrides its ORDER and its commit boundaries.

**Fact 1.** `scripts/hooks/pre-commit` runs `make gate-commit`, whose lint step
is an unscoped workspace clippy with `-D warnings` and whose test step is the
sub-floor tier. So every commit needs the whole workspace to compile AND that
tier green. There is no such thing as a commit that leaves the tree broken.

**Fact 2.** Flipping the representation changes behaviour immediately and
workspace-wide, because `from_std_days` rounds at construction. Measured: it
reddens `hornvale-vessel liveness::tests::the_hoisted_walk_emits_exactly_what_the_loop_emitted`
and `hornvale-worldgen hazard::tests::the_window_is_half_open_at_both_ends`.
The majority of the affected comparison sites are in `windows/vessel`, which
`campaign/the-hand` holds off. So a single-phase migration forces the very
first commit to touch vessel.

**Therefore the migration is two phases.**

### Phase A — additive API and renames. Behaviour-preserving. Vessel untouched.

`WorldTime` KEEPS its `f64` field. Add the full new surface as accessors over
it:

```rust
pub struct WorldTime { day: f64 }              // UNCHANGED in Phase A

pub const fn from_ticks(t: i64) -> WorldTime   // stores t as f64 / TICKS_PER_STD_DAY
pub fn ticks(self) -> i64                      // (self.day * TICKS_PER_STD_DAY).round() as i64
pub fn from_std_days(d: f64) -> Result<..>     // stores d EXACTLY — no rounding yet;
                                               // REJECTS non-finite AND out-of-tick-range
                                               // (Ruling 9 — the one deliberate Phase A
                                               //  behaviour change, front-loaded so Phase B
                                               //  is purely representational)
pub fn as_std_days(self) -> f64                // returns self.day
pub fn whole_days(self) -> i64                 // floor, via as_std_days
pub fn tick_of_day(self) -> i64                // rem_euclid over ticks()
```

`new`/`day` stay as shims. **Nothing rounds on storage, so nothing changes
behaviour and every commit is green.** Then port crate by crate — Tasks 4, 5,
6, 7, 10 — each a pure rename, each its own green reviewable commit. Vessel is
NOT ported in Phase A: the shims are behaviour-preserving, so leaving it
unported costs nothing and keeps The Hand's files untouched.

`Ord`/`Eq`/`Hash` CANNOT be derived in Phase A (the field is still `f64`), so
the four `.day().to_bits()` memo-key collapses wait for Phase B. They are all
in vessel anyway.

Phase A delivers **no user-visible value** — it is preparation, and that is
fine. Its job is to get 332 mechanical renames out of the risky commit.

### Three crates outside the workspace break invisibly — and the plan never named them

`Cargo.toml`'s `exclude` list keeps these out of the workspace, so
`gate-commit`'s workspace clippy does NOT build them, and a break here is
silent until someone runs their own command. Measured on this branch:

| crate | hornvale deps | `WorldTime` refs | what checks it |
|---|---|---|---|
| `tools/digest` | 1 | 1 (`src/store.rs:164`) | **nothing** — `cargo test --manifest-path tools/digest/Cargo.toml`, by hand only |
| `clients/vessel/wasm` | 4 | 2 | `make vessel-check` |
| `clients/game/bin` | 5 | 1 | the `clients` set / `make world-check` |

`tools/digest` is the dangerous one: CLAUDE.md states plainly that none of the
gates build it, and there has been no CI since decision 0125, so the only thing
that would ever catch it is a person remembering. Port all three in the Phase A
sweep and run each crate's own check.

### The Hand is readable now, and the vessel gate is GONE

`origin/campaign/the-hand` was pushed (tip `4f05c14a6`, unmerged, 49 commits
ahead of main). Its diff answers the question the campaign had been sequencing
around: **`Body` does NOT move where time lives.** Every `WorldTime` parameter
is untouched; `Npc` becomes `Body` in type position only (`agent_position`,
`latest_committed_position`, `room_entry_day`). So the vessel port is a
**straight port**, and vessel's *rename* is behaviour-preserving exactly like
every other crate's.

**Consequence: vessel's rename moves OUT of Phase B and into the rename
phase**, as its own commit. Re-derived on the merged tree:
`windows/vessel` 127 `new` + 64 `.day()`; `windows/lab/src/{synthetic,health}.rs`
11 `new` + 1 `.day()`. Same pure-rename rules, same prohibition on tick-domain
conversion.

Phase B therefore narrows to what is genuinely atomic: the flip itself.

### Phase B — the representation flip. One commit.

Flip the field to `i64`, derive `Ord`/`Eq`/`Hash`, delete the shims, and in the
SAME commit: the tick-domain comparison fixes (§2.1 of the spec — one domain per
comparison, convert at the draw), the vessel port, and the artifact
regeneration. These cannot be separated; the campaign proved that twice.

Phase B is where every golden moves. Expect it to be a large commit and review
it as a unit.

---

## File Structure

| File | Responsibility | Change |
|---|---|---|
| `kernel/src/field.rs` | `WorldTime` (the instant) and the `Field` trait | **Modify** — retype, add hatch |
| `kernel/src/units.rs` | shared quantity newtypes, `UnitError` | **Modify** — add `TickSpan` beside `Years` |
| `kernel/src/ledger.rs` | `Fact`, `Ledger::commit` | **Modify** — delete the day-quantize |
| `kernel/tests/suite/determinism.rs` | kernel determinism assertions | **Modify** — wire-shape test |
| `domains/astronomy/src/provider.rs` | the one `WorldTime`→`StdDays` funnel (`fn t`) | **Modify** — port + clamp decision |
| `domains/astronomy/src/calendar.rs` | `Calendar`, `local_day` | **Modify** — negative-time fix |
| `domains/astronomy/tests/suite/genesis_properties.rs` | pin isolation / stream order | **Append tests only** — its EXISTING pin-isolation tests must stay green and unmodified (Tasks 4 and 5 each add a new test here) |
| `windows/scene/src/lib.rs` | `scene/eclipses/v1` wire structs | **Modify** — additive `*_ticks` |
| `docs/decisions/0186…0188` | the three records this mints | **Create** |

`TickSpan` goes in `units.rs`, not `field.rs`: it is a quantity newtype and `units.rs` is where those live, next to `Years` whose pattern it copies.

---

## Stage 1 — the kernel

### Task 1: `WorldTime` becomes `i64` ticks, with the hatch

**Files:**
- Modify: `kernel/src/field.rs:10-58` (the type, its impl) and `:118-136` (its tests)
- Modify: `kernel/src/units.rs` — append `TickSpan` after the `Years` impl (ends `kernel/src/units.rs:376`)

**Interfaces:**
- Consumes: `crate::units::UnitError { unit: &'static str, value: f64, reason: &'static str }` — note `value` is `f64`, so a tick-valued error passes `ticks as f64`; that field is display-only and this avoids changing a shared error type.
- Produces:
  - `WorldTime::GENESIS: WorldTime`
  - `WorldTime::TICKS_PER_STD_DAY: i64` (= `100_000`)
  - `WorldTime::from_ticks(i64) -> WorldTime` (infallible, `const`)
  - `WorldTime::ticks(self) -> i64` (`const`)
  - `WorldTime::from_std_days(f64) -> Result<WorldTime, UnitError>` (**rounds**)
  - `WorldTime::as_std_days(self) -> f64` (lossless below 2^53 ticks)
  - `WorldTime::whole_days(self) -> i64` (floor, correct for negatives)
  - `WorldTime::tick_of_day(self) -> i64` (0..TICKS_PER_STD_DAY, correct for negatives)
  - `TickSpan(i64)` with `TickSpan::ticks(self) -> i64`, `TickSpan::as_std_days(self) -> f64`
  - `impl Sub for WorldTime { type Output = TickSpan }`, `impl Add<TickSpan> for WorldTime`

- [ ] **Step 1: Write the failing tests**

Replace the two existing `WorldTime` tests in `kernel/src/field.rs` (`a_world_time_cannot_be_built_from_a_non_finite_value` at `:121` and `a_world_time_may_be_negative_because_a_day_is_a_point_not_a_duration` at `:131`) with these. The second is *kept in spirit* — negative time is still legal — but now asserts exactness rather than `f64` round-tripping.

```rust
    #[test]
    fn a_world_time_cannot_be_built_from_a_non_finite_number_of_days() {
        assert!(WorldTime::from_std_days(f64::NAN).is_err(), "NaN is not a point on the time axis");
        assert!(WorldTime::from_std_days(f64::INFINITY).is_err());
        assert!(WorldTime::from_std_days(f64::NEG_INFINITY).is_err());
    }

    #[test]
    fn a_world_time_cannot_be_built_from_days_beyond_the_tick_range() {
        // 1e300 days is ~1e305 ticks: far outside i64. Must be a typed error,
        // never a saturating `as` cast.
        assert!(WorldTime::from_std_days(1e300).is_err());
        assert!(WorldTime::from_std_days(-1e300).is_err());
    }

    #[test]
    fn a_world_time_may_be_negative_because_a_day_is_a_point_not_a_duration() {
        // The Particular's founders are born before the history record begins.
        let t = WorldTime::from_std_days(-20_164.663).expect("a negative day is legal");
        assert!(t.ticks() < 0, "a pre-genesis instant has negative ticks");
    }

    #[test]
    fn a_tick_count_round_trips_exactly_where_an_f64_day_could_not() {
        // The whole point of the campaign: at deep time, an f64 day quantized
        // to 8 significant digits lost sub-12-hour resolution. Ticks do not.
        let deep = 200_000.0 * hornvale_kernel_days_per_year();
        let t = WorldTime::from_std_days(deep).expect("finite");
        let one_tick_later = WorldTime::from_ticks(t.ticks() + 1);
        assert_ne!(t, one_tick_later, "adjacent ticks stay distinct at 200,000 years");
        assert_eq!(one_tick_later.ticks() - t.ticks(), 1);
    }

    fn hornvale_kernel_days_per_year() -> f64 {
        crate::units::Years::DAYS_PER_YEAR
    }

    #[test]
    fn whole_days_and_tick_of_day_floor_rather_than_truncate() {
        // The defect this replaces: `trunc() as u64` rounds toward zero and
        // saturates. div_euclid/rem_euclid floor, which is what a calendar
        // means, and negative days are legal.
        let t = WorldTime::from_ticks(-1);
        assert_eq!(t.whole_days(), -1, "one tick before genesis is day -1, not day 0");
        assert_eq!(t.tick_of_day(), WorldTime::TICKS_PER_STD_DAY - 1);

        let g = WorldTime::GENESIS;
        assert_eq!(g.whole_days(), 0);
        assert_eq!(g.tick_of_day(), 0);
    }

    #[test]
    fn a_difference_of_two_instants_is_a_signed_span() {
        let a = WorldTime::from_ticks(10);
        let b = WorldTime::from_ticks(4);
        assert_eq!((a - b).ticks(), 6);
        assert_eq!((b - a).ticks(), -6, "a span is signed; order is not lost");
        assert_eq!(b + (a - b), a, "add is the inverse of sub");
    }

    #[test]
    fn world_time_is_ordered_and_equatable_so_it_can_key_a_map() {
        let mut m: std::collections::BTreeMap<WorldTime, &str> = std::collections::BTreeMap::new();
        m.insert(WorldTime::from_ticks(2), "later");
        m.insert(WorldTime::from_ticks(1), "earlier");
        let order: Vec<&str> = m.values().copied().collect();
        assert_eq!(order, vec!["earlier", "later"], "an f64 day could not do this");
    }
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-kernel --lib field 2>&1 | tail -30`
Expected: FAIL to **compile**, with `no function or associated item named 'from_std_days' found`. A compile failure is the correct red here — the surface does not exist yet. Note the distinction: for a *behavioural* claim a compile error would prove nothing, but these tests assert the existence and shape of a new API, so "does not exist" is the real red.

- [ ] **Step 3: Retype `WorldTime`**

Replace `kernel/src/field.rs:19-58` (the doc comment, struct and impl) with:

```rust
/// Simulated time as an exact count of ticks since world genesis. There is no
/// wall-clock time anywhere in Hornvale.
///
/// **Why an integer and not `f64` days** (The Escapement, decision 0186): a
/// `WorldTime` used to be emitted through [`crate::quantize`] like every
/// other float, and 8 *significant* digits give precision proportional to
/// MAGNITUDE. Time is the only unbounded quantity in the system, so a
/// committed day decayed with world age — measured at 43.2 s of resolution at
/// world-year 100 and **12 hours** at 200,000, a horizon
/// `windows/worldgen/src/hazard.rs` actually constructs. An integer is
/// exactly representable in JSON and needs no quantization at all, so time
/// left the quantization contract entirely rather than being repositioned
/// inside it.
///
/// The field is private and the crossings are named, because this type's whole
/// job is that a value in some *other* unit cannot be stored here. A year
/// stamped into a day-typed slot is what made `person-died` uncommittable in
/// every world (The Ell); decision 0014 declined this wrapper, 0126
/// superseded it, and 0186 made it exact.
///
/// **Negative is legal.** A day is a *point on an axis*, not a duration:
/// a founder born before the history record begins has a negative birth
/// day. Do not copy `Years`'s non-negative rule here.
#[derive(
    Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[serde(transparent)]
pub struct WorldTime {
    ticks: i64,
}

impl WorldTime {
    /// World genesis — tick zero.
    pub const GENESIS: WorldTime = WorldTime { ticks: 0 };

    /// Ticks per STANDARD day; one tick is 0.864 s.
    ///
    /// Promoted from `windows/vessel`'s scheduler clock (`BASE_TICKS_PER_STD_DAY`)
    /// rather than invented, so the walk band's clock and the kernel's agree by
    /// construction instead of by a lossy bridge. `Years::DAYS_PER_YEAR` is
    /// 365.25, so a year is exactly 36,525,000 ticks — no repeating fraction.
    /// type-audit: bare-ok(count)
    pub const TICKS_PER_STD_DAY: i64 = 100_000;

    /// Build from an exact tick count. Infallible: every `i64` is a point on
    /// the time axis.
    /// type-audit: bare-ok(count: ticks)
    pub const fn from_ticks(ticks: i64) -> WorldTime {
        WorldTime { ticks }
    }

    /// Ticks since genesis.
    /// type-audit: bare-ok(count: return)
    pub const fn ticks(self) -> i64 {
        self.ticks
    }

    /// Build from fractional standard days, **rounding to the nearest tick**.
    ///
    /// This is the inward half of the continuous-time hatch and it always
    /// rounds; [`WorldTime::as_std_days`] is the outward half and is lossless
    /// below 2^53 ticks (~2.5e8 years). The two are deliberately named
    /// differently because they are not symmetric.
    /// type-audit: bare-ok(constructor-edge: days)
    pub fn from_std_days(days: f64) -> Result<WorldTime, crate::units::UnitError> {
        if !days.is_finite() {
            return Err(crate::units::UnitError {
                unit: "standard days",
                value: days,
                reason: "must be finite",
            });
        }
        // `round` is IEEE-exact and platform-stable (see kernel/src/math.rs's
        // module doc); only transcendentals need the libm crate.
        let ticks = (days * Self::TICKS_PER_STD_DAY as f64).round();
        if ticks < i64::MIN as f64 || ticks > i64::MAX as f64 {
            return Err(crate::units::UnitError {
                unit: "standard days",
                value: days,
                reason: "outside the representable tick range",
            });
        }
        Ok(WorldTime { ticks: ticks as i64 })
    }

    /// Fractional standard days since genesis — the outward half of the hatch.
    ///
    /// Lossless below 2^53 ticks (~2.5e8 years), which is every horizon the
    /// project has used. Use this at a compute boundary that genuinely needs a
    /// continuous parameter (orbital mechanics, field sampling, statistics),
    /// never to do time arithmetic that ticks can do exactly.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn as_std_days(self) -> f64 {
        self.ticks as f64 / Self::TICKS_PER_STD_DAY as f64
    }

    /// Whole standard days since genesis, **flooring** — so one tick before
    /// genesis is day `-1`, not day `0`. `trunc`-style truncation toward zero
    /// is what this type exists to make impossible.
    /// type-audit: bare-ok(count: return)
    pub const fn whole_days(self) -> i64 {
        self.ticks.div_euclid(Self::TICKS_PER_STD_DAY)
    }

    /// Tick within the current standard day, always in
    /// `0..TICKS_PER_STD_DAY` even for negative instants.
    /// type-audit: bare-ok(count: return)
    pub const fn tick_of_day(self) -> i64 {
        self.ticks.rem_euclid(Self::TICKS_PER_STD_DAY)
    }
}

impl std::ops::Sub for WorldTime {
    type Output = crate::units::TickSpan;
    fn sub(self, rhs: WorldTime) -> crate::units::TickSpan {
        crate::units::TickSpan(self.ticks - rhs.ticks)
    }
}

impl std::ops::Add<crate::units::TickSpan> for WorldTime {
    type Output = WorldTime;
    fn add(self, rhs: crate::units::TickSpan) -> WorldTime {
        WorldTime { ticks: self.ticks + rhs.0 }
    }
}
```

Then update the test-module constant at `kernel/src/field.rs:110`, which is a bare-literal construction that will no longer compile:

```rust
    const NOON: WorldTime = WorldTime::from_ticks(WorldTime::TICKS_PER_STD_DAY / 2);
```

- [ ] **Step 4: Add `TickSpan` to `units.rs`**

Append after the `Years` impl block (which closes at `kernel/src/units.rs:376`):

```rust
/// A signed difference between two [`crate::field::WorldTime`] instants, in
/// ticks. Signed because a span is directional: `earlier - later` is negative
/// and that ordering must not be silently lost.
///
/// Distinct from `Years`, which is a NON-NEGATIVE coarse span, and from
/// astronomy's `StdDays`. The field is `pub(crate)` so the kernel's own `Sub`
/// impl can build one without a fallible constructor, while no crate outside
/// the kernel can bypass the named crossings.
/// type-audit: bare-ok(count)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TickSpan(pub(crate) i64);

impl TickSpan {
    /// The span in ticks.
    /// type-audit: bare-ok(count: return)
    pub const fn ticks(self) -> i64 {
        self.0
    }

    /// Build a span from an exact tick count.
    /// type-audit: bare-ok(count: ticks)
    pub const fn from_ticks(ticks: i64) -> TickSpan {
        TickSpan(ticks)
    }

    /// The span in fractional standard days.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn as_std_days(self) -> f64 {
        self.0 as f64 / crate::field::WorldTime::TICKS_PER_STD_DAY as f64
    }
}
```

- [ ] **Step 5: Export `TickSpan`**

`kernel/src/lib.rs:51` currently reads
`pub use field::{ConstantField, Field, NoiseField, Position, WorldTime};`.
Add `TickSpan` to the `units` re-export on the same pattern; find the existing `pub use units::{...}` line and add `TickSpan` to it alphabetically.

- [ ] **Step 6: Run the tests to verify they pass**

Run: `cargo test -p hornvale-kernel --lib field 2>&1 | tail -20`
Expected: PASS, 7 tests in the `field` module.

- [ ] **Step 6b: Add the transitional migration shims — REQUIRED, see Ruling 5**

`scripts/hooks/pre-commit` runs `make gate-commit`, whose lint step is an
unscoped `cargo clippy --workspace --all-targets -- -D warnings`. So **the whole workspace must compile for ANY
commit to land**, and `--no-verify` is forbidden (global CLAUDE.md,
unconditional). Without shims, no commit is possible between here and Task 9 —
the plan's original "stages 1-4 land as one commit series" was never achievable.

Verified: the entire external surface is `WorldTime::new(f64)` (224 sites) and
`.day()` (110 sites). There are **zero** bare `WorldTime { .. }` struct literals
outside the kernel (the three `WorldTime {` greps are return types), and
astronomy's `.day` field accesses are on `StdDays`, an unrelated type. So two
shims restore compilation workspace-wide.

Add to the `impl WorldTime` block:

```rust
    /// MIGRATION SHIM — deleted in Task 9, do not use in new code.
    ///
    /// Preserves the pre-Escapement `new(days)` signature so the workspace
    /// keeps compiling while crates are ported one at a time. Every call site
    /// is a not-yet-ported site, and the check that no site survives is the
    /// build itself, once Task 9 deletes this.
    /// type-audit: bare-ok(constructor-edge: days)
    pub fn new(days: f64) -> Result<WorldTime, crate::units::UnitError> {
        WorldTime::from_std_days(days)
    }

    /// MIGRATION SHIM — deleted in Task 9, do not use in new code.
    /// See [`WorldTime::new`]. At a ported site prefer
    /// [`WorldTime::as_std_days`] (a continuous crossing) or
    /// [`WorldTime::ticks`] (exact).
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn day(self) -> f64 {
        self.as_std_days()
    }
```

Do **not** mark them `#[deprecated]`: `-D warnings` turns a deprecation warning
into an error, breaking the workspace exactly as hard as having no shim.

`kernel/src/ledger.rs` needs no production edit once the shims exist — it calls
`WorldTime::new` and `.day()`, which still resolve. **Leave that file's
production code alone; deleting its quantize call is Task 2's job.** A kernel
TEST asserting the old *float* wire shape does belong to this task, because this
task is what changed the wire shape.


- [ ] **Step 7: `cargo fmt` and commit**

```bash
cargo fmt
git add kernel/src/field.rs kernel/src/units.rs kernel/src/lib.rs
git commit -m "feat(kernel)!: WorldTime becomes an exact i64 tick count

100,000 ticks per standard day, promoted from vessel's scheduler clock so
the two agree by construction. Gains Ord/Eq/Hash, which an f64 day could
never have. from_std_days rounds; as_std_days is lossless below 2^53 ticks.
whole_days/tick_of_day floor rather than truncate, so a pre-genesis instant
is day -1 and not day 0."
```

---

### Task 2: delete the day-quantize from `Ledger::commit`

**Files:**
- Modify: `kernel/src/ledger.rs:323-326`
- Modify: `kernel/src/ledger.rs:985` (`committed_numbers_and_days_are_quantized`) and `:1011`

**Interfaces:**
- Consumes: `WorldTime` from Task 1.
- Produces: nothing new. `Fact.day` stays `Option<WorldTime>`.

- [ ] **Step 1: Write the failing test**

Add to the test module in `kernel/src/ledger.rs`, and *rename* the existing `committed_numbers_and_days_are_quantized` to `committed_numbers_are_quantized_but_days_are_exact`, replacing its day half. The new assertion:

```rust
    #[test]
    fn a_committed_day_is_exact_because_a_tick_count_needs_no_quantization() {
        // The defect The Escapement repairs: a day used to be quantized to 8
        // SIGNIFICANT digits on commit, so at deep time a fact could not
        // distinguish day from night. A tick is exactly representable in JSON.
        let deep = WorldTime::from_std_days(200_000.0 * Years::DAYS_PER_YEAR).expect("finite");
        let odd = WorldTime::from_ticks(deep.ticks() + 1);

        let mut ledger = Ledger::default();
        let registry = test_registry();
        let mut fact = test_fact();
        fact.day = Some(odd);
        ledger.commit(fact, &registry).expect("commits");

        assert_eq!(
            ledger.facts()[0].day,
            Some(odd),
            "the committed day must be the exact tick handed in, not a rounded one"
        );
    }
```

Note for the implementer: `test_registry()` and `test_fact()` are placeholders for whatever the surrounding test module already uses to build a committable fact — **read the neighbouring tests in that module and reuse their helpers verbatim** rather than introducing new ones. If a helper does not exist, build the `Fact` literal the way the test at `kernel/src/ledger.rs:1008` does.

- [ ] **Step 2: Run it to verify it fails**

Run: `cargo test -p hornvale-kernel --lib ledger 2>&1 | tail -20`
Expected: FAIL — the quantize call at `:324` rounds the deep tick value, so the committed day differs from `odd`.

- [ ] **Step 3: Delete the quantize call**

Remove `kernel/src/ledger.rs:323-326` entirely:

```rust
        fact.day = fact.day.map(|d| {
            crate::field::WorldTime::new(crate::quantize::quantize(d.day()))
                .expect("quantizing an already-finite WorldTime cannot produce a non-finite one")
        });
```

Amend the comment above it (`:315-319`) so it no longer claims days are canonicalized. The `Value::Number` quantize at `:320-322` **stays** — numeric objects still need it.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-kernel --lib ledger 2>&1 | tail -20`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add kernel/src/ledger.rs
git commit -m "feat(kernel)!: a committed day is exact, not quantized

Time leaves the quantize contract surface entirely: an integer tick count is
exactly representable in JSON, so there is nothing to canonicalize. Numeric
fact objects still quantize -- that half is unchanged."
```

---

### Task 3: assert the wire shape changed, and that generation did not

**Files:**
- Modify: `kernel/tests/suite/determinism.rs`
- Read only: `domains/astronomy/tests/suite/genesis_properties.rs`, `domains/terrain/tests/suite/tectonic_properties.rs`

**Interfaces:**
- Consumes: `WorldTime::from_ticks`, `WorldTime::from_std_days`, `serde_json`.
- Produces: nothing.

- [ ] **Step 1: Write the failing tests**

Append to `kernel/tests/suite/determinism.rs`:

```rust
/// The direct wire-shape assertion `#[serde(transparent)]` exists for. A
/// `WorldTime` must serialize as a bare JSON INTEGER now, not a float --
/// this is the save-format half of the epoch and the one thing a reader of
/// an old world file would notice first.
#[test]
fn a_world_time_serializes_as_a_bare_integer() {
    let t = hornvale_kernel::WorldTime::from_ticks(36_525_000);
    let s = serde_json::to_string(&t).expect("serializes");
    assert_eq!(s, "36525000", "no decimal point, no exponent, no wrapper object");

    let back: hornvale_kernel::WorldTime = serde_json::from_str(&s).expect("round-trips");
    assert_eq!(back, t);
}

/// One year is exactly 36,525,000 ticks -- 365.25 days at 100,000 ticks a
/// day. Asserted because a non-integral tick count per year would put a
/// rounding step inside every calendar computation.
#[test]
fn a_year_is_a_whole_number_of_ticks() {
    let year = hornvale_kernel::WorldTime::from_std_days(hornvale_kernel::Years::DAYS_PER_YEAR)
        .expect("finite");
    assert_eq!(year.ticks(), 36_525_000);
    assert_eq!(year.tick_of_day(), 25_000, "365.25 days is a quarter-day past a whole day");
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-kernel --test suite -- determinism 2>&1 | tail -20`
Expected: FAIL to compile before Task 1 lands; PASS after. If this task is run after Tasks 1–2 (it should be), expect these two to pass immediately — that is fine, they are regression locks on the epoch's wire contract, not a TDD red.

- [ ] **Step 3: Verify generation did NOT move**

This is the load-bearing claim of the whole epoch (spec §4). Run the two property batteries **unmodified**:

```bash
cargo test -p hornvale-astronomy --test suite -- genesis_properties 2>&1 | tail -20
cargo test -p hornvale-terrain   --test suite -- tectonic_properties 2>&1 | tail -20
```

Branch table — do not "fix" a red here without attributing it:

- **Both green** → generation is untouched. Proceed.
- **A pin-isolation test red** → a pin now consumes a different number of draws than the unpinned path. That means a `WorldTime` construction moved inside a *draw* path, which this campaign must not do. **STOP** and find the site.
- **A stream-order test red** → same conclusion, worse. **STOP.**
- **Red only from a compile error** → expected mid-stage while other crates are unported; note it and move on, but re-run at the end of Task 10 and require green there.

- [ ] **Step 4: Commit**

```bash
cargo fmt
git add kernel/tests/suite/determinism.rs
git commit -m "test(kernel): lock the integer wire shape and the exact year

A WorldTime serializes as a bare JSON integer, and one year is exactly
36,525,000 ticks. Both are save-format facts a future change must not move
silently."
```

---

## Stage 2 — astronomy

### Task 4: port the single `WorldTime` -> `StdDays` funnel, and decide the clamp

**Files:**
- Modify: `domains/astronomy/src/provider.rs:1490-1493`
- Create: `docs/decisions/0187-a-pre-genesis-query-is-answered-not-clamped.md` (slug depends on the decision reached — see Step 3)

**Interfaces:**
- Consumes: `WorldTime::as_std_days` from Task 1.
- Produces: `fn t(&self, time: WorldTime) -> StdDays` — same signature, different body.

- [ ] **Step 1: Write the failing test**

The current body is `StdDays(time.day().max(0.0))`, which silently maps **both** negative time and NaN to genesis. NaN disappears by construction under ticks. The clamp does not, and it must become a recorded choice rather than an inherited comment (spec §1).

Add to `domains/astronomy/tests/suite/genesis_properties.rs`:

```rust
/// The clamp at the WorldTime -> StdDays funnel is a DECISION, not an
/// accident. Whatever it is, it must be asserted somewhere, because today it
/// is stated only in a comment and nothing would notice if it changed.
#[test]
fn a_pre_genesis_query_has_a_documented_answer() {
    let (sky, _t) = night_sky();
    let before = hornvale_kernel::WorldTime::from_ticks(-1);
    let at_genesis = hornvale_kernel::WorldTime::GENESIS;

    // Decision 0187: a pre-genesis query is CLAMPED to genesis, deliberately
    // -- the sky before the world exists is not a physical question, and an
    // Option or an error at this depth would force every caller to handle a
    // case no caller can produce (traced: the only unclamped call sites pass
    // a hardcoded StdDays::new(0.0)).
    assert_eq!(
        sky.sky_at(before).summary(),
        sky.sky_at(at_genesis).summary(),
        "a pre-genesis query answers as genesis, per decision 0187"
    );
}
```

Note for the implementer: `night_sky()` is the existing helper at `domains/astronomy/src/provider.rs:38`; if it is not reachable from the integration suite, build the sky the way the neighbouring tests in `genesis_properties.rs` already do — **read them first and reuse their construction verbatim.** `.summary()` is a stand-in for whatever cheap comparable projection `SkyReport` already exposes; **read `SkyReport`'s public surface and pick a real one** rather than adding a method.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-astronomy --test suite -- genesis_properties 2>&1 | tail -20`
Expected: FAIL to compile — `WorldTime::from_ticks` needs Task 1, and `.day()` no longer exists in `provider.rs`.

- [ ] **Step 3: Port the funnel**

Replace `domains/astronomy/src/provider.rs:1490-1493`:

```rust
    /// This domain's ONE crossing from exact kernel ticks to continuous
    /// standard days. Everything downstream of here works in `StdDays`.
    ///
    /// Clamps a pre-genesis instant to genesis, per decision 0187 — the sky
    /// before the world exists is not a physical question. The NaN half of
    /// the old `max(0.0)` guard is gone by construction: a `WorldTime` is an
    /// integer and cannot be NaN.
    fn t(&self, time: WorldTime) -> StdDays {
        StdDays(time.as_std_days().max(0.0))
    }
```

- [ ] **Step 4: Write the decision record**

Create `docs/decisions/0187-a-pre-genesis-query-is-answered-not-clamped.md` — **rename the slug to match the decision actually reached.** Follow the front-matter and section shape of an existing record; read `docs/decisions/0126-*.md` first and copy its structure exactly. The record must state: what was inherited (a `max(0.0)` justified only by a comment), what was traced (no caller can reach the negative path today — `provider.rs`'s funnel clamps, and the only bypassing sites in `windows/worldgen/src/lib.rs` pass a hardcoded `StdDays::new(0.0).unwrap()`), and the choice made.

- [ ] **Step 5: Run to verify it passes**

Run: `cargo test -p hornvale-astronomy --test suite -- genesis_properties 2>&1 | tail -20`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add domains/astronomy/src/provider.rs domains/astronomy/tests/suite/genesis_properties.rs docs/decisions/0187-*.md
git commit -m "feat(astronomy)!: port the one tick->StdDays funnel; record the clamp

The domain has exactly one WorldTime -> StdDays crossing and it inherited a
max(0.0) whose only justification was a comment. Decision 0187 records it as
a choice and a test now holds it. The NaN half of the old guard is gone by
construction."
```

---

### Task 5: fix `local_day` for negative time

**Files:**
- Modify: `domains/astronomy/src/calendar.rs:576-581`
- Modify (only if the index binding reaches them — **all six production callers currently DISCARD the index**, so most need no edit): `heliacal.rs:81` `(_, f)`, `night_sky.rs:122` `(_, f)`, `provider.rs:1538` `.map(|d| d.1)`, `eclipses.rs:277` `(_, fraction)`, `calendar.rs:643` `?.1`, `calendar.rs:687` `?.1`
- Also in `calendar.rs`'s own test module: `:66` binds `index` (an integer-literal comparison still infers), `:78`, `:204`
- Modify: `domains/astronomy/tests/suite/genesis_properties.rs`

**Interfaces:**
- Consumes: nothing from earlier tasks; this is an independent defect fix.
- Produces: `Calendar::local_day(&self, t: StdDays) -> Option<(i64, f64)>` — **`i64`, was `u64`**.

- [ ] **Step 1: Write the failing test**

```rust
/// `local as u64` SATURATES: every negative local day collapsed to 0 and the
/// returned fraction went negative. Latent rather than live -- no caller can
/// reach it today (traced in the spec) -- but it is a landmine on a public
/// API with zero coverage, and this is the first test ever to pass a
/// negative StdDays to a calendar method.
#[test]
fn local_day_floors_for_a_pre_genesis_instant_instead_of_saturating() {
    let cal = spinning_calendar();
    let day_len = cal.day_length().expect("a spinning world has a local day").0;

    let (idx, frac) = cal
        .local_day(hornvale_astronomy::StdDays::new(-1.5 * day_len).expect("finite"))
        .expect("a spinning world answers");

    assert!(idx < 0, "a pre-genesis instant is a NEGATIVE local day, not day 0");
    assert!(
        (0.0..1.0).contains(&frac),
        "the fraction of a day is always in [0,1), even before genesis: got {frac}"
    );
}
```

Note for the implementer: `spinning_calendar()` stands for the existing helper that builds a `Calendar` over a spinning system — `domains/astronomy/src/calendar.rs` has `calendar_of(&spinning_system())` in its own test module. **Read that module and reuse its helpers**; if they are `#[cfg(test)]`-private, build the calendar in the integration suite the same way `night_sky_regimes.rs` already does.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-astronomy --test suite -- local_day 2>&1 | tail -20`
Expected: FAIL — `idx` comes back `0` (saturated) and `frac` comes back negative. **Capture this behavioural red before touching the code**: it is the proof the defect is real, and a later compile error would not substitute for it.

- [ ] **Step 3: Fix it**

Replace `domains/astronomy/src/calendar.rs:576-581`:

```rust
    /// Local day index and fraction at absolute time `t`.
    ///
    /// Floors, so a pre-genesis instant is a negative day index and the
    /// fraction stays in `[0, 1)`. The previous `local as u64` SATURATED
    /// every negative value to day 0 and returned a negative fraction
    /// (The Escapement); negative days are legal per decision 0126.
    /// type-audit: bare-ok(count: return), bare-ok(ratio: return)
    pub fn local_day(&self, t: StdDays) -> Option<(i64, f64)> {
        let day = self.day?;
        let local = t.0 / day.0;
        let index = local.floor();
        if !index.is_finite() || index < i64::MIN as f64 || index > i64::MAX as f64 {
            return None;
        }
        let fraction = (local - index + self.forcing.day_phase_offset).rem_euclid(1.0);
        Some((index as i64, fraction))
    }
```

Two changes beyond the sign: `local - index` replaces `local.fract()` (the same value for positives, correct for negatives), and `.rem_euclid(1.0)` replaces the outer `.fract()` so a phase offset cannot push the result negative.

- [ ] **Step 4: Run to verify it passes, and that nothing else moved**

```bash
cargo test -p hornvale-astronomy --test suite 2>&1 | tail -20
```

Branch table:

- **All green** → correct. The two callers use only the fraction, and for non-negative input the new expression is identical to the old.
- **`golden_seed_42` or a night-sky/eclipse golden red** → **STOP.** The negative path is unreachable today (spec §1), so a moved golden means the port changed *reachable* behaviour. That is a bug in this task, not a consequence of the fix. Diff the golden and find which non-negative input changed.
- **A type error at `heliacal.rs:81` or `night_sky.rs:122`** → expected only if those bind the index; they destructure `(_, f)` so they should not. Fix by keeping the discard.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add domains/astronomy/src/calendar.rs domains/astronomy/tests/suite/genesis_properties.rs
git commit -m "fix(astronomy): local_day floors instead of saturating

`local as u64` collapsed every negative local day to 0 and returned a
negative fraction. Latent, not live -- traced: no caller can reach it -- but
a landmine on a public API with no coverage. Returns (i64, f64) now, and the
fraction is rem_euclid so a phase offset cannot push it negative. First test
in the tree to pass a negative StdDays to a calendar method."
```

---

## Stage 3 — the remaining domains and unblocked windows

### Task 6: mechanical sweep — domains

**Files:**
- Modify: `domains/climate/src/**` (4 `WorldTime::new` sites), `domains/species/src/**` (3), `domains/person/src/**` (1 + 2 `.day()`), `domains/terrain/src/**` (1)

**Interfaces:**
- Consumes: `WorldTime::{from_ticks, from_std_days, as_std_days, whole_days, tick_of_day}`, `TickSpan`.
- Produces: nothing new.

This is a transformation, not a design. The table **is** the content:

| old | new | when |
|---|---|---|
| `WorldTime::new(x)` where `x` is a literal or an f64 day | `WorldTime::from_std_days(x)` | the value is genuinely a day count |
| `WorldTime::new(0.0)` | `WorldTime::GENESIS` | it means genesis |
| `t.day()` feeding a continuous computation | `t.as_std_days()` | a real f64 crossing |
| `t.day()` feeding integer/whole-day logic | `t.whole_days()` | it wanted a day index |
| `t.day() - u.day()` | `(t - u).as_std_days()` or `(t - u).ticks()` | prefer ticks unless divided by another duration |
| `a.day() < b.day()` | `a < b` | `Ord` exists now |
| `t.day().to_bits()` as a key | `t` | `Eq`/`Hash`/`Ord` exist now |
| `t.day().trunc() as u64` | `t.whole_days()` | and note it now FLOORS |
| `t.day() - t.day().floor()` | `t.tick_of_day()` as an integer, or `t.tick_of_day() as f64 / TICKS_PER_STD_DAY as f64` | prefer the integer |

- [ ] **Step 1: Enumerate the sites**

```bash
grep -rn "WorldTime::new(\|\.day()" --include=*.rs domains/climate domains/species domains/person domains/terrain
```

- [ ] **Step 2: Apply the table, one crate at a time**

After each crate: `cargo check -p <crate> --all-targets`

- [ ] **Step 3: Run each crate's tests**

```bash
for c in hornvale-climate hornvale-species hornvale-person hornvale-terrain; do
  cargo test -p "$c" 2>&1 | tail -3
done
```
Expected: all green. A red is a mis-applied row of the table, not a design question.

- [ ] **Step 4: Commit per crate**

```bash
cargo fmt
git add domains/
git commit -m "refactor(domains)!: port to WorldTime ticks"
```

---

### Task 7: mechanical sweep — unblocked windows and the CLI

**Files:**
- Modify: `windows/worldgen/src/**` (32 `new` + 31 `.day()`), `windows/locale/src/**` (3 + 2), `windows/almanac/src/**` (3), `windows/historiography/src/**` (1 + 1), `windows/scene/src/**`, `windows/lab/src/metrics.rs` (8 `.day()`), `cli/src/**` (8), `tools/digest` (1)
- **Do NOT touch** `windows/vessel/`, `windows/lab/src/synthetic.rs`, `windows/lab/src/health.rs`

**Interfaces:** as Task 6.

- [ ] **Step 1: Enumerate, excluding the gated files**

```bash
grep -rn "WorldTime::new(\|\.day()" --include=*.rs windows cli tools \
  | grep -v "^windows/vessel/" \
  | grep -v "^windows/lab/src/synthetic.rs" \
  | grep -v "^windows/lab/src/health.rs"
```

- [ ] **Step 2: Apply the Task 6 table**

Two sites deserve named attention:

- `windows/worldgen/src/hazard.rs:588` — `events.sort_by(|a, b| a.day.day().total_cmp(&b.day.day()));` becomes `events.sort_by_key(|e| e.day);`. The artificial tie-break is gone because equal instants are now genuinely equal.
- `windows/worldgen/src/history_emit.rs:63,101` — the year↔day seams (`year * DAYS_PER_YEAR`, `day / DAYS_PER_YEAR`). These are the named conversions The Ell installed; keep them named, route them through `as_std_days`/`from_std_days`, and do **not** collapse them into callers.

- [ ] **Step 3: Check the whole workspace except vessel**

```bash
cargo check -p hornvale-worldgen -p hornvale-locale -p hornvale-almanac \
            -p hornvale-historiography -p hornvale-scene -p hornvale --all-targets 2>&1 | tail -20
```

- [ ] **Step 4: Re-run the determinism batteries from Task 3, Step 3**

They must be green now that the domains are ported. This is the real checkpoint for "generation did not move."

- [ ] **Step 5: `cargo fmt` and commit**

```bash
cargo fmt
git add windows/ cli/ tools/
git commit -m "refactor(windows,cli)!: port to WorldTime ticks

hazard.rs's total_cmp-plus-tie-break becomes sort_by_key: equal instants are
genuinely equal now. The Ell's year<->day seams in history_emit stay named
and route through the hatch rather than collapsing into callers."
```

---

## Stage 4 — vessel (GATED)

### Task 8: clear the gate before writing a line

**Files:** none — this is a check.

- [ ] **Step 1: Determine whether `campaign/the-hand` has landed**

```bash
git fetch origin
git log --oneline origin/main | head -20
make board-sync && make board 2>&1 | grep -iA3 "the-hand"
```

Branch table:

- **The Hand is merged into `origin/main`** → absorb main into this branch FIRST (`make sluice-stage BRANCH=campaign/the-escapement REF=<full-sha>`, or a local merge), *then* start Task 9 against the merged tree. Absorb before porting, never after: a port written against the pre-merge shape is thrown away.
- **The Hand replied on board thread `escapement-vessel-ordering`** → read the reply. If it names a new shape for the time-carrying fields, write Task 9 against that shape.
- **The Hand is still unlanded and silent** → **ask Nathan** whether to wait or to port against today's tree and accept the conflict. Do not decide this alone; it is a sequencing call with a real cost either way.

- [ ] **Step 2: Post the outcome to the board**

```bash
make board-post KIND=reply BY=campaign/the-escapement \
  FIELDS='thread=escapement-vessel-ordering' \
  NOTE='<what was decided and why, single line, no double quotes, no dollar-paren>'
```

---

### Task 9: mechanical sweep — vessel

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (53 `.day()` + a share of 123 `new`), `windows/vessel/src/session.rs`, `windows/vessel/src/clock.rs`, `windows/lab/src/synthetic.rs:85`, `windows/lab/src/health.rs`

**Interfaces:** as Task 6, plus `windows/vessel::clock::{Ticks, BASE_TICKS_PER_STD_DAY, days_of, ticks_per_local_day}`.

- [ ] **Step 1: Apply the Task 6 table, then the four named sites**

| site | today | becomes |
|---|---|---|
| `liveness.rs:1093` | `(npc.entity, day.day().to_bits())` | `(npc.entity, day)` |
| `liveness.rs:1247` | `let tbits = t.day().to_bits();` | `let tbits = t;` — then rename the binding, it is no longer bits |
| `liveness.rs:7228`, `:7604` | `f.day.map(\|d\| d.day().to_bits())` | `f.day` |
| `liveness.rs:510` | `let frac = day.day() - day.day().floor();` | `day.tick_of_day()` — and check whether the consumer wants a ratio or a tick |
| `liveness.rs:4689-4690` | `(self.to.day() * scale).round() as u64` | derive from `self.to.ticks()` directly; this hand-rolled conversion is what the kernel type now does |
| `session.rs:2767` | `WorldTime::new(self.day.day() + days)` | `self.day + TickSpan::from_ticks(...)`, with a **checked** add — the overflow mode is now `i64` overflow, not infinity, and the `wait 1e308` path must still return `Turn::Out` rather than panicking |
| `session.rs:3546` | `self.day.day().trunc() as u64` | `self.day.whole_days()` — **note this now FLOORS**; if a negative session day is possible, that is a behaviour change to state in the commit |

- [ ] **Step 2: Reconcile vessel's own clock with the kernel's**

`BASE_TICKS_PER_STD_DAY` (`clock.rs:22`) is now `WorldTime::TICKS_PER_STD_DAY`. Delete the local constant and re-export or reference the kernel's, so there is exactly one. `days_of` (`clock.rs:69`) becomes a thin wrapper over `as_std_days` for the *local*-day case, or is deleted if `ticks_per_local_day` is its only remaining need. **Leave `Ticks(u64)` alone** — it is an action cost, legitimately unsigned (spec §2).

- [ ] **Step 3: Verify the `wait` overflow path still fails gracefully**

This is the one behavioural regression risk in the task. Write a test asserting that an absurd `wait` returns an error rather than panicking or wrapping:

```rust
    #[test]
    fn an_absurd_wait_is_refused_rather_than_overflowing() {
        // Was reachable live from `possess` stdin via two `wait 1e308`s. The
        // failure mode used to be f64 infinity; it is i64 overflow now, and it
        // must still come back through wait's own error channel.
        let mut s = test_session();
        let out = s.wait_days(f64::MAX);
        assert!(matches!(out, Turn::Out(ref m) if m.starts_with("error:")), "got {out:?}");
    }
```

Note for the implementer: `test_session()` and `wait_days` stand for the real helpers — **read `session.rs`'s test module and the `wait` command's actual entry point and use their real names.**

- [ ] **Step 4: Full workspace check and test**

```bash
cargo check --workspace --all-targets 2>&1 | tail -20
cargo nextest run --workspace 2>&1 | tee /tmp/hv-escapement.txt | tail -20
```
Run ONCE, then grep `/tmp/hv-escapement.txt` freely. Do not re-run the suite to find a second failure line.

- [ ] **Step 4b: Delete the migration shims — the loud break, deferred to here**

Task 1 kept `WorldTime::new` and `WorldTime::day` as shims so every intermediate
commit could pass the unscoped workspace clippy gate. Vessel is the last crate
to port, so they go now.

```bash
# must return ZERO before you delete anything.
# THREE spellings, not one — a literal `.day()` grep misses two of them.
grep -rnE 'WorldTime::new\(|WorldTime::day|\.day\(\)' \
     --include=*.rs kernel domains windows cli tools clients
```

**Why three patterns and not the obvious one.** The campaign learned each of
these the hard way:

- `WorldTime::day` as a **bare function reference** passed to `.map(...)` is
  invisible to a `.day()` search. Four such sites existed in
  `windows/vessel/src/liveness.rs` and one still sits at
  `kernel/src/ledger.rs:613`. In a file whose only usage is this form, the
  naive grep returns a **false all-clear**.
- `clients/` is outside the cargo workspace, so no gate compiles it. It must be
  in the search path or a break there is silent until someone runs
  `make vessel-check` or `make world-check`.
- **Expect false positives and read them.** `windows/vessel/src/session.rs`
  defines `pub fn day(&self) -> WorldTime`, so `s.day()` is a *session*
  accessor that must NOT be renamed. The grep cannot tell it from
  `WorldTime::day()`. Zero hits is the goal; a handful of `Session::day()` hits
  is the correct steady state, not a miss.

Branch table:

- **Zero hits** → delete both shims from `kernel/src/field.rs`, then run
  `cargo check --workspace --all-targets`. Green means the migration is complete,
  mechanically proven rather than asserted.
- **Any hits** → unported sites the sweeps missed. Port them with Task 6's
  table, then re-run. Do not delete the shims while a caller remains.

This step is why the shims were safe. The "332 sites break loudly" property the
design wanted is not lost — it is collected into one controlled moment instead
of blocking every commit for nine tasks.

- [ ] **Step 5: `cargo fmt` and commit**

```bash
cargo fmt
git add windows/vessel windows/lab kernel/src/field.rs
git commit -m "refactor(vessel)!: port to WorldTime ticks

Four .day().to_bits() memo keys collapse to the value: Ord/Eq exist now. The
hand-rolled (t.day() * scale).round() as u64 conversion is what the kernel
type does. session.rs's whole-day extraction FLOORS where it truncated.
BASE_TICKS_PER_STD_DAY is the kernel's constant now, not a second copy."
```

---

## Stage 5 — the epoch

### Task 10: additive `*_ticks` on the scene schemas

**Files:**
- Modify: `windows/scene/src/lib.rs:1355` (`EclipseElem.day`), `:1379-1382` (`EclipsesScene.from_day`, `until_day`)

**Interfaces:**
- Consumes: `WorldTime::ticks`.
- Produces: new `pub day_ticks: i64`, `pub from_day_ticks: i64`, `pub until_day_ticks: i64`.

The `f64` fields **stay**, quantized, at `scene/eclipses/v1`. Adding beside them is permitted by the additive-or-versioned-only rule; changing them in place is not, and a `v2` bump would break the external Orrery to buy exactness most consumers do not need (spec §5).

- [ ] **Step 1: Write the failing test**

```rust
/// scene/eclipses/v1 emits an eclipse's time as an f64 day quantized to 8
/// SIGNIFICANT digits, so at world-year 20,000 a minutes-long event is
/// resolvable only to ~1.2 hours. The f64 field stays for compatibility --
/// this is a cross-repo contract -- and an exact tick field is ADDED beside
/// it. Additive at v1, so no existing consumer breaks.
#[test]
fn an_eclipse_carries_an_exact_tick_alongside_its_quantized_day() {
    let scene = eclipses_scene_at_deep_time();
    let json = serde_json::to_value(&scene).expect("serializes");

    assert_eq!(json["schema"], "scene/eclipses/v1", "still v1: the addition is additive");
    assert!(json["from_day"].is_f64(), "the f64 field is unchanged");
    assert!(json["from_day_ticks"].is_i64(), "and an exact tick sits beside it");

    let elem = &json["events"][0];
    assert!(elem["day"].is_f64());
    assert!(elem["day_ticks"].is_i64());
}
```

Note for the implementer: `eclipses_scene_at_deep_time()` stands for building an `EclipsesScene` over a seed-42 world at a far horizon — **read how `windows/scene`'s existing tests build a scene and follow that construction.** If no eclipse falls in the window, pick a window from the committed eclipse golden rather than inventing one.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-scene 2>&1 | tail -20`
Expected: FAIL — `from_day_ticks` is absent.

- [ ] **Step 3: Add the fields**

Add to each struct beside its `f64` sibling, with `type-audit: bare-ok(count: <field>)` tags and one-line docs. No `serialize_with` — an integer needs no quantization, which is the point.

- [ ] **Step 4: Run to verify it passes; then check the client bundles**

```bash
cargo test -p hornvale-scene 2>&1 | tail -5
make vessel-check 2>&1 | tail -10
make world-check  2>&1 | tail -10
```
Branch table: a **byte-identity smoke failure** in either client check is expected — the scene JSON gained fields — and is resolved by rebaselining that client's golden in Task 11, not by reverting the addition. A **lint or size-gate** failure is a real problem; fix it.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/scene
git commit -m "feat(scene): exact tick fields beside the quantized f64 days

scene/eclipses/v1 stays v1. The f64 day fields are a cross-repo contract the
external Orrery consumes, so they are untouched; exact *_ticks fields are
ADDED, which the additive-or-versioned rule permits and which fixes eclipse
timing where the 8-significant-digit quantization actually hurt."
```

---

### Task 11: regenerate every artifact and adjudicate the drift

**Files:** whatever `docs/generated-paths.txt` declares, plus the hand-authored goldens.

- [ ] **Step 1: Absorb main first**

A rebaseline taken before absorbing encodes the change on the old world and then agrees with neither side (spec §8). Absorb, *then* rebaseline.

```bash
git fetch origin && git merge origin/main
```

- [ ] **Step 2: Regenerate**

```bash
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

- [ ] **Step 3: Adjudicate by branch table — do not "accept" a diff you cannot explain**

- **`docs/audits/` moved** → expected; the pub boundary changed. Commit with the code.
- **`docs/digest/` moved** → expected once Task 4's decision record exists. Regenerate through the **redirect** (`… render decisions > docs/digest/decisions-in-force.md`) — running the command bare writes nothing and the drift check then reads as clean.
- **An almanac's day fields moved from `1234.5` to an integer** → expected; this is the epoch.
- **A lab study CSV or a census column moved** → **STOP.** Representation changed a *measured value*, contradicting spec §4's load-bearing claim. Attribute it before going further.
- **`book/src/domesday/` moved with no census change** → **STOP**; it is a pure read over the census and should not move on its own.
- **`book/src/gallery/` moved** → **STOP**, escalate.

- [ ] **Step 4: Rebaseline the hand-authored goldens separately**

**Read this distinction before running anything.** `docs/generated-paths.txt` (what is DRIFT-CHECKED) and `scripts/regenerate-artifacts.sh` (what is GENERATED) are deliberately different halves, and all four fixture families are in the *first* list. Verified: `regenerate-artifacts.sh` mentions only `clients/game/core/tests/fixtures/` and writes none of `world-seed-42.json`, `snapshot-seed-42-chamber.json`, or the proto-goblinoid table. So `make rebaseline` drift-checks three families it cannot regenerate, and they need their own command:

```bash
make rebaseline-goldens   # REBASELINE=1
```

That target is eight specific test invocations, and these are exactly the goldens this epoch will move:

```
lens_purity (hornvale)                golden (hornvale-scene)
proto_goblinoid_golden (worldgen)     architecture (hornvale)
session_snapshot (hornvale-vessel)    solitary_tongue (worldgen)
affect_trace_golden (hornvale-lab)    channel_golden (hornvale-terrain)
```

`clients/game/core/tests/fixtures/` is the one family Step 2 already handled.

- [ ] **Step 5: Commit the regeneration as its own commit**

```bash
git add -A
git commit -m "chore(artifacts): regenerate for the WorldTime tick epoch

Committed days are integers now. Every moved byte is attributable to the day
encoding; no metric moved, which is the epoch's load-bearing claim."
```

---

### Task 12: decisions, book, retrospective

**Files:**
- Create: `docs/decisions/0186-world-time-is-an-exact-tick-count.md`, `0188-time-leaves-the-quantize-contract.md`
- Create: `book/src/chronicle/the-escapement.md`
- Create: `docs/retrospectives/the-escapement.md`
- Modify: `docs/decisions/README.md`, `book/src/SUMMARY.md`, `book/src/open-questions.md` if a bet moved

- [ ] **Step 1: Mint the decision records**

Numbers come from **this campaign's reserved block, 0186–0195** — never `decision-block.sh ceiling`, which prints main's top number with no knowledge of reservations. 0187 was minted in Task 4. Read `docs/decisions/0126-*.md` and copy its structure exactly.

- [ ] **Step 2: Write the chronicle entry**

`book/src/chronicle/the-escapement.md`, added to `book/src/SUMMARY.md`. Written at the book's altitude: technical and mathematical, comprehensible without reading the code. The measured resolution table from spec §1 belongs here — it is the whole story.

- [ ] **Step 3: Freshness sweep**

`CLAUDE.md`'s determinism section says **"Time is `WorldTime { day: f64 }` — absolute standard days."** That is now false in the repo's own top-level guide; fix it. Then grep the book for the same claim:

```bash
grep -rn "day: f64\|fractional days\|WorldTime { day" book/src/ *.md */CLAUDE.md
```

- [ ] **Step 4: Promote the scratch before teardown**

`.superpowers/sdd/decision-ledger.md` and `.superpowers/sdd/followups.md` are git-ignored and **die with the worktree**. Copy the ledger's material entries into the decision records and the followups into the retrospective's follow-up section. Do this BEFORE any teardown.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add docs/ book/ CLAUDE.md
git commit -m "docs(the-escapement): decisions, chronicle, retrospective

Also corrects CLAUDE.md's determinism section, which still described time as
WorldTime { day: f64 }."
```

- [ ] **Step 6: Stage-gate, then hand to Nathan for G6**

```bash
git push -u origin campaign/the-escapement
make sluice-stage BRANCH=campaign/the-escapement REF=$(git rev-parse HEAD)
```
A census refresh is a **carve-out requiring Nathan's explicit authorization** and runs only on lefford. Do not run one; present the question at G6.

---

## Self-Review

**Spec coverage.** §1 precision → Tasks 1–3. §1 negative-time → Tasks 1 (`whole_days`/`tick_of_day`), 5 (`local_day`), 9 (vessel's two casts). §1 clamp → Task 4. §2 non-goals → honoured: `Ticks(u64)` untouched (Task 9 Step 2), vessel gated (Task 8), no constant retuned. §3.1 quantum → Task 1 Step 3, asserted Task 3. §3.2 types → Task 1. §3.3 hatch in kernel → Task 1 (`from_std_days`/`as_std_days`), consumed by astronomy (Task 4), climate/lab (Tasks 6–7). §3.4 `Calendar` above the hatch → Task 4 ports the funnel, Task 5 touches only the defect; no other signature moves. §4 epoch → Task 11. §5 scene additive → Task 10. §6 stages → Tasks 1–12. §7 verification → Task 3 Step 3, Task 11 Step 3, both as branch tables. §8 risks → Task 8 (collision), Task 11 Step 1 (absorb-then-rebaseline), Task 9 Step 3 (overflow).

**Placeholder scan.** No TBD/TODO. Four helper names are deliberately marked as stand-ins (`test_registry`/`test_fact`, `night_sky`/`.summary()`, `spinning_calendar`, `test_session`/`wait_days`, `eclipses_scene_at_deep_time`), each with an instruction to read the neighbouring tests and use the real names. That is the spec's own rule: **never prescribe a specific mutation or fixture from outside the code** — a plan author does not know which helpers are `#[cfg(test)]`-private; the implementer does, after reading.

**Type consistency.** `from_ticks`/`ticks`/`from_std_days`/`as_std_days`/`whole_days`/`tick_of_day`/`TICKS_PER_STD_DAY` and `TickSpan::{ticks, from_ticks, as_std_days}` are used under exactly those names in Tasks 2–10. `local_day` returns `Option<(i64, f64)>` in Task 5 and is referenced as `i64` in Task 5's callers. `UnitError.value` is `f64` and Task 1 passes `days` (already an `f64`) — no cast needed, which the earlier draft of this note got wrong.

**Gap found and fixed during review:** Task 11 originally relied on `make rebaseline` alone, which would have left three fixture families un-regenerated — byte-identity tests red in the chamber with a green artifacts phase, a documented failure shape another campaign already hit. Step 4 now covers them.

**A wrong reason, corrected, because the plan is worth more than my first draft of it:** that gap note originally said the three families are "outside `docs/generated-paths.txt`". They are not — all four are in it. They are outside `scripts/regenerate-artifacts.sh`, which is the *other* of the two halves CLAUDE.md deliberately keeps separate ("WHAT IS GENERATED" vs "WHICH PATHS ARE DRIFT-CHECKED"). The remedy was right and the reasoning was wrong, which is the more dangerous combination: a reader chasing the stated reason would have edited the wrong file. Verified by grepping both.
