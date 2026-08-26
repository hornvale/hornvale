# The Foliot Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Finish the redenomination The Escapement began — remove every
downstream surface that still quotes time in float days, so the kernel's exact
signed tick is the only instant and the named hatch is the only crossing.

**Architecture:** `StdDays` narrows to a duration (non-negative is correct
there); a new signed `StdInstant` takes the 27 instant positions in astronomy;
`WorldTime` stays out of astronomy's public surface because it is
tick-quantized and would round non-tick-aligned callers. `windows/vessel`'s
private tick lattice is proven identical to the kernel's and the `f64` bridge
between them is deleted. `domains/climate` and `windows/scene` move to kernel
types.

**Tech Stack:** Rust edition 2024, `serde`/`serde_json`/`libm` only
(`ALLOWED_EXTERNAL` in `cli/tests/architecture.rs`), `cargo nextest`,
`tools/type-audit`.

**Spec:** `docs/superpowers/specs/2026-08-26-the-foliot-design.md`

## Global Constraints

- **Layering is constitutional:** `kernel/` → `domains/*` → `windows/*` →
  `cli/`. A domain depends on `hornvale-kernel` and **nothing else, never a
  sibling**. This is why `domains/climate` takes `WorldTime` and not
  `StdDays`.
- **No new dependencies.** `serde`, `serde_json`, `libm` only.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec`. Enforced by
  `clippy.toml` `disallowed-types`.
- **No wall-clock time.**
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field and
  variant needs a one-line doc comment.
- **Every primitive at a `pub` boundary carries a `type-audit:` verdict tag.**
  A new `pub fn` without one fails `make gate-commit`.
- **`cargo fmt` is the final step before every commit.** Skipped fmt is the
  project's most common review finding.
- **Byte-identity is constitutional.** Any task that moves a golden stops and
  reports rather than accepting it. Goldens are accepted only by
  `make rebaseline-goldens`, never `make rebaseline`.
- **Gate before each commit:** `make gate-commit`. At each stage boundary,
  submit `make sluice-stage BRANCH=campaign/the-foliot REF=<full-sha>`.

## How to run things

```bash
cargo test -p hornvale-astronomy --test suite -- <filter>   # one integration filter
cargo test -p hornvale-vessel <filter>                      # one crate's unit tests
make gate-commit                                            # the commit gate
make seam-guard-list                                        # seam roster, no build
```

Integration tests live behind one `tests/suite.rs` per crate, so a former
per-file target is now a libtest **filter** after `--test suite --`.

---

## Stage 1 — vessel's second tick lattice (follow-up 2)

**Independent of stages 2–4.** Ships value on its own.

### The finding this stage rests on

`windows/vessel/src/clock.rs` declares `BASE_TICKS_PER_STD_DAY = 100_000`,
tied to `WorldTime::TICKS_PER_STD_DAY` by nothing but agreement.
`Session::charge` crosses `Ticks -> f64 days -> WorldTime` on every action.

The algebra says the crossing carries no information. With
`ticks_per_local_day(d) = round(d × B)` where `B = 100_000`:

```
days_of(t, d)          = t × d / round(d × B)          (standard days)
kernel ticks           = days_of(t, d) × B
                       = t × (d × B) / round(d × B)
                       ≈ t                              exactly t when d × B ∈ ℤ
```

The module's own doc confirms the intent: "An Earth-like world therefore has
`10_000` ticks per `MoveTo`, the historical `MOVE_DURATION` of `0.1` days" —
and 0.1 std days is 10,000 kernel ticks. **A vessel tick is a kernel tick.**
The `f64` bridge is therefore pure loss, and the fix is integer addition.

**Do not take that on trust — Task 1.1 proves it empirically before Task 1.2
acts on it.** If the probe contradicts the algebra, stop and report; the
design is wrong and the stage needs redesigning.

### Task 1.1: Prove the lattices coincide, and that the bridge loses ticks

**Files:**
- Create: `windows/vessel/tests/suite/clock_lattice.rs`
- Modify: `windows/vessel/tests/suite.rs` (register the module)

**Interfaces:**
- Consumes: `hornvale_vessel::clock::{Ticks, days_of, ticks_per_local_day, BASE_TICKS_PER_STD_DAY}`, `hornvale_kernel::WorldTime`
- Produces: nothing consumed by later tasks; this is a proof and a regression pin.

- [ ] **Step 1: Write the probe as a failing test**

Name the *property*, and let the search find the discriminating input — do not
hard-code a triple guessed from outside the code.

```rust
//! The vessel scheduler's tick and the kernel's tick are the same unit, and
//! the `f64` bridge between them loses whole ticks (The Foliot, stage 1).

use hornvale_kernel::WorldTime;
use hornvale_vessel::clock::{BASE_TICKS_PER_STD_DAY, Ticks, days_of, ticks_per_local_day};

/// The two lattices are declared by separate literals; assert they agree.
#[test]
fn the_vessel_base_rate_is_the_kernel_tick_rate() {
    assert_eq!(
        BASE_TICKS_PER_STD_DAY as i64,
        WorldTime::TICKS_PER_STD_DAY,
        "vessel and kernel declare the same rate through separate literals"
    );
}

/// A round trip through `f64` days must return the tick count it started
/// with. It does not, for some day lengths — find one rather than assume it.
#[test]
fn the_f64_bridge_loses_ticks_for_some_day_length() {
    let mut witnesses = Vec::new();
    // Day lengths the rotation pin actually admits: 16h to 40h, in minutes.
    for minutes in (16 * 60)..=(40 * 60) {
        let d = minutes as f64 / 24.0 / 60.0;
        for t in [1_u64, 150, 1_000, 3_000, 10_000] {
            let days = days_of(Ticks(t), Some(d));
            let round_tripped = WorldTime::from_std_days(days)
                .expect("a finite day value converts")
                .ticks();
            if round_tripped != t as i64 {
                witnesses.push((minutes, t, round_tripped));
            }
        }
    }
    assert!(
        !witnesses.is_empty(),
        "expected the f64 bridge to lose at least one tick somewhere in the \
         admitted rotation range; if this is EMPTY the stage-1 premise is \
         wrong -- STOP and report rather than proceeding to Task 1.2"
    );
    // Record what was found, so the fix has a named target.
    println!("bridge-loss witnesses: {}", witnesses.len());
    println!("first: {:?}", witnesses[0]);
}

/// `ticks_per_local_day` is exact by construction; the local lattice is a
/// whole number of kernel ticks per local day.
#[test]
fn a_local_day_is_a_whole_number_of_ticks() {
    for minutes in (16 * 60)..=(40 * 60) {
        let d = minutes as f64 / 24.0 / 60.0;
        assert!(ticks_per_local_day(Some(d)) >= 1);
    }
    assert_eq!(
        ticks_per_local_day(None),
        BASE_TICKS_PER_STD_DAY,
        "a locked world takes the base rate"
    );
}
```

- [ ] **Step 2: Register the module**

Add to `windows/vessel/tests/suite.rs`:

```rust
mod clock_lattice;
```

- [ ] **Step 3: Run the probe**

Run: `cargo test -p hornvale-vessel --test suite -- clock_lattice --nocapture`

Expected: `the_f64_bridge_loses_ticks_for_some_day_length` **passes** and
prints a non-zero witness count. That pass is the RED signal here — it proves
the defect exists.

**If the witness list is empty, STOP.** Report it; the stage's premise is
refuted and Task 1.2 must not proceed.

- [ ] **Step 4: Commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/tests/suite/clock_lattice.rs windows/vessel/tests/suite.rs
git commit -m "test(vessel): pin the two tick lattices and the f64 bridge's loss

Proves the vessel scheduler's tick and the kernel's tick are the same unit
declared through separate literals, and exhibits day lengths where the
Ticks -> f64 days -> WorldTime round trip loses whole ticks. The premise
stage 1's fix rests on, established before the fix."
```

### Task 1.2: Charge in integers

**Files:**
- Modify: `windows/vessel/src/clock.rs` (module header, `days_of`)
- Modify: `windows/vessel/src/session.rs:1970-1986` (`charge`)
- Test: `windows/vessel/tests/suite/clock_lattice.rs`

**Interfaces:**
- Consumes: Task 1.1's proof.
- Produces: `hornvale_vessel::clock::kernel_span(t: Ticks, day_length_std: Option<f64>) -> hornvale_kernel::units::TickSpan`, replacing `days_of` at the charge site. `days_of` itself stays for any presentation caller.

- [ ] **Step 1: Write the failing test**

Append to `clock_lattice.rs`:

```rust
use hornvale_vessel::clock::kernel_span;

/// The conversion the charge path uses is exact across the admitted
/// rotation range: no tick is created or destroyed.
#[test]
fn kernel_span_is_exact_across_the_rotation_range() {
    for minutes in (16 * 60)..=(40 * 60) {
        let d = minutes as f64 / 24.0 / 60.0;
        for t in [1_u64, 150, 1_000, 3_000, 10_000] {
            assert_eq!(
                kernel_span(Ticks(t), Some(d)).ticks(),
                t as i64,
                "a vessel tick is a kernel tick: {t} ticks at day length {d}"
            );
        }
    }
}

/// A locked world has no local day and takes the base rate, still exactly.
#[test]
fn kernel_span_is_exact_for_a_locked_world() {
    assert_eq!(kernel_span(Ticks(10_000), None).ticks(), 10_000);
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-vessel --test suite -- kernel_span`
Expected: FAIL — `kernel_span` does not exist (`unresolved import`).

- [ ] **Step 3: Implement `kernel_span`**

Add to `windows/vessel/src/clock.rs`, beside `days_of`:

```rust
/// The kernel-tick span one scheduler cost occupies.
///
/// A vessel tick and a kernel tick are the SAME unit: a local day is
/// [`ticks_per_local_day`] vessel ticks and also `day_length_std ×
/// WorldTime::TICKS_PER_STD_DAY` kernel ticks, and `ticks_per_local_day` is
/// defined as `round` of exactly that product. So the two lattices differ
/// only by that already-taken rounding, and the conversion is the identity.
///
/// This replaces the `Ticks -> f64 days -> WorldTime` round trip at the
/// charge site, which round-tripped through the continuous domain and lost
/// whole ticks (The Foliot, stage 1; the loss is exhibited in
/// `tests/suite/clock_lattice.rs`).
///
/// **Rounding rule, named at the call as decision 0186 requires:** there is
/// none. The conversion is exact integer identity. `days_of` survives for
/// presentation callers that genuinely want continuous days.
/// type-audit: bare-ok(ratio: day_length_std), bare-ok(count: return)
pub fn kernel_span(t: Ticks, _day_length_std: Option<f64>) -> TickSpan {
    TickSpan::from_ticks(t.0 as i64)
}
```

Add the import at the top of `clock.rs`:

```rust
use hornvale_kernel::units::TickSpan;
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-vessel --test suite -- kernel_span`
Expected: PASS, both tests.

- [ ] **Step 5: Move `charge` onto it**

Replace `windows/vessel/src/session.rs:1977-1985` with:

```rust
        let ticks = cost_ticks(action, self.body_mass_kg, terrain_factor);
        let span = crate::clock::kernel_span(ticks, self.day_length_std());
        self.day = WorldTime::from_ticks(self.day.ticks() + span.ticks());
        Ok(())
```

`advanced_by` returned a `Result` because `from_std_days` could refuse a
non-finite value; integer addition cannot fail, so the `match` goes. Check
whether `advanced_by` has other callers before deleting it:

Run: `grep -rn "advanced_by" windows/vessel/src/`

If it has none, delete it. If it has others, leave it and note them in the
commit message.

- [ ] **Step 6: Correct the module header**

`clock.rs:6-15` documents the defect as live. Replace that paragraph with a
statement that the bridge is gone from the charge path, citing this campaign.
Do not delete the history — say what changed and when.

- [ ] **Step 7: Run the vessel suite**

Run: `cargo test -p hornvale-vessel 2>&1 | tee /tmp/hv-foliot-s1.txt`

Byte goldens under `windows/vessel/tests/fixtures/` are **expected to move**.
Branch on what you see:

- **Only golden mismatches fail** → expected. Proceed to Task 1.3; the goldens
  are accepted in Task 1.4, not here.
- **A behavioural test fails** (anything not a golden) → STOP and report. The
  change was supposed to be sub-tick; a logic failure means it was not.
- **Nothing fails at all** → suspicious, not a success. It would mean no
  golden covers the charge path. Report it.

- [ ] **Step 8: Commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/src/clock.rs windows/vessel/src/session.rs windows/vessel/tests/suite/clock_lattice.rs
git commit -m "fix(vessel): charge in integer ticks, deleting the f64 bridge

Session::charge crossed Ticks -> f64 days -> WorldTime on every action, and
days_of's own doc conceded the round trip was lossy. The two lattices are
the same unit -- a local day is ticks_per_local_day vessel ticks and
day_length_std x TICKS_PER_STD_DAY kernel ticks, and the former is round()
of the latter -- so the crossing carried no information and only lost ticks.

kernel_span is the identity, and names its (absent) rounding rule at the
call as decision 0186 requires.

Byte goldens move; accepted separately under review."
```

### Task 1.3: End the name collision

**Files:**
- Modify: `windows/vessel/src/clock.rs` (the `Ticks` type and every use)
- Modify: every in-crate caller

**Interfaces:**
- Consumes: Task 1.2's `kernel_span`.
- Produces: `hornvale_vessel::clock::ActionCost` replacing `Ticks`. Same shape: `pub struct ActionCost(pub u64)`.

- [ ] **Step 1: Find every use**

Run: `grep -rn "\bTicks\b" windows/vessel/ cli/ windows/ --include=*.rs | grep -v TickSpan | grep -v TICKS`

- [ ] **Step 2: Rename**

`Ticks` → `ActionCost` throughout. It is an action *cost* — a duration,
legitimately unsigned — so the name states what it is and no longer collides
with the kernel's instant concept. Update `base_ticks` → `base_cost` and
`cost_ticks` → `cost_of` in the same pass, since their names encode the old
one.

Update the doc comment on the type:

```rust
/// What one in-character act costs the scheduler, in ticks.
///
/// A duration, legitimately unsigned — distinct from the kernel's
/// `WorldTime` (an instant) and `TickSpan` (a signed duration). Named
/// `ActionCost` rather than `Ticks` because it shared a name with the
/// kernel's tick concept while meaning something narrower (The Foliot).
/// Internal; never serialized.
/// type-audit: bare-ok(count)
pub struct ActionCost(pub u64);
```

- [ ] **Step 3: Verify it compiles and behaviour is unchanged**

Run: `cargo test -p hornvale-vessel 2>&1 | tail -30`

Expected: the same set of failures as Task 1.2 Step 7 — a rename changes no
behaviour. **A new failure means the rename was not mechanical.**

- [ ] **Step 4: Commit**

```bash
cargo fmt
make gate-commit
git add -A windows/vessel
git commit -m "refactor(vessel): rename Ticks to ActionCost

It is an action cost -- a duration, legitimately unsigned -- and shared a
name with the kernel's tick concept while meaning something narrower.
base_ticks/cost_ticks renamed with it. No behaviour change."
```

### Task 1.4: Retire the f64 accumulation in liveness

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (near `:5079`, `:5164`, `:5223`)

- [ ] **Step 1: Read the three sites**

Run: `sed -n '5070,5090p;5155,5175p;5215,5235p' windows/vessel/src/liveness.rs`

The registry row records these as comparing an accumulated raw `f64` against
a tick-derived bound — the shape decision 0186 clause 1 names.

- [ ] **Step 2: Establish what each site is actually doing before changing it**

For each site, write down in the commit message: what accumulates, what the
bound is, and whether the comparison can straddle a tick boundary. **If a
site turns out not to have the shape the registry describes, say so** — the
row is evidence-cited but was written from a fix-wave analysis, not from a
test.

- [ ] **Step 3: Move each genuine instance to integer comparison**

Compare `WorldTime`/`TickSpan` values directly. They derive `Ord`, so no
`total_cmp` and no epsilon is needed.

- [ ] **Step 4: Run the suite**

Run: `cargo test -p hornvale-vessel 2>&1 | tail -30`
Expected: same failure set as Task 1.3 — goldens only.

- [ ] **Step 5: Commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/src/liveness.rs
git commit -m "fix(vessel): compare instants as integers in liveness

Three sites accumulated a raw f64 and compared it against a tick-derived
bound -- the shape decision 0186 clause 1 names. WorldTime and TickSpan
derive Ord, so the comparison is exact with no epsilon."
```

### Task 1.5: Accept the goldens, under review

**Files:**
- Modify: `windows/vessel/tests/fixtures/*.json` (5 files)

**This task requires Nathan. Do not run it unattended.**

- [ ] **Step 1: Show the diff before accepting anything**

```bash
cargo test -p hornvale-vessel 2>&1 | grep -A5 "golden"
```

- [ ] **Step 2: Characterize the movement**

For each fixture, state how far it moved — in ticks, and in whatever the
fixture's own units are. A sub-tick-per-action rounding fix should produce
small, monotone movement. **Large or erratic movement is a signal the fix is
wrong, not a signal to accept harder.**

- [ ] **Step 3: Present to Nathan and wait**

Report: which fixtures moved, by how much, and why. Get an explicit yes.

- [ ] **Step 4: Accept**

```bash
REBASELINE=1 cargo test -p hornvale-vessel
```

Or `make rebaseline-goldens` if it covers vessel. **Never `make rebaseline`** —
it does not write goldens and must not.

- [ ] **Step 5: Verify the accept is clean**

Run: `cargo test -p hornvale-vessel`
Expected: PASS, everything.

- [ ] **Step 6: Commit**

```bash
git add windows/vessel/tests/fixtures
git commit -m "chore(vessel): accept goldens after the integer-charge fix

The charge path no longer round-trips through f64 days, so accumulated
positions move by the ticks the old bridge was losing. Reviewed by Nathan;
movement characterized in the campaign chronicle."
```

### Stage 1 gate

- [ ] Submit the stage gate:

```bash
git push -u origin campaign/the-foliot
make sluice-stage BRANCH=campaign/the-foliot REF=$(git rev-parse HEAD)
```

- [ ] Read the result with `make sluice-log`. Do not start stage 2 until green.

---

## Stage 2 — the instant/duration split (follow-up 5)

### Task 2.1: Add a `signed` arm to the `quantity!` macro

**Files:**
- Modify: `domains/astronomy/src/units.rs:31-90` (the macro)
- Test: `domains/astronomy/src/units.rs` (the in-module `mod tests`)

**Interfaces:**
- Produces: a third macro arm `signed`, generating the same shape as
  `non_negative` but validating finiteness only.

- [ ] **Step 1: Write the failing test**

Add to the test module at the bottom of `units.rs`:

```rust
#[test]
fn a_signed_quantity_admits_both_signs_and_refuses_nonfinite() {
    assert!(StdInstant::new(0.0).is_ok(), "genesis is representable");
    assert!(
        StdInstant::new(-0.5).is_ok(),
        "an instant before genesis is a point on the axis (decision 0126)"
    );
    assert!(StdInstant::new(12_345.0).is_ok());
    assert!(StdInstant::new(f64::NAN).is_err());
    assert!(StdInstant::new(f64::INFINITY).is_err());
}

#[test]
fn a_duration_still_refuses_a_negative() {
    assert!(
        StdDays::new(-0.5).is_err(),
        "StdDays is a duration now; non-negative is correct for it"
    );
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-astronomy signed_quantity`
Expected: FAIL — `StdInstant` not found.

- [ ] **Step 3: Add the macro arm**

After the `non_negative` arm in `units.rs`:

```rust
    ($name:ident, $label:literal, signed, $doc:literal) => {
        #[doc = $doc]
        #[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
        pub struct $name(pub(crate) f64);
        impl $name {
            /// Validating constructor: finite, either sign.
            ///
            /// An instant is a point on an axis with genesis at zero, so a
            /// negative value is legal and meaningful (decision 0126). Only
            /// non-finiteness is refused.
            pub fn new(value: f64) -> Result<Self, UnitError> {
                if !value.is_finite() {
                    return Err(UnitError {
                        unit: $label,
                        value,
                        reason: "must be finite",
                    });
                }
                Ok(Self(value))
            }
            /// The raw value.
            pub fn get(self) -> f64 {
                self.0
            }
        }
    };
```

- [ ] **Step 4: Declare `StdInstant`**

Beside the `StdDays` invocation:

```rust
quantity!(
    StdInstant,
    "standard days since genesis",
    signed,
    "An instant on the world's time axis, in standard days since genesis. \
     Negative values are pre-genesis and legal (decision 0126). Distinct \
     from `StdDays`, which is a DURATION and correctly non-negative."
);
```

And correct `StdDays`'s own doc, which currently reads "Absolute time or
duration in standard days" — the conflation this campaign exists to remove:

```rust
quantity!(
    StdDays,
    "standard days",
    non_negative,
    "A DURATION in standard days. For an instant, see `StdInstant`."
);
```

- [ ] **Step 5: Run to verify it passes**

Run: `cargo test -p hornvale-astronomy signed_quantity`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
cargo fmt
make gate-commit
git add domains/astronomy/src/units.rs
git commit -m "feat(astronomy): add StdInstant, a signed instant on the time axis

StdDays's doc comment read 'Absolute time or duration in standard days' --
both meanings in one type, with a non_negative rule correct for the
duration half and wrong for the instant half. StdInstant takes the instant
meaning; StdDays keeps the duration meaning and its validation."
```

### Task 2.2: Add the conversions the boundary needs

**Files:**
- Modify: `domains/astronomy/src/units.rs`
- Test: same file's test module

**Interfaces:**
- Produces:
  - `StdInstant::since_genesis(TickSpan) -> StdInstant` is **not** added — see below.
  - `impl Sub for StdInstant -> StdDays` is **not** added — see below.
  - `StdInstant::from_world_time(WorldTime) -> StdInstant` is **not** added — astronomy already crosses at `GeneratedSky::t`, and adding a second crossing would defeat the funnel.

Only what the call sites actually need:

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn the_difference_of_two_instants_is_a_duration() {
    let a = StdInstant::new(-10.0).unwrap();
    let b = StdInstant::new(15.0).unwrap();
    assert_eq!(b.since(a).get(), 25.0);
    assert_eq!(a.since(b).get(), 25.0, "a duration is unsigned");
}

#[test]
fn an_instant_offset_by_a_duration_is_an_instant() {
    let t = StdInstant::new(-5.0).unwrap();
    assert_eq!(t.plus(StdDays::new(10.0).unwrap()).get(), 5.0);
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-astronomy difference_of_two_instants`
Expected: FAIL — no method `since`.

- [ ] **Step 3: Implement**

```rust
impl StdInstant {
    /// The duration between two instants, unsigned.
    ///
    /// A duration has no direction; if you need the signed difference, take
    /// `self.get() - other.get()` and say at the call site what the sign
    /// means.
    /// type-audit: bare-ok(constructor-edge)
    pub fn since(self, other: StdInstant) -> StdDays {
        StdDays((self.0 - other.0).abs())
    }

    /// This instant advanced by a duration.
    /// type-audit: bare-ok(constructor-edge)
    pub fn plus(self, span: StdDays) -> StdInstant {
        StdInstant(self.0 + span.0)
    }
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-astronomy -- instant`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt
make gate-commit
git add domains/astronomy/src/units.rs
git commit -m "feat(astronomy): instant/duration arithmetic

since() is instant - instant -> duration; plus() is instant + duration ->
instant. Only the two operations the call sites need; a signed difference
is deliberately not offered, so a caller that wants direction states what
the sign means at its own site."
```

### Task 2.3: Retype the 27 instant positions

**Files:**
- Modify: `domains/astronomy/src/calendar.rs` (17 methods)
- Modify: `domains/astronomy/src/eclipses.rs` (6 functions, plus `EclipseEvent::day`)
- Modify: `domains/astronomy/src/star.rs` (2 functions)
- Modify: `domains/astronomy/src/heliacal.rs` (1 function)
- Modify: `domains/astronomy/src/night_sky.rs` (1 function)
- Modify: `domains/astronomy/src/provider.rs:1523` (`GeneratedSky::t`)

**Interfaces:**
- Consumes: `StdInstant` from Task 2.1, `since`/`plus` from Task 2.2.
- Produces: every listed signature takes/returns `StdInstant` in instant
  position. `GeneratedSky::t` becomes `fn t(&self, time: WorldTime) -> StdInstant`.

- [ ] **Step 1: Enumerate the exact set, by parser not by grep**

A line-grep for `t: StdDays` misses multi-line signatures (`eclipse_events`,
`heliacal_events`) and unconventional names (`t_now`). Run:

```bash
python3 - <<'EOF'
import re, glob
for p in sorted(glob.glob('domains/astronomy/src/*.rs')):
    src = open(p).read()
    for m in re.finditer(r'pub fn (\w+)\s*\(([^)]*)\)', src, re.S):
        params = m.group(2)
        if 'StdDays' not in params: continue
        names = [a.split(':')[0].strip() for a in params.split(',') if 'StdDays' in a]
        line = src[:m.start()].count('\n') + 1
        kind = 'INSTANT' if any(n in ('t','t0','t1','t_now','from','until') for n in names) else 'duration'
        print(f"{kind:8} {p}:{line} {m.group(1)} {names}")
EOF
```

Expected: 27 INSTANT, 8 duration. **If the counts differ from 27/8, the tree
has moved since the plan was written — report the new numbers before
proceeding.**

- [ ] **Step 2: Retype instant positions only**

Change `StdDays` → `StdInstant` at every INSTANT position, and **only** there.
Leave every `duration` line alone. Also:

- `EclipseEvent::day: StdDays` → `StdInstant` (a syzygy is an instant).
- `Calendar::alignment_epoch_of` — takes `t_now: StdInstant` **and** returns
  `Option<StdInstant>` (an epoch is an instant). The mixed case.
- `GeneratedSky::t` returns `StdInstant`.

- [ ] **Step 3: Fix the internal arithmetic the change exposes**

`eclipses.rs` and `heliacal.rs` construct values through the `pub(crate)`
tuple field. Where a construction is an instant, use `StdInstant(...)`; where
it is a duration, keep `StdDays(...)`. `heliacal.rs:82`'s
`StdDays(day_start + fraction * day_length)` is an **instant** — this is the
site decision 0190 identified as reaching the negative path.

- [ ] **Step 4: Compile**

Run: `cargo check -p hornvale-astronomy --all-targets 2>&1 | head -40`

Fix until clean. Type errors here are the point: each one is a place the old
type was doing both jobs.

- [ ] **Step 5: Run the astronomy suite**

Run: `cargo test -p hornvale-astronomy 2>&1 | tail -30`
Expected: PASS. **This retype must not change any value** — it is a type
change over identical arithmetic. A behavioural failure means an instant and
a duration got swapped; find it rather than adjusting the test.

- [ ] **Step 6: Commit**

```bash
cargo fmt
make gate-commit
git add domains/astronomy/src
git commit -m "refactor(astronomy): instants take StdInstant, durations keep StdDays

27 public functions, one public field (EclipseEvent::day) and the
GeneratedSky funnel move to the signed instant type. Durations are
untouched. No arithmetic changes -- the same numbers, in types that no
longer claim a duration and an instant are the same thing."
```

### Task 2.4: Move the external callers

**Files:**
- Modify: `windows/worldgen/src/lib.rs`, `windows/worldgen/src/chorus.rs`
- Modify: `windows/book/src/lib.rs`
- Modify: `windows/vessel/src/session.rs:4950`, `windows/vessel/src/eyes.rs`
- Modify: `windows/scene/src/lib.rs`, `windows/scene/examples/*.rs`
- Modify: `windows/lab/`, `cli/src/` as the compiler directs

- [ ] **Step 1: Compile the workspace and let it name the sites**

Run: `cargo check --workspace --all-targets 2>&1 | grep -E "^error" | head -40`

- [ ] **Step 2: Fix each**

`StdDays::new(x)` in an instant position becomes `StdInstant::new(x)`.
`windows/hearsay` should need **no change at all** — its 82 uses are `span`,
`generation` and `lifespan`, all durations. **If hearsay needs changes, stop
and re-read: something was retyped that should not have been.**

- [ ] **Step 3: Full workspace test**

Run: `cargo nextest run --workspace 2>&1 | tee /tmp/hv-foliot-s2.txt`
Expected: PASS. No golden should move — this is a type change.

- [ ] **Step 4: Commit**

```bash
cargo fmt
make gate-commit
git add -A
git commit -m "refactor: move external callers onto StdInstant

worldgen, book, vessel, scene, lab and cli construct instants through
StdInstant now. hearsay is untouched: all 82 of its StdDays uses are
durations (span, generation, lifespan), which is the split working."
```

### Task 2.5: Trace the clamp, then decide

**Files:**
- Modify: `domains/astronomy/src/provider.rs:1516-1525`
- Create: `docs/decisions/0192-<slug>.md` (check the next free number first)

**This task's output is a decision, and the decision depends on a trace whose
answer is not known at planning time. Follow the branch table; do not
pre-commit to an outcome.**

- [ ] **Step 1: Find the next decision number**

Run: `ls docs/decisions/ | tail -5`

- [ ] **Step 2: Run the trace**

Determine whether a pre-genesis `WorldTime` can reach `GeneratedSky::t`.
Enumerate callers of `sky_at` / `sky_at_visibility` outward until you reach
either a caller that constructs time from world state (and can show its
lower bound) or one that takes time from an external input (and cannot).

```bash
grep -rn "sky_at\|sky_at_visibility\|sky_report_at" --include=*.rs . | grep -v target
```

**Decision 0190 exists because a confident trace of exactly this question was
wrong.** State explicitly what you checked and what you could not.

- [ ] **Step 3: Branch**

- **Trace closes, no caller reaches negative** → delete the clamp. Write the
  decision as "the clamp is removed; the type now expresses the honest
  answer", superseding 0187, citing the trace and 0190. Run the workspace
  suite; expect no golden movement. If a golden moves, the trace was wrong —
  go to the second branch.
- **A caller can reach it** → keep or remove per what the output shows, but
  **stop and bring the diff to Nathan first**. Do not accept bytes in this
  task.
- **Trace inconclusive** → keep the clamp. Write the decision to supersede
  0187's *rationale* only, replacing the retracted "not a physical question"
  reasoning with the structural one that actually holds.

- [ ] **Step 4: Fix the code comment either way**

`provider.rs:1519-1521` still cites 0187's retracted rationale ("the sky
before the world exists is not a physical question"). It must not survive
this stage in that form, regardless of which branch ran.

- [ ] **Step 5: Regenerate the decision index**

```bash
cargo run --manifest-path tools/digest/Cargo.toml -- render decisions > docs/digest/decisions-in-force.md
```

- [ ] **Step 6: Commit**

```bash
cargo fmt
make gate-commit
git add domains/astronomy/src/provider.rs docs/decisions docs/digest
git commit -m "decision: settle the pre-genesis sky query on the split types

0187 chose the clamp because StdDays foreclosed the honest answer at the
public boundary. StdInstant reopens it, so the choice is made on its merits
and recorded. The code comment citing 0187's retracted rationale is gone."
```

### Stage 2 gate

- [ ] `make sluice-stage BRANCH=campaign/the-foliot REF=$(git rev-parse HEAD)`

---

## Stage 3 — the negative-time sweep (follow-up 4)

### Task 3.1: Sweep the 17 instant-taking Calendar methods at negative time

**Files:**
- Create: `domains/astronomy/tests/suite/calendar_negative_time.rs`
- Modify: `domains/astronomy/tests/suite.rs`

**Interfaces:**
- Consumes: `StdInstant` (Task 2.1), the retyped `Calendar` (Task 2.3).

The point of doing this after stage 2: today the only negative-time test lives
*inside* `calendar.rs` and its own comment records that it had to bypass
`StdDays::new`. This sweep runs through the **public API**, from the
integration suite, with no bypass.

- [ ] **Step 1: Write the sweep**

```rust
//! Every Calendar method that takes an instant, exercised at negative time.
//!
//! `year_phase`, `season_phase` and `moon_phase` use `.fract()`, which is
//! negative for negative input; before The Foliot none of the three was
//! tested there and the behaviour was unknown rather than known-wrong.
//! Decision 0126: a time point may be negative because it is a point on an
//! axis, not a duration.

use hornvale_astronomy::units::StdInstant;
use hornvale_astronomy::{calendar_of, Calendar};
use hornvale_kernel::Seed;
use hornvale_worldgen::{SkyChoice, build_world, sky_of};

fn earthlike_calendar() -> Calendar {
    let world = build_world(
        Seed(42),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds");
    let sky = sky_of(&world).expect("generated sky");
    calendar_of(sky.system().expect("system"))
}

/// Instants spanning genesis, so each assertion sees both signs.
fn probes() -> Vec<StdInstant> {
    [-100_000.0, -365.25, -1.5, -0.25, 0.0, 0.25, 1.5, 365.25, 100_000.0]
        .into_iter()
        .map(|d| StdInstant::new(d).expect("finite"))
        .collect()
}

/// A phase is a fraction of a cycle: it must lie in [0, 1) on BOTH sides of
/// genesis. `.fract()` returns a negative for negative input, which is the
/// specific defect this sweep exists to catch.
#[test]
fn every_phase_stays_in_the_unit_interval_across_genesis() {
    let cal = earthlike_calendar();
    for t in probes() {
        let yp = cal.year_phase(t);
        assert!(
            (0.0..1.0).contains(&yp),
            "year_phase({:?}) = {yp}, outside [0,1)",
            t.get()
        );
        if let Some(sp) = cal.season_phase(t) {
            assert!(
                (0.0..1.0).contains(&sp),
                "season_phase({:?}) = {sp}, outside [0,1)",
                t.get()
            );
        }
        if let Some(mp) = cal.moon_phase(t, 0) {
            assert!(
                (0.0..1.0).contains(&mp),
                "moon_phase({:?}, 0) = {mp}, outside [0,1)",
                t.get()
            );
        }
    }
}

/// A local day index falls as time falls, and the fraction stays in [0,1)
/// — the property The Escapement fixed for local_day, pinned publicly here.
#[test]
fn local_day_is_monotone_and_its_fraction_is_bounded_across_genesis() {
    let cal = earthlike_calendar();
    let mut previous: Option<(i64, f64)> = None;
    for t in probes() {
        let Some((index, fraction)) = cal.local_day(t) else {
            continue;
        };
        assert!(
            (0.0..1.0).contains(&fraction),
            "local_day({:?}) fraction {fraction} outside [0,1)",
            t.get()
        );
        if let Some((prev_index, _)) = previous {
            assert!(
                index >= prev_index,
                "local_day index went backwards as time advanced"
            );
        }
        previous = Some((index, fraction));
    }
}

/// A fraction is a fraction on both sides of genesis.
#[test]
fn daylight_and_altitude_stay_in_range_across_genesis() {
    let cal = earthlike_calendar();
    for t in probes() {
        if let Some(f) = cal.daylight_fraction(t) {
            assert!((0.0..=1.0).contains(&f), "daylight_fraction {f} at {:?}", t.get());
        }
        for latitude in [-80.0, -23.5, 0.0, 23.5, 80.0] {
            if let Some(f) = cal.daylight_fraction_at(t, latitude) {
                assert!(
                    (0.0..=1.0).contains(&f),
                    "daylight_fraction_at({:?}, {latitude}) = {f}",
                    t.get()
                );
            }
            if let Some(alt) = cal.solar_altitude_at(t, latitude) {
                assert!(
                    (-90.0..=90.0).contains(&alt),
                    "solar_altitude_at({:?}, {latitude}) = {alt}",
                    t.get()
                );
            }
            if let Some(az) = cal.solar_azimuth_at(t, latitude) {
                assert!(
                    (0.0..360.0).contains(&az),
                    "solar_azimuth_at({:?}, {latitude}) = {az}",
                    t.get()
                );
            }
        }
    }
}

/// Nothing in the instant-taking surface panics or returns a non-finite
/// number at negative time. The blunt half of the sweep, covering the
/// methods with no tighter invariant of their own.
#[test]
fn no_instant_taking_method_produces_a_non_finite_value() {
    let cal = earthlike_calendar();
    for t in probes() {
        assert!(cal.solar_declination(t).is_finite());
        assert!(cal.precession_offset_deg(t).is_finite());
        let eq = cal.solar_equatorial(t);
        assert!(eq.right_ascension_deg.is_finite() && eq.declination_deg.is_finite());
        for latitude in [-45.0, 0.0, 45.0] {
            if let Some(a) = cal.solstice_rise_azimuth_at(latitude, t) {
                assert!(a.is_finite());
            }
            let _ = cal.sky_band(t, latitude);
        }
        let _ = cal.is_daylight(t);
    }
}

/// The duration between two instants is unsigned and symmetric, including
/// when one is pre-genesis.
#[test]
fn alignment_drift_is_defined_across_genesis() {
    let cal = earthlike_calendar();
    let before = StdInstant::new(-500.0).unwrap();
    let after = StdInstant::new(500.0).unwrap();
    if let Some(d) = cal.alignment_drift_deg(45.0, before, after) {
        assert!(d.is_finite(), "alignment_drift_deg across genesis: {d}");
    }
}
```

- [ ] **Step 2: Register the module**

Add `mod calendar_negative_time;` to `domains/astronomy/tests/suite.rs`.

- [ ] **Step 3: Run it**

Run: `cargo test -p hornvale-astronomy --test suite -- calendar_negative_time --nocapture`

**Branch on the result — do not assume either way:**

- **All pass** → the behaviour was correct and is now pinned. That is a real
  result; say so in the chronicle. The follow-up said the behaviour was
  *unknown*, not *wrong*.
- **Some fail** → you have found live defects. Do **not** weaken the
  assertion to make it pass. For each failure decide whether the invariant or
  the implementation is wrong, fix the implementation where it is, and record
  each one.

- [ ] **Step 4: Fix what the sweep catches**

For each genuine defect, fix it in `calendar.rs` and re-run. `.fract()` on a
negative wants `rem_euclid(1.0)` where a phase is intended — but verify per
site rather than substituting mechanically.

- [ ] **Step 5: Full crate suite**

Run: `cargo test -p hornvale-astronomy 2>&1 | tail -20`

If a fix changed behaviour at *non*-negative time too, goldens may move —
stop and report rather than accepting.

- [ ] **Step 6: Commit**

```bash
cargo fmt
make gate-commit
git add domains/astronomy/tests
git commit -m "test(astronomy): sweep every instant-taking Calendar method at negative time

17 methods, exercised through the public API from the integration suite --
which stage 2 made possible: the pre-existing negative-time test lives
inside calendar.rs and had to bypass StdDays::new to construct its input.
Decision 0126 makes a negative instant legal; nothing had checked whether
the calendar agreed."
```

### Stage 3 gate

- [ ] `make sluice-stage BRANCH=campaign/the-foliot REF=$(git rev-parse HEAD)`

---

## Stage 4 — climate and scene (follow-ups 1, 3)

### Task 4.1: climate takes `WorldTime`

**Files:**
- Modify: `domains/climate/src/provider.rs:325` (`temperature_at`), and `is_frozen_at`
- Modify: `windows/locale/src/surface.rs` (5 sites), `windows/locale/src/lib.rs` (2 sites)
- Modify: `windows/worldgen/src/graph_derive.rs:226`
- Modify: `windows/scene/src/lib.rs:660,1899`, `windows/scene/src/region.rs:571`
- Modify: `windows/lab/tests/suite/the_fare_calibration.rs:341`, `the_mire_calibration.rs:349`
- Modify: `windows/scene/examples/illumination_probe.rs`
- Modify: `windows/locale/tests/suite/surface_mixture.rs:216`

**Interfaces:**
- Produces: `ClimateProvider::temperature_at(&self, vertex: Vertex, at: WorldTime) -> Temperature` and `is_frozen_at(&self, vertex: Vertex, at: WorldTime) -> bool`.

`WorldTime` and not `StdDays`: a domain may never depend on a sibling, and
`StdDays` lives in `domains/astronomy`. This is forced by the layering rule.

- [ ] **Step 1: Write the failing test**

Create `domains/climate/tests/suite/typed_time.rs` (register it in
`domains/climate/tests/suite.rs`):

```rust
//! Climate's sampling API takes a typed instant, not a bare f64 day.

use hornvale_kernel::WorldTime;

#[test]
fn the_sampling_api_takes_a_world_time() {
    let provider = /* build the crate's usual test provider — follow the
                      pattern already used in domains/climate/tests/ */
        todo_build_provider();
    let vertex = /* the crate's usual test vertex */ todo_vertex();
    let genesis = provider.temperature_at(vertex, WorldTime::GENESIS);
    let later = provider.temperature_at(
        vertex,
        WorldTime::from_std_days(180.0).expect("finite"),
    );
    assert!(genesis.get().is_finite() && later.get().is_finite());
}
```

**Before writing this, read an existing test in `domains/climate/tests/` and
copy its provider-construction pattern.** The two `todo_` calls above are
placeholders for that pattern and must be replaced with the real one — do not
commit them.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-climate --test suite -- typed_time`
Expected: FAIL — a `WorldTime` does not coerce to `f64`.

- [ ] **Step 3: Retype the two methods**

In `domains/climate/src/provider.rs`, change the parameter to `at: WorldTime`
and convert once at the top of each body:

```rust
    pub fn temperature_at(&self, vertex: Vertex, at: WorldTime) -> Temperature {
        let day = at.as_std_days();
        // ... body unchanged, using `day`
    }
```

`as_std_days` is the kernel's named hatch (decision 0188) and is lossless
below ~2.47e8 years.

- [ ] **Step 4: Fix each call site, recording provenance**

For every site, record in the commit message where its `day` came from:

- A literal `0.0` → `WorldTime::GENESIS`. Byte-neutral.
- A value already derived from a `WorldTime` (e.g. `at.as_std_days()` in
  `windows/locale`) → pass the `WorldTime` straight through. Byte-neutral, and
  strictly better: it removes a round trip.
- An independent `f64` (`windows/worldgen/src/graph_derive.rs:226` passes a
  loop variable — **trace where `day` is bound**) → converting requires
  `WorldTime::from_std_days`, which **rounds**. Record the site, state the
  rounding, and check whether a golden moves.

**Do not absorb a rounding silently.** If a site needs one, name it.

- [ ] **Step 5: Run the workspace**

Run: `cargo nextest run --workspace 2>&1 | tee /tmp/hv-foliot-s4a.txt`

Expected: PASS with no golden movement. A moved golden means a site in the
third category above — stop and report which.

- [ ] **Step 6: Commit**

```bash
cargo fmt
make gate-commit
git add -A
git commit -m "refactor(climate): sampling API takes WorldTime, not a bare f64 day

is_frozen_at and temperature_at took an untyped f64 that nothing stopped a
caller passing ticks into. WorldTime and not StdDays because a domain may
never depend on a sibling and StdDays lives in astronomy.

Call-site provenance recorded per site; sites that already held a WorldTime
now pass it through instead of round-tripping."
```

### Task 4.2: scene's window bounds take `StdInstant`

**Files:**
- Modify: `windows/scene/src/lib.rs` (`eclipses_scene`)
- Modify: `clients/world-wasm/src/lib.rs:428`, `cli/src/main.rs:1972`, `windows/scene/examples/profile_scene.rs:180`, `cli/tests/suite/scene_cost.rs:385`

- [ ] **Step 1: Retype the signature**

```rust
pub fn eclipses_scene(
    world: &World,
    from: StdInstant,
    until: StdInstant,
) -> Result<EclipsesScene, SceneError> {
```

Remove the now-redundant `StdDays::new(from)` / `StdDays::new(until)`
conversions inside the body — the type already guarantees finiteness.

- [ ] **Step 2: Hoist the bound conversions above the events map**

This closes Minor 5 from The Escapement's review while the file is open: the
bound conversions currently run *after* the events map, so an out-of-range
`until` hits an `expect` and panics before it can reach the graceful error.

- [ ] **Step 3: Fix the callers**

`clients/world-wasm` is outside the cargo workspace and has its own gate —
build it explicitly:

Run: `make world-check`

- [ ] **Step 4: Run**

Run: `cargo nextest run --workspace 2>&1 | tail -20`

- [ ] **Step 5: Commit**

```bash
cargo fmt
make gate-commit
git add -A
git commit -m "refactor(scene): eclipse window bounds are typed instants

eclipses_scene took two bare f64 days. Also hoists the bound conversions
above the events map, closing The Escapement review's Minor 5: an
out-of-range 'until' hit an expect() and panicked before reaching the
graceful error path."
```

### Task 4.3: Drop the float day from the eclipse scene, bump to v2

**Files:**
- Modify: `windows/scene/src/lib.rs:1371-1460` (`EclipseElem`, `EclipsesScene`)
- Rename: `book/src/reference/scene-eclipses-v1.md` → `scene-eclipses-v2.md`
- Modify: `book/src/SUMMARY.md` (the reference entry)
- Modify: `docs/generated-paths.txt` **only if** the regenerated JSON lands at a new path

**Nathan has ruled both external consumers out of scope** — the Orrery and
goldengrove (`hornvale/goldengrove`). Verified: no in-repo client reads these
fields.

- [ ] **Step 1: Delete the f64 fields and rename the tick fields**

`EclipseElem`: drop `day`, rename `day_ticks` → `day`, typed `WorldTime`.
`EclipsesScene`: drop `from_day`/`until_day`, rename `from_day_ticks` →
`from`, `until_day_ticks` → `until`.

Set `schema` to `scene/eclipses/v2`.

Update the doc comments: they currently justify the `f64` halves by the
Orrery's existence. Replace with a note that the external consumers were
declared out of scope by The Foliot and the exact tick is now the only
representation.

- [ ] **Step 2: Update the schema reference page**

`book/src/reference/scene-eclipses-v1.md` documents the v1 shape. Rename to
`-v2.md`, update the field table, and state that v1 carried a quantized `f64`
day beside the tick and v2 carries only the tick.

- [ ] **Step 3: Regenerate artifacts**

```bash
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

Expected to move: `book/src/gallery/scene-eclipses-seed-42.json`,
`book/src/reference/`, and `docs/audits/type-audit-report.md` (the pub
boundary changed).

**`deno` must be on `PATH` or `make rebaseline` silently skips the atlas
bundle** (registry row `TOOL-rebaseline-skips-the-atlas-bundle-without-deno`).
It lives at `~/.deno/bin`. Check before running:

```bash
command -v deno || export PATH="$HOME/.deno/bin:$PATH"
```

- [ ] **Step 4: `git add` the renamed reference page explicitly**

A renamed file inside an already-declared directory is invisible to
`git diff --exit-code` until it is in the index (the hazard
`docs/generated-paths.txt` documents at length).

- [ ] **Step 5: Verify the drift check is clean**

```bash
git add -A
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

Expected: exit 0 after staging.

- [ ] **Step 6: Full workspace**

Run: `cargo nextest run --workspace && cargo test --workspace --doc`

- [ ] **Step 7: Commit**

```bash
cargo fmt
make gate-commit
git add -A
git commit -m "feat(scene)!: eclipse scene carries only the exact tick, schema v2

The quantized f64 day existed solely for the external Orrery, which Nathan
has declared out of scope along with goldengrove; no in-repo client reads
these fields. The tick needs no quantization and does not decay with world
age (decision 0188).

Bumped to scene/eclipses/v2 rather than edited in place so a consumer that
ever reappears fails loudly on an unknown schema instead of silently
reading a missing field."
```

### Task 4.4: Settle the type-audit verdicts

**Files:**
- Modify: the `type-audit:` tags on every struct and function this campaign retyped
- Modify: `docs/audits/type-audit-report.md` (regenerated)

- [ ] **Step 1: Find what is still pending on the surfaces we moved**

```bash
grep -rn "pending(wave-2" windows/scene/src windows/locale/src domains/climate/src
```

- [ ] **Step 2: Settle only the tags this campaign's changes resolve**

A field that is now a `WorldTime` or `StdInstant` is no longer a bare
primitive and its tag goes. A field that is still a bare `f64` for a reason
keeps a tag — but the reason must be stated, not inherited.

**Do not settle the wider `pending(wave-2)` backlog.** 70 tags exist
workspace-wide and the spec puts them out of scope.

- [ ] **Step 3: Regenerate the report**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
```

- [ ] **Step 4: Commit**

```bash
make gate-commit
git add -A
git commit -m "chore(type-audit): settle the verdicts this campaign's retypes resolve

Fields that became WorldTime or StdInstant are no longer bare primitives.
The wider pending(wave-2) backlog is out of scope and untouched."
```

### Stage 4 gate

- [ ] `make sluice-stage BRANCH=campaign/the-foliot REF=$(git rev-parse HEAD)`

---

## Close

Definition of Done for every merged plan (CLAUDE.md):

- [ ] **Chronicle entry** — `book/src/chronicle/the-foliot.md`, added to
      `book/src/SUMMARY.md`. Cover: the redenomination framing, the
      vessel-tick-is-a-kernel-tick finding, what the negative-time sweep
      actually found, and the clamp decision's outcome.
- [ ] **Freshness sweep** — any chapter describing scene's `f64` day, or
      `StdDays` as "absolute time or duration", is now stale.
- [ ] **Confidence Gradient** — re-score `book/src/open-questions.md` if this
      campaign moved a bet.
- [ ] **Retrospective** — `docs/retrospectives/the-foliot.md`.
- [ ] **Registry rows** — the capture manifest in
      `.superpowers/sdd/decision-ledger.md`: rows for The Escapement
      follow-ups 1, 3, 4, 5, plus the `worktree-take` name-collision `TOOL-`
      row. Mark `TOOL-vessel-clock-duplicates-the-kernel-tick-lattice`
      shipped.
- [ ] **Promote the ledger** — `.superpowers/sdd/` is git-ignored and dies
      with the worktree. Move its material entries into the retrospective
      before teardown.
- [ ] **Merge** — `make sluice BRANCH=campaign/the-foliot REF=<full-sha>`.

## Self-review notes

Checked against the spec:

- Spec §2 type design → Tasks 2.1–2.4. Spec §3 stage 1 → Tasks 1.1–1.5.
  Stage 2 → 2.1–2.5. Stage 3 → 3.1. Stage 4 → 4.1–4.4.
- Spec §5 success criteria: (1) Tasks 2.1/2.3; (2) Task 1.2; (3) Task 3.1;
  (4) Tasks 4.1/4.2/4.3; (5) Tasks 1.5, 4.1 step 5, 4.3 step 3; (6) Task 2.5.
- **One placeholder is deliberate and marked**: Task 4.1 Step 1's
  `todo_build_provider()` / `todo_vertex()`, which the step explicitly
  instructs the implementer to replace by copying the crate's existing test
  pattern. It is flagged rather than invented because guessing a provider
  constructor from outside the crate is exactly the class of plan defect the
  autopilot rules warn about.
- Type consistency: `ActionCost` (1.3) is used consistently after `Ticks`
  (1.1, 1.2); `kernel_span` (1.2) is the only new vessel entry point;
  `StdInstant`/`since`/`plus` (2.1, 2.2) are used unchanged in 2.3, 2.4, 3.1,
  4.2.
