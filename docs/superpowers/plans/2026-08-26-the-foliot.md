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
tick-quantized and would round non-tick-aligned callers. A world's local day
becomes an exact integer of kernel ticks, which removes the reason
`windows/vessel` kept a second tick lattice at all — so that lattice is
deleted rather than converted. `domains/climate` and `windows/scene` move to
kernel types.

**Stage 1 was revised in execution** after its own probe refuted the premise
it was specced on; see its heading. The header above states the corrected
design, not the original one.

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

## Stage 1 — one tick, defined once (follow-up 2)

**REVISED IN EXECUTION, 2026-08-26.** Tasks 1.2 and 1.3 as originally written
are **withdrawn**: Task 1.1's probe refuted the premise they rested on. Read
the spec's stage 1 note before starting anything here.

The short version: a vessel tick is NOT a kernel tick. A local day is an exact
integer of *vessel* ticks but `d * B` *kernel* ticks, which is not an integer,
so `days_of`'s conversion was correct and the planned identity `kernel_span`
would have introduced an error. Measured cost of the old bridge: 213 losses vs
211 gains over 5,764 samples, net -2 ticks — symmetric noise, no accumulation.

The fix Nathan chose removes the *reason* two lattices exist rather than
managing the conversion between them: quantize the day length at the draw, so
a local day IS an integer of kernel ticks.

### Task 1.1: Prove the lattices coincide — COMPLETE (`9504368b9`)

Landed, and it did its job by **failing** its premise. Two sampling errors are
recorded in its module doc because either alone gives a wrong answer: sampling
costs to only 10,000 finds zero witnesses (the defect needs `t` on the order of
a local day), and sampling to `MASS_BAND_KG`'s 100,000 kg overstates it (no
authored species exceeds 6,000 kg).

- [ ] **Step 1: Reframe its language from "loss" to "conversion noise"**

The file and its commit message call the effect a *loss*. It is not — it is
symmetric. Update the module doc, rename
`the_f64_bridge_loses_ticks_for_some_day_length` to
`the_f64_bridge_differs_by_a_tick_for_some_day_length`, and record the
213/211/net-minus-2 measurement beside the 438-witness count already there.

- [ ] **Step 2: Gate and commit**

Run `cargo fmt`, then `make gate-commit`, then commit
`windows/vessel/tests/suite/clock_lattice.rs` with a message saying the effect
is symmetric — 213 losses, 211 gains, net -2 ticks over 5,764 samples — so
"loses" overstated it in exactly the way the registry row did.

### Task 1.2: Quantize the day length at the draw

**Files:**
- Modify: `domains/astronomy/src/anchor.rs:14-25` (`Rotation`), `:75-95` (the draw and the pin path)
- Modify: `domains/astronomy/src/calendar.rs:608` (`day_length`)
- Test: `domains/astronomy/tests/suite/day_is_a_whole_tick_count.rs` (new)

**Interfaces:**
- Produces: `Rotation::Spinning { day: TickSpan, retrograde: bool }`.
  `Calendar::day_length() -> Option<StdDays>` is UNCHANGED in signature — it
  becomes an exact derived conversion, so its 31 call sites do not move.

**The scoping fact that governs this task: the draw does not change.**
`anchor.rs` takes `stream.next_f64()` twice, in that order, before and after.
Only the derived value is snapped to the lattice. So no seed label takes an
epoch suffix and the pin-isolation tests hold unmodified. **If you find
yourself editing `streams.rs`, stop — you have changed the draw, and that is a
different and much larger decision.**

- [ ] **Step 1: Write the failing test**

Register it in `domains/astronomy/tests/suite.rs` with an explicit
`#[path = "suite/day_is_a_whole_tick_count.rs"]` attribute — that file uses
`#[path]`, not bare `mod`.

```rust
//! A world's local day is an exact integer of kernel ticks (The Foliot).
//!
//! Before this, the day was a drawn f64 and `d * TICKS_PER_STD_DAY` had a
//! fractional part, which is why `windows/vessel` had to keep a second tick
//! lattice and convert between them. Quantizing at the draw removes the
//! reason two lattices existed.

use hornvale_astronomy::Rotation;
use hornvale_kernel::{Seed, WorldTime};
use hornvale_worldgen::{SkyChoice, build_world, sky_of};

/// Across many seeds, every spinning world's day divides the tick lattice
/// exactly. A single seed would not establish this — the drawn day length
/// varies per world and the property is about all of them.
#[test]
fn every_spinning_worlds_day_is_a_whole_number_of_ticks() {
    let mut spinning = 0;
    for seed in 1..=40_u64 {
        let world = build_world(
            Seed(seed),
            &Default::default(),
            SkyChoice::Generated,
            &Default::default(),
            &Default::default(),
        )
        .expect("world builds");
        let Ok(sky) = sky_of(&world) else { continue };
        let Some(system) = sky.system() else { continue };
        if let Rotation::Spinning { day, .. } = &system.anchor.rotation {
            spinning += 1;
            let ticks = day.ticks();
            assert!(ticks > 0, "seed {seed}: a spinning day is positive");
            assert_eq!(
                WorldTime::from_ticks(ticks).ticks(),
                ticks,
                "seed {seed}: the day is not lattice-aligned"
            );
        }
    }
    assert!(spinning > 0, "no spinning world in 40 seeds — the probe is vacuous");
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-astronomy --test suite -- day_is_a_whole_tick_count`
Expected: FAIL to compile — `day.ticks()` does not exist on a `StdDays`.

- [ ] **Step 3: Change the field**

`Rotation::Spinning { day: StdDays }` becomes `day: TickSpan`. At each of the
two construction sites in `anchor.rs`, quantize once:

```rust
// The draw is UNCHANGED — same stream, same two next_f64() calls, same
// order. Only the derived value is snapped to the tick lattice, which is
// what makes a local day an exact integer of kernel ticks and lets
// windows/vessel drop its second lattice (The Foliot, decision <NNNN>).
// Rounding rule, named at the call as decision 0186 requires: nearest tick.
let std_days = (16.0 + stream.next_f64() * 24.0) / 24.0;
Some(TickSpan::from_ticks(
    (std_days * WorldTime::TICKS_PER_STD_DAY as f64).round() as i64,
))
```

Apply the same quantization to the `PeriodHours(h)` pin path, keeping its
existing 4–100 hour range validation ahead of it.

- [ ] **Step 4: Keep `day_length()`'s signature**

```rust
    /// Length of one local day, if the world has one.
    ///
    /// Derived from the stored exact tick count (The Foliot): the day is an
    /// integer of kernel ticks, and this is its lossless continuous view for
    /// the orbital mathematics. A caller needing exactness should read the
    /// tick count rather than round-tripping through this.
    pub fn day_length(&self) -> Option<StdDays> {
        self.day
            .map(|d| StdDays(d.ticks() as f64 / WorldTime::TICKS_PER_STD_DAY as f64))
    }
```

- [ ] **Step 5: Compile and fix what the change names**

Run: `cargo check --workspace --all-targets 2>&1 | grep -E "^error" | head -40`

- [ ] **Step 6: Run the astronomy suite, reading the pin tests specifically**

Capture once and grep the file — do not re-run to ask a second question:

```bash
cargo test -p hornvale-astronomy > /tmp/hv-foliot-s1.log 2>&1; echo "exit=$?"
grep -E "^test result|FAILED|panicked" /tmp/hv-foliot-s1.log
```

**Branch:**
- **Pin-isolation tests pass, value assertions fail** → expected. The draw is
  intact and the derived value moved, which is the whole change.
- **A pin-isolation test fails** → STOP. You changed the draw. Revert and
  re-read Step 3.

- [ ] **Step 7: Commit** (goldens are accepted in Task 1.6, not here)

Message: `feat(astronomy)!: a local day is an exact integer of kernel ticks`,
recording that the draw is unchanged so no seed label takes an epoch suffix
and pin isolation holds, that only the derived day length moves (by up to
0.432 s), and that this removes the reason vessel kept a second lattice.

### Task 1.3: Vessel drops its second lattice

**Files:**
- Modify: `windows/vessel/src/clock.rs` (`Ticks`, `days_of`, `ticks_per_local_day`)
- Modify: `windows/vessel/src/session.rs:1943` (`day_length_std`), `:1970-1986` (`charge`)

**Interfaces:**
- Produces: `cost_of(action, mass_kg, terrain_factor) -> TickSpan`, replacing
  `cost_ticks`. `Ticks` and `days_of` are **deleted**, not renamed — with the
  day lattice-aligned there is one tick concept and no second name for it.

- [ ] **Step 1: Write the failing test**

Append to `windows/vessel/tests/suite/clock_lattice.rs` a test asserting that a
cost is an exact kernel span: no action is free, and charging it from genesis
lands exactly on the span with no residue. Build the `MoveTo` action the way
the existing tests in that file already do.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-vessel --test suite -- a_cost_is_an_exact_kernel_span`
Expected: FAIL — `cost_of` does not exist.

- [ ] **Step 3: Delete the lattice**

- `Ticks` deleted; `base_ticks`/`cost_ticks` return `TickSpan` and are renamed
  `base_cost`/`cost_of`.
- `days_of` deleted. Anything genuinely wanting continuous days converts
  through the kernel hatch at its own call site.
- `ticks_per_local_day` reads the world's stored day tick count instead of
  recomputing `round(d * B)` from an `f64`.
- `Session::charge` becomes integer addition on `WorldTime`. Check
  `advanced_by`'s other callers before deleting it:
  `grep -rn "advanced_by" windows/vessel/src/`.

- [ ] **Step 4: Rewrite the module header**

`clock.rs:6-15` documents the two-lattice situation as live. It is now
historical — say what changed and when, and keep the history rather than
deleting it.

- [ ] **Step 5: Run**

Capture once, then grep. Expected: byte goldens move; **no behavioural test
fails**. A behavioural failure means the conversion changed semantics, not just
precision — stop and report.

- [ ] **Step 6: Commit**

Message: `refactor(vessel)!: delete the second tick lattice` — `Ticks` and
`days_of` are gone rather than renamed, because with the day lattice-aligned
there is one tick concept and no second name for it.

### Task 1.4: Liveness accumulates in integers — THE REAL DEFECT

**Files:**
- Modify: `windows/vessel/src/liveness.rs:4644`, `:5233`, and the
  `day`/`entry_day`/`horizon` locals around them

This is the genuine accumulating drift, and it was found by checking a defect
that turned out not to exist. The catch-up replay does `day += days_of(...)` in
a loop over a bare `f64` while the live walk it reconstructs advances on the
integer lattice — so the two diverge, which the site's own comment says must
not happen.

- [ ] **Step 1: Write the failing test**

Name the property: a catch-up replay of N steps must land on the same instant
the live walk would. Find a discriminating N by search rather than guessing —
the divergence is per-step and small, so a short replay will not show it.

- [ ] **Step 2: Run to verify it fails**

If it does **not** fail, say so and stop. The drift may be masked by the
horizon comparison rather than observable at the endpoint, which is a different
and smaller finding than this task assumes.

- [ ] **Step 3: Convert the accumulators**

`day`, `entry_day` and `horizon` become `WorldTime`; the `+=` becomes
`TickSpan` addition. `WorldTime` derives `Ord`, so `while day < horizon` is
exact with no epsilon.

- [ ] **Step 4: Run, then commit**

Message: `fix(vessel): the catch-up replay accumulates in integer ticks` —
noting it added `f64` days in a loop while the live walk it reconstructs
advances on the integer lattice, which the site's own comment says would be a
failure by construction.

### Task 1.5: The epoch decision record

**Files:**
- Create: `docs/decisions/NNNN-a-local-day-is-a-whole-number-of-ticks.md`
- Modify: `docs/digest/decisions-in-force.md` (regenerated)

- [ ] **Step 1: Find the next number** — `ls docs/decisions/ | tail -5`
- [ ] **Step 2: Write it.** Cover: why two lattices existed; that the
      conversion between them was correct rather than buggy, and the
      measurement that showed it (213 losses / 211 gains / net -2 over 5,764
      samples); that the fix removes the reason rather than managing the
      conversion; that the **draw is unchanged**, so no seed label takes an
      epoch suffix and pin isolation holds; and that every world regenerates.
      Cite 0186 and 0188. Record that the shallow vessel-only alternative was
      offered and declined, and why.
- [ ] **Step 3: Regenerate the index**

```bash
cargo run --manifest-path tools/digest/Cargo.toml -- render decisions > docs/digest/decisions-in-force.md
```

- [ ] **Step 4: Commit**

### Task 1.6: Accept the goldens and artifacts, under review

**This task requires Nathan. Do not run it unattended.**

- [ ] **Step 1: Show what moved, before accepting anything**

```bash
command -v deno || export PATH="$HOME/.deno/bin:$PATH"
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

`deno` must be on `PATH` or `make rebaseline` silently skips the atlas bundle
(registry row `TOOL-rebaseline-skips-the-atlas-bundle-without-deno`).

- [ ] **Step 2: Characterize the movement**

The day length moved by at most 0.432 s, so almanac times should move by
seconds, not hours. **A large movement means something other than the
quantization is in the diff** — investigate rather than accept harder.

- [ ] **Step 3: Present to Nathan and wait for an explicit yes**

- [ ] **Step 4: Accept the byte goldens**

`REBASELINE=1` on the vessel suite, or `make rebaseline-goldens`. Never
`make rebaseline` for goldens — it does not write them and must not.

- [ ] **Step 5: Commit the artifacts and the goldens separately from the code**

**The census is NOT refreshed here.** It is one run on lefford at the pre-merge
close (authorized; `docs/timings.md` puts the last six at 895-918 s), so it
measures the finished world rather than an intermediate one.

### Stage 1 gate

- [ ] `git push -u origin campaign/the-foliot`
- [ ] `make sluice-stage BRANCH=campaign/the-foliot REF=$(git rev-parse HEAD)`
- [ ] Read it back with `make sluice-log`. Do not start stage 2 until green.


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
