# The Beholding Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Colour reaches the possession panes, computed in the sim through the
possessed agent's own species' eyes, with the false-colour translation
declared on the wire rather than invented at the renderer.

**Architecture:** The kernel learns that a channel has a *role* (chromatic or
achromatic) and that a screen image is a *named projection declaring what it
preserves*. `windows/worldgen` derives an `Observer` from a species'
`PerceptionVector`, beside the `pack_depths` that already derives its colour
lexicon from the same vector. `windows/scene` takes its illuminant from the
caller and emits a `sight` declaration block. `windows/vessel` selects the
observer, lights it with the sun's real altitude, and exposes an `eyes` verb.
The browser panes stop returning `string[]` and return a grid of cells.

**Tech Stack:** Rust 2024 (workspace: `serde`, `serde_json`, `libm` only);
Deno 2.9.2 exactly for `clients/vessel`; `cargo nextest`; `mdbook`.

**Spec:** `docs/superpowers/specs/2026-08-07-the-beholding-design.md` —
approved at G3. Read §4 before Task 1; it holds the formulas verbatim.

**Worktree:** `.claude/worktrees/the-beholding`, branch `the-beholding`,
already warmed. **Every path below is relative to that worktree**, and every
scratch file you create must be built from `pwd` — a report assembled from
the repo root lands in the main checkout and dies at teardown.

## Global Constraints

- **No new dependencies.** `serde`, `serde_json`, `libm` only. Enforced by
  `ALLOWED_EXTERNAL` in `cli/tests/architecture.rs`.
- **No `HashMap`/`HashSet`, no `std::time::*`** — anywhere, *including test
  code*. `clippy.toml` `disallowed-types` is workspace-wide and the gate runs
  `-D warnings`.
- **No `f64::powf`/`sin`/`exp`/etc.** — every transcendental routes through
  `hornvale_kernel::math`. `f64::sqrt`/`abs`/`floor`/`round` stay inherent.
  This is a `disallowed-methods` lint; it will fail the build, not warn.
- **Never `a.mul_add(b, c)`** in the colour path. Use `a * b + c`. Both are
  IEEE-exact but they round differently, and mixing forms is a silent
  cross-platform byte-identity hazard. `kernel/src/color.rs` says so in three
  places and has a test that fingerprints the unfused form.
- **`#![warn(missing_docs)]`** — every `pub` item, field and variant gets a
  one-line doc comment.
- **`type-audit:` tags** on every primitive at a `pub` boundary
  (`bare-ok(<class>)` / `waiver(<reason>)`). Tag `return` on tuple/Option
  returns too. The audit is default-deny and runs inside `make gate`.
- **`cargo fmt` as the final step before every commit.** Fmt-gate skips are
  this project's most common review finding.
- **No epoch.** This campaign adds no seeded draw and no `streams.rs`
  constant. If you find yourself writing one, stop — that is a spec violation,
  not an implementation detail.

## Verification discipline (applies to every task)

These are not style notes; each one is a defect this project has actually
shipped.

1. **A mutation test must prove it mutated.** Before substituting text to
   check a guard bites, assert the target text exists
   (`assert old in s, "TARGET NOT FOUND"`). A `cargo fmt` rewrap once made a
   single-line replacement match nothing, and the green looked exactly like a
   robust implementation.
2. **A mutation must fail where you predicted.** Record which assertion the
   mutation should kill and check the failure message names *that* assertion.
   "It went red" is not the observation; "it went red *there*" is.
3. **Assert the probe discriminates.** A colour assertion that cannot tell
   "withheld" from "rendered" passes green on a grey map. Every colour test
   asserts a *difference*, never merely non-`None`.
4. **A negative control needs a positive control** in the same test.
5. **Never pin one seed for evidence.** Sweep seeds and fail loudly if none
   qualifies.
6. **Run tests once, capture, inspect many.** `cargo nextest run -p <crate>
   2>&1 | tee /tmp/hv-beholding-<task>.txt`, then grep the file. Never re-run
   the suite to read a second line.

---

## File Structure

| file | responsibility | task |
|---|---|---|
| `kernel/src/color.rs` (modify) | `ChannelRole`, `Projection`, roles on `Observer`, `to_srgb` via projection, chromaticity | 1 |
| `kernel/src/lib.rs` (modify) | re-export the two new types | 1 |
| `windows/worldgen/src/observer.rs` (**create**) | `observer_for`, `ocular_reason`, the model card | 2 |
| `windows/worldgen/src/lib.rs` (modify) | `pub mod observer;` only | 2 |
| `windows/worldgen/tests/beholding_probe.rs` (modify) | promoted from spec-time probe to the H1/H2/H3 calibration test | 2 |
| `windows/scene/src/surrounds.rs` (modify) | caller-supplied illuminant; the `Sight` block | 3 |
| `windows/scene/src/surrounds_ascii.rs` (modify) | caption reads `sight` instead of asserting the observer | 3 |
| `cli/src/main.rs` (modify) | pass the illuminant it already has a star for | 3 |
| `windows/vessel/src/eyes.rs` (**create**) | `Eyes`, resolution to an `Observer`, the light | 4 |
| `windows/vessel/src/purview.rs` (modify) | colour the chart | 4 |
| `windows/vessel/src/session.rs` (modify) | hold `Eyes`; the `eyes` verb; `map`'s lens | 5 |
| `windows/vessel/src/plan.rs` (modify) | `PaletteEntry.color` slot | 6 |
| `clients/vessel/src/pane_cell.ts` (**create**) | the shared `PaneCell` type and the run-length splitter | 7 |
| `clients/vessel/src/pane_chart.ts`, `pane_plan.ts` (modify) | return cells, not strings | 7 |
| `clients/vessel/src/main.ts` (modify) | build spans; render the caption | 8 |
| `book/src/reference/scene-surrounds-v2.md` (modify) | **hand-authored** — document `sight` | 9 |

`windows/worldgen/src/lib.rs` is ~6k lines and **merge-hot across parallel
sessions**; `observer_for` goes in a new `observer.rs` submodule rather than
into it, which is already the idiomatic shape there (`components`, `schedule`,
`settlement_pins`).

---

### Task 1: Kernel — channel roles, the projection, chromaticity

**Files:**
- Modify: `kernel/src/color.rs`
- Modify: `kernel/src/lib.rs:33-37` (the `pub use color::{…}` block)
- Test: `kernel/src/color.rs`'s own `mod tests`

**Interfaces:**
- Consumes: nothing.
- Produces:
  - `pub enum ChannelRole { Chromatic, Achromatic }` (`Debug, Clone, Copy, PartialEq, Eq`)
  - `pub struct Projection` with `pub fn new(name: &'static str, preserves: &'static str, rgb: [usize; 3], norms: [f64; 3]) -> Result<Self, UnitError>` and accessors `name() -> &'static str`, `preserves() -> &'static str`
  - `pub fn Observer::with_roles(channels: Vec<Spectrum>, roles: Vec<ChannelRole>, projection: Option<Projection>) -> Result<Observer, UnitError>`
  - `pub fn Observer::roles(&self) -> &[ChannelRole]`
  - `pub fn Observer::projection(&self) -> Option<&Projection>`
  - `pub fn Observer::chromatic_channels(&self) -> usize`
  - `pub fn Observer::chromaticity(&self, signal: &Signal) -> Vec<f64>`
  - `pub fn Observer::chromatic_distance(&self, a: &Signal, b: &Signal) -> f64`
  - `standard_observer()` unchanged in signature, now carrying roles and a `native` projection.

**Read first:** `kernel/CLAUDE.md`, and `kernel/src/color.rs` in full. Its
module doc, its three "never `mul_add`" comments, and the doc comment on
`a_four_channel_synthetic_observer_still_has_no_srgb_image` are load-bearing
and must survive this task.

- [ ] **Step 1: Write the failing tests**

Add to `kernel/src/color.rs`'s `mod tests`:

```rust
#[test]
fn a_projection_may_not_name_an_achromatic_channel() {
    // The whole point of roles: a rod carries no hue, so no projection may
    // read one. Without this, `observer_for` could silently build an eye
    // that shows brightness as blue.
    let curves = vec![
        Spectrum::new([0.5; BANDS]).unwrap(),
        Spectrum::new([0.5; BANDS]).unwrap(),
        Spectrum::new([0.5; BANDS]).unwrap(),
    ];
    let roles = vec![
        ChannelRole::Chromatic,
        ChannelRole::Chromatic,
        ChannelRole::Achromatic,
    ];
    // Index 2 is the achromatic channel.
    let p = Projection::new("bad", "nothing", [2, 1, 0], [1.0, 1.0, 1.0]).unwrap();
    let err = Observer::with_roles(curves, roles, Some(p)).unwrap_err();
    assert_eq!(err.unit, "observer");
}

#[test]
fn a_projection_may_not_index_past_the_channel_set() {
    let curves = vec![Spectrum::new([0.5; BANDS]).unwrap()];
    let roles = vec![ChannelRole::Chromatic];
    let p = Projection::new("bad", "nothing", [0, 0, 7], [1.0, 1.0, 1.0]).unwrap();
    assert!(Observer::with_roles(curves, roles, Some(p)).is_err());
}

#[test]
fn an_observer_needs_a_role_per_channel_and_one_chromatic_channel() {
    let one = || Spectrum::new([0.5; BANDS]).unwrap();
    // Mismatched lengths.
    assert!(Observer::with_roles(vec![one(), one()], vec![ChannelRole::Chromatic], None).is_err());
    // An eye that carries no hue at all is not an eye this model can use.
    assert!(Observer::with_roles(vec![one()], vec![ChannelRole::Achromatic], None).is_err());
}

#[test]
fn chromaticity_ignores_the_achromatic_channel() {
    // THE POINT OF THE WHOLE TASK, and the spec's M3. Two observers whose
    // chromatic channels are identical and whose achromatic channel differs
    // wildly must report the SAME chromaticity. Before roles existed, the
    // rod carried hue information and every dichromat measured as a
    // trichromat.
    let short = Spectrum::new([1.0, 1.0, 1.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]).unwrap();
    let long = Spectrum::new([0.0, 0.0, 0.0, 0.0, 0.5, 1.0, 1.0, 1.0, 0.0, 0.0]).unwrap();
    let quiet_rod = Spectrum::new([0.0; BANDS]).unwrap();
    let loud_rod = Spectrum::new([1.0; BANDS]).unwrap();
    let roles = vec![
        ChannelRole::Chromatic,
        ChannelRole::Chromatic,
        ChannelRole::Achromatic,
    ];
    let a = Observer::with_roles(vec![short, long, quiet_rod], roles.clone(), None).unwrap();
    let b = Observer::with_roles(vec![short, long, loud_rod], roles, None).unwrap();
    let r = Reflectance::new([0.4; BANDS]).unwrap();
    let light = Illuminant::new([1.0; BANDS]).unwrap();
    let ca = a.chromaticity(&a.sense(&r, &light));
    let cb = b.chromaticity(&b.sense(&r, &light));
    assert_eq!(ca.len(), 2, "chromaticity has one entry per CHROMATIC channel");
    assert_eq!(ca, cb, "a louder rod must not move the chromaticity");
}

#[test]
fn the_standard_observers_bytes_have_not_moved() {
    // The byte-identity pin for this refactor. `Projection` CARRIES its
    // normalizers rather than deriving them, because the shipped constants
    // are the ROUNDED channel sums — deriving live would move every colour
    // the standard observer has ever emitted. If this test fails, someone
    // "simplified" `norms` into a computed sum.
    let obs = standard_observer();
    let light = Illuminant::new([1.0; BANDS]).unwrap();
    let mid = obs.sense(&Reflectance::new([0.5; BANDS]).unwrap(), &light);
    assert_eq!(obs.to_srgb(&mid).unwrap(), [188, 188, 188]);
    let p = obs.projection().expect("the standard observer projects");
    assert_eq!(p.name(), "native");
}
```

**The `[188, 188, 188]` literal is a placeholder you must replace with the
measured value in Step 3** — see that step. Do not guess it, and do not
"fix" the test by changing the implementation to match a guess.

- [ ] **Step 2: Run to verify they fail**

```bash
cargo test -p hornvale-kernel --lib color 2>&1 | tee /tmp/hv-beholding-t1.txt
```

Expected: FAIL to compile — `ChannelRole`, `Projection`, `with_roles`,
`chromaticity`, `projection()` do not exist.

**Note what this red does and does not prove.** A compile failure proves the
API is absent, not that the assertions would catch anything. For
`the_standard_observers_bytes_have_not_moved`, capture the *behavioural*
baseline before touching code:

```bash
cargo run -q -p hornvale --example - 2>/dev/null || true
```

— there is no such example; instead add a scratch test on **unmodified**
`color.rs` that prints `standard_observer().to_srgb(&…)` for a `[0.5; BANDS]`
reflectance under flat light, run it, record the triple, delete the scratch
test, and use that triple as the literal. That number is the pre-refactor
truth; a literal derived after the refactor would pin the bug.

- [ ] **Step 3: Implement**

In `kernel/src/color.rs`:

1. Add `ChannelRole` and `Projection` with the doc comments from spec §4.1
   and §4.2 (including the sentence explaining why `norms` is carried).
   `Projection::new` validates only that `norms` are finite and non-zero —
   **it cannot validate `rgb` against roles, because roles live on the
   observer**; that check belongs in `Observer::with_roles`.
2. Replace `Observer`'s `srgb_native: bool` with
   `roles: Vec<ChannelRole>` and `projection: Option<Projection>`.
3. `Observer::new(channels)` keeps its exact current contract: every channel
   `Chromatic`, `projection: None`. Its doc comment already promises
   `to_srgb` returns `None`; that promise now holds because there is no
   projection, and the doc must be updated to say so.
4. `Observer::with_roles` validates: `channels.len() == roles.len()`,
   non-empty, at least one `Chromatic`, and — when a projection is present —
   every `rgb` index is in range *and* names a `Chromatic` channel.
5. `to_srgb` reads `self.projection`; for each output slot `i` it takes
   `signal[p.rgb[i]] / p.norms[i]`, clamps to `[0,1]`, and calls the
   existing `encode_srgb_byte`. Delete the `signal.get().len() != 4` arity
   check — arity is now covered by the in-range validation at construction.
   Keep a guard for `signal.get().len() != self.channels.len()` (a signal
   from a different observer).
6. `chromaticity` sums only the chromatic channels and divides each chromatic
   channel by that total; a zero total returns an all-zero vector rather than
   NaN. `chromatic_distance` is the squared Euclidean distance between two
   chromaticities, `f64::INFINITY` on a length mismatch — the same posture
   `Signal::distance_to` already takes.
7. `standard_observer()` declares
   `roles = [Chromatic, Chromatic, Chromatic, Achromatic]` and
   `Projection::new("native", "the observer's own channels are the screen's; \
   this is not a translation", [2, 1, 0], [LONG_NORM, MEDIUM_NORM, SHORT_NORM])`.
   **Mind the order:** `rgb` indexes *channels* (long=2 → red), and `norms`
   is indexed by *output slot* (slot 0 = red = `LONG_NORM`). Getting these
   two orderings crossed is the most likely bug in this task and
   `the_standard_observers_bytes_have_not_moved` is what catches it.
8. Update `standard_observer_channels_sum_to_the_declared_norms` to read the
   norms off the projection rather than the file-level constants, keeping its
   two-decimal comparison and its "a curve edit that does not update them
   would make a white surface stop projecting to white" reasoning.
9. Update `a_four_channel_synthetic_observer_still_has_no_srgb_image`'s doc
   comment: it no longer isolates "the `srgb_native` half" but "the
   *no-projection* half", and its stated mutation becomes *give
   `Observer::new` a native projection*.
10. Add the two new types to `kernel/src/lib.rs`'s `pub use color::{…}` list,
    keeping it alphabetical.

- [ ] **Step 4: Run to verify they pass**

```bash
cargo test -p hornvale-kernel --lib color 2>&1 | tee /tmp/hv-beholding-t1.txt
```

Expected: PASS, all of them, including every pre-existing colour test.

- [ ] **Step 5: Prove the new guards bite**

Run each mutation, confirm it reddens **the named test at the named
assertion**, then revert:

| mutation | must redden | at |
|---|---|---|
| `Observer::new` attaches the native projection | `a_four_channel_synthetic_observer_still_has_no_srgb_image` | its final `is_none()` assert |
| `with_roles` drops the chromatic-index check | `a_projection_may_not_name_an_achromatic_channel` | the `unwrap_err()` |
| `chromaticity` divides by the total over **all** channels | `chromaticity_ignores_the_achromatic_channel` | the `assert_eq!(ca, cb)` |
| `Projection::new` for `native` uses `[0, 1, 2]` | `the_standard_observers_bytes_have_not_moved` | the triple assert |

Record the observed failing assertion for each in the commit message. A
mutation that reddens at an *earlier* assertion proved nothing.

- [ ] **Step 6: Gate and commit**

```bash
cargo fmt
cargo clippy -p hornvale-kernel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo test -p hornvale-kernel 2>&1 | tee /tmp/hv-beholding-t1.txt
git add kernel/src/color.rs kernel/src/lib.rs
git commit -m "feat(kernel): channel roles and named projections

An achromatic channel carries no hue, so no projection reads one and no
chromaticity metric counts one. Without this every observer with a rod
measures as a trichromat, which is why the spec's candidate dichromat did
not confuse red and green.

Projection carries its normalizers rather than deriving them: the shipped
constants are ROUNDED channel sums, so a derived version would move every
colour the standard observer has ever emitted."
```

---

### Task 2: worldgen — the observer a species implies

**Files:**
- Create: `windows/worldgen/src/observer.rs`
- Modify: `windows/worldgen/src/lib.rs` (add `pub mod observer;` — **that one
  line only**; the file is merge-hot across parallel sessions)
- Modify: `windows/worldgen/tests/beholding_probe.rs` (promote the spec-time
  probe into the calibration test)

**Interfaces:**
- Consumes: Task 1's `ChannelRole`, `Projection`, `Observer::with_roles`,
  `Observer::chromatic_distance`.
- Produces:
  - `pub fn observer_for(p: &hornvale_species::PerceptionVector) -> hornvale_kernel::color::Observer`
  - `pub fn ocular_reason(p: &hornvale_species::PerceptionVector) -> String`
  - `pub fn observer_named(name: &str) -> Option<hornvale_kernel::color::Observer>`
    — resolves `"standard"` plus every `KindId` in
    `hornvale_species::perception_registry()`. Task 5's `eyes <name>` verb
    calls this.
  - `pub fn observer_roster() -> Vec<String>` — the names `observer_named`
    accepts, ascending, for the verb's error message.

**Read first:** spec §4.4 — the formulas are stated there verbatim so they
are transcribed, not invented. `pack_depths` at `windows/worldgen/src/lib.rs`
is the model this one extends; read its doc comment and `perceptual_reason`
beside it.

- [ ] **Step 1: Write the failing tests**

In `windows/worldgen/tests/beholding_probe.rs`, **replace** the two
`probe_*` functions (the file's `candidate_observer` helper is superseded by
the real `observer_for`; delete it and the copied curves with it):

```rust
use hornvale_kernel::color::{BANDS, Illuminant, Reflectance};
use hornvale_language::exemplars::{HUE_CONCEPTS, hue_exemplar};
use hornvale_species::perception_registry;
use hornvale_worldgen::observer::{observer_for, observer_named, observer_roster};

fn flat_light() -> Illuminant {
    Illuminant::new([1.0; BANDS]).unwrap()
}

/// Every hue exemplar's screen triple under one observer.
fn swatches(p: &hornvale_species::PerceptionVector) -> Vec<[u8; 3]> {
    let obs = observer_for(p);
    let light = flat_light();
    HUE_CONCEPTS
        .iter()
        .map(|c| {
            let r = hue_exemplar(c).expect("every hue concept has an exemplar");
            obs.to_srgb(&obs.sense(&r, &light))
                .expect("a derived observer always declares a projection")
        })
        .collect()
}

/// H2 — the human row is not privileged.
#[test]
fn the_human_row_derives_exactly_the_standard_observer() {
    let reg = perception_registry();
    let human = reg
        .get(&hornvale_species::KindId("human"))
        .expect("human is a speaking kind and must carry a perception row");
    let derived = observer_for(human);
    let standard = hornvale_kernel::color::standard_observer();
    let light = flat_light();
    for step in 0..=10 {
        let v = step as f64 / 10.0;
        let r = Reflectance::new([v; BANDS]).unwrap();
        assert_eq!(
            derived.to_srgb(&derived.sense(&r, &light)),
            standard.to_srgb(&standard.sense(&r, &light)),
            "the standard observer is a DERIVED row, not a privileged base \
             case; reflectance {v} disagreed"
        );
    }
    assert_eq!(derived.channels(), standard.channels());
    assert_eq!(derived.roles(), standard.roles());
}

/// H1 — the model resolves the axis it reads.
#[test]
fn species_with_distinct_night_vision_see_distinctly() {
    let reg = perception_registry();
    let mut rows: Vec<(String, f64, Vec<[u8; 3]>)> = reg
        .iter()
        .map(|(k, p)| (k.0.to_string(), p.night_vision, swatches(p)))
        .collect();
    rows.sort_by(|a, b| a.0.cmp(&b.0));

    let mut compared = 0usize;
    for (i, a) in rows.iter().enumerate() {
        for b in rows.iter().skip(i + 1) {
            if a.1 == b.1 {
                // The honest converse, asserted rather than left implicit.
                assert_eq!(
                    a.2, b.2,
                    "{} and {} share night_vision {}, and the eye model reads \
                     only that axis, so they must see identically",
                    a.0, b.0, a.1
                );
                continue;
            }
            compared += 1;
            assert_ne!(
                a.2, b.2,
                "{} (nv {}) and {} (nv {}) must not see identically — this is \
                 the collapse the spec's M1 measured",
                a.0, a.1, b.0, b.1
            );
        }
    }
    // The probe must discriminate: a roster where every night_vision is equal
    // would pass the loop above vacuously.
    assert!(
        compared >= 6,
        "only {compared} distinct-night_vision pairs were compared; this test \
         is not exercising what it claims"
    );
}

/// H3 — dichromacy is real once roles are declared. THE SPEC EXPECTS THIS
/// MAY FAIL; see the panic message.
#[test]
fn a_dichromat_separates_red_from_green_less_than_a_trichromat_does() {
    let reg = perception_registry();
    let human = reg.get(&hornvale_species::KindId("human")).unwrap();
    let bugbear = reg.get(&hornvale_species::KindId("bugbear")).unwrap();
    let light = flat_light();
    let red = hue_exemplar("red").unwrap();
    let green = hue_exemplar("green").unwrap();

    let sep = |p: &hornvale_species::PerceptionVector| {
        let o = observer_for(p);
        o.chromatic_distance(&o.sense(&red, &light), &o.sense(&green, &light))
    };
    let (h, b) = (sep(human), sep(bugbear));
    // Anti-vacuity: a metric that returns 0 for everyone would "pass" a
    // naive `b < h`.
    assert!(h > 0.0, "a trichromat must separate red from green at all");
    assert!(
        b < h,
        "H3 FALSIFIED — bugbear separates red/green by {b}, human by {h}. \
         Ship the null: the model produces species that see differently but \
         not species that are colour-blind. Do NOT retune the merge to \
         rescue this."
    );
}

#[test]
fn the_roster_names_resolve_and_an_unknown_one_does_not() {
    let roster = observer_roster();
    assert!(roster.contains(&"standard".to_string()));
    assert!(roster.contains(&"bugbear".to_string()));
    for name in &roster {
        assert!(observer_named(name).is_some(), "{name} is advertised but does not resolve");
    }
    assert!(observer_named("wyvern").is_none());
}
```

- [ ] **Step 2: Run to verify they fail**

```bash
cargo test -p hornvale-worldgen --test beholding_probe 2>&1 | tee /tmp/hv-beholding-t2.txt
```

Expected: FAIL to compile — `hornvale_worldgen::observer` does not exist.

- [ ] **Step 3: Implement `windows/worldgen/src/observer.rs`**

Transcribe spec §4.4 exactly. The four standard curves are copied from
`standard_observer()`; the merge formulas are:

```rust
// hue 5  -> channels [S, M, L, R]                     (identical to standard)
// hue 4  -> M' = (M + (M+L)/2) / 2
//           L' = (L + (M+L)/2) / 2
//           channels [S, M', L', R]
// hue<=3 -> t = clamp((night_vision - 0.5) / 0.5, 0, 1)
//           C = (1 - t)*L + t*(M + L)/2
//           channels [S, C, R]
```

Roles are `Chromatic` for every channel but the last, which is `Achromatic`.
Projections:

- hue 5 and 4: `rgb = [2, 1, 0]`, name `"native"` for hue 5 (so H2 holds
  exactly) and `"native-anomalous"` for hue 4, preserving *"three chromatic
  channels; the red–green axis is narrowed, not removed"*.
- hue ≤ 3: `rgb = [1, 1, 0]`, name `"yellow-blue"`, preserving *"the
  short-to-long opposition; the red–green axis is not carried"*.

**Norms are computed per observer at construction** by sensing a
`Reflectance::new([1.0; BANDS])` under `Illuminant::new([1.0; BANDS])` and
reading the relevant channels — *except* for hue 5, which must use the
kernel's `native` projection unchanged so H2 holds byte-for-byte. Expose the
standard projection from the kernel (Task 1 already returns it via
`standard_observer().projection()`), clone it for the hue-5 arm, and do not
recompute it.

The scotopic channel's **gain** multiplies its curve by
`1.0 + 0.25 * (pack_depths(p).luminance as f64 - 1.0)`. It is achromatic, so
this can never move a colour; it exists so a later naming campaign has the
axis. State that in the doc comment.

`ocular_reason` returns a sentence in `perceptual_reason`'s register, e.g.
`"night-vision 0.7 gives hue depth 3: the medium and long channels are merged
0.4 of the way, so red and green fall on one axis"`.

`observer_named` matches `"standard"` → `standard_observer()`, else looks up
`perception_registry()` by `KindId` and calls `observer_for`.

- [ ] **Step 4: Run to verify they pass**

```bash
cargo test -p hornvale-worldgen --test beholding_probe 2>&1 | tee /tmp/hv-beholding-t2.txt
```

Expected: four PASS. **`a_dichromat_separates_red_from_green_less_than_a_trichromat_does`
may FAIL — that is a legitimate outcome.** If it does: do **not** retune the
merge. Stop, record the two numbers, and report it as the campaign's null
result for the chronicle. Mark the test `#[ignore = "H3 falsified: see the
chronicle; the model differentiates species but does not produce dichromacy"]`
and carry the finding forward.

- [ ] **Step 5: Prove the guards bite**

| mutation | must redden | at |
|---|---|---|
| hue-5 arm returns the hue-4 curves | `the_human_row_derives_exactly_the_standard_observer` | the per-reflectance `assert_eq!` |
| `t` is replaced by the constant `0.5` | `species_with_distinct_night_vision_see_distinctly` | the `assert_ne!` for hobgoblin/bugbear |
| the `yellow-blue` projection uses `[2, 1, 0]` | `a_dichromat_separates_red_from_green_…` (if live) | the `b < h` assert |

- [ ] **Step 6: Gate and commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/worldgen/src/observer.rs windows/worldgen/src/lib.rs windows/worldgen/tests/beholding_probe.rs
git commit -m "feat(worldgen): the observer a species' perception implies

Channel count reads off pack_depths' own hue ladder so the eye and the
lexicon cannot disagree: a species with no word for green lacks the channel
that would distinguish it. The DEGREE of merging is continuous in
night_vision, because a tiered eye derived from a tiered gate gave
hobgoblin, bugbear and kobold the identical swatch set.

observer_for(human) reproduces standard_observer() exactly — the standard
observer is a derived row, not a privileged base case."
```

---

### Task 3: scene — a caller's illuminant, and the `sight` declaration

**Files:**
- Modify: `windows/scene/src/surrounds.rs` (the `SurroundsScene` struct;
  `surrounds_scene_colored_in`)
- Modify: `windows/scene/src/surrounds_ascii.rs` (the colour disclosure block)
- Modify: `cli/src/main.rs` (the `lens == "colour"` arm)
- Test: `windows/scene/tests/golden.rs`, plus `surrounds.rs`'s `mod tests`

**Interfaces:**
- Consumes: Task 1's `Projection`; Task 2 is **not** a dependency — scene
  takes an `&Observer` and stays ignorant of where it came from.
- Produces:
  - `pub struct Sight { pub observer: String, pub channels: u32, pub chromatic: u32, pub projection: String, pub preserves: String, pub sun_altitude_deg: f64 }` (`Debug, Clone, PartialEq, Serialize`)
  - `pub sight: Option<Sight>` on `SurroundsScene`, **appended after
    `legend`**, `#[serde(skip_serializing_if = "Option::is_none")]`
  - `pub fn surrounds_scene_colored_in(world, ctx, room, radius, at, observer: &Observer, light: &Illuminant, sight: Sight) -> Result<SurroundsScene, SceneError>`

`sight` is passed in rather than derived because the *observer's name* (a
species) is not knowable from an `Observer`, and `sun_altitude_deg` is the
caller's. The builder fills `channels`, `chromatic`, `projection` and
`preserves` from the observer itself and **overwrites** whatever the caller
put there — so those four cannot lie about the eye that was actually used.
Document that overwrite; it is the whole reason the block is trustworthy.

- [ ] **Step 1: Write the failing tests**

In `windows/scene/src/surrounds.rs`'s `mod tests`:

```rust
#[test]
fn an_uncoloured_document_is_byte_identical_to_one_built_before_sight_existed() {
    // `sight` and `color` are both skipped when absent, so the uncoloured
    // path must emit not one extra byte. This is what protects the three
    // committed gallery charts and the gallery scene JSON.
    let (w, ctx, room) = fixture_world();
    let s = surrounds_scene_in(&w, &ctx, &room, 2, WorldTime { day: 0.0 }).unwrap();
    let json = crate::surrounds_json(&s);
    assert!(!json.contains("\"sight\""), "uncoloured documents carry no sight block");
    assert!(!json.contains("\"color\""), "uncoloured documents carry no colour");
}

#[test]
fn the_sight_block_reports_the_observer_actually_used_not_the_one_claimed() {
    // A caller that lies about the projection must be corrected by the
    // builder, or the caption is unenforceable and RENDER-9's honesty is
    // decorative.
    let (w, ctx, room) = fixture_world();
    let obs = hornvale_kernel::color::standard_observer();
    let light = hornvale_astronomy::illuminant::daylight(
        &hornvale_astronomy::star::generate_star(
            w.seed.derive(hornvale_astronomy::streams::ROOT),
        ),
    );
    let claimed = Sight {
        observer: "bugbear".to_string(),
        channels: 99,
        chromatic: 99,
        projection: "a lie".to_string(),
        preserves: "everything".to_string(),
        sun_altitude_deg: 12.5,
    };
    let s = surrounds_scene_colored_in(
        &w, &ctx, &room, 2, WorldTime { day: 0.0 }, &obs, &light, claimed,
    )
    .unwrap();
    let sight = s.sight.expect("a coloured document carries its declaration");
    assert_eq!(sight.projection, "native", "the builder overwrites the claim");
    assert_eq!(sight.channels, 4);
    assert_eq!(sight.chromatic, 3);
    // The two fields the builder CANNOT know are the caller's and survive.
    assert_eq!(sight.observer, "bugbear");
    assert_eq!(sight.sun_altitude_deg, 12.5);
}

#[test]
fn a_dimmer_light_yields_dimmer_colour() {
    // The caller-supplied illuminant must actually reach the pixels — the
    // positive control for Task 4's H4.
    let (w, ctx, room) = fixture_world();
    let obs = hornvale_kernel::color::standard_observer();
    let bright = hornvale_kernel::color::Illuminant::new([1.0; hornvale_kernel::color::BANDS]).unwrap();
    let dim = hornvale_kernel::color::Illuminant::new([0.2; hornvale_kernel::color::BANDS]).unwrap();
    let mk = |l| {
        surrounds_scene_colored_in(
            &w, &ctx, &room, 2, WorldTime { day: 0.0 }, &obs, l, sight_of("standard", 0.0),
        )
        .unwrap()
    };
    let (a, b) = (mk(&bright), mk(&dim));
    let lit: Vec<_> = a.cells.iter().filter_map(|c| c.color).collect();
    assert!(!lit.is_empty(), "the probe must find coloured cells at all");
    let mut moved = 0;
    for (x, y) in a.cells.iter().zip(&b.cells) {
        if let (Some(p), Some(q)) = (x.color, y.color) {
            if q[0] < p[0] { moved += 1; }
        }
    }
    assert!(moved > 0, "dimming the illuminant must darken at least one cell");
}
```

Add a `fixture_world()` helper and a `sight_of(observer, alt)` helper beside
the module's existing test scaffolding; reuse whatever world-building helper
`surrounds.rs`'s tests already have rather than adding a second one.

- [ ] **Step 2: Run to verify they fail**

```bash
cargo test -p hornvale-scene 2>&1 | tee /tmp/hv-beholding-t3.txt
```

Expected: FAIL to compile — `Sight` and the new parameters do not exist.

- [ ] **Step 3: Implement**

1. Add `Sight` with `sun_altitude_deg` serialized through
   `hornvale_kernel::quantize::quantize_serde::f64_field`.
2. Append `pub sight: Option<Sight>` to `SurroundsScene` **after `legend`**
   with `skip_serializing_if`. Update the struct's `type-audit:` tag line.
   The struct doc says field order is contract — add a sentence noting that
   `sight` is an *append*, not a reorder, and that it is skipped when absent.
3. `surrounds_scene_in` sets `sight: None`.
4. `surrounds_scene_colored_in` gains `light: &Illuminant` and `sight: Sight`,
   **deletes** its internal `generate_star` + `daylight` call, and overwrites
   `channels`/`chromatic`/`projection`/`preserves` from the observer before
   storing. Update its doc comment: the paragraph explaining that the star's
   daylight is used and that the sun angle is a v3 question is now **wrong**
   and must be replaced by the new contract.
5. `surrounds_ascii.rs`: the `if lens == "colour"` disclosure gains a second
   line, emitted only when `scene.sight` is `Some`, naming the observer, the
   arity, and what the projection preserves. It must read `scene.sight`, not
   assume the standard observer.
6. `cli/src/main.rs`: the `lens == "colour"` arm builds
   `daylight(&generate_star(...))` (moved out of scene, so the CLI keeps its
   current behaviour exactly) and passes
   `Sight { observer: "standard", …, sun_altitude_deg: 0.0 }`. **The CLI's
   emitted bytes for `--render ascii` must not move except for the one new
   disclosure line** — the three committed gallery charts go through the
   `terrain` lens and are untouched either way.

- [ ] **Step 4: Run to verify they pass**

```bash
cargo test -p hornvale-scene 2>&1 | tee /tmp/hv-beholding-t3.txt
cargo test -p hornvale --test docs_consistency
```

Expected: PASS, **including `windows/scene/tests/golden.rs`'s byte-identity
tests, unchanged**. If a golden moved, the `skip_serializing_if` is wrong —
fix the code, never rebaseline that fixture.

- [ ] **Step 5: Prove the guards bite**

| mutation | must redden | at |
|---|---|---|
| drop `skip_serializing_if` from `sight` | `an_uncoloured_document_is_byte_identical_…` **and** `golden.rs` | the `!json.contains` assert |
| the builder trusts the caller's `projection` | `the_sight_block_reports_the_observer_actually_used_…` | the `"native"` assert |
| `surrounds_scene_colored_in` ignores `light` and rebuilds daylight | `a_dimmer_light_yields_dimmer_colour` | the `moved > 0` assert |

- [ ] **Step 6: Gate and commit**

```bash
cargo fmt && cargo clippy -p hornvale-scene -p hornvale --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/scene/src cli/src/main.rs
git commit -m "feat(scene): the sight declaration, and the caller's light

A client cannot caption what it cannot see, and RENDER-9 makes the caption
the load-bearing honesty. The builder overwrites the caller's claims about
arity and projection from the observer actually used, so the declaration
cannot lie about the eye.

surrounds_scene_colored_in stops computing its own daylight and takes an
Illuminant, which is what its own doc invited."
```

---

### Task 4: vessel — Eyes, the observer, and the sun's real altitude

**Files:**
- Create: `windows/vessel/src/eyes.rs`
- Modify: `windows/vessel/src/lib.rs` (`pub mod eyes;`, `PossessOpts.eyes`)
- Modify: `windows/vessel/src/purview.rs` (`purview_scene` colours)
- Modify: `windows/vessel/src/session.rs` (hold `Eyes`; thread it to `purview`)
- Test: `windows/vessel/src/purview.rs`'s `mod tests`, `windows/vessel/tests/session_snapshot.rs`

**Interfaces:**
- Consumes: Task 2's `observer_for` / `observer_named`; Task 3's
  `surrounds_scene_colored_in` and `Sight`.
- Produces:
  - `pub enum Eyes { Own, Named(String), Off }` (`Debug, Clone, PartialEq`), `Default = Own`
  - `pub fn resolve(eyes: &Eyes, agent: &Agent) -> Option<(Observer, String)>` — the observer and the name for `Sight.observer`; `None` for `Off`
  - `pub fn daylight_at(world: &World, calendar: Option<&Calendar>, day: WorldTime, latitude: f64) -> (Illuminant, f64)` — the illuminant and the altitude that produced it
  - `PossessOpts.eyes: Eyes`

`daylight_at` returns the altitude alongside the light so `Sight` records the
number that was actually used, rather than a second caller re-deriving it —
two independent copies of one calculation is exactly how a caption and a
picture end up disagreeing.

**Adding a field to `PossessOpts` breaks every construction site. Do not
guess how many there are — let the compiler enumerate them:**

```bash
cargo check --workspace --all-targets 2>&1 | grep -c "missing field \`eyes\`"
```

Fix every one; never silence an exhaustiveness error with `..Default::default()`
at a site that was explicitly listing its fields, because that hides the next
field's arrival too.

- [ ] **Step 1: Write the failing tests**

In `windows/vessel/src/purview.rs`'s `mod tests`:

```rust
/// The positive control. "Is it coloured" cannot tell a withheld colour from
/// a rendered one — both look grey — so this asserts a DIFFERENCE.
#[test]
fn two_eyes_on_one_world_disagree_about_colour() {
    let w = world();
    let mut own = PossessOpts::default();
    own.eyes = crate::eyes::Eyes::Own;
    let mut human = PossessOpts::default();
    human.eyes = crate::eyes::Eyes::Named("standard".to_string());

    let (a, _) = Session::start(&w, &own).unwrap();
    let (b, _) = Session::start(&w, &human).unwrap();
    let (sa, sb) = (a.purview(0).unwrap(), b.purview(0).unwrap());

    let coloured = sa.cells.iter().filter(|c| c.color.is_some()).count();
    assert!(coloured > 0, "the possessed agent's eyes must colour the chart at all");

    let differ = sa
        .cells
        .iter()
        .zip(&sb.cells)
        .filter(|(x, y)| x.color != y.color)
        .count();
    // If the flagship species IS human, the two are legitimately identical.
    let species = a.agent().species.clone();
    if species == "human" {
        assert_eq!(differ, 0, "a human possession and the standard observer are one eye");
    } else {
        assert!(
            differ > 0,
            "possessing a {species} must not produce the human's colours on \
             any cell; got {differ} differing of {coloured} coloured"
        );
    }
}

#[test]
fn eyes_off_restores_a_byte_identical_uncoloured_chart() {
    // The negative control — WITH its positive control, because a
    // suppress-everything path passes green against nothing.
    let w = world();
    let mut off = PossessOpts::default();
    off.eyes = crate::eyes::Eyes::Off;
    let (a, _) = Session::start(&w, &off).unwrap();
    let s = a.purview(0).unwrap();
    let json = hornvale_scene::surrounds_json(&s);
    assert!(!json.contains("\"color\""), "declining the observer step emits no colour");
    assert!(!json.contains("\"sight\""), "and no declaration either");

    let (b, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let lit = hornvale_scene::surrounds_json(&b.purview(0).unwrap());
    assert!(lit.contains("\"color\""), "the DEFAULT path must colour, or the test above proves nothing");
}

/// H4 — the light moves the colour.
#[test]
fn a_low_sun_reddens_the_chart_relative_to_a_high_one() {
    let w = world();
    let mut mk = |day: f64| {
        let mut o = PossessOpts::default();
        o.day = hornvale_kernel::WorldTime { day };
        Session::start(&w, &o).unwrap().0.purview(0).unwrap()
    };
    // Noon against a little before dawn, at the flagship's own latitude.
    let noon = mk(0.5);
    let dusk = mk(0.27);
    let ratio = |s: &hornvale_scene::SurroundsScene| -> Option<f64> {
        let mut r = 0.0f64;
        let mut b = 0.0f64;
        for c in &s.cells {
            if let Some(px) = c.color {
                r += px[0] as f64;
                b += px[2] as f64;
            }
        }
        (b > 0.0).then_some(r / b)
    };
    let (n, d) = (ratio(&noon), ratio(&dusk));
    let (n, d) = (n.expect("noon colours some cells"), d.expect("dusk colours some cells"));
    assert_ne!(
        noon.sight.as_ref().map(|s| s.sun_altitude_deg),
        dusk.sight.as_ref().map(|s| s.sun_altitude_deg),
        "the two probes must actually sit at different sun altitudes, or this \
         test measures nothing"
    );
    assert!(
        d > n,
        "H4 FALSIFIED — a low sun did not redden the chart (dusk R:B {d}, \
         noon R:B {n}). Report the measured altitudes; do not retune K."
    );
}
```

- [ ] **Step 2: Run to verify they fail**

```bash
cargo test -p hornvale-vessel --lib purview 2>&1 | tee /tmp/hv-beholding-t4.txt
```

Expected: FAIL to compile — `crate::eyes` does not exist.

- [ ] **Step 3: Implement**

1. `eyes.rs` per the Interfaces block. `resolve` maps `Own` →
   `observer_for(&agent.perception)` with the name `agent.species`;
   `Named(n)` → `observer_named(&n)` with the name `n`; `Off` → `None`.
2. `daylight_at` builds the star's `daylight`, reads
   `calendar.solar_altitude_at(StdDays(day.day), latitude)` — falling back to
   `0.0` (and a flat `daylight`) when the calendar is absent, the same
   `Option` posture `Session::calendar` already takes — and returns
   `at_elevation(&base, alt)` with the altitude.
3. `purview_scene` gains `eyes: &Eyes` and calls
   `surrounds_scene_colored_in` when `resolve` returns `Some`, else the
   existing uncoloured path. **The epistemic and agent overlays are applied
   after, exactly as now** — the colour layer must not reorder them, or
   `remembered` cells and NPC marks will move.
4. `Session` holds `eyes: Eyes` from `PossessOpts`, and `Session::purview`
   passes `&self.eyes`.
5. `PossessOpts::default()` sets `eyes: Eyes::Own` — colour on by default,
   through the possessed agent's own eyes.

- [ ] **Step 4: Run to verify they pass, then re-pin the fixtures**

```bash
cargo test -p hornvale-vessel 2>&1 | tee /tmp/hv-beholding-t4.txt
```

The four vessel fixtures **will** fail; that is expected and planned.
Re-pin them **in this commit**, never deferred to the close:

```bash
REBASELINE=1 cargo test -p hornvale-vessel 2>&1 | tail -20
git diff --stat windows/vessel/tests/fixtures/
```

**Read the fixture diff before accepting it.** Confirm the only changes are
added `"color"` keys and one `"sight"` block per walk-band snapshot. A moved
`room`, `state`, or mark ordering means the colour layer perturbed something
it must not have touched — that is a bug, not a rebaseline.

- [ ] **Step 5: Prove the guards bite**

| mutation | must redden | at |
|---|---|---|
| `resolve(Own)` returns the standard observer | `two_eyes_on_one_world_disagree_about_colour` | the `differ > 0` assert |
| `PossessOpts::default()` sets `Eyes::Off` | `eyes_off_restores_…` | its **second** (positive-control) assert |
| `daylight_at` ignores the altitude and returns the base | `a_low_sun_reddens_the_chart_…` | the `d > n` assert |

- [ ] **Step 6: Gate and commit**

```bash
cargo fmt && cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/src windows/vessel/tests
git commit -m "feat(vessel): possession colours the chart with the agent's own eyes

Colour is on by default through the possessed agent's species, lit by the
sun's real altitude at the observer's hour and latitude. Eyes::Off declines
the observer step — the same mechanism as a screen reader — and restores
byte-identical uncoloured output.

Re-pins the four vessel fixtures in the drifting commit."
```

---

### Task 5: vessel — the `eyes` verb, and `map`'s lens

**Files:**
- Modify: `windows/vessel/src/session.rs` (`HELP`, `handle`'s verb match, the
  `map` arm's hardcoded `"terrain"`)
- Test: `windows/vessel/tests/session.rs`

**Interfaces:**
- Consumes: Task 4's `Eyes`, `resolve`; Task 2's `observer_roster`,
  `ocular_reason`.
- Produces: no new public API — a verb.

- [ ] **Step 1: Write the failing tests**

In `windows/vessel/tests/session.rs`:

```rust
#[test]
fn the_eyes_verb_reports_whose_eyes_and_what_the_projection_drops() {
    let w = world();
    let (mut s, _) = Session::start(&w, &opts()).unwrap();
    let out = match s.handle("eyes") {
        Turn::Out(t) => t,
        other => panic!("expected output, got {other:?}"),
    };
    let species = s.agent().species.clone();
    assert!(out.contains(&species), "the report must name whose eyes: {out}");
    assert!(out.contains("channel"), "and the arity: {out}");
}

#[test]
fn eyes_switches_the_chart_and_an_unknown_name_lists_the_roster() {
    let w = world();
    let (mut s, _) = Session::start(&w, &opts()).unwrap();
    let before = s.purview(0).unwrap();
    s.handle("eyes kobold");
    let after = s.purview(0).unwrap();
    if s.agent().species != "kobold" {
        assert_ne!(
            before.cells.iter().map(|c| c.color).collect::<Vec<_>>(),
            after.cells.iter().map(|c| c.color).collect::<Vec<_>>(),
            "switching eyes must change the chart"
        );
    }
    let refusal = match s.handle("eyes wyvern") {
        Turn::Out(t) => t,
        other => panic!("expected a refusal, got {other:?}"),
    };
    assert!(refusal.contains("wyvern"), "name what was refused: {refusal}");
    assert!(refusal.contains("bugbear"), "and list the roster: {refusal}");
}

#[test]
fn map_renders_the_colour_lens_unless_the_eyes_are_off() {
    let w = world();
    let (mut s, _) = Session::start(&w, &opts()).unwrap();
    let lit = match s.handle("map") { Turn::Out(t) => t, o => panic!("{o:?}") };
    assert!(lit.contains("[lens: colour"), "possession draws the colour lens: {lit}");
    s.handle("eyes off");
    let bare = match s.handle("map") { Turn::Out(t) => t, o => panic!("{o:?}") };
    assert!(bare.contains("[lens: terrain"), "eyes off falls back to terrain: {bare}");
    assert!(!bare.contains('\u{1b}'), "and emits no escape sequences");
}
```

- [ ] **Step 2: Run to verify they fail**

```bash
cargo test -p hornvale-vessel --test session 2>&1 | tee /tmp/hv-beholding-t5.txt
```

Expected: FAIL — `eyes` is an unknown verb; `map` says `[lens: terrain]`.

**Note:** `windows/vessel/tests/session.rs:4071` already asserts
`chart.contains("[lens: terrain")`. That assertion is now **wrong for the
default path** and must be updated, not deleted — change it to assert
`[lens: colour` and add the `eyes off` case above beside it.

- [ ] **Step 3: Implement**

1. Add an `"eyes"` arm to `handle`'s verb match. Bare → a report built from
   `ocular_reason` plus the projection's `preserves`. `eyes own` / `eyes off`
   / `eyes <name>` set `self.eyes`; an unknown name returns a refusal naming
   the input and listing `observer_roster().join(", ")`.
2. `map`'s `render_surrounds_ascii(&scene, "terrain", &ways)` becomes
   `"colour"` when `self.eyes != Eyes::Off`.
3. Add two `HELP` lines, matching the existing two-column layout exactly:

```
  eyes [who]       whose eyes you see colour through (a species, 'own',
                   'standard', or 'off'); bare, it says what yours drop
```

4. **Grep the observable, not the function.** `[lens: terrain` may be
   asserted in more than the one place you found. Before committing:

```bash
grep -rn "lens: terrain\|lens: colour" --include=*.rs --include=*.ts --include=*.md . | grep -v target
```

Every hit is a site this change reaches. `examine` had two matchers and
fixing one never fixed the other, twice.

- [ ] **Step 4: Run to verify they pass**

```bash
cargo test -p hornvale-vessel 2>&1 | tee /tmp/hv-beholding-t5.txt
```

- [ ] **Step 5: Prove the guards bite**

| mutation | must redden | at |
|---|---|---|
| the unknown-name arm returns a bare "no" | `eyes_switches_the_chart_and_an_unknown_name_…` | the roster-listing assert |
| `map` hardcodes `"colour"` regardless of `Eyes` | `map_renders_the_colour_lens_unless_…` | the `[lens: terrain` assert |

- [ ] **Step 6: Gate and commit**

```bash
cargo fmt && cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add windows/vessel/src/session.rs windows/vessel/tests/session.rs
git commit -m "feat(vessel): the eyes verb

Named over a registry rather than a three-value toggle: the accessibility
observers the Pigment already committed to are 'an Observer with a shifted
or absent channel — the same code path as a goblin', and each would need an
enum variant. It also makes the campaign's thesis demonstrable in one world:
which species you possess is decided by the seed's flagship, so a closed
toggle would mean comparing eyes across two worlds."
```

---

### Task 6: vessel — the chamber band's palette slot

**Files:**
- Modify: `windows/vessel/src/plan.rs:62-71` (`PaletteEntry`)
- Test: `windows/vessel/src/plan.rs`'s `mod tests`

The spec's §8 decision: **ship the slot, not the value.** The building-fabric
and interior-illuminant models are unshipped, and inventing either is what
`RENDER-sourced-effects` forbids.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn a_palette_entry_carries_a_colour_slot_that_is_empty_this_campaign() {
    // The slot is additive and unpopulated ON PURPOSE. A building's fabric
    // has no material model (CellKind::Wall is "the building's fabric" and
    // carries nothing), and indoors the light is not the noon sun. Filling
    // this from bedrock under daylight would assert two things the world
    // does not model. See MAP-building-fabric and MAP-interior-light.
    let plan = minimal_plan_for_test();
    for e in &plan.palette {
        assert!(e.color.is_none(), "{} must not claim a colour yet", e.kind);
    }
    let json = serde_json::to_string(&plan).unwrap();
    assert!(!json.contains("\"color\""), "an absent slot emits no key");
}
```

- [ ] **Step 2: Run to verify it fails**

```bash
cargo test -p hornvale-vessel --lib plan 2>&1 | tee /tmp/hv-beholding-t6.txt
```
Expected: FAIL to compile — no field `color`.

- [ ] **Step 3: Implement**

Add to `PaletteEntry`, with `#[serde(skip_serializing_if = "Option::is_none")]`:

```rust
/// The cell type's display colour, absent until a building has a fabric to
/// read a reflectance from. `CellKind::Wall` is "the building's fabric" and
/// carries no material, and indoors the illuminant is not the sun — so this
/// stays `None` rather than borrowing the bedrock's colour under daylight,
/// which would assert two things the world does not model.
/// type-audit: bare-ok(artifact: color)
pub color: Option<[u8; 3]>,
```

Set `color: None` at the single construction site. Update the module doc's
"a colour triple" sentence to note the slot now exists and why it is empty.

- [ ] **Step 4: Run to verify it passes, and that the chamber fixtures did not move**

```bash
cargo test -p hornvale-vessel 2>&1 | tee /tmp/hv-beholding-t6.txt
git diff --stat windows/vessel/tests/fixtures/snapshot-seed-42-chamber.json
```
Expected: PASS, and **no diff** on the chamber fixtures.

- [ ] **Step 5: Commit**

```bash
cargo fmt && cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add windows/vessel/src/plan.rs
git commit -m "feat(vessel): the plan palette's colour slot, deliberately empty"
```

---

### Task 7: client — the panes return cells, not strings

**Files:**
- Create: `clients/vessel/src/pane_cell.ts`
- Modify: `clients/vessel/src/pane_chart.ts`, `clients/vessel/src/pane_plan.ts`
- Modify: `clients/vessel/src/pane_chart_test.ts`, `pane_plan_test.ts`, `pane_plan_marks_test.ts`

**Interfaces:**
- Produces:
  - `export interface PaneCell { glyph: string; color: [number, number, number] | null }`
  - `export type PaneGrid = PaneCell[][]`
  - `export function runsOf(row: PaneCell[]): { text: string; color: [number, number, number] | null }[]`
  - `chartCells(snap): PaneGrid | null`, `planCells(snap): PaneGrid | null`

**This tree is outside the cargo workspace.** Deno 2.9.2 exactly; `make gate`
cannot see it and will not run these tests.

- [ ] **Step 1: Write the failing tests**

In `clients/vessel/src/pane_cell_test.ts` (new):

```ts
import { assertEquals } from "jsr:@std/assert";
import { runsOf } from "./pane_cell.ts";

Deno.test("runsOf coalesces adjacent like-coloured cells", () => {
  const row = [
    { glyph: ".", color: [1, 2, 3] as [number, number, number] },
    { glyph: ",", color: [1, 2, 3] as [number, number, number] },
    { glyph: "~", color: null },
    { glyph: "@", color: [9, 9, 9] as [number, number, number] },
  ];
  assertEquals(runsOf(row), [
    { text: ".,", color: [1, 2, 3] },
    { text: "~", color: null },
    { text: "@", color: [9, 9, 9] },
  ]);
});

Deno.test("runsOf does not merge across a colour change", () => {
  const row = [
    { glyph: "a", color: [1, 1, 1] as [number, number, number] },
    { glyph: "b", color: [1, 1, 2] as [number, number, number] },
  ];
  assertEquals(runsOf(row).length, 2);
});
```

In `pane_chart_test.ts`, add:

```ts
Deno.test("a chart cell carries the sim's colour, and only where it is ground", () => {
  const snap = snapshotWithChart([
    { v: 0, w: 0, up: true, seam: false, state: "sensed", water: 3, color: [10, 20, 30] },
    { v: 1, w: 0, up: false, seam: false, state: "sensed", water: 0, color: [40, 50, 60] },
  ]);
  const grid = chartCells(snap)!;
  const flat = grid.flat();
  const land = flat.find((c) => c.glyph === ".")!;
  assertEquals(land.color, [10, 20, 30]);
  const water = flat.find((c) => c.glyph === "~")!;
  assertEquals(
    water.color,
    null,
    "the tint is BEDROCK; a river must not be drawn the colour of the rock beneath it",
  );
});

Deno.test("a cell with no colour key is uncoloured, not crashed", () => {
  const snap = snapshotWithChart([
    { v: 0, w: 0, up: true, seam: false, state: "sensed", water: 3 },
  ]);
  assertEquals(chartCells(snap)!.flat()[0].color, null);
});

Deno.test("a malformed colour is refused, not passed through", () => {
  for (const bad of [[1, 2], [1, 2, 3, 4], ["1", 2, 3], "red", 7, [1, 2, 300], [1, 2, -1]]) {
    const snap = snapshotWithChart([
      { v: 0, w: 0, up: true, seam: false, state: "sensed", water: 3, color: bad },
    ]);
    assertEquals(chartCells(snap)!.flat()[0].color, null, `${JSON.stringify(bad)} must not survive`);
  }
});
```

`water: 3` is dry-land and `water: 0` is `ocean` under
`WaterKind::LEGEND`; the existing `snapshotWithChart` helper (or the
equivalent in the current test file) must supply a `water_legend`.

- [ ] **Step 2: Run to verify they fail**

```bash
cd clients/vessel && deno task test 2>&1 | tee /tmp/hv-beholding-t7.txt
```
Expected: FAIL — `pane_cell.ts` and `chartCells` do not exist.

- [ ] **Step 3: Implement**

1. `pane_cell.ts` with `PaneCell`, `PaneGrid`, `runsOf`, and a
   `parseColor(raw: unknown): [number, number, number] | null` that accepts
   only a 3-length array of integers in `0..=255`. Everything else is `null`
   — the same refuse-don't-guess posture `parseCell` already takes.
2. `chartRows` → `chartCells`, returning `PaneGrid`. Keep every existing
   guard verbatim (the schema allowlist, `MAX_COORD`, the seam skip, the
   `.map`-not-`.filter` legend note, the `+ w` placement term and its
   comment). The empty slot becomes `{ glyph: EMPTY, color: null }`.
3. **The ground rule**: a cell's colour is carried only when its glyph draws
   the bedrock — i.e. not `YOU`, not a water glyph. Port the reasoning
   comment from `surrounds_ascii.rs`'s `terrain_glyph`, because a reader of
   this file must be able to see *why* a river is untinted without opening
   the Rust.
4. `planRows` → `planCells`, same treatment. Every palette colour is `None`
   this campaign (Task 6), so every plan cell's colour is `null` — keep the
   plumbing anyway so the chamber-band campaign is a one-line change, and say
   so in a comment.

- [ ] **Step 4: Run to verify they pass**

```bash
cd clients/vessel && deno fmt --check && deno lint && deno task check && deno task test
```

- [ ] **Step 5: Commit**

```bash
git add clients/vessel/src
git commit -m "feat(casement): the panes return cells, not strings

A string cannot carry colour, and parallel arrays are the shape plan.rs
already rejected one layer down: 'every later attribute would become
another array to keep length-synced with the grid'. The attributes it named
as coming are the ones arriving here."
```

---

### Task 8: client — spans, the caption, and the injection guard

**Files:**
- Modify: `clients/vessel/src/main.ts` (`drawMap` and the four
  `map.textContent = ""` sites)
- Modify: `clients/vessel/src/snapshot.ts` (expose `sight` off the spatial
  channel)
- Create: `clients/vessel/src/main_render_test.ts`

- [ ] **Step 1: Write the failing tests**

```ts
import { assertEquals, assertStringIncludes } from "jsr:@std/assert";
import { renderInto } from "./main.ts";

Deno.test("a sim-authored noun containing markup becomes text, never an element", () => {
  // pane_plan.ts draws mark.noun.charAt(0) — a sim-authored character. The
  // map pane used to set textContent, which was injection-safe by
  // construction; building DOM is what makes this a live question.
  const host = document.createElement("pre");
  renderInto(host, [[{ glyph: "<", color: [1, 2, 3] }, { glyph: "img", color: null }]], null);
  assertEquals(host.querySelectorAll("img").length, 0);
  assertEquals(host.textContent, "<img");
});

Deno.test("like-coloured neighbours share one span", () => {
  const host = document.createElement("pre");
  const c: [number, number, number] = [1, 2, 3];
  renderInto(host, [[
    { glyph: "a", color: c },
    { glyph: "b", color: c },
    { glyph: "c", color: null },
  ]], null);
  // Two runs on one row, not three cells.
  assertEquals(host.querySelectorAll("span").length, 2);
});

Deno.test("the caption states whose eyes and what the projection drops", () => {
  const host = document.createElement("pre");
  renderInto(host, [[{ glyph: ".", color: null }]], {
    observer: "bugbear",
    channels: 3,
    chromatic: 2,
    projection: "yellow-blue",
    preserves: "the short-to-long opposition; the red-green axis is not carried",
  });
  assertStringIncludes(host.textContent ?? "", "bugbear");
  assertStringIncludes(host.textContent ?? "", "yellow-blue");
});
```

`clients/vessel` tests run under `jsdom`-style DOM shims; check how
`pane_plan_marks_test.ts` obtains a document and follow it. **jsdom sees no
layout and no paint** — assert structure and text, never computed colour.

- [ ] **Step 2: Run to verify they fail**

```bash
cd clients/vessel && deno task test 2>&1 | tee /tmp/hv-beholding-t8.txt
```

- [ ] **Step 3: Implement**

1. Extract the rendering out of `mount`'s closure into
   `export function renderInto(host: HTMLElement, grid: PaneGrid | null, sight: Sight | null): void`
   so it is testable without mounting a page. `drawMap` calls it.
2. `renderInto` clears with `replaceChildren()`, then per row emits one
   `document.createElement("span")` per run from `runsOf`, setting
   `span.textContent = run.text` and, when `run.color` is non-null,
   `span.style.color = \`rgb(${r} ${g} ${b})\``. **Never `innerHTML`.**
   Rows are separated by a `"\n"` text node.
3. The caption is a final `<span class="casement-sight">` with
   `textContent` naming the observer, the arity, and `preserves`.
4. Keep `drawMap`'s existing `try/catch` and its comment — a pane throw
   before `setIdle` locks the Casement with no error shown.
5. `snapshot.ts` reads `sight` off the walk-band spatial channel with the
   same field-by-field validation `parseSnapshot` already applies; an
   unparseable `sight` is `null`, never a partial object.

- [ ] **Step 4: Run to verify they pass**

```bash
cd clients/vessel && deno fmt --check && deno lint && deno task check && deno task test
cd ../.. && make wasm-vessel && make vessel-check 2>&1 | tee /tmp/hv-beholding-t8.txt
```

**`make wasm-vessel` first, always.** The wasm and the bundle are
deploy-built and git-ignored; a stale one silently demos old code, and this
has fooled this project twice.

- [ ] **Step 5: Rebuild the committed bundles and verify they moved**

```bash
cd clients/vessel && deno task build && cd ../..
git diff --stat book/src/gallery/vessel.js book/src/gallery/vessel-worker.js
```
A **zero** diff here means you tested a stale bundle. Investigate rather
than proceeding.

- [ ] **Step 6: Commit**

```bash
git add clients/vessel/src book/src/gallery/vessel.js book/src/gallery/vessel-worker.js
git commit -m "feat(casement): tinted spans and the sight caption

One span per RUN of like-coloured cells, so node count stays near the old
textContent cost. createElement + textContent per span, never innerHTML:
pane_plan.ts draws a sim-authored character, so a settlement name
containing '<' must never reach the parser."
```

---

### Task 9: artifacts, the book, and the close

**Files:**
- Modify: `book/src/reference/scene-surrounds-v2.md` (**hand-authored**, not
  generated — the drift check will not write it for you)
- Modify: `docs/audits/type-audit-report.md` (regenerated)
- Create: `book/src/chronicle/the-beholding.md`
- Modify: `book/src/SUMMARY.md` (the one always-hand-authored book file)
- Create: `docs/retrospectives/the-beholding.md`
- Modify: `book/src/frontier/idea-registry.md` (RENDER-9's status)

- [ ] **Step 1: Document `sight` in the schema reference**

Add a section to `book/src/reference/scene-surrounds-v2.md` covering the
block's six fields, that it is absent from uncoloured documents, and — the
load-bearing part — **that four of its fields are overwritten by the builder
from the observer actually used**, so a consumer may trust them.

- [ ] **Step 2: Regenerate every artifact and read the diff**

```bash
make rebaseline
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
```

**Expected to move:** `docs/audits/type-audit-report.md` only.
**Expected NOT to move:** `book/src/gallery/scene-surrounds-seed-42.json` and
the three `generated/surrounds-seed-42/*.txt` charts — they go through the
CLI's `terrain`/uncoloured path. **If a gallery artifact moved, stop**: the
`skip_serializing_if` discipline broke somewhere, and rebaselining would
freeze the bug.

A generated file has no merge. If you absorbed main during this campaign,
regenerate *after* the absorption and before reading this diff — a
conflict-free merge of a generated aggregate is silently wrong.

- [ ] **Step 3: Write the chronicle**

`book/src/chronicle/the-beholding.md`, at the book's usual altitude —
technical and mathematical, comprehensible without the code. It must carry:

- the three-way product, and why a bugbear needs a *declared* translation;
- **the M1/M2/M3 measurements**, including that the first candidate model was
  falsified before any code was written;
- **H3's outcome, whichever way it went.** If falsified, that is the
  headline, and the chronicle says the model produces species that see
  differently but not species that are colour-blind;
- H4's measured altitudes;
- what the chamber band does not do, and why (`MAP-building-fabric`,
  `MAP-interior-light`).

Add its `SUMMARY.md` entry by hand — chapter titles are code-generated
elsewhere, but `SUMMARY.md` never is.

- [ ] **Step 4: Freshness sweep and the Confidence Gradient**

```bash
grep -rn "no colour\|colourless\|has no observer\|standard observer is the only" book/src/ | grep -v chronicle
```
Every hit is a chapter this campaign made stale. Re-score
`book/src/open-questions.md` if this campaign moved one of its bets.

- [ ] **Step 5: Retrospective and the registry**

`docs/retrospectives/the-beholding.md` — process lessons, not product. The
one this campaign has already earned: **a spec-time probe that needs no world
build falsified three claims before they were written down.** Promote the
`.superpowers/sdd/` ledger's findings *before* the worktree is torn down;
that directory is git-ignored and dies with it.

Update `RENDER-9`'s row: its third open question — *"whether a lens should
declare its own provenance and fidelity the way the scene documents do"* — is
answered **yes**.

- [ ] **Step 6: The full gate, then the close**

```bash
make preflight
make gate 2>&1 | tee /tmp/hv-beholding-gate.txt
make vessel-check 2>&1 | tee /tmp/hv-beholding-vessel.txt
```

`make gate` measures 22–37 min in a worktree; budget `timeout: 3600000` and
**stagger against the `the-delvers` worktree** — two concurrent gates cost
about thirty minutes each and both look hung. Note that `make gate` runs
**none** of `vessel-check`, `world-check`, `census-check`, or `shellcheck`;
this campaign needs `vessel-check` explicitly.

Read the wrapper's exit code *and* the failure list — a wrapper's exit code
has masked the command's here before.

Then invoke `closing-a-campaign`, and present the post-G3 ledger digest at
G6 with any save-format or determinism entries leading.

---

## Self-review

**Spec coverage.** §4.1 → T1; §4.2 → T1; §4.3 → T1; §4.4 → T2; §4.5 → T3;
§4.6 → T4/T5/T6; §4.7 → T7/T8; §5 determinism → T1 Step 5, T3 Step 5, T9
Step 2; §6 H1/H2 → T2, H3 → T2, H4 → T4; §7 testing discipline → the
per-task mutation tables; §8 out-of-scope → T6 and the registry rows already
committed; §9 risks 1–4 → T2 Step 4, T4 Step 1, T7 Step 3, T1 Step 5.

**Type consistency.** `observer_for`/`observer_named`/`observer_roster`
(T2) are consumed under those exact names in T4 and T5. `Sight`'s six fields
are constructed in T3 and read in T4 and T8. `PaneCell`/`PaneGrid`/`runsOf`
(T7) are consumed in T8. `Eyes`'s three variants are constructed in T4 and
matched in T5.

**Known open point, deliberately left to the implementer:** T1's
`the_standard_observers_bytes_have_not_moved` literal cannot be written here
— it must be *measured* on unmodified `color.rs` (Step 2). A literal invented
at plan time would pin whatever the refactor happened to produce, which is
the opposite of a byte-identity test.
