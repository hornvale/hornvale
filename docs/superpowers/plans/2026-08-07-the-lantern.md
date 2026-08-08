# The Lantern Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A stone-walled chamber and a timber one, visibly different from each other, both warm under a hearth — by giving `CellKind::Wall` a fabric, giving interiors a light, and filling `PaletteEntry.color`.

**Architecture:** The blackbody moves down into the kernel and becomes a band integral (accuracy). Fabric is *derived* from lithology/biome/climate — no seeded draw, no epoch. Light is a *derived view*: each source's reach is the shipped symmetric `shadowcast`, and a cell's incident light is the sum of the illuminants reaching it. `sense(reflectance, illuminant)` then produces the colour that fills the palette. A presentation-layer **lens** recovers the look, built last so it cannot hide a failed hypothesis.

**Tech Stack:** Rust 2024, workspace deps `serde`/`serde_json`/`libm` only. `cargo nextest`, `make gate`.

**Spec:** `docs/superpowers/specs/2026-08-07-the-lantern-design.md` (G3 approved, amended at approval).

## Global Constraints

- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (`clippy.toml` enforces workspace-wide, tests included). No `std::time::*`.
- **No new dependencies.** `serde`, `serde_json`, `libm` only.
- **No seeded draw and no `streams.rs` label** anywhere in this campaign. Fabric is derived; light is derived. A window that draws has become a domain (`windows/CLAUDE.md`).
- **Every crate sets `#![warn(missing_docs)]`** — every `pub` item, field and variant gets a one-line doc comment.
- **Every primitive at a `pub` boundary carries a `type-audit:` tag** (`bare-ok(<class>)` / `waiver(<reason>)`). Tag `return` on tuple/`Option` returns too.
- **Any new `pub` boundary drifts `docs/audits/type-audit-report.md`, and the pre-commit hook runs `make quick` workspace-wide regardless of what you staged** — so the regenerated report must land **in the same commit**, not at the close. Found the hard way in Task 1 (kernel `bare-ok(ratio)` 556 → 560). Tasks 2 (`blackbody`), 3 (`fabric.rs`), 4 (`light.rs`) and 8 (`lens.rs`) each add one. Regenerate with:
  ```bash
  cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
  ```
- **`cargo fmt` is the final step before every commit.** Fmt skips are the most common review finding.
- **Layering:** `kernel/ → domains/* → windows/* → cli/`. A domain depends on the kernel and nothing else.
- **Every guard must state what would make it fire, and be mutation-proven.** A mutation must redden **at the assertion it targets** — "it went red" is not the observation, and a mutation test must first prove it mutated.
- **The last campaign shipped seven green-and-vacuous guards, every one originating in plan text.** Live list, to be pasted into every dispatch: a `u8` rounding step absorbed a regression; a spectrally flat probe cancelled another; an assertion checked for a string that could no longer appear; two withholding rules had no test at all; an injection fixture put its two glyphs in separate DOM spans so they could never form a tag; a wire parser had zero coverage with unit tests on either side of it.
- **Write the probe before writing the claim.** Three framing errors in one session were all "a shipped, plausible-sounding thing answers a different question." Every measurement was right; every attribution was wrong.

## File Structure

| file | responsibility |
|---|---|
| `kernel/src/color.rs` | **modify** — gains `BAND_WIDTH_NM`, `pub fn planck_relative`, `pub fn blackbody`, and later the scotopic term in `to_srgb` |
| `domains/astronomy/src/illuminant.rs` | **modify** — `daylight` delegates to the kernel; `planck_relative` and `C2_NM_K` leave |
| `windows/vessel/src/fabric.rs` | **create** — what a cell is made of, and its reflectance |
| `windows/vessel/src/light.rs` | **create** — sources, reach, attenuation, the additive sum: the light field over a lattice |
| `windows/vessel/src/plan.rs` | **modify** — `entry_for` becomes fabric+light aware; interning widens |
| `windows/vessel/src/lens.rs` | **create** — the presentation filter (Task 8, last) |
| `windows/worldgen/tests/lantern_probe.rs` | **modify** — refresh under the band integral |

---

### Task 1: Move the blackbody into the kernel, bit-identically

Spec §5.1. A hearth is not astronomy; `windows/vessel` must not import `hornvale_astronomy` to light a fire. **This task changes no number** — that is its entire claim.

**Files:**
- Modify: `kernel/src/color.rs`
- Modify: `domains/astronomy/src/illuminant.rs:13-26,44-62`
- Test: `domains/astronomy/src/illuminant.rs` (the existing `mod tests`)

**Interfaces:**
- Consumes: `hornvale_kernel::color::{BANDS, BAND_CENTERS_NM, Illuminant}`, `hornvale_kernel::math`.
- Produces: `hornvale_kernel::color::planck_relative(wavelength_nm: f64, t_kelvin: f64) -> f64`.

- [ ] **Step 1: Write the failing test** — pin the exact bits of today's `daylight`, so the move cannot change a digit.

Add to `domains/astronomy/src/illuminant.rs`'s `mod tests`:

```rust
/// The move in Task 1 relocates `planck_relative` into the kernel and must
/// change NO number. These are the exact `f64` bit patterns `daylight`
/// produced at `eea687dc`, before the move — a relocation that perturbs one
/// of them is not a relocation.
///
/// FIRES WHEN: any band of `daylight(5772 K)` differs in a single bit.
/// It is deliberately bit-exact, not epsilon-based: an approximate
/// assertion here would pass through exactly the drift it exists to catch.
#[test]
fn the_move_into_the_kernel_changes_no_bit_of_daylight() {
    let light = daylight(&star_at(5772.0));
    let bits: Vec<u64> = light.get().iter().map(|v| v.to_bits()).collect();
    assert_eq!(bits, EXPECTED_5772_BITS, "daylight(5772 K) moved");
}

/// Captured from `daylight(&star_at(5772.0))` at `eea687dc`. Regenerate
/// ONLY in a commit that deliberately changes the sampling (Task 2).
const EXPECTED_5772_BITS: [u64; 10] = [/* filled in Step 2 */];
```

- [ ] **Step 2: Capture the real bits, then run the test to verify it passes BEFORE the move**

Run:
```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-lantern
cargo test -p hornvale-astronomy --lib illuminant -- --nocapture
```

The test will fail with a length mismatch. Print the real values, paste them into `EXPECTED_5772_BITS`, and re-run. **Expected: PASS, before any move has happened.** A golden that is not green before the refactor is measuring the refactor's compile errors, not its numbers.

- [ ] **Step 3: Move the function into the kernel**

In `kernel/src/color.rs`, add next to `BAND_CENTERS_NM`:

```rust
/// The width of one band, nanometres. The grid is ten uniform bands whose
/// edges span 340–740, so band `i` covers
/// `BAND_CENTERS_NM[i] ± BAND_WIDTH_NM / 2`.
/// type-audit: bare-ok(ratio)
pub const BAND_WIDTH_NM: f64 = 40.0;

/// Planck's second radiation constant, `hc/k`, in nanometre-kelvin.
/// type-audit: bare-ok(ratio)
const C2_NM_K: f64 = 1.438_776_877e7;

/// Spectral radiance of a blackbody at `t_kelvin`, at `wavelength_nm`, up to
/// a constant factor. The leading `c1` is omitted because every consumer
/// works in ratios or renormalizes — carrying it would only scale all ten
/// bands together.
///
/// **This lives in the kernel, not in astronomy, because it takes no
/// world-state.** A star, a hearth and a forge are the same law at three
/// temperatures; the temperature is the datum and belongs to whoever owns
/// the thing, but the law is substrate. (Astronomy's `at_elevation` stays in
/// astronomy for the mirror-image reason: it is parameterized by a sun's
/// elevation.)
/// type-audit: bare-ok(ratio: wavelength_nm), bare-ok(ratio: t_kelvin), bare-ok(ratio: return)
pub fn planck_relative(wavelength_nm: f64, t_kelvin: f64) -> f64 {
    let l5 = wavelength_nm.powi(5);
    let x = C2_NM_K / (wavelength_nm * t_kelvin);
    1.0 / (l5 * (math::exp(x) - 1.0))
}
```

In `domains/astronomy/src/illuminant.rs`, delete `C2_NM_K` and `planck_relative`, import `hornvale_kernel::color::planck_relative`, and call it in `daylight` unchanged.

- [ ] **Step 4: Run the tests**

Run:
```bash
cargo test -p hornvale-astronomy --lib illuminant
cargo test -p hornvale-kernel color
```
Expected: PASS, including `the_move_into_the_kernel_changes_no_bit_of_daylight`.

- [ ] **Step 5: Mutation-prove the guard**

Temporarily change `l5` to `wavelength_nm.powi(4)` in the kernel. Run the same command.
**Expected: `the_move_into_the_kernel_changes_no_bit_of_daylight` FAILS** — and confirm the failure names *that* assertion, not a compile error. Revert the mutation and confirm green again. Record both observations in the commit body.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add kernel/src/color.rs domains/astronomy/src/illuminant.rs
git commit -m "refactor(kernel): the blackbody is substrate, not astronomy

A spectral law that takes no world-state belongs to the kernel; a law
parameterized by domain state stays in its domain. planck_relative takes
(nm, K) and no world-state, so windows/vessel can light a hearth without
importing hornvale_astronomy.

Bit-pinned: daylight(5772 K) is unchanged to the last bit. Mutation-proven
(powi(5) -> powi(4) reddens that assertion)."
```

---

### Task 2: The band integral

Spec §5.2. Owner's call, 2026-08-07: accuracy in the model. **Measured at G3 to cost nothing** — a 13-node spike left the suite at 3135/3135 and moved no committed colour byte.

**Files:**
- Modify: `kernel/src/color.rs`
- Modify: `domains/astronomy/src/illuminant.rs`
- Modify: `windows/worldgen/tests/lantern_probe.rs`
- Modify: `docs/superpowers/specs/2026-08-07-the-lantern-design.md` (§2 table; §11 risk 5 requires it)

**Interfaces:**
- Consumes: `planck_relative` from Task 1, `BAND_WIDTH_NM`.
- Produces: `hornvale_kernel::color::blackbody(t_kelvin: f64) -> Illuminant` — peak-normalized to 1.0, band-integrated. This is the constructor Tasks 5 and 6 build every flame from.

- [ ] **Step 1: Write the failing tests**

In `kernel/src/color.rs`'s test module:

```rust
/// Simpson's rule is exact for cubics, so integrating a cubic over one band
/// must return its analytic mean. This checks the QUADRATURE, independent of
/// Planck — a Planck-only test cannot tell a broken rule from a broken law.
///
/// FIRES WHEN: the weights, the node spacing, or the final division is wrong.
#[test]
fn the_band_quadrature_is_exact_for_a_cubic() {
    // mean of x^3 over [c - w/2, c + w/2] = c^3 + c * w^2 / 4
    let c = 500.0;
    let w = BAND_WIDTH_NM;
    let got = band_mean(c, &|x: f64| x * x * x);
    let want = c * c * c + c * w * w / 4.0;
    assert!(
        (got - want).abs() / want < 1e-12,
        "cubic band mean: got {got}, want {want}"
    );
}

/// The node count is a PERMANENT CONTRACT: change it and every colour in the
/// world moves. It was chosen by measurement (spec §5.2) to hold at least
/// 20x below a u8 quantization step (3.9e-3) down to 700 K, so that a later
/// ember or forge cannot force it to change.
///
/// FIRES WHEN: someone lowers BAND_NODES. Five nodes fails by 900 K.
#[test]
fn the_node_count_is_converged_down_to_a_dull_red_glow() {
    for t in [700.0, 1100.0, 1900.0, 5800.0] {
        for center in BAND_CENTERS_NM {
            let coarse = band_mean(center, &|nm| planck_relative(nm, t));
            let fine = band_mean_with_nodes(center, 4097, &|nm| planck_relative(nm, t));
            let rel = (coarse - fine).abs() / fine;
            assert!(
                rel < 3.9e-4,
                "T={t} band {center}: relative error {rel} exceeds 20x below a u8 step"
            );
        }
    }
}

/// A blackbody is peak-normalized, finite and positive — the contract every
/// consumer relies on to compare COLOUR rather than distance from a source.
///
/// FIRES WHEN: normalization is dropped or a band goes non-positive.
#[test]
fn a_blackbody_is_peak_normalized_and_positive() {
    let light = blackbody(1900.0);
    let peak = light.get().iter().copied().fold(0.0f64, f64::max);
    assert_eq!(peak, 1.0, "peak band should be exactly 1.0");
    for (b, v) in light.get().iter().enumerate() {
        assert!(v.is_finite() && *v > 0.0, "band {b} is {v}");
    }
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-kernel color`
Expected: FAIL — `band_mean`, `band_mean_with_nodes` and `blackbody` do not exist.

- [ ] **Step 3: Implement**

In `kernel/src/color.rs`:

```rust
/// Simpson nodes per band. **A permanent contract**: changing it moves every
/// colour in the world. Chosen by measurement, not taste (spec §5.2) — 13
/// nodes hold at least 20x below a `u8` quantization step down to 700 K, a
/// dull red glow colder than anything the project names, so a later ember or
/// forge cannot force it to change. Five nodes already fails by 900 K.
/// Must be odd.
/// type-audit: bare-ok(count)
const BAND_NODES: usize = 13;

/// The mean of `f` across the band centred at `center_nm`, by Simpson's rule.
fn band_mean(center_nm: f64, f: &dyn Fn(f64) -> f64) -> f64 {
    band_mean_with_nodes(center_nm, BAND_NODES, f)
}

/// [`band_mean`] at an explicit node count, so a test can compare the shipped
/// count against a converged reference. Not public: the node count is a
/// contract, not a caller's choice.
fn band_mean_with_nodes(center_nm: f64, nodes: usize, f: &dyn Fn(f64) -> f64) -> f64 {
    debug_assert!(nodes >= 3 && nodes % 2 == 1, "Simpson needs an odd node count >= 3");
    let a = center_nm - BAND_WIDTH_NM / 2.0;
    let h = BAND_WIDTH_NM / (nodes - 1) as f64;
    let mut sum = 0.0;
    for i in 0..nodes {
        let weight = if i == 0 || i == nodes - 1 {
            1.0
        } else if i % 2 == 1 {
            4.0
        } else {
            2.0
        };
        sum += weight * f(a + i as f64 * h);
    }
    sum * h / 3.0 / BAND_WIDTH_NM
}

/// A blackbody at `t_kelvin` on the band grid, normalized so the brightest
/// band is 1.0.
///
/// **A band integral, not a midpoint sample.** `BAND_CENTERS_NM`'s own doc
/// says anything integrating over a band wants the *edges*. The midpoint
/// rule's error is 0.26 % at 5800 K but 34 % at 1100 K, because below the
/// grid the visible range is the steep, strongly convex Wien tail and a
/// midpoint sample underestimates a convex mean. A star could afford that; a
/// hearth cannot.
/// type-audit: bare-ok(ratio: t_kelvin)
pub fn blackbody(t_kelvin: f64) -> Illuminant {
    let mut bands = [0.0f64; BANDS];
    let mut peak = 0.0f64;
    for (band, center) in bands.iter_mut().zip(BAND_CENTERS_NM.iter()) {
        let value = band_mean(*center, &|nm| planck_relative(nm, t_kelvin));
        *band = value;
        if value > peak {
            peak = value;
        }
    }
    if peak > 0.0 {
        for value in bands.iter_mut() {
            *value /= peak;
        }
    }
    Illuminant::new(bands).expect("a normalized Planck curve is finite and non-negative")
}
```

Then in `domains/astronomy/src/illuminant.rs`, `daylight` becomes a delegation:

```rust
pub fn daylight(star: &Star) -> Illuminant {
    hornvale_kernel::color::blackbody(star.t_eff.get())
}
```

Update `daylight`'s doc: delete the "**A midpoint sample, not an integral**" paragraph and replace it with a pointer to `blackbody`, keeping the normalization/containment paragraph verbatim.

- [ ] **Step 4: Update the bit-pin from Task 1 and confirm the fixture guard**

`the_move_into_the_kernel_changes_no_bit_of_daylight` now fails **by design** — this is the commit that deliberately changes the sampling. Re-capture `EXPECTED_5772_BITS`, rename the test to `daylight_is_bit_pinned_under_the_band_integral`, and rewrite its doc to say the pin now guards the *integral*.

Then run the guard that actually protects committed colour:
```bash
HV_TEST_OK=1 cargo nextest run -p hornvale-vessel -E 'test(the_client_fixtures_are_current)'
```
**Expected: PASS.** Measured at G3: the band integral moves no committed colour byte, because at 5800 K the shift is `1.56e-3` relative and a `u8` step is `3.9e-3`.

**Do not use `make rebaseline` + `git diff` as evidence here.** It regenerates nothing carrying a daylight-derived colour and returns an empty diff even under a gross mutation (proven at G3).

- [ ] **Step 5: Refresh the probe and the spec's §2 table**

Run:
```bash
cargo test -p hornvale-worldgen --test lantern_probe -- --nocapture
```
The probe has its own private copy of `planck_relative`. Delete it and call `hornvale_kernel::color::blackbody` so the probe and the shipped path cannot diverge. Paste the new five-light triples into spec §2's table, and **delete §11 risk 5**, which exists only until this step runs.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add kernel/src/color.rs domains/astronomy/src/illuminant.rs \
        windows/worldgen/tests/lantern_probe.rs \
        docs/superpowers/specs/2026-08-07-the-lantern-design.md
git commit -m "feat(kernel): the blackbody is a band integral

Accuracy in the model (owner's call); the look is recovered by the lens.
The midpoint rule is 0.26% wrong at 5800 K and 34% at 1100 K, because below
the grid the visible range is the steep convex Wien tail.

Costs nothing: no committed colour byte moves — the shift lands below u8
quantization. the_client_fixtures_are_current stays green.

13 nodes, chosen by measurement: the count is a permanent contract, and 13
holds 20x below quantization down to 700 K."
```

---

### Task 3: Fabric, and the H1 measurement

Spec §3 and H1. **This is the task that can falsify the campaign** (§11 risk 1), so it is deliberately third — before any light exists to flatter it, and long before the lens.

**Files:**
- Create: `windows/vessel/src/fabric.rs`
- Modify: `windows/vessel/src/lib.rs` (add `pub mod fabric;`)
- Test: `windows/worldgen/tests/lantern_fabric.rs`

**Interfaces:**
- Consumes: `hornvale_terrain::{GeneratedTerrain, lithology}`, `hornvale_kernel::color::{Mixture, Reflectance}`, `hornvale_kernel::CellId`.
- Produces:
  - `pub enum Fabric { Stone, Timber, Cob, Thatch }`
  - `pub fn fabric_of(kind: CellKind, ctx: &FabricContext) -> Option<Fabric>` — `None` for `Threshold` (an opening is not a fabric).
  - `pub fn reflectance_of(fabric: Fabric, ctx: &FabricContext) -> Reflectance`
  - `pub struct FabricContext { pub rock: RockClass, pub material: MaterialBuffer, pub forested: bool, pub temperate: bool, pub deep_soil: bool, pub dry: bool }`

- [ ] **Step 1: Write the failing H1 test — on REAL terrain, swept across seeds**

Create `windows/worldgen/tests/lantern_fabric.rs`:

```rust
//! H1: stone fabrics derived from real bedrock are distinguishable.
//!
//! MEASURED ON REAL TERRAIN, NOT FIXTURES. An authored `MaterialBuffer`
//! would answer a different question — The Beholding's 28-of-255 on authored
//! fixtures collapsed to 2-of-255 on real terrain, and this claim is exactly
//! the one that failure mode would fake.

/// H1 — two settlements on different bedrock produce stone walls differing
/// by more than one `u8` step in at least one channel.
///
/// FIRES WHEN: bedrock variation across settlements is too small to survive
/// the fabric transform — the §11 risk 1 outcome, in which the campaign
/// ships walls that all look alike. **A falsified H1 is a finding, not a
/// failure**: it means fabric needs a second axis, and it must be reported
/// rather than rescued by widening the threshold.
#[test]
fn h1_stone_fabrics_differ_across_settlements() {
    let mut triples = Vec::new();
    for seed in [1u64, 7, 42, 99, 256, 1024, 4096, 9001] {
        for (rock, material) in settlement_bedrock(seed) {
            let ctx = stone_context(rock, material);
            let refl = fabric::reflectance_of(Fabric::Stone, &ctx);
            triples.push(srgb_under_reference_light(&refl));
        }
    }
    let spread = max_channel_spread(&triples);
    assert!(
        spread > 1,
        "H1 FALSIFIED: {} sampled settlements span only {spread} u8 steps \
         in every channel — derived stone cannot vary. Report this; do not \
         widen the threshold.",
        triples.len()
    );
    eprintln!("H1: {} settlements, max channel spread {spread} u8 steps", triples.len());
}

/// The distribution, not the extremum. A single pair of outlier settlements
/// can satisfy H1 while every ordinary pair is identical, and a max-only
/// reading cannot tell those apart.
///
/// FIRES WHEN: the median pairwise difference collapses even though the
/// extremes are far apart.
#[test]
fn h1_reports_the_whole_distribution_not_just_the_extremes() {
    let triples = all_settlement_stone_triples();
    let mut diffs = pairwise_max_channel_diffs(&triples);
    diffs.sort_by(|a, b| a.total_cmp(b));
    let median = diffs[diffs.len() / 2];
    let p10 = diffs[diffs.len() / 10];
    eprintln!("H1 distribution: p10 {p10}, median {median}, max {}", diffs[diffs.len() - 1]);
    assert!(
        median > 0.0,
        "H1 median pairwise difference is zero: the typical pair of \
         settlements is IDENTICAL even if the extremes differ"
    );
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-worldgen --test lantern_fabric`
Expected: FAIL — `hornvale_vessel::fabric` does not exist.

- [ ] **Step 3: Implement `fabric.rs`**

Stone's reflectance is **derived**: `lithology::reflectance(&ctx.material, ctx.rock).integrate()`. The other three are authored constants. Resolve the settlement's `CellId` through **the same dominant-corner path `LocaleContext` uses for biome** — fabric and biome must not disagree about which cell a settlement sits on.

```rust
/// What a built cell is made of. **Derived, never drawn** — from lithology,
/// biome and climate, all of which already ship. No seed label, no epoch.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Fabric {
    /// Vernacular stone. Its reflectance is DERIVED from the local bedrock,
    /// so a village on granite and one on basalt are visibly different.
    Stone,
    /// Timber: a forested, temperate place.
    Timber,
    /// Cob or brick: deep soil, dry climate.
    Cob,
    /// Thatch: grassland and wet — roofs and floors only.
    Thatch,
}
```

- [ ] **Step 4: Run the tests**

Run: `cargo test -p hornvale-worldgen --test lantern_fabric -- --nocapture`
Expected: PASS, **or a reported H1 falsification.** If H1 fails, STOP and report — do not widen the threshold, do not add an axis without saying so. A falsified prediction is the headline of several merged campaigns.

- [ ] **Step 5: Mutation-prove H1**

Temporarily make `reflectance_of(Fabric::Stone, ..)` ignore `ctx` and return a constant `Reflectance`. Run the tests.
**Expected: both H1 tests FAIL, naming their own assertions** — a derived-stone claim that survives a constant stone is vacuous. Revert; confirm green. Record both observations in the commit body.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add windows/vessel/src/fabric.rs windows/vessel/src/lib.rs \
        windows/worldgen/tests/lantern_fabric.rs
git commit -m "feat(vessel): a built cell has a fabric, derived from its ground

H1 measured on real terrain across 8 seeds, not on authored buffers.
Mutation-proven: a constant stone reflectance reddens both H1 assertions."
```

---

### Task 4: The light field

Spec §4.1, §4.2, §4.3. Reach is the shipped symmetric `shadowcast`; the sum is the kernel's declared additive law.

**Files:**
- Create: `windows/vessel/src/light.rs`
- Modify: `windows/vessel/src/lib.rs`
- Test: `windows/vessel/src/light.rs` (`mod tests`) + `windows/worldgen/tests/lantern_light.rs`

**Interfaces:**
- Consumes: `crate::lattice::{Cell, CellKind, Lattice, sight::shadowcast}`, `hornvale_kernel::color::{BANDS, Illuminant, blackbody}`.
- Produces:
  - `pub struct Source { pub at: Cell, pub illuminant: Illuminant, pub radius: i32 }`
  - `pub fn light_field(lattice: &Lattice, sources: &[Source]) -> BTreeMap<Cell, Illuminant>`
  - `pub fn attenuate(illuminant: &Illuminant, distance: f64) -> Illuminant`
  - `pub const TORCH_KELVIN: f64 = 1900.0;` `pub const HEARTH_KELVIN: f64 = 1200.0;`

- [ ] **Step 1: Write the failing tests**

```rust
/// The additive law, which the kernel declared and deferred to this campaign
/// BY NAME (`color.rs`: "two torches on one wall — sum the illuminants, not
/// the reflectances"). A POSITIVE CONTROL: two sources must make a cell
/// strictly brighter than either alone, in every band.
///
/// FIRES WHEN: the sum is replaced by a max, an average, or a
/// last-writer-wins — all of which look plausible and none of which is the
/// declared law.
#[test]
fn two_sources_are_strictly_brighter_than_either_alone() {
    let lattice = open_room(9, 9);
    let a = Source { at: Cell(2, 4), illuminant: blackbody(1900.0), radius: 8 };
    let b = Source { at: Cell(6, 4), illuminant: blackbody(1900.0), radius: 8 };
    let both = light_field(&lattice, &[a.clone(), b.clone()]);
    let just_a = light_field(&lattice, &[a]);
    let just_b = light_field(&lattice, &[b]);
    let mid = Cell(4, 4);
    for band in 0..BANDS {
        let s = both[&mid].get()[band];
        assert!(
            s > just_a[&mid].get()[band] && s > just_b[&mid].get()[band],
            "band {band} at the midpoint is not strictly brighter under both sources"
        );
    }
}

/// Attenuation is the ONLY thing producing a gradient under the implicit
/// torch, because shadowcast is symmetric and the torch rides on the
/// observer — so the lit set IS the FOV set (spec §4.2). If this is flat,
/// the whole pane is uniformly lit and H4a can never read anything.
///
/// FIRES WHEN: attenuation is made distance-independent.
#[test]
fn light_falls_off_with_distance() {
    let lattice = open_room(15, 3);
    let field = light_field(&lattice, &[Source {
        at: Cell(1, 1), illuminant: blackbody(1900.0), radius: 14,
    }]);
    let near = field[&Cell(2, 1)].get()[5];
    let far = field[&Cell(12, 1)].get()[5];
    assert!(far < near, "far cell {far} is not dimmer than near cell {near}");
}

/// A wall blocks light exactly as it blocks sight, because it is the same
/// call. This is the claim that "light needs no new geometry" rests on.
///
/// FIRES WHEN: light_field stops routing through shadowcast and starts
/// computing its own reach.
#[test]
fn a_wall_casts_a_light_shadow() {
    let lattice = room_with_a_wall_pillar();
    let field = light_field(&lattice, &[Source {
        at: Cell(1, 4), illuminant: blackbody(1900.0), radius: 12,
    }]);
    assert!(
        !field.contains_key(&Cell(8, 4)),
        "the cell directly behind the pillar received light"
    );
}

/// An unlit cell has NO illuminant — not a dark one. `illuminant x
/// reflectance x observer` then correctly yields nothing, which is what
/// makes H4 reachable at the model level at all.
///
/// FIRES WHEN: absent cells are filled with a zero illuminant, which reads
/// the same on screen and is a different model.
#[test]
fn an_unreached_cell_is_absent_not_zero() {
    let lattice = open_room(20, 3);
    let field = light_field(&lattice, &[Source {
        at: Cell(1, 1), illuminant: blackbody(1900.0), radius: 3,
    }]);
    assert!(!field.contains_key(&Cell(18, 1)));
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-vessel --lib light`
Expected: FAIL — `light_field` does not exist.

- [ ] **Step 3: Implement**

`light_field` runs `shadowcast(lattice, source.at, source.radius)` per source, attenuates each reached cell by its Chebyshev distance from the source, and **sums band-wise** into a `BTreeMap<Cell, Illuminant>`. Attenuation is `1/(1 + d²)`.

```rust
/// Distance falloff. **Not a free parameter** (spec §4.2): because
/// `shadowcast` is symmetric and the implicit torch rides on the observer,
/// every visible cell is lit by construction, so this is the sole source of
/// light gradient in a possessed chamber. H4a rides on it directly, and it
/// may NOT be tuned after unblinding (§11 risk 2).
/// type-audit: bare-ok(ratio: distance), bare-ok(ratio: return)
pub fn attenuate(illuminant: &Illuminant, distance: f64) -> Illuminant { /* ... */ }
```

- [ ] **Step 4: Run the tests**

Run: `cargo test -p hornvale-vessel --lib light`
Expected: PASS.

- [ ] **Step 5: Mutation-prove the additive law**

Replace the band-wise sum with `f64::max`. Run.
**Expected: `two_sources_are_strictly_brighter_than_either_alone` FAILS**, and no other test does — a max passes every non-positive-control check. Revert; confirm green.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add windows/vessel/src/light.rs windows/vessel/src/lib.rs
git commit -m "feat(vessel): light is a derived view over the shipped shadowcaster

shadowcast is symmetric, so what can see a cell and what lights it are the
same set: light needs no new geometry. The sum is the kernel's declared
additive law, arriving with the campaign it was deferred to by name.

Mutation-proven: replacing the sum with a max reddens the positive control
and nothing else."
```

---

### Task 5: The hearth gets a cell

**Discovered at planning (ledger #19), not in the spec.** `Cell` appears nowhere in `windows/vessel/src/interior/`: the interior model is topological, the lattice is spatial, and nothing joins them. So of §4.2's three sources only the torch (`inside.cell`) and the doorway (`Lattice::doorways`) are placeable today.

`CellKind::Wall`'s own doc already states the intent — *"a place in its own right — an alcove, a screen or **a fireplace** is an anchor AT one of these"* — so the model always meant a hearth to sit at a wall cell; only the join was never built.

**Files:**
- Modify: `windows/vessel/src/light.rs`
- Test: `windows/vessel/src/light.rs` (`mod tests`)

**Interfaces:**
- Produces: `pub fn hearth_cell(lattice: &Lattice, chamber: usize) -> Option<Cell>`

- [ ] **Step 1: Write the failing tests**

```rust
/// A hearth sits at a WALL cell of its own chamber — the placement
/// `CellKind::Wall`'s doc already describes ("a fireplace is an anchor AT
/// one of these"). Not the centroid: a fire in the middle of the floor is a
/// campfire, and this is a built interior.
///
/// FIRES WHEN: the chosen cell is passable, or belongs to another chamber.
#[test]
fn a_hearth_sits_at_a_wall_of_its_own_chamber() {
    let lattice = two_chamber_house();
    let cell = hearth_cell(&lattice, 0).expect("chamber 0 has walls");
    assert_eq!(lattice.cells.get(&cell), Some(&CellKind::Wall));
    assert!(walls_adjacent_to_chamber(&lattice, 0).contains(&cell));
}

/// DERIVED, never drawn. A window that draws has quietly become a domain
/// with no registry entry and no pin-isolation test
/// (`windows/CLAUDE.md`) — and this campaign declares no stream label at
/// all (spec §5).
///
/// FIRES WHEN: the choice becomes seed-dependent or order-dependent.
#[test]
fn the_hearth_cell_is_a_pure_function_of_the_lattice() {
    let lattice = two_chamber_house();
    let a = hearth_cell(&lattice, 0);
    let b = hearth_cell(&lattice.clone(), 0);
    assert_eq!(a, b);
}

/// A chamber with no wall of its own yields None rather than a wall
/// belonging to somebody else. Fail loudly, never guess.
#[test]
fn a_chamber_with_no_wall_of_its_own_has_no_hearth_cell() {
    assert_eq!(hearth_cell(&wall_less_lattice(), 0), None);
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-vessel --lib light::tests::a_hearth`
Expected: FAIL — `hearth_cell` does not exist.

- [ ] **Step 3: Implement**

Collect the wall cells orthogonally adjacent to a `Floor(chamber)` cell, sort with a total order (`Cell`'s `Ord`), and take the first. Deterministic, pure, no draw.

- [ ] **Step 4: Run the tests** — Expected: PASS.

- [ ] **Step 5: Mutation-prove determinism**

Change the selection to take the *last* wall cell instead of the first. **Expected: `a_hearth_sits_at_a_wall_of_its_own_chamber` still passes** (both are walls) **while nothing else fails** — which shows the determinism test is checking repeatability, not stability across implementations. Then add the missing pin: assert the exact `Cell` for `two_chamber_house()`. Re-run under the mutation; **it must now fail**. Revert.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add windows/vessel/src/light.rs
git commit -m "feat(vessel): a hearth is at a wall, and now has the cell to prove it

Found at planning: Cell appears nowhere in windows/vessel/src/interior/ —
the interior model is topological, the lattice is spatial, and nothing
joined them, so AnchorKind::Hearth could not be positioned as a light.
CellKind::Wall's doc already said a fireplace is an anchor at a wall; this
builds the join it described. Derived, never drawn."
```

---

### Task 6: The palette fills — the whole seam, end to end

Spec §3, §6 H2. **This is where the campaign becomes visible**, and the test drives the entire path in one go: fabric → light → `sense` → `to_srgb` → `PaletteEntry.color`.

**A structural note the spec does not cover.** `plan_of` interns the palette on `CellKind` alone, so every wall in a chamber shares one entry — a per-cell light gradient **cannot be expressed**. Widen the intern key to `(CellKind, Option<[u8; 3]>)`. This needs no schema change: the palette is already an intern table and the client keys on the index. The `u8` triple is itself the quantization, so the palette stays bounded (extent is 19×19 = 361 cells, and radial attenuation collapses to far fewer distinct colours). **Measure the resulting palette size rather than assuming it.**

**Files:**
- Modify: `windows/vessel/src/plan.rs:188-275`
- Modify: `windows/vessel/src/session.rs:770-780`
- Modify: `windows/vessel/tests/fixtures/snapshot-seed-42-chamber.json`, `snapshot-seed-1-chamber-occupied.json`, `session-seed-42.json`
- Test: `windows/worldgen/tests/lantern_seam.rs`

- [ ] **Step 1: Write the failing tests**

```rust
/// THE WHOLE SEAM IN ONE TEST. Unit tests at every node with none on the
/// path is the shape that hid The Beholding's `sightOf` defect: a `return
/// null` left 59 of 59 tests green. This drives fabric -> light -> sense ->
/// to_srgb -> PaletteEntry.color and asserts on the LAST value.
///
/// FIRES WHEN: any link in the chain silently returns None or a default.
#[test]
fn the_chamber_palette_carries_a_colour_derived_through_the_whole_seam() {
    let plan = chamber_plan_at_seed(42);
    let coloured = plan.palette.iter().filter(|e| e.color.is_some()).count();
    assert!(coloured > 0, "no palette entry carries a colour: the seam is broken");
    let walls: Vec<_> = plan.palette.iter().filter(|e| e.kind == "wall").collect();
    assert!(
        walls.iter().any(|e| e.color.is_some()),
        "walls have a fabric and a light but no colour"
    );
}

/// H2 — a hearth-lit cell and a doorway-lit cell in the same room differ.
/// Measured on DERIVED fabric, not the authored limestone the spec's probe
/// used; that the probe showed it on limestone is why this claim is about
/// SURVIVAL, not about existence.
///
/// FIRES WHEN: the two lights collapse to one colour on real stone.
#[test]
fn h2_hearth_light_and_doorway_light_differ_on_derived_fabric() {
    let (hearth_lit, doorway_lit) = two_lit_cells_in_one_room(42);
    assert_ne!(hearth_lit, doorway_lit, "1200 K and daylight render the same stone identically");
}

/// A threshold has no fabric — an opening is not a material, which The
/// Beholding already established. Its colour stays absent.
///
/// FIRES WHEN: a threshold picks up its neighbour's fabric.
#[test]
fn a_threshold_carries_no_colour() {
    let plan = chamber_plan_at_seed(42);
    for entry in plan.palette.iter().filter(|e| e.kind == "threshold") {
        assert_eq!(entry.color, None, "a threshold was given a fabric colour");
    }
}

/// The palette must stay an INDEX, not degenerate into one entry per cell.
/// Reported, then pinned — this number was measured, not assumed.
#[test]
fn the_palette_stays_bounded_after_interning_on_colour() {
    let plan = chamber_plan_at_seed(42);
    eprintln!("palette entries: {} over {} cells", plan.palette.len(), plan.cells.len());
    assert!(
        plan.palette.len() < plan.cells.len() / 4,
        "palette has {} entries for {} cells — interning has stopped working",
        plan.palette.len(),
        plan.cells.len()
    );
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-worldgen --test lantern_seam`
Expected: FAIL — palette colours are all `None`.

- [ ] **Step 3: Implement**

Thread a `FabricContext` and the light field into `plan_of`; widen the intern key to `(CellKind, Option<[u8; 3]>)`; `entry_for` gains the colour. Update `PaletteEntry::color`'s doc comment — **it explains why the slot was empty; rewrite it to say what fills it, do not delete it.**

- [ ] **Step 4: Run the tests, then re-pin the fixtures IN THIS COMMIT**

Run:
```bash
cargo test -p hornvale-worldgen --test lantern_seam -- --nocapture
REBASELINE=1 HV_TEST_OK=1 cargo nextest run -p hornvale-vessel -E 'test(the_client_fixtures_are_current)'
git diff --stat windows/vessel/tests/fixtures/
```
Expected: the seam tests PASS and the chamber fixtures move. **Re-pin here, in the drifting commit, never at the close** (§11 risk 3). Read the fixture diff before accepting it: a colour that is `[0,0,0]` everywhere is a broken seam that a re-pin would freeze.

- [ ] **Step 5: Mutation-prove the seam**

Make `light_field` return an empty map. Run the seam tests.
**Expected: `the_chamber_palette_carries_a_colour_derived_through_the_whole_seam` FAILS** — not a compile error, a behavioural red. Then separately make `fabric_of` return `None` always; **expected: the same test fails again**. Both halves must be individually load-bearing. Revert; confirm green.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add windows/vessel/src/plan.rs windows/vessel/src/session.rs \
        windows/vessel/tests/fixtures/ windows/worldgen/tests/lantern_seam.rs
git commit -m "feat(vessel): PaletteEntry.color fills

The slot The Beholding shipped deliberately empty, filled by the campaign it
named as the filler. Interning widens from CellKind to (CellKind, colour) so
a per-cell light gradient can be expressed at all; the u8 triple is its own
quantization, so the palette stays an index.

Fixtures re-pinned in this commit, not at the close. Mutation-proven from
both ends: an empty light field and a fabric-less cell each redden the
whole-seam assertion."
```

---

### Task 7: The scotopic term

Spec §4.4, §6 H3/H4/H4a. Below an authored photopic threshold the achromatic channel contributes **equally to all three output slots** — grey sight, which is what night vision is.

**Files:**
- Modify: `kernel/src/color.rs` (`to_srgb`)
- Test: `kernel/src/color.rs` (`mod tests`) + `windows/worldgen/tests/lantern_night.rs`

- [ ] **Step 1: Write the failing tests**

```rust
/// H3 — the scotopic term is EXACTLY zero in daylight, so every colour The
/// Beholding emits is unchanged. A REQUIREMENT with a test, not a
/// prediction. Bit-exact deliberately: an epsilon assertion would pass
/// through the drift it exists to catch.
///
/// FIRES WHEN: the blend leaks above the photopic threshold.
#[test]
fn h3_the_scotopic_term_is_exactly_zero_in_daylight() {
    let observer = human();
    let refl = mid_grey();
    let light = blackbody(5800.0);
    let before = observer.to_srgb(&observer.sense(&refl, &light));
    // after the term ships, recompute — must be identical
    assert_eq!(before, DAYLIGHT_GREY_PINNED);
}

/// The rod carries NO hue. Contributing equally to R, G and B, it can never
/// shift one — so a scotopic contribution must leave the three slots equal
/// where the photopic signal is gone.
///
/// FIRES WHEN: the rod is wired into one slot, or weighted per channel.
#[test]
fn the_rod_can_never_shift_a_hue() {
    let observer = kobold();
    let [r, g, b] = observer.to_srgb(&observer.sense(&mid_grey(), &very_dim())).unwrap();
    assert_eq!((r, g), (g, b), "a rod-only signal produced a hue: [{r}, {g}, {b}]");
}

/// H4 — stated at the MODEL level deliberately (spec §6). Below the
/// threshold a human emits [0,0,0] where a kobold does not.
///
/// FIRES WHEN: night vision still cannot reach the screen — the measured
/// pre-campaign state, where both eyes went black together while their rod
/// signals differed (0.0003 vs 0.0004).
#[test]
fn h4_a_rod_dominant_eye_sees_where_a_human_does_not() {
    let light = scaled(&blackbody(1900.0), 0.001);
    let human_rgb = human().to_srgb(&human().sense(&limestone(), &light)).unwrap();
    let kobold_rgb = kobold().to_srgb(&kobold().sense(&limestone(), &light)).unwrap();
    assert_eq!(human_rgb, [0, 0, 0], "the human is not actually in the dark");
    assert_ne!(kobold_rgb, [0, 0, 0], "H4 FALSIFIED: the kobold is blind too");
}
```

And in `windows/worldgen/tests/lantern_night.rs`, **H4a — a reading, not a claim**:

```rust
/// H4a — REPORTED, NOT PREDICTED (spec §6). How dark does a chamber cell
/// actually get under the implicit torch? Because shadowcast is symmetric
/// and the torch rides on the observer, the lit set IS the FOV set, so only
/// attenuation darkens anything — and chambers are small.
///
/// This test asserts only that the reading was TAKEN. It may report that
/// H4's regime is unreachable on the chamber band; that is a finding about
/// where the campaign's drama lives, not a failure. THE ATTENUATION
/// CONSTANT MAY NOT BE TUNED TO CHANGE THIS NUMBER (§11 risk 2).
#[test]
fn report_h4a_how_dark_a_chamber_gets() {
    let dimmest = dimmest_visible_cell_across_seeds(&[1, 42, 99, 256]);
    eprintln!("H4a: dimmest visible chamber cell renders {dimmest:?}");
    assert!(dimmest.is_some(), "no chamber produced a visible cell at all");
}
```

- [ ] **Step 2: Run to verify they fail** — Run: `cargo test -p hornvale-kernel color`. Expected: FAIL on `the_rod_can_never_shift_a_hue` and `h4_...`; `h3_...` PASSES already (it pins the status quo, which is the point).

- [ ] **Step 3: Implement the term in `to_srgb`**

- [ ] **Step 4: Run everything colour-touching**

```bash
HV_TEST_OK=1 cargo nextest run -p hornvale-kernel -p hornvale-vessel -p hornvale-worldgen --no-fail-fast 2>&1 | tee /tmp/hv-lantern-t7.txt
```
Expected: PASS, including `the_client_fixtures_are_current` — H3 says the surface is untouched.

- [ ] **Step 5: Mutation-prove H3 in the direction that matters**

Set the photopic threshold high enough that daylight enters the blend. **Expected: `h3_...` AND `the_client_fixtures_are_current` both FAIL.** A scotopic term that cannot perturb daylight even when mis-tuned is not being tested. Revert; confirm green.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add kernel/src/color.rs windows/worldgen/tests/lantern_night.rs
git commit -m "feat(kernel): night vision reaches the screen

The rod was ChannelRole::Achromatic, so no projection read it and night
vision could not reach the screen at all — a human and a kobold went black
together while their rod signals differed. to_srgb now blends the
achromatic channel equally into all three slots below an authored photopic
threshold: grey sight, which is what night vision is.

H3 holds bit-exact: zero in daylight, mutation-proven by raising the
threshold until the fixtures redden."
```

---

### Task 8: The lens

Spec §7. **Built last, deliberately.** §6's claims read unlensed colour; a saturation boost would hide exactly the H1 failure the campaign must be able to detect.

**Files:**
- Create: `windows/vessel/src/lens.rs`
- Modify: `cli/src/main.rs` (the flag), `windows/vessel/src/session.rs`

- [ ] **Step 1: Write the failing tests**

```rust
/// The lens is ONE-WAY. It transforms the emitted triple and nothing else —
/// never the illuminant, never the reflectance, never a fact. Brightening an
/// illuminant changes the world; brightening an output changes the picture.
///
/// FIRES WHEN: the lens is applied before `sense` rather than after.
#[test]
fn the_lens_never_touches_the_illuminant_or_the_reflectance() {
    let before = light_field_snapshot(42);
    let _ = lens::apply(&Lens::default(), [12, 34, 56]);
    assert_eq!(light_field_snapshot(42), before);
}

/// Disclosable and defeatable (RENDER-9). An unlensed mode is what makes
/// this a lens rather than a lie.
///
/// FIRES WHEN: the lens cannot be turned off.
#[test]
fn the_lens_can_be_declined() {
    assert_eq!(lens::apply(&Lens::Off, [12, 34, 56]), [12, 34, 56]);
}

/// Lensed colour must never land in a committed artifact — if it did, the
/// lens parameters would become a save-format-class contract. Screen only.
///
/// FIRES WHEN: a plan or snapshot is built with the lens applied.
#[test]
fn a_committed_snapshot_carries_unlensed_colour() {
    let a = chamber_plan_with_lens(42, Lens::Off);
    let b = chamber_plan_with_lens(42, Lens::default());
    assert_eq!(a.palette, b.palette, "the lens reached a serialized artifact");
}
```

- [ ] **Step 2: Run to verify they fail** — Expected: FAIL, `lens` does not exist.
- [ ] **Step 3: Implement** — a pure `[u8; 3] -> [u8; 3]` transform plus a CLI flag.
- [ ] **Step 4: Run the tests, and confirm the fixtures did NOT move** — `git diff --exit-code windows/vessel/tests/fixtures/` must be clean.
- [ ] **Step 5: Mutation-prove the artifact boundary** — apply the lens inside `plan_of`. **Expected: `a_committed_snapshot_carries_unlensed_colour` AND `the_client_fixtures_are_current` both FAIL.** Revert.
- [ ] **Step 6: Commit.**

---

### Task 9: Close

- [ ] **Step 1: Absorb main** — `make preflight` from the branch; on an ancestry NO-GO merge main INTO the branch and re-run. Then **regenerate artifacts and re-read the diff** — a conflict-free merge of a generated file is silently wrong.
- [ ] **Step 2: Re-run the readout after the absorption.** A confirmed prediction has a shelf life; H1 and H2 must be re-read against post-absorption physics.
- [ ] **Step 3: The full gate** — `make gate` (budget `timeout: 3600000`; it measures 22–37 min in a worktree). Then the four checks the gate does **not** run: `make vessel-check`, `make world-check`, `make census-check`, `shellcheck`.
- [ ] **Step 4: Artifact freshness** — `make rebaseline`, then `git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/`. `docs/audits/` drifts on any pub-boundary change and is the common miss.
- [ ] **Step 5: The book** — a chronicle entry in `book/src/chronicle/`, a freshness sweep of stale chapters, and a re-score of any Confidence Gradient bet this campaign moved.
- [ ] **Step 6: The registry** — flip `MAP-building-fabric`, `MAP-interior-light` and `RENDER-emission-term` to their new statuses and repoint **Where**. **Repointing REPLACES a row's prose; it does not append.**
- [ ] **Step 7: The retrospective** — `docs/retrospectives/the-lantern.md`. **Promote the decision ledger's findings before the worktree is torn down** — `.superpowers/sdd/` is git-ignored and dies with it.
- [ ] **Step 8: G6** — present the post-G3 ledger digest to Nathan. Merge is his hard stop.

---

## Self-Review

**Spec coverage.** §3 → Task 3. §4.1/§4.2/§4.3 → Task 4 (+ Task 5 for the gap §4.2 did not know about). §4.4 → Task 7. §5.1 → Task 1. §5.2 → Task 2. §6 H1 → Task 3; H2 → Task 6; H3/H4 → Task 7; H4a → Task 7. §7 → Task 8. §8 testing rules → folded into every task's mutation step. §9 out-of-scope → nothing here reaches the underworld, roads, or a real emission term. §11 risks → risk 1 measured in Task 3 (early, as required); risk 2 named in Tasks 4 and 7; risk 3 discharged in Task 6; risk 4 checked at G3; risk 5 discharged in Task 2 Step 5.

**Placeholder scan.** No TBDs. `EXPECTED_5772_BITS` is filled from a real run in Task 1 Step 2, which is a capture step, not a placeholder.

**Type consistency.** `blackbody(f64) -> Illuminant` is used identically in Tasks 2, 4, 6, 7. `Source { at, illuminant, radius }` is consistent across Tasks 4 and 5. `fabric_of` returns `Option<Fabric>` in Task 3 and is consumed as an `Option` in Task 6. `PaletteEntry.color` stays `Option<[u8; 3]>` throughout — no schema change.

**Known gap, deliberately left.** The `FabricContext` construction site in `session.rs` needs the settlement's `CellId`, resolved through the same dominant-corner path `LocaleContext` uses for biome. Task 3 names the constraint (fabric and biome must not disagree about which cell a settlement sits on) but not the exact call, because the exact call was not read at planning time. **The implementer must read it rather than infer it** — this is the one place in the plan where a signature is described instead of quoted.
