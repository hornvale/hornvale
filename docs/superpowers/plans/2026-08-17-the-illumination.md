# The Illumination Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the chart's colour mean the surface a character can see, its
glyph mean how hard the ground is to cross, and its weight mean how sure the
character is — then orient it to north and draw the seam cells for the first
time.

**Architecture:** Three stages. Stage 1 changes what the *existing* `color`
field means, composing a surface mixture in `windows/locale` (above the
domain layer, where climate and terrain legally meet) and moving epistemic
encoding from glyph-substitution to weight — no wire change at all. Stage 2
makes one batched **additive** change to `scene/surrounds/v2`, adding the
post-eye `Signal`, its calibration, a nominal cover class, and per-cell
bearing/distance. Stage 3 uses that position to project north-up across all
three renderers.

**Tech Stack:** Rust 2024 (workspace: `kernel`, `domains/*`, `windows/*`,
`cli`), TypeScript/Deno (`clients/vessel`), Rust-outside-workspace
(`clients/game/core`). Dependencies limited to `serde`, `serde_json`, `libm`.

**Spec:** `docs/superpowers/specs/2026-08-17-the-illumination-design.md` —
read it alongside this plan. Every task argues from a numbered section there.

## Global Constraints

- **Dependencies are frozen.** `serde`, `serde_json`, `libm` only. No new
  crates. Randomness comes from the kernel's `Seed`/`Stream`.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only, enforced by
  `clippy.toml`. Float sorting uses `total_cmp` with deterministic tie-breaks.
- **No wall-clock time.** Time is `WorldTime { day: f64 }`. `Instant` is
  banned in tests too.
- **Quantize at emit only**, never in the compute path.
- **`#![warn(missing_docs)]`** — every public item, field and variant gets a
  one-line doc comment.
- **`type-audit:` tags go on the struct's doc comment, never on a field's.**
- **`cargo fmt` is the final step before every commit.** Skipped fmt is this
  project's most common review finding.
- **No epoch.** No new stream label, no new draw, no world moves. If a task
  appears to need one, stop and escalate — that is a scope error.
- **`domains/terrain` is not edited by this campaign.** If a task seems to
  require it, re-read spec §0(a); the layering rule is enforced by
  `cli/tests/architecture.rs::domains_depend_only_on_the_kernel`.
- **Commit messages go to a file, then `git commit -F`.** Never
  `git commit -m "$(cat <<'EOF' …)"` — it has corrupted three messages in one
  campaign. Use `git commit -F <file> -- <paths>`, never a bare `git commit`,
  which takes the whole index.
- **A green `gate-commit` says nothing about a test you just wrote** (spec
  §9.1). New tests are absent from the sub-floor roster and are therefore
  compiled and not run. Always run new tests directly:
  `cargo test -p <crate> --test <name>`.

---

## File Structure

**Stage 1 — no wire change**

| File | Responsibility |
|---|---|
| `windows/locale/src/surface.rs` *(new)* | The surface cover model: endmember curves, `CoverClass`, and the weighting from climate. One file because these three change together and nothing else needs them. |
| `windows/locale/src/lib.rs` | `reflectance_at` keeps the `Mixture`, gains a `WorldTime`, and composes through `surface.rs`. |
| `windows/scene/src/surrounds_ascii.rs` | Epistemic moves to weight (`faded()` deleted); the ordinal becomes impedance; the disclosure sentence changes. |
| `cli/src/main.rs:75` | Help text stops saying "bedrock". |
| `clients/vessel/src/pane_chart.ts`, `pane_cell.ts` | Comments and the `ground` rule stop saying "bedrock". |

**Stage 2 — one additive shape move**

| File | Responsibility |
|---|---|
| `kernel/src/room.rs` | `RoomAddr::distance_rad_to` — great-circle angular distance. |
| `windows/scene/src/surrounds.rs` | `SurroundsCell` gains four fields; `Sight` gains the calibration; `cover_legend` on the scene. |
| `windows/locale/src/surface.rs` | `cover_class_at` — the nominal coarsening. |

**Stage 3 — north-up**

| File | Responsibility |
|---|---|
| `scripts/regenerate-artifacts.sh` | Writes the chart reference fixture from the sim's own renderer. |
| `clients/game/core/tests/fixtures/` | Holds that generated reference. |
| `windows/scene/src/surrounds_ascii.rs`, `clients/game/core/src/chart.rs`, `clients/vessel/src/pane_chart.ts` | The three projections. |

---

# STAGE 1 — the vocabulary and what colour means

## Task 1: Probe before designing

**Files:**
- Create: `windows/scene/examples/illumination_probe.rs`

**Why `windows/scene`, not `windows/locale`:** `windows/scene/Cargo.toml`
declares `hornvale-locale`, and locale declares no dependency on scene. A
probe that builds a `SurroundsScene` therefore cannot live in `locale` —
it would be a layering inversion that does not compile. `windows/scene`
already carries six examples; `profile_scene.rs` is the closest model.

**Interfaces:**
- Consumes: nothing.
- Produces: measured answers that Tasks 2b and 4 consume. **No other task
  starts until this reports.**

This task writes no production code. It answers spec §6.1–§6.3, each of which
is a **branch table, not a prediction** — record what you actually see.

- [ ] **Step 1: Write the probe as an example**

```rust
//! Illumination task-1 probe. Answers spec §6.1, §6.2, §6.3 with real
//! output, and captures H1's bedrock baseline. Not a test: it measures,
//! it does not assert.
//!
//! Run: `cargo run -p hornvale-scene --example illumination_probe`
use hornvale_worldgen::build_world;

fn main() {
    // Verified signatures — use these, do not invent:
    //   build_world(seed: Seed, pins: &SkyPins, sky: SkyChoice,
    //               terrain_pins: &TerrainPins,
    //               settlement_pins: &SettlementPins) -> Result<World, _>
    //       (windows/worldgen/src/lib.rs:7922)
    //   hornvale_locale::LocaleContext::build(&world) -> Result<_, _>
    //       (as windows/scene/tests/golden.rs:227 does it)
    // Copy the construction from `windows/scene/tests/golden.rs` rather
    // than inventing one; `examples/profile_scene.rs` is the closest
    // example-shaped model.

    // §6.1 — does seed 42 have a visible season at its high ground?
    // For the highest-relief land cells reachable in a walk band, print
    // temperature_at(cell, day) and is_frozen_at(cell, day) at 8 days
    // evenly spaced across one year.

    // §6.2 — how many callers does reflectance_at have?
    // (answered by grep, not here — record the count in the report)

    // §6.3 — is `color` populated in the committed fixtures?
    // Print whether the scene built by the committed path carries Some(color).

    // §7/H1 BASELINE — the bedrock-era distinct-colour count.
    // THIS IS THE ONLY MOMENT IT CAN BE TAKEN: every later task changes
    // colour, and the pre-change value is unrecoverable afterwards.
    // Over the seed-42 walk band at the shipped radius, print:
    //   cells.len()
    //   cells.iter().map(|c| c.color).collect::<BTreeSet<_>>().len()
    // Define that band ONCE here, in a `pub fn baseline_band(..)` this file
    // exports, because Task 6 must count over the IDENTICAL population —
    // a floor measured on a different cell set is vacuous.
}
```

- [ ] **Step 2: Run it and capture real output**

Run: `cargo run -p hornvale-locale --example illumination_probe 2>&1 | tee /tmp/hv-illum-probe.txt`

Do not summarise from memory — read the file.

- [ ] **Step 3: Record the branch taken for each question**

Write `/tmp/hv-illum-probe-report.md` answering each with the observed
numbers and which branch of the spec's table it lands in:

- §6.1: *frozen fraction varies* → H2 proceeds. *Constant* → widen to a seed
  sweep before concluding; if unobservable everywhere, ship snow as an annual
  term and say so in the chronicle. *`is_frozen_at` constant by construction*
  → read its implementation; a constant is a scope error, not a hard
  measurement.
- §6.2: *one caller* → change in place. *Several* → thread the day and delete
  the old signature; do not add an overload to avoid an edit.
- §6.3: *fixtures carry `color`* → the value-only shapecheck argument holds.
  *Uncoloured* → a coloured fixture is needed before anything is measurable;
  that is a task, not a surprise.

- [ ] **Step 4: Commit the probe**

```bash
git add windows/locale/examples/illumination_probe.rs
git commit -F /tmp/msg.txt -- windows/locale/examples/illumination_probe.rs
```

**STOP. Report the three answers before Task 2a begins.**

---

## Task 2a: Stop integrating early — a byte-identical refactor

**Files:**
- Modify: `windows/locale/src/lib.rs:599-611`
- Test: `windows/locale/tests/surface_mixture.rs` *(new)*

**Interfaces:**
- Consumes: `hornvale_terrain::lithology::reflectance(&MaterialBuffer, RockClass) -> Mixture`, `Mixture::integrate() -> Reflectance`.
- Produces: `LocaleContext::reflectance_mixture_at(&self, addr: &RoomAddr) -> Result<Mixture, LocaleError>`. `reflectance_at` keeps its current signature and delegates.

**This task must change no bytes.** It is the positive control for Task 2b:
it separates "did I disturb the mineral path" from "did the cover weighting
do what I want" (spec §3.2, ledger #10).

- [ ] **Step 1: Write the failing test**

```rust
//! The mixture-keeping refactor must not move a single colour.
use hornvale_kernel::RoomId;

#[test]
fn integrating_the_kept_mixture_equals_integrating_immediately() {
    // Build the same LocaleContext the scene tests build.
    // For a spread of at least 200 room addresses:
    //   let via_mixture = ctx.reflectance_mixture_at(&addr).unwrap().integrate();
    //   let direct      = ctx.reflectance_at(&addr).unwrap();
    //   assert_eq!(via_mixture, direct, "addr {addr:?} moved");
    // Reflectance derives PartialEq, so this is exact, not approximate.
}
```

- [ ] **Step 2: Run it to verify it fails**

Run: `cargo test -p hornvale-locale --test surface_mixture -- --nocapture`
Expected: FAIL — `no method named reflectance_mixture_at`.

- [ ] **Step 3: Implement**

```rust
    /// The surface mixture at `addr`, un-integrated, so a caller can reach
    /// the components. [`LocaleContext::reflectance_at`] is this, integrated.
    pub fn reflectance_mixture_at(
        &self,
        addr: &RoomAddr,
    ) -> Result<hornvale_kernel::color::Mixture, LocaleError> {
        let geo = self.climate.geosphere();
        let weights = addr
            .corner_weights(geo, &self.index)
            .ok_or(LocaleError::AboveGrid)?;
        let cell = dominant_corner(&weights).0;
        let buffer = self.terrain.material_at(cell);
        let rock = self.terrain.rock_at(cell);
        Ok(hornvale_terrain::lithology::reflectance(&buffer, rock))
    }

    /// The integrated reflectance at `addr`.
    pub fn reflectance_at(
        &self,
        addr: &RoomAddr,
    ) -> Result<hornvale_kernel::color::Reflectance, LocaleError> {
        Ok(self.reflectance_mixture_at(addr)?.integrate())
    }
```

- [ ] **Step 4: Run the test**

Run: `cargo test -p hornvale-locale --test surface_mixture`
Expected: PASS.

- [ ] **Step 5: Prove no artifact moved — the real control**

Run:
```bash
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```
Branch table:
- *Empty diff* → expected; the refactor is byte-identical. Proceed.
- *`docs/audits/` moved* → a `pub` boundary changed (it did — you added a
  method). Regenerate the type-audit report and commit it in **this** commit.
- *Anything under `book/src/gallery/` moved* → **STOP.** The refactor was not
  byte-identical and Task 2b's control is void until it is.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add windows/locale/src/lib.rs windows/locale/tests/surface_mixture.rs docs/audits/
git commit -F /tmp/msg.txt -- windows/locale/src/lib.rs windows/locale/tests/surface_mixture.rs docs/audits/
```

---

## Task 2b: The surface mixture

**Files:**
- Create: `windows/locale/src/surface.rs`
- Modify: `windows/locale/src/lib.rs` (`reflectance_mixture_at` gains a day; `mod surface;`)
- Test: `windows/locale/tests/surface_mixture.rs` (extend)

**Interfaces:**
- Consumes: Task 2a's `reflectance_mixture_at`; `GeneratedClimate::{temperature_at, is_frozen_at, snow_fraction_at, moisture_at, biome_expr_at}`; `Mixture::{components, weights, new}`.
- Produces: `surface::cover_weights(climate, cell, at) -> Vec<(Reflectance, f64)>` and `surface::endmembers::{CHLOROPHYLL, LITTER, SNOW, SAND, SILT}: [f64; BANDS]`. Task 9 consumes `surface::cover_class_at`.

- [ ] **Step 1: Write the endmember module**

Follow `domains/terrain/src/lithology.rs::endmembers` exactly — authored
`[f64; BANDS]` curves with a `type-audit: bare-ok(ratio)` tag each.
`BANDS = 10` (`kernel/src/color.rs:27`).

```rust
/// Visible-band reflectance curves for surface cover, the surface analogue
/// of [`hornvale_terrain::lithology::endmembers`]. Authored constants, not
/// measured spectra: they need to be ordinally right against each other,
/// not photometrically exact.
pub(crate) mod endmembers {
    use hornvale_kernel::color::BANDS;

    /// Living foliage — low in red, a rise into green, dark again at the
    /// blue end. The near-infrared "red edge" is deliberately absent: it
    /// sits at 720 nm, outside the visible bands this array spans.
    /// type-audit: bare-ok(ratio)
    pub const CHLOROPHYLL: [f64; BANDS] =
        [0.04, 0.05, 0.09, 0.18, 0.14, 0.07, 0.06, 0.06, 0.07, 0.08];
    /// Dead leaf litter and dry stems — flat and brownish, brighter than
    /// foliage at the red end and darker in green.
    /// type-audit: bare-ok(ratio)
    pub const LITTER: [f64; BANDS] =
        [0.08, 0.10, 0.13, 0.16, 0.19, 0.23, 0.26, 0.28, 0.29, 0.30];
    /// Fresh snow — the brightest surface in the set and nearly flat, which
    /// is what makes a peak read white rather than pale-blue.
    /// type-audit: bare-ok(ratio)
    pub const SNOW: [f64; BANDS] =
        [0.92, 0.93, 0.94, 0.94, 0.94, 0.93, 0.93, 0.92, 0.91, 0.90];
    /// Quartz sand — bright and warm, rising steadily toward red.
    /// type-audit: bare-ok(ratio)
    pub const SAND: [f64; BANDS] =
        [0.18, 0.22, 0.28, 0.34, 0.40, 0.45, 0.49, 0.52, 0.54, 0.55];
    /// Wet silt and mud — dark, slightly warm, and flatter than sand.
    /// type-audit: bare-ok(ratio)
    pub const SILT: [f64; BANDS] =
        [0.07, 0.08, 0.10, 0.12, 0.14, 0.16, 0.18, 0.19, 0.20, 0.21];
}
```

- [ ] **Step 2: Write the failing test for the seasonal claim**

Use the addresses and days **Task 1's probe actually found** — do not invent
a latitude.

```rust
#[test]
fn high_ground_is_brighter_in_the_cold_half_of_the_year() {
    // At the high-relief address Task 1 reported, integrate the mixture at
    // four evenly spaced days of one year and collect the sRGB lightness.
    // Assert: the distinct-value count is > 1, AND the brightest day falls
    // in that cell's cold half (compare temperature_at at those days).
    //
    // If Task 1's probe found NO seasonal variation at any address, this
    // test is not written. Record the null in the chronicle instead and
    // ship snow as an annual term — a falsified prediction is a finding.
}
```

- [ ] **Step 3: Run it to verify it fails**

Run: `cargo test -p hornvale-locale --test surface_mixture -- --nocapture`
Expected: FAIL — cover is not yet weighted, so every day is identical.

- [ ] **Step 4: Implement the weighting**

`cover_weights` returns endmember/weight pairs. The **shape** is fixed here;
the coefficients are yours to fit against Task 1's numbers:

```rust
/// The surface cover at `cell` on `at`, as endmember/weight pairs summing
/// to the covered fraction. The bare-ground remainder is the caller's
/// mineral mixture, weighted `1 - covered`.
pub(crate) fn cover_weights(
    climate: &GeneratedClimate,
    cell: CellId,
    at: WorldTime,
) -> Vec<(Reflectance, f64)> {
    // Snow first: it occludes everything under it, so it takes its weight
    // off the top rather than competing. `is_frozen_at` is the seasonal
    // term; `snow_fraction_at` is the annual one.
    // Then vegetation from the formation, split chlorophyll/litter.
    // Then sand and silt from the variant and moisture.
    // Every weight non-negative; the total must not exceed 1.0.
}
```

- [ ] **Step 5: Compose in `reflectance_mixture_at`**

```rust
    pub fn reflectance_mixture_at(
        &self,
        addr: &RoomAddr,
        at: WorldTime,
    ) -> Result<Mixture, LocaleError> {
        let mineral = /* as Task 2a */;
        let cover = surface::cover_weights(&self.climate, cell, at);
        let covered: f64 = cover.iter().map(|(_, w)| w).sum();
        let mut components: Vec<Reflectance> = mineral.components().to_vec();
        let mut weights: Vec<f64> =
            mineral.weights().iter().map(|w| w * (1.0 - covered)).collect();
        for (r, w) in cover {
            components.push(r);
            weights.push(w);
        }
        Mixture::new(components, weights)
            .map_err(|e| LocaleError::Build(e.to_string()))
    }
```

Thread `at: WorldTime` to every caller Task 1 §6.2 found.
`surrounds_scene_colored_in` already holds one (`surrounds.rs:599`).

- [ ] **Step 6: Run the tests**

Run: `cargo test -p hornvale-locale --test surface_mixture`
Expected: PASS, including Task 2a's byte-identity test — which now must be
**deleted or amended**, because colours legitimately move here. Amend it to
assert equality only when `covered == 0.0`; do not delete a control, narrow
it and say why in the commit message.

- [ ] **Step 7: Regenerate and review by eye**

Run: `make rebaseline` then open `book/src/gallery/possession-seed-42.md`.
Branch table:
- *Colours moved and read plausibly* (green where forest, white on peaks) →
  proceed.
- *Everything went one colour* → the covered fraction is saturating; check
  the total, not the curves.
- *Nothing moved* → `cover_weights` is returning empty. That is a bug, not a
  null; the null is "no seasonal variation", which Task 1 already settled.

- [ ] **Step 8: Commit**

```bash
cargo fmt
git commit -F /tmp/msg.txt -- windows/locale/ windows/scene/ docs/audits/ book/src/gallery/ clients/game/core/tests/fixtures/
```

---

## Task 3: Epistemic becomes weight

**Files:**
- Modify: `windows/scene/src/surrounds_ascii.rs` (delete `faded()`, lines 84-96)
- Test: `windows/scene/tests/` — add to the existing ascii test file

**Interfaces:**
- Consumes: nothing new.
- Produces: a `remembered` cell that dims rather than substituting. Task 4
  depends on the glyph slots `,` and `;` being free.

Spec §2. `faded()`'s seven substitutions are the current epistemic encoding
and they consume the glyph budget Task 4 needs.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn a_remembered_cell_keeps_its_glyph_and_changes_only_its_weight() {
    // Build one scene twice, identical but for a cell's `state`
    // ("sensed" vs "remembered"). Assert the glyph CHARACTER at that
    // position is identical in both renders, and that the two renders
    // differ (the weight/dim escape moved).
    //
    // Both halves matter: the first pins the rule, the second stops the
    // test passing vacuously if the renderer stopped distinguishing them.
}
```

- [ ] **Step 2: Run it to verify it fails**

Run: `cargo test -p hornvale-scene --test <ascii test file> -- --nocapture`
Expected: FAIL — the glyph changes, because `faded()` substitutes it.

- [ ] **Step 3: Delete `faded()` and dim instead**

Remove the function and its call site. A `remembered` cell emits the same
glyph wrapped in the terminal's dim attribute (`\u{1b}[2m` … `\u{1b}[0m`),
composed with the existing `colored()` wrapper rather than replacing it.

- [ ] **Step 4: Run the test**

Expected: PASS.

- [ ] **Step 5: Confirm the glyph slots are free**

Run: `grep -n "','\|';'" windows/scene/src/surrounds_ascii.rs`
Expected: no matches. If any remain, Task 4 cannot use those slots.

- [ ] **Step 6: Commit** (`cargo fmt` first)

---

## Task 4: The ordinal becomes impedance

**Files:**
- Modify: `windows/scene/src/surrounds_ascii.rs:26-56` (`terrain_glyph`)
- Test: same file's test module

**Interfaces:**
- Consumes: Task 3's freed slots; `SurroundsCell::{relief, water, micro}`.
- Produces: a documented ladder Task 12's renderers mirror in spirit.

Spec §2.2. The ladder's **rungs** are designed against fixtures — the probe
output from Task 1 plus a sweep of real cells. What is fixed: one ordinal,
one glyph per cell, and the caption names which ordinal is drawn.

- [ ] **Step 1: Survey the real distribution first**

Run a short example printing, over a real walk band, the joint distribution
of `relief`, `micro.openness` and `water`. **Design the rungs against this
output**, not against an idea of terrain. Record the distribution in the
commit message.

- [ ] **Step 2: Write the failing test for the ladder's ordering property**

```rust
#[test]
fn the_impedance_ladder_is_monotone_in_cost() {
    // Assert the ORDER, not the characters: for cells whose impedance
    // inputs are strictly ordered, the rendered glyph's rank in the ladder
    // is non-decreasing. Pinning characters here would make the test a
    // second copy of the implementation.
}
```

- [ ] **Step 3: Run it to verify it fails.** Expected: FAIL.

- [ ] **Step 4: Implement the ladder** in `terrain_glyph`, keeping its
`(char, bool)` return. Update the doc comment: the `bool` no longer means
"draws the bedrock", it means "the colour describes what this glyph draws"
(see Task 5).

- [ ] **Step 5: Run the test.** Expected: PASS.

- [ ] **Step 6: Count the glyph budget**

Run: `grep -oE "'.'" windows/scene/src/surrounds_ascii.rs | sort -u | wc -l`
Expected: **11 or fewer** distinct glyphs (spec §2.1). A twelfth is a
deliberate act and needs a sentence in the commit message saying which axis
bought it.

- [ ] **Step 7: Commit** (`cargo fmt` first)

---

## Task 5: The bedrock→surface sweep

**Files:**
- Modify: `windows/scene/src/surrounds_ascii.rs:164` (the disclosure sentence)
- Modify: `cli/src/main.rs:75`
- Modify: `clients/vessel/src/pane_chart.ts:167,170`, `clients/vessel/src/pane_cell.ts:15`
- Modify: `book/src/gallery/possession-seed-42.md` (regenerated, not hand-edited)

**Interfaces:**
- Consumes: Tasks 2b, 3, 4.
- Produces: no code interface; this is the cross-tree prose change.

**This is the Rhumb §11 class.** A rendered sentence is a cross-tree
interface, and `gate-commit` and `cargo nextest` are structurally incapable
of seeing the `.ts` consumers.

- [ ] **Step 1: Find every consumer — `.ts`, `.mjs`, `.js` AND `.rs`**

```bash
grep -rn 'bedrock' --include=*.ts --include=*.mjs --include=*.js \
  --include=*.rs clients/ cli/ windows/ book/src/ | tee /tmp/hv-bedrock.txt
wc -l /tmp/hv-bedrock.txt
```

List every hit in the commit message. Spec §3.3 names five known sites; if
the grep finds more, the spec undercounted — trust the grep.

- [ ] **Step 2: Rewrite the disclosure sentence**

It currently claims the colour is "each cell's bedrock, tinted only where the
glyph draws that ground", and reports how many tints it withheld. Colour is
now the **surface**. Decide against the code what the withholding rule
becomes and rewrite the sentence to match it. Branch table:
- *Water still occludes* → keep withholding for water glyphs; the count
  shrinks but does not vanish.
- *Nothing occludes any more* → delete the withholding path rather than
  carrying dead code, and say so in the chronicle.

- [ ] **Step 3: Run the client checks — the only thing that sees `.ts`**

Run: `make vessel-check`
Expected: PASS. A red here is the real signal; `gate-commit` cannot produce
one for these files.

- [ ] **Step 4: Run `make game-check`.** Expected: PASS.

- [ ] **Step 5: Regenerate and commit**

```bash
make rebaseline
make rebaseline-goldens   # the eight hardcoded targets; see spec §8
cargo fmt
```
Commit all moved paths together.

---

## Task 6: Stage 1 measurement and the stage gate

**Files:**
- Create: `windows/locale/tests/illumination_hypotheses.rs`

**Interfaces:**
- Consumes: Tasks 2b–5.
- Produces: H1/H2/H3 results for the chronicle.

- [ ] **Step 1: Write H1 as a count with a floor AND a ceiling**

```rust
#[test]
fn h1_the_surface_mixture_increases_distinguishable_colours() {
    // Over a fixed seed-42 walk band at the shipped radius:
    //   let distinct = cells.iter().map(|c| c.color).collect::<BTreeSet<_>>().len();
    // Floor:   distinct > BEDROCK_BASELINE
    //   BEDROCK_BASELINE is the number TASK 1's PROBE PRINTED, pasted here
    //   as a literal with the commit SHA it was taken at in the comment.
    //   Count over the IDENTICAL band the probe used — reuse its exported
    //   `baseline_band(..)`, do not rebuild one. A floor measured on a
    //   different population is vacuous (ledger F1).
    // Ceiling: distinct != cells.len()
    // The ceiling is load-bearing: a unique colour per cell means the
    // mixture is tracking address noise, not cover — a defect dressed as
    // a success.
}
```

- [ ] **Step 2: Run it.** Record the actual numbers, both bounds.

- [ ] **Step 3: Write H3 — the degradation rule**

```rust
#[test]
fn h3_a_monochrome_observer_loses_the_nominal_axis_and_says_so() {
    // Build the same scene for an observer with no chromatic channel.
    // Assert: distinct rendered cells < the coloured render's count, AND
    // the caption names the lost axis, AND the glyph multiset is UNCHANGED
    // between the two (no reallocation — spec §2.3).
    // The third clause is the one that keeps the rule from decaying.
}
```

- [ ] **Step 4: Run every new test directly — the commit gate will not**

```bash
cargo test -p hornvale-locale --test illumination_hypotheses
cargo test -p hornvale-locale --test surface_mixture
cargo test -p hornvale-scene 2>&1 | tee /tmp/hv-stage1.txt
```

- [ ] **Step 5: Name the roster tests you expect, in advance**

Write the exact test names you expect the stage gate's roster rewrite to pick
up into the commit message. After the gate, check them **by name** in the
roster commit — "did a roster commit appear?" passes even when the mechanism
records a subset (spec §9.1).

- [ ] **Step 6: `make gate-commit`, then push, then the stage gate**

```bash
make gate-commit
git push -u origin campaign/the-illumination
make sluice-stage BRANCH=campaign/the-illumination REF=$(git rev-parse HEAD)
```
Order matters: the queue entry is the request. If the enqueue fails it prints
so and prints no read-it-back line.

- [ ] **Step 7: After the gate, verify the roster picked up your named tests**

```bash
git fetch origin && git log --oneline -3 -- docs/timings/subfloor-roster.tsv
grep -c 'illumination_hypotheses' docs/timings/subfloor-roster.tsv
```

---

# STAGE 2 — the wire, additive at v2

## Task 7: The kernel gains great-circle distance

**Files:**
- Modify: `kernel/src/room.rs` (beside `bearing_to`, line 474)
- Test: `kernel/src/room.rs` test module

**Interfaces:**
- Produces: `RoomAddr::distance_rad_to(&self, other: &RoomAddr) -> f64`.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn distance_to_self_is_zero_and_the_antipode_is_pi() {
    let a = RoomAddr::containing([1.0, 0.0, 0.0], 4);
    let b = RoomAddr::containing([-1.0, 0.0, 0.0], 4);
    assert_eq!(a.distance_rad_to(&a), 0.0);
    assert!((a.distance_rad_to(&b) - std::f64::consts::PI).abs() < 1e-9);
}

#[test]
fn distance_is_symmetric() {
    // Over a spread of address pairs, assert d(a,b) == d(b,a) exactly.
}
```

- [ ] **Step 2: Run to verify it fails.** Expected: `no method named distance_rad_to`.

- [ ] **Step 3: Implement**

```rust
    /// Great-circle angular distance to `other`'s centroid, in radians.
    /// Pairs with [`RoomAddr::bearing_to`]: together they are a polar
    /// coordinate for `other` about `self`, which is what a client needs
    /// to place a cell without doing spherical trigonometry itself.
    /// type-audit: pending(wave-1)
    pub fn distance_rad_to(&self, other: &RoomAddr) -> f64 {
        let a = self.centroid();
        let b = other.centroid();
        let dot = (a[0] * b[0] + a[1] * b[1] + a[2] * b[2]).clamp(-1.0, 1.0);
        math::acos(dot)
    }
```

Use `kernel::math`, not `f64::acos` — spec's determinism rule routes
transcendentals through the `libm` crate.

- [ ] **Step 4: Run the tests.** Expected: PASS.

- [ ] **Step 5: Regenerate the type-audit report in this commit**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
```

- [ ] **Step 6: Commit** — note this is a **kernel-layer** edit, so
`gate-commit` costs ~470 s. Budget for it; do not interpret the wait as a hang.

---

## Task 8: `Sight` gains the calibration

**Files:**
- Modify: `windows/scene/src/surrounds.rs:228-247` (`Sight`), `:599-632` (the builder)
- Test: `windows/scene/tests/`

**Interfaces:**
- Consumes: `Observer::roles() -> &[ChannelRole]` (`kernel/src/color.rs:565`).
- Produces: `Sight` fields `channel_roles: Vec<String>` and `projection_slots: Option<[u32; 3]>`.

Spec §4.1. **Without this, `signal` is decoration** — a client cannot tell
which index is chromatic or which drives R/G/B. This lands with Task 9, never
after it.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn sight_carries_enough_to_interpret_a_signal() {
    // For a coloured scene:
    //   assert_eq!(sight.channel_roles.len(), sight.channels as usize);
    //   assert_eq!(
    //       sight.channel_roles.iter().filter(|r| *r == "chromatic").count(),
    //       sight.chromatic as usize
    //   );
    // The cross-check is the point: two independently emitted facts about
    // the same observer must agree, so a wrong one cannot pass quietly.
}
```

- [ ] **Step 2: Run to verify it fails.** Expected: no field `channel_roles`.

- [ ] **Step 3: Add the fields**

Append after `preserves` so the field order change is additive-at-the-end.
Extend the struct's existing `type-audit:` tag — **on the struct's doc
comment, never on a field's**.

- [ ] **Step 4: Populate from the observer** in `surrounds_scene_colored_in`,
mapping `ChannelRole::{Chromatic, Achromatic}` to `"chromatic"`/`"achromatic"`.

- [ ] **Step 5: Run the test.** Expected: PASS.

- [ ] **Step 6: Commit.**

---

## Task 9: `SurroundsCell` gains four fields

**Files:**
- Modify: `windows/scene/src/surrounds.rs` (`SurroundsCell`, `SurroundsScene`)
- Create: `windows/locale/src/surface.rs` — add `cover_class_at`
- Test: `windows/scene/tests/`

**Interfaces:**
- Consumes: Task 7's `distance_rad_to`, Task 8's `Sight`, Task 2b's cover model.
- Produces: `cell.signal: Option<Vec<f64>>`, `cell.cover: u32`, `cell.bearing_deg: f64`, `cell.distance_rad: f64`, `scene.cover_legend: Vec<String>`.

- [ ] **Step 1: Write the failing invariant test — the migration control**

```rust
#[test]
fn projecting_a_cells_signal_reproduces_its_colour_exactly() {
    // For every cell in a coloured scene that has BOTH signal and color:
    //   let round = observer.to_srgb(&Signal::from(cell.signal.clone()));
    //   assert_eq!(round, cell.color, "cell {} diverged", cell.room);
    // Byte-for-byte. This is spec §4.2's migration control, now a live
    // invariant inside ONE document rather than a cross-version test.
    // Also assert the loop body ran (count > 0) — an empty scene would
    // pass vacuously.
}
```

The final clause is not optional. A test that iterates nothing is green.

- [ ] **Step 2: Run to verify it fails.** Expected: no field `signal`.

- [ ] **Step 3: Add the fields**

`signal` and `cover` follow `color`'s `skip_serializing_if = "Option::is_none"`
discipline so an uncoloured document's bytes do not move. `bearing_deg` and
`distance_rad` are present on **every** cell including seams — a seam cell
carries `room` (a `u64`, not an `Option`), so both are computable there.
Quantize at emit with the same `quantize_serde` helpers the neighbouring
float fields use.

- [ ] **Step 4: Populate** — bearing and distance from the observer's own
`RoomAddr` to each cell's; `signal` from `observer.sense(&reflectance, light)`
in the coloured path; `cover` as an index into `cover_legend`.

- [ ] **Step 5: Run the tests.** Expected: PASS.

- [ ] **Step 6: Verify a seam cell is populated**

```bash
cargo test -p hornvale-scene seam -- --nocapture
```
Assert a scene containing a seam cell has finite `bearing_deg`/`distance_rad`
on it. This is what Task 12 depends on.

- [ ] **Step 7: Make the shape argument on its own**

```bash
git stash && make rebaseline && cp <a committed scene fixture> /tmp/old.json
git stash pop && make rebaseline
make shapecheck OLD=/tmp/old.json NEW=<the same fixture>
```
Expected: the key-path set **grows** by exactly the new keys and no existing
key-path disappears. A *disappeared* key path is a breaking change, not an
additive one — stop and escalate.

- [ ] **Step 8: Commit and stage-gate** (name expected roster tests first).

---

## Task 10: Stage 2 close

- [ ] **Step 1:** `make vessel-check` and `make game-check` — the additive
fields must be invisible to both. Branch table: *both green* → additive
confirmed. *Either red* → a consumer was not as tolerant as
`deny_unknown_fields`'s absence suggested; report before proceeding.
- [ ] **Step 2:** `make rebaseline` + `make rebaseline-goldens`, commit drift.
- [ ] **Step 3:** stage gate; verify named roster tests landed.

---

# STAGE 3 — north-up

## Task 11: The chart reference becomes a generated fixture

**Files:**
- Modify: `scripts/regenerate-artifacts.sh`
- Create: `clients/game/core/tests/fixtures/chart-reference-seed-42.txt`
- Modify: `clients/game/core/tests/chart.rs:81,154`, `tests/plan.rs:156`

**Interfaces:**
- Produces: a drift-checked reference generated by the sim, consumed by both
  client tests.

Spec §5.3, ledger #10. This converts a discipline into a mechanism **before**
Task 12 needs it.

- [ ] **Step 1: Emit the reference from the sim's own renderer**

Add to `regenerate-artifacts.sh`, next to the existing fixture writes, a
command rendering the same seed-42 walk band through
`windows/scene`'s ASCII renderer, redirected (`>`) to the fixture path. **The
redirect is what writes the file** — a bare command regenerates nothing and
the drift check that follows reads as "no drift".

- [ ] **Step 2: Read the fixture in both tests**

```rust
const REFERENCE_SHAPE: &str =
    include_str!("fixtures/chart-reference-seed-42.txt");
```

- [ ] **Step 3: `git add` the fixture — the check is vacuous without it**

```bash
git add clients/game/core/tests/fixtures/chart-reference-seed-42.txt
```
`git diff --exit-code` against a path with no index entry is silently
vacuous. The directory is already in `docs/generated-paths.txt`, so nothing
else is needed there.

- [ ] **Step 4: Prove the drift check can fire — a positive control**

```bash
printf 'x' >> clients/game/core/tests/fixtures/chart-reference-seed-42.txt
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$'); echo "rc=$?"
git checkout -- clients/game/core/tests/fixtures/
```
Expected: `rc=1`. If it is 0 the check is vacuous and Task 12 has no control.

- [ ] **Step 5: Run both tests.** Expected: PASS, unchanged behaviour.
- [ ] **Step 6: Commit.**

---

## Task 12: The north-up projection

**Files:**
- Modify: `windows/scene/src/surrounds_ascii.rs`, `clients/game/core/src/chart.rs`, `clients/vessel/src/pane_chart.ts`
- Modify: `windows/scene/src/surrounds.rs:304,502` (`orientation`)

**Interfaces:**
- Consumes: Task 9's `bearing_deg`/`distance_rad`; Task 11's fixture.
- Produces: `orientation == "north-up"`.

- [ ] **Step 1: Measure collisions before writing a rule (spec §6.4)**

Project a real walk band by bearing and distance at the shipped radius and
count cells landing in an occupied box. Branch table:
- *Zero at this radius* → still write the guard, with a test that **forces**
  a collision; never trust unreachability.
- *Collisions occur* → extend `chart.rs::draw`'s existing salience discipline
  to terrain cells, with a deterministic tie-break (`total_cmp`, then a
  stable key — no `HashMap`).

- [ ] **Step 2: Settle the weight-vs-salience interaction (spec §2)**

A `remembered` cell holding a flagship mark is bold-because-salient and
dim-because-remembered. Decide it against fixtures, pin it in a test, and
apply the same answer in all three renderers — this is the *rule*, and §2.3
does not license divergence in the rule.

- [ ] **Step 3: Implement in the sim first, then re-capture the reference**

```bash
make rebaseline
git diff -- clients/game/core/tests/fixtures/chart-reference-seed-42.txt
```
The reference moves **because the sim moved**. This is the only sanctioned
direction (spec §5.3).

- [ ] **Step 4: Port to the two clients independently**

Write each projection from the geometry, **not** by reading the other's
source. The agreement test is only meaningful if the implementations are
independent.

- [ ] **Step 5: Run the agreement tests**

```bash
cargo test -p hornvale-game-core --test chart
cargo test -p hornvale-game-core --test plan
```
Expected: PASS. If red, **fix the renderer** — do not paste `chart.rs`'s
output into the fixture. The generator is the sim; that is now mechanical.

- [ ] **Step 6: Assert seam cells are drawn**

```rust
#[test]
fn a_seam_cell_is_drawn_under_north_up() {
    // On a band known from Task 9 Step 6 to contain a seam cell, assert the
    // rendered grid has a mark at that cell's projected position.
}
```

- [ ] **Step 7: `make vessel-check`, `make game-check`.**
- [ ] **Step 8: Commit and stage-gate.**

---

## Task 13: Close

- [ ] **Step 1: Registry rows** — write the three new rows and **correct**
(not supersede) `RENDER-surface-mixture` and `RENDER-appearance-signal-protocol`;
flip `NAV-north-up-needs-per-cell-position` to `shipped`. Full list in
`.superpowers/sdd/followups.md`. Check `campaign/the-burr`'s hold-off on
`book/src/frontier/` is clear first: `make board-sync && make board`.
- [ ] **Step 2: Decision 0142** — write the record; re-verify the number is
still free (`make board-sync && make board`), because `ls docs/decisions/`
cannot see a number an unmerged branch holds.
- [ ] **Step 3: Chronicle entry** (`book/src/chronicle/`) + freshness sweep;
re-score any Confidence Gradient bet this campaign moved.
- [ ] **Step 4: Retrospective** (`docs/retrospectives/the-illumination.md`) —
promote `.superpowers/sdd/followups.md` **before teardown**; the scratch is
git-ignored and dies with the worktree.
- [ ] **Step 5: `make rebaseline` + `make rebaseline-goldens`**, commit drift.
- [ ] **Step 6: Add a `Sluice-Headline:` trailer**

In the body of any commit in the range, adjacent to other trailers with **no
blank line between** (the check reads git's trailer parser, which sees only
the final block), carrying **no `merge(...)` prefix** — the chamber composes
that:

```
Sluice-Headline: colour is a surface, the chart faces north, and the seams are drawn
```

- [ ] **Step 7: Merge**

```bash
make sluice BRANCH=campaign/the-illumination REF=$(git rev-parse HEAD)
```

- [ ] **Step 8: G6 — HARD STOP.** Present the post-G3 ledger digest to Nathan
before closing. Then `closing-a-campaign`, unchanged.

---

## Self-Review

**Spec coverage.** §2 → Tasks 3, 4, 12 Step 2. §2.1 → Task 4 Step 6. §2.2 →
Task 4. §2.3 → Task 6 Step 3. §3 → Tasks 2a, 2b. §3.3 → Task 5. §4.1 →
Tasks 8, 9. §4.2 → Task 9 Step 1. §4.4 → Task 9 Step 7. §4.5 → Task 7. §5.1 →
Task 9. §5.2 → Tasks 9 Step 6, 12 Step 6. §5.3 → Tasks 11, 12. §6.1–6.3 →
Task 1. §6.4 → Task 12 Step 1. §6.5 → Global Constraints. §7 → Task 6. §8 →
Tasks 2b Step 7, 5 Step 5, 13 Step 5. §9 → Task 6. §9.1 → Tasks 6 Steps 4–5,
7. §12.1 → Task 13 Steps 6–7.

**Known gap, deliberate:** H2 (Task 2b Step 2) is contingent on Task 1's
probe. If the probe finds no seasonal signal, H2 is not written and the null
is the chronicle's headline — that is the preregistration discipline working,
not a plan hole.

**Type consistency.** `reflectance_mixture_at` gains its `at: WorldTime`
parameter in Task 2b and is named identically in Tasks 2a and 9.
`distance_rad_to` (Task 7) matches its use in Task 9. `channel_roles` /
`projection_slots` (Task 8) match Task 9's invariant test.
