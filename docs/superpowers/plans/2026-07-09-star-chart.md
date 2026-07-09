# The Star Chart Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix SKY-20 (synodic moon phase), give neighbor stars drawn celestial positions from a new seed stream, and render the star chart (72×24 equirectangular ASCII + 256×128 planisphere-pair PNG) as `hornvale star-chart` with committed gallery artifacts.

**Architecture:** Three strictly ordered layers. (1) `calendar.rs` gains a sidereal→synodic conversion; illumination phase and month counts cycle on it. (2) `neighborhood.rs` draws a fixed (declination, right ascension) per neighbor from a **new** stream `astronomy/neighbor-positions`, leaving every existing draw untouched. (3) A new `domains/astronomy/src/render.rs` renders the chart in both media via `hornvale_kernel::png::encode_rgb`, surfaced by a new CLI command in the `map`-command shape.

**Tech Stack:** Rust edition 2024, std only. Crates touched: `hornvale-astronomy`, `hornvale-worldgen` (one accessor), `hornvale` (CLI), plus gallery/CI/book files.

**Spec:** `docs/superpowers/specs/2026-07-09-star-chart-design.md` (governs; cite it on any judgment call).

## Global Constraints

- No new dependencies (decision 0004); no wall-clock; no `HashMap`/`HashSet`; float ordering via `total_cmp`.
- Domains depend on `hornvale-kernel` and nothing else (decision 0002).
- **Save-format contracts:** existing seed streams must keep their exact consumption order — positions come ONLY from the new `neighbor-positions` stream. `Moon.period` stays sidereal. The synodic change is a bug fix toward the model card: no epoch suffix.
- Every public item gets a one-line doc comment (`#![warn(missing_docs)]`).
- Determinism: same seed + pins → byte-identical artifacts; CI drift-checks committed gallery/reference files.
- Full gate before every commit: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings` (run `cargo fmt` first). Never `--no-verify`.
- Workspace root: `/Users/nathan/Projects/hornvale/hornvale` (or the worktree). All commands run from the root.

---

### Task 1: SKY-20 — synodic moon phase

**Files:**
- Modify: `domains/astronomy/src/calendar.rs`
- Modify (regenerate): `book/src/gallery/almanac-seed-42-sky.md`, `book/src/gallery/almanac-seed-42-locked.md`, `book/src/gallery/almanac-seed-42.md`
- Possibly modify: any test elsewhere asserting sidereal-derived months/phases (Step 4 finds them)

**Interfaces:**
- Consumes: existing `Calendar { moon_periods: Vec<StdDays>, year: StdDays, .. }`.
- Produces: `Calendar::synodic_month(&self, index: usize) -> Option<StdDays>` (public; `None` if the moon doesn't exist or `P_sid ≥ Y`). `moon_phase` and `months_per_year` now derive from it. Tasks 3/5 call `synodic_month`.

- [ ] **Step 1: Write the failing tests**

In `domains/astronomy/src/calendar.rs`'s test module, add (and adjust the existing `moon_phase_and_months_derive_from_kepler_periods` test per Step 3):

```rust
    /// Luna-like check: 27.32 d sidereal in a 365.25 d year → 29.53 d synodic.
    #[test]
    fn synodic_month_matches_the_luna_check_value() {
        let cal = calendar_with_moon(27.32, 365.25); // helper per Step 3
        let synodic = cal.synodic_month(0).unwrap().0;
        assert!((synodic - 29.5306).abs() < 0.01, "got {synodic}");
        assert!((cal.months_per_year(0).unwrap() - 365.25 / synodic).abs() < 1e-12);
    }

    /// Illumination phase cycles on the synodic period, not the sidereal.
    #[test]
    fn moon_phase_cycles_on_the_synodic_period() {
        let cal = calendar_with_moon(27.32, 365.25);
        let synodic = cal.synodic_month(0).unwrap().0;
        assert!((cal.moon_phase(StdDays(synodic * 0.5), 0).unwrap() - 0.5).abs() < 1e-9);
        assert!(cal.moon_phase(StdDays(synodic), 0).unwrap() < 1e-9);
    }

    /// A sidereal period at or beyond the year is degenerate: no synodic cycle.
    #[test]
    fn synodic_month_guards_the_degenerate_case() {
        let cal = calendar_with_moon(400.0, 365.25);
        assert!(cal.synodic_month(0).is_none());
        assert!(cal.moon_phase(StdDays(1.0), 0).is_none());
        assert!(cal.months_per_year(0).is_none());
    }
```

The `calendar_with_moon(sidereal_days, year_days)` helper builds a `Calendar` the same way the existing tests in this file do (via `calendar_of` over a constructed system, or by whatever construction the existing test module already uses — reuse its pattern, parameterizing the moon period and year).

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-astronomy calendar`
Expected: FAIL — `synodic_month` not found.

- [ ] **Step 3: Implement**

In `impl Calendar`, add, and rewire the two existing methods:

```rust
    /// The synodic month of moon `index` — the illumination cycle seen from
    /// the anchor: `P_syn = P_sid × Y / (Y − P_sid)` (spec §2, fixing
    /// SKY-20). `None` if the moon doesn't exist or `P_sid ≥ Y` (degenerate:
    /// the moon never laps the sun).
    pub fn synodic_month(&self, index: usize) -> Option<StdDays> {
        let sidereal = self.moon_periods.get(index)?;
        if sidereal.0 >= self.year.0 {
            return None;
        }
        Some(StdDays(sidereal.0 * self.year.0 / (self.year.0 - sidereal.0)))
    }

    /// Illumination phase of moon `index` at `t` (0 = new, 0.5 = full),
    /// cycling on the synodic month.
    pub fn moon_phase(&self, t: StdDays, index: usize) -> Option<f64> {
        let synodic = self.synodic_month(index)?;
        Some((t.0 / synodic.0).fract())
    }

    /// How many synodic months of moon `index` fit in a year.
    pub fn months_per_year(&self, index: usize) -> Option<f64> {
        let synodic = self.synodic_month(index)?;
        Some(self.year.0 / synodic.0)
    }
```

Update the existing test `moon_phase_and_months_derive_from_kepler_periods`: its assertions currently encode sidereal expectations (`months == year / period`, phase 0.5 at half the sidereal period). Rewrite its assertions to the synodic equivalents (`months == year / synodic`, phase 0.5 at half the *synodic* period) — keep its name-and-shape, or rename it `..._derive_from_synodic_periods` if the name would lie. Extract the shared `calendar_with_moon` helper here.

- [ ] **Step 4: Run the workspace and fix every downstream assertion**

Run: `cargo test --workspace 2>&1 | grep -E "FAILED|panicked" -A2`

Expected failures beyond calendar.rs: any provider/worldgen/almanac test asserting month counts or phase words at specific days (the phase timeline shifted ~8%), and possibly fixture-comparison tests. For each: update the expected value to the synodic-correct one — never widen a tolerance or delete an assertion. Grep helpers: `grep -rn "months" --include=*.rs domains windows cli | grep -i "assert\|expect"`.

- [ ] **Step 5: Regenerate the almanac fixtures**

```bash
cargo run -p hornvale -- new --seed 42 --sky constant --out /tmp/hv-sc-42.json
cargo run -p hornvale -- almanac --world /tmp/hv-sc-42.json > book/src/gallery/almanac-seed-42.md
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-sc-sky.json
cargo run -p hornvale -- almanac --world /tmp/hv-sc-sky.json > book/src/gallery/almanac-seed-42-sky.md
cargo run -p hornvale -- new --seed 42 --rotation locked --out /tmp/hv-sc-locked.json
cargo run -p hornvale -- almanac --world /tmp/hv-sc-locked.json > book/src/gallery/almanac-seed-42-locked.md
git diff --stat book/src/gallery/
```

Expected: the sky and locked almanacs change only in month counts / phase wording (eyeball the diff — e.g. "23.0 months to a year" becomes a smaller synodic count); the constant-sun almanac is unchanged. Any *other* change is a bug — stop and investigate.

- [ ] **Step 6: Full gate, then commit**

```bash
git add domains/astronomy/src/calendar.rs book/src/gallery/almanac-seed-42*.md <any test files from Step 4>
git commit -m "fix(astronomy): moon phase cycles on the synodic month (SKY-20)

P_syn = P_sid·Y/(Y−P_sid); Moon.period stays sidereal. Bug fix toward
the model card (fixture prose already quoted synodic values) — no epoch.
Almanac fixtures regenerated."
```

---

### Task 2: Neighbor star positions at genesis

**Files:**
- Modify: `domains/astronomy/src/streams.rs`, `domains/astronomy/src/lib.rs` (stream_labels), `domains/astronomy/src/neighborhood.rs`
- Modify: `domains/astronomy/tests/genesis_properties.rs` (pin-isolation test)
- Modify (regenerate): `book/src/reference/stream-manifest-generated.md`

**Interfaces:**
- Consumes: `Seed::derive(label).stream()`, `Stream::next_f64() -> f64` in [0, 1).
- Produces: `Neighbor` gains `declination: f64` (degrees, −90…90) and `right_ascension: f64` (degrees, [0, 360)); `class_name(class: NeighborClass) -> &'static str` (public, for legends); `streams::NEIGHBOR_POSITIONS = "neighbor-positions"`. Tasks 3–5 read the new fields.

- [ ] **Step 1: Write the failing tests**

In `domains/astronomy/src/neighborhood.rs`'s test module add:

```rust
    #[test]
    fn positions_are_on_the_sphere_and_deterministic() {
        let seed = Seed(7).derive("astronomy");
        let a = generate_neighbors(seed, &SkyPins::default());
        let b = generate_neighbors(seed, &SkyPins::default());
        assert_eq!(a, b);
        for n in &a {
            assert!((-90.0..=90.0).contains(&n.declination), "dec {}", n.declination);
            assert!((0.0..360.0).contains(&n.right_ascension), "ra {}", n.right_ascension);
        }
    }
```

In `domains/astronomy/tests/genesis_properties.rs` add (mirroring the file's existing pin-matrix style — see `pin_isolation_holds_at_the_system_level` for the generation idiom):

```rust
/// The neighbor-class pin must not move any star: positions draw from
/// their own stream (spec §3), so the pinned and unpinned skies hold the
/// same position multiset even though one star's class differs.
#[test]
fn neighbor_pin_does_not_move_any_star() {
    let seed = Seed(42);
    let unpinned = hornvale_astronomy::generate(seed, &SkyPins::default()).unwrap();
    let pinned = hornvale_astronomy::generate(
        seed,
        &SkyPins { neighbor: Some(NeighborClass::RedGiant), ..SkyPins::default() },
    )
    .unwrap();
    let mut a: Vec<(f64, f64)> = unpinned.system.neighbors.iter()
        .map(|n| (n.declination, n.right_ascension)).collect();
    let mut b: Vec<(f64, f64)> = pinned.system.neighbors.iter()
        .map(|n| (n.declination, n.right_ascension)).collect();
    a.sort_by(|x, y| x.0.total_cmp(&y.0).then(x.1.total_cmp(&y.1)));
    b.sort_by(|x, y| x.0.total_cmp(&y.0).then(x.1.total_cmp(&y.1)));
    assert_eq!(a, b);
}
```

(Positions are compared as a sorted multiset because the brightness sort may order the two skies differently once the pinned star's brightness changes; no star's *position* may change. Adjust the `generate`/field paths to the file's actual imports — `GenesisOutcome.system` etc.)

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-astronomy`
Expected: FAIL — no field `declination`.

- [ ] **Step 3: Implement**

`domains/astronomy/src/streams.rs`, after `NEIGHBORS`:

```rust
/// Per-neighbor celestial position draws (declination, right ascension).
pub const NEIGHBOR_POSITIONS: &str = "neighbor-positions";
```

`domains/astronomy/src/lib.rs` `stream_labels()`, after the `astronomy/neighbors` entry:

```rust
        (
            "astronomy/neighbor-positions",
            "per-neighbor celestial position draws (declination, right ascension)",
        ),
```

`domains/astronomy/src/neighborhood.rs` — add the two fields (with doc comments) to `Neighbor`:

```rust
    /// Declination in degrees from the celestial equator (drawn, −90…90).
    /// The celestial equator is the anchor's rotational equator (spec §3).
    pub declination: f64,
    /// Right ascension in degrees (drawn, 0…360).
    pub right_ascension: f64,
```

In `generate_neighbors`, derive the position stream beside the existing one and draw one pair per neighbor **inside the loop** (the struct carries its position through the brightness sort):

```rust
    let mut stream = astronomy_seed.derive(streams::NEIGHBORS).stream();
    let mut positions = astronomy_seed.derive(streams::NEIGHBOR_POSITIONS).stream();
```

and in the per-neighbor closure, before constructing the struct:

```rust
            let declination = (positions.next_f64() * 2.0 - 1.0).asin().to_degrees();
            let right_ascension = positions.next_f64() * 360.0;
```

(Uniform on the sphere: `sin δ` uniform in [−1, 1). RA uniform in [0°, 360°).) Add both fields to the `Neighbor { .. }` literal. Also add the legend helper next to `class_color`:

```rust
/// The prose name of a spectral class, for chart legends.
pub fn class_name(class: NeighborClass) -> &'static str {
    match class {
        NeighborClass::RedDwarf => "red dwarf",
        NeighborClass::SunLike => "sun-like star",
        NeighborClass::WhiteDwarf => "white dwarf",
        NeighborClass::OrangeGiant => "orange giant",
        NeighborClass::RedGiant => "red giant",
        NeighborClass::BlueGiant => "blue giant",
    }
}
```

Fix any other `Neighbor { .. }` struct literals the compiler reports (tests constructing neighbors elsewhere gain the two fields with any in-range values).

- [ ] **Step 4: Run to verify pass, regenerate the manifest**

Run: `cargo test -p hornvale-astronomy && cargo test --workspace`
Expected: PASS.

```bash
cargo run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
git diff book/src/reference/stream-manifest-generated.md
```

Expected: exactly one added row for `astronomy/neighbor-positions`.

- [ ] **Step 5: Full gate, then commit**

```bash
git add domains/astronomy/src/streams.rs domains/astronomy/src/lib.rs domains/astronomy/src/neighborhood.rs domains/astronomy/tests/genesis_properties.rs book/src/reference/stream-manifest-generated.md
git commit -m "feat(astronomy): neighbor stars draw fixed celestial positions

New stream astronomy/neighbor-positions (existing draws byte-preserved);
uniform-on-sphere dec/RA per neighbor; pin-isolation property test:
the class pin moves no star."
```

---

### Task 3: Render — ASCII chart and moon phase strips

**Files:**
- Create: `domains/astronomy/src/render.rs`
- Modify: `domains/astronomy/src/lib.rs` (declare `pub mod render;`), `domains/astronomy/src/provider.rs` (`size_word` → `pub(crate)`)

**Interfaces:**
- Consumes: `Neighbor` fields incl. `declination`/`right_ascension` (Task 2); `Calendar::synodic_month` (Task 1); `Moon.angular_diameter_rel`; `crate::provider::size_word`.
- Produces: `render::chart_ascii(neighbors: &[Neighbor]) -> String` (72×24 + newlines); `render::moon_lines(moons: &[Moon], calendar: &Calendar) -> Vec<String>`; constants `ASCII_WIDTH`/`ASCII_HEIGHT`. Task 5 calls both.

- [ ] **Step 1: Write the module with failing tests**

Create `domains/astronomy/src/render.rs`:

```rust
//! Deterministic star-chart renders (spec: the star chart, 2026-07-09):
//! a 72×24 equirectangular ASCII chart — RA 0→360° across, dec 90→−90°
//! down, each star plotted as its brightness-rank digit — and timeless
//! moon phase-cycle strips. Same sky, same bytes.

use crate::calendar::Calendar;
use crate::moons::Moon;
use crate::neighborhood::Neighbor;
use crate::provider::size_word;

/// ASCII chart width in characters.
pub const ASCII_WIDTH: usize = 72;
/// ASCII chart height in characters.
pub const ASCII_HEIGHT: usize = 24;

/// One synodic cycle as a 16-column glyph strip: `o` new, `)` waxing,
/// `O` full, `(` waning — the provider's phase-word thresholds sampled
/// at k/16.
const PHASE_STRIP: &str = "oo))))))OO((((oo";

/// Render the fixed night sky as a 72×24 equirectangular ASCII chart.
/// Stars plot as their 1-based brightness-rank digit; on a collision the
/// brighter star's digit wins. The celestial equator is a dashed line.
pub fn chart_ascii(neighbors: &[Neighbor]) -> String {
    let mut grid = vec![vec![' '; ASCII_WIDTH]; ASCII_HEIGHT];
    let equator = ASCII_HEIGHT / 2;
    for (col, cell) in grid[equator].iter_mut().enumerate() {
        if col % 2 == 0 {
            *cell = '-';
        }
    }
    // Dimmest first, so a brighter star's digit overwrites on collision.
    for (index, n) in neighbors.iter().enumerate().rev() {
        let col = ((n.right_ascension / 360.0) * ASCII_WIDTH as f64) as usize;
        let row = (((90.0 - n.declination) / 180.0) * ASCII_HEIGHT as f64) as usize;
        let digit = char::from_digit(index as u32 + 1, 10).expect("≤ 5 neighbors");
        grid[row.min(ASCII_HEIGHT - 1)][col.min(ASCII_WIDTH - 1)] = digit;
    }
    let mut out = String::with_capacity((ASCII_WIDTH + 1) * ASCII_HEIGHT);
    for row in grid {
        out.extend(row);
        out.push('\n');
    }
    out
}

/// One timeless phase-cycle line per moon: the strip, the moon's size
/// word, and its synodic month. Skips (with an honest note) a moon whose
/// synodic cycle is degenerate.
pub fn moon_lines(moons: &[Moon], calendar: &Calendar) -> Vec<String> {
    moons
        .iter()
        .enumerate()
        .map(|(index, moon)| match calendar.synodic_month(index) {
            Some(synodic) => format!(
                "Moon {} ({}): `{}` — one cycle every {:.1} standard days.",
                index + 1,
                size_word(moon.angular_diameter_rel),
                PHASE_STRIP,
                synodic.0
            ),
            None => format!(
                "Moon {} ({}): no phase cycle — its orbit outpaces the year.",
                index + 1,
                size_word(moon.angular_diameter_rel)
            ),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pins::NeighborClass;

    fn star(dec: f64, ra: f64, brightness: f64) -> Neighbor {
        Neighbor {
            class: NeighborClass::SunLike,
            distance: crate::units::LightYears(10.0),
            apparent_brightness: brightness,
            color: "warm yellow".to_string(),
            declination: dec,
            right_ascension: ra,
        }
    }

    #[test]
    fn chart_is_72x24_with_every_star_digit_present() {
        let stars = vec![star(45.0, 30.0, 3.0), star(-20.0, 200.0, 2.0), star(0.0, 359.0, 1.0)];
        let chart = chart_ascii(&stars);
        assert_eq!(chart.lines().count(), ASCII_HEIGHT);
        for line in chart.lines() {
            assert_eq!(line.chars().count(), ASCII_WIDTH);
        }
        for digit in ["1", "2", "3"] {
            assert!(chart.contains(digit), "missing {digit}");
        }
        assert_eq!(chart, chart_ascii(&stars));
    }

    #[test]
    fn collision_keeps_the_brighter_digit() {
        // Same cell: identical dec/RA. Index 0 is the brighter by contract.
        let stars = vec![star(10.0, 100.0, 5.0), star(10.0, 100.0, 1.0)];
        let chart = chart_ascii(&stars);
        assert!(chart.contains('1'));
        assert!(!chart.contains('2'));
    }

    #[test]
    fn equator_is_dashed() {
        let chart = chart_ascii(&[]);
        let row = chart.lines().nth(ASCII_HEIGHT / 2).unwrap();
        assert!(row.starts_with("- - "));
    }
}
```

Note on `moon_lines` tests: build the `Calendar` via the same construction the `calendar.rs` test module uses (the `calendar_with_moon` helper from Task 1 is private to that module — mirror its construction here or route through `calendar_of` on a hand-built system, whichever `calendar.rs`'s tests demonstrate). Add one test asserting a Luna-like moon's line contains `29.5` and the strip, and one asserting the degenerate branch's wording.

In `domains/astronomy/src/lib.rs`, declare the module alongside the others: `pub mod render;`. In `provider.rs`, change `fn size_word` to `pub(crate) fn size_word` (doc comment stays).

- [ ] **Step 2: Run to verify the tests pass — and bite**

Run: `cargo test -p hornvale-astronomy render`
Expected: PASS. Module and tests land together here, so prove the collision test bites: temporarily change the plot loop from `.rev()` to forward order, rerun (expect `collision_keeps_the_brighter_digit` to FAIL), then restore `.rev()` and rerun green.

- [ ] **Step 3: Full gate, then commit**

```bash
git add domains/astronomy/src/render.rs domains/astronomy/src/lib.rs domains/astronomy/src/provider.rs
git commit -m "feat(astronomy): ASCII star chart and moon phase-cycle strips

72×24 equirectangular, stars as brightness-rank digits, dashed celestial
equator; timeless synodic strips per moon (spec §4)."
```

---

### Task 4: Render — planisphere-pair PNG

**Files:**
- Modify: `domains/astronomy/src/render.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::png::encode_rgb(u32, u32, &[u8]) -> Vec<u8>`; `Neighbor` fields; `NeighborClass`.
- Produces: `render::chart_png(neighbors: &[Neighbor]) -> Vec<u8>` (256×128 PNG bytes); constants `MAP_WIDTH: u32 = 256`, `MAP_HEIGHT: u32 = 128`. Task 5 calls it.

- [ ] **Step 1: Write the failing test**

Add to `render.rs` tests:

```rust
    #[test]
    fn png_is_well_formed_and_byte_deterministic() {
        let stars = vec![star(45.0, 30.0, 3.0), star(-20.0, 200.0, 2.0)];
        let a = chart_png(&stars);
        assert_eq!(a, chart_png(&stars));
        assert!(a.starts_with(&[0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A]));
        assert_eq!(&a[16..20], &MAP_WIDTH.to_be_bytes());
        assert_eq!(&a[20..24], &MAP_HEIGHT.to_be_bytes());
    }
```

Run: `cargo test -p hornvale-astronomy render` — Expected: FAIL, `chart_png` not found.

- [ ] **Step 2: Implement**

Add to `render.rs` (constants near the ASCII ones, functions below `chart_ascii`):

```rust
use crate::pins::NeighborClass;

/// Raster chart width in pixels.
pub const MAP_WIDTH: u32 = 256;
/// Raster chart height in pixels.
pub const MAP_HEIGHT: u32 = 128;

/// Near-black sky field (spec §4).
const FIELD: [u8; 3] = [10, 14, 28];
/// Ring and rim gray-blue.
const RING: [u8; 3] = [60, 72, 100];
/// Index-digit label color.
const LABEL: [u8; 3] = [180, 195, 220];
/// Disc radius in pixels.
const DISC_RADIUS: f64 = 56.0;
/// North-hemisphere disc center (x, y).
const NORTH_CENTER: (f64, f64) = (64.0, 64.0);
/// South-hemisphere disc center (x, y).
const SOUTH_CENTER: (f64, f64) = (192.0, 64.0);

/// 3×5 pixel digit bitmaps for 1–5, one `u8` of 3 bits per row.
const DIGITS: [[u8; 5]; 5] = [
    [0b010, 0b110, 0b010, 0b010, 0b111],
    [0b110, 0b001, 0b010, 0b100, 0b111],
    [0b111, 0b001, 0b011, 0b001, 0b111],
    [0b101, 0b101, 0b111, 0b001, 0b001],
    [0b111, 0b100, 0b110, 0b001, 0b110],
];

/// Star dot color by spectral class.
fn class_rgb(class: NeighborClass) -> [u8; 3] {
    match class {
        NeighborClass::RedDwarf => [200, 70, 50],
        NeighborClass::SunLike => [255, 214, 120],
        NeighborClass::WhiteDwarf => [235, 235, 245],
        NeighborClass::OrangeGiant => [255, 150, 60],
        NeighborClass::RedGiant => [255, 90, 70],
        NeighborClass::BlueGiant => [160, 200, 255],
    }
}

/// Write one pixel, ignoring out-of-bounds coordinates.
fn set_px(pixels: &mut [u8], x: i64, y: i64, color: [u8; 3]) {
    if x < 0 || y < 0 || x >= i64::from(MAP_WIDTH) || y >= i64::from(MAP_HEIGHT) {
        return;
    }
    let i = (y as usize * MAP_WIDTH as usize + x as usize) * 3;
    pixels[i..i + 3].copy_from_slice(&color);
}

/// Solid rim (dec 0) and dashed rings (dec 30, 60) for one disc.
fn draw_disc(pixels: &mut [u8], center: (f64, f64)) {
    for y in 0..i64::from(MAP_HEIGHT) {
        for x in 0..i64::from(MAP_WIDTH) {
            let (dx, dy) = (x as f64 + 0.5 - center.0, y as f64 + 0.5 - center.1);
            let d = (dx * dx + dy * dy).sqrt();
            if (d - DISC_RADIUS).abs() < 0.6 {
                set_px(pixels, x, y, RING);
                continue;
            }
            for ring in [DISC_RADIUS / 3.0, DISC_RADIUS * 2.0 / 3.0] {
                if (d - ring).abs() < 0.5 {
                    let dash = dy.atan2(dx) / std::f64::consts::TAU * 24.0;
                    if dash.rem_euclid(2.0) < 1.0 {
                        set_px(pixels, x, y, RING);
                    }
                }
            }
        }
    }
}

/// Pixel position of a star on its hemisphere's disc: azimuthal
/// equidistant, r = (90 − |δ|)/90 × R; north disc counterclockwise,
/// south clockwise, so RA reads consistently across both.
fn star_xy(n: &Neighbor) -> (f64, f64) {
    let north = n.declination >= 0.0;
    let center = if north { NORTH_CENTER } else { SOUTH_CENTER };
    let r = (90.0 - n.declination.abs()) / 90.0 * DISC_RADIUS;
    let theta = n.right_ascension.to_radians();
    let sy = if north { -theta.sin() } else { theta.sin() };
    (center.0 + r * theta.cos(), center.1 + r * sy)
}

/// Filled dot of the given radius.
fn draw_dot(pixels: &mut [u8], cx: f64, cy: f64, radius: i64, color: [u8; 3]) {
    let (px, py) = (cx as i64, cy as i64);
    for y in -radius..=radius {
        for x in -radius..=radius {
            if x * x + y * y <= radius * radius {
                set_px(pixels, px + x, py + y, color);
            }
        }
    }
}

/// Stamp digit `n` (1–5) with its top-left at (x, y).
fn draw_digit(pixels: &mut [u8], n: usize, x: i64, y: i64) {
    let glyph = DIGITS[n - 1];
    for (row, bits) in glyph.iter().enumerate() {
        for col in 0..3 {
            if bits >> (2 - col) & 1 == 1 {
                set_px(pixels, x + col, y + row as i64, LABEL);
            }
        }
    }
}

/// Render the fixed night sky as a 256×128 planisphere-pair PNG: north
/// celestial hemisphere left, south right, dots sized by brightness rank
/// and colored by class, each tagged with its rank digit (spec §4).
pub fn chart_png(neighbors: &[Neighbor]) -> Vec<u8> {
    let mut pixels = Vec::with_capacity((MAP_WIDTH * MAP_HEIGHT * 3) as usize);
    for _ in 0..MAP_WIDTH * MAP_HEIGHT {
        pixels.extend_from_slice(&FIELD);
    }
    draw_disc(&mut pixels, NORTH_CENTER);
    draw_disc(&mut pixels, SOUTH_CENTER);
    // Dimmest first, so the brighter dot and label win any overlap.
    for (index, n) in neighbors.iter().enumerate().rev() {
        let (x, y) = star_xy(n);
        let radius = match index {
            0 => 3,
            1 | 2 => 2,
            _ => 1,
        };
        draw_dot(&mut pixels, x, y, radius, class_rgb(n.class));
        draw_digit(&mut pixels, index + 1, x as i64 + radius + 2, y as i64 - 2);
    }
    hornvale_kernel::png::encode_rgb(MAP_WIDTH, MAP_HEIGHT, &pixels)
}
```

- [ ] **Step 3: Run to verify pass, eyeball once**

Run: `cargo test -p hornvale-astronomy render` — Expected: PASS.

One-off visual sanity (not committed): write a tiny scratch that calls `chart_png` on a generated seed-42 sky and writes `/tmp/chart-sanity.png`, then `open /tmp/chart-sanity.png` — two ringed discs, colored labeled dots, nothing clipped weirdly. (E.g. temporarily via the Task 5 CLI once it exists, or a throwaway `#[test]` you delete before committing.)

- [ ] **Step 4: Full gate, then commit**

```bash
git add domains/astronomy/src/render.rs
git commit -m "feat(astronomy): planisphere-pair PNG star chart

Two azimuthal-equidistant discs (north/south), dashed declination rings,
class-colored brightness-ranked dots, hand-rolled 3×5 digit labels;
encoded via the kernel PNG encoder."
```

---

### Task 5: CLI `star-chart`, gallery artifacts, CI

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (Sky accessor), `cli/src/main.rs` (command + usage), `.github/workflows/ci.yml`, `book/src/SUMMARY.md`
- Create (generated): `book/src/gallery/star-chart-seed-42.md`, `book/src/gallery/star-chart-seed-42.png`

**Interfaces:**
- Consumes: `render::{chart_ascii, chart_png, moon_lines}` (Tasks 3–4); `class_name` (Task 2); `world_builder::sky_of`.
- Produces: `Sky::system(&self) -> Option<&hornvale_astronomy::StarSystem>`; `hornvale star-chart [--world PATH] [--out PNG]`.

- [ ] **Step 1: Add the Sky accessor**

In `windows/worldgen/src/lib.rs`, `impl Sky`, after `calendar()`:

```rust
    /// The generated star system, if this world has one. `None` for the
    /// tier-0 constant sun. The star-chart command reads this.
    pub fn system(&self) -> Option<&hornvale_astronomy::StarSystem> {
        match self {
            Sky::Constant(_) => None,
            Sky::Generated(sky) => Some(sky.system()),
        }
    }
```

- [ ] **Step 2: Add the CLI command**

In `cli/src/main.rs`: dispatch arm `Some("star-chart") => cmd_star_chart(&args),` after the `settlement-map` arm; usage line after settlement-map's:

```text
  hornvale star-chart [--world <PATH>] [--out <PNG>] render the star chart (markdown to stdout)
```

and the command, in `cmd_map`'s shape:

```rust
/// Render the world's star chart: a markdown page (title, sun line, ASCII
/// chart, star legend, moon phase strips) to stdout and, with `--out`, the
/// planisphere PNG to disk. Both are deterministic; CI drift-checks the
/// committed copies. Errors on a world with no generated sky.
fn cmd_star_chart(args: &[String]) -> Result<(), String> {
    let world = load_world(args)?;
    let sky = world_builder::sky_of(&world).map_err(|e| e.to_string())?;
    let Some(system) = sky.system() else {
        return Err("this world has no generated sky; no chart to draw".to_string());
    };
    let calendar = sky.calendar().expect("a generated sky always has a calendar");
    let mut doc = format!("# The Night Sky of Seed {}\n\n", world.seed.0);
    doc.push_str(&format!("The sun is a {}.\n\n", system.star.class_name));
    doc.push_str("```text\n");
    doc.push_str(&hornvale_astronomy::render::chart_ascii(&system.neighbors));
    doc.push_str("```\n\n");
    for (index, n) in system.neighbors.iter().enumerate() {
        doc.push_str(&format!(
            "{}. A {} — {}, {:.1} light-years away, apparent brightness {:.3}.\n",
            index + 1,
            hornvale_astronomy::class_name(n.class),
            n.color,
            n.distance.0,
            n.apparent_brightness
        ));
    }
    doc.push('\n');
    for line in hornvale_astronomy::render::moon_lines(&system.moons, calendar) {
        doc.push_str(&format!("{line}\n"));
    }
    doc.push('\n');
    if let Some(out) = flag_value(args, "--out") {
        let png = hornvale_astronomy::render::chart_png(&system.neighbors);
        std::fs::write(out, png).map_err(|e| format!("writing {out}: {e}"))?;
        let name = std::path::Path::new(out)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or(out);
        doc.push_str(&format!("![Full-color render](./{name})\n\n"));
    }
    doc.push_str("---\n\n*Generated deterministically: this seed always yields this page.*\n");
    print!("{doc}");
    Ok(())
}
```

If `class_name`/`render` aren't re-exported at the astronomy crate root, either add them to the existing `pub use` list in `domains/astronomy/src/lib.rs` or use the module paths — match how the crate exports `SkyPins` and friends. Check `star.class_name` is the field's real name (it is used in `provider.rs`'s day description). Add a CLI test beside the existing command tests: `star-chart` on a constant-sun world returns the loud error string.

- [ ] **Step 3: Generate the gallery artifacts, wire CI and SUMMARY**

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-sc-sky.json
cargo run -p hornvale -- star-chart --world /tmp/hv-sc-sky.json --out book/src/gallery/star-chart-seed-42.png > book/src/gallery/star-chart-seed-42.md
file book/src/gallery/star-chart-seed-42.png
```

Expected: `PNG image data, 256 x 128, 8-bit/color RGB, non-interlaced`. Open the PNG and the md once — discs, labeled dots, legend and strips reading sanely.

`.github/workflows/ci.yml`, after the second `settlement-map` line:

```yaml
          cargo run -p hornvale -- star-chart --world /tmp/hv-ci-sky.json --out book/src/gallery/star-chart-seed-42.png > book/src/gallery/star-chart-seed-42.md
```

`book/src/SUMMARY.md`, after the "The Sky of Seed 42" line:

```markdown
- [The Night Sky of Seed 42](./gallery/star-chart-seed-42.md)
```

- [ ] **Step 4: Full gate, then commit**

```bash
git add windows/worldgen/src/lib.rs cli/src/main.rs .github/workflows/ci.yml book/src/SUMMARY.md book/src/gallery/star-chart-seed-42.md book/src/gallery/star-chart-seed-42.png
git commit -m "feat(cli): hornvale star-chart — the almanac's promised chart

Markdown page (sun, ASCII chart, legend, moon strips) + planisphere PNG;
gallery artifact drift-checked in CI; Constitution §3.4 paid."
```

---

### Task 6: Book, registry, retrospective

**Files:**
- Create: `book/src/chronicle/19-the-star-chart.md`, `docs/retrospectives/campaign-19.md`
- Modify: `book/src/SUMMARY.md` (chronicle line), `book/src/domains/astronomy.md` (freshness), `docs/vision/idea-registry.md` (SKY-20 flip)

**Interfaces:**
- Consumes: everything merged in Tasks 1–5.
- Produces: the campaign's Definition-of-Done documents (decisions 0013, 0020).

- [ ] **Step 1: Chronicle entry**

Confirm `18-the-meeting.md` is the highest-numbered chronicle, then write `book/src/chronicle/19-the-star-chart.md` (book altitude: technical, comprehensible without the code) covering: the synodic correction (what was wrong, the formula, why the fixture prose already knew), the drawn starfield (uniform on the sphere, a new stream so old skies stand), and the chart itself (both media; show the ASCII chart for seed 42 inline). 60–100 lines, matching the tone of `17-audible-phonology.md`/`18-the-meeting.md`. Add its SUMMARY.md line after the `18-` entry, mirroring the existing chronicle lines' format.

- [ ] **Step 2: Freshness sweep**

`book/src/domains/astronomy.md`: update wherever it describes the neighborhood as position-less or moon phase as sidereal-derived; mention the chart as the domain's committed artifact. Sweep check:

```bash
grep -rn -i "sidereal\|no position\|position-less" book/src/domains/ book/src/gallery/the-sky.md
```

Fix living-chapter hits only (chronicles/decisions/retros stay historical).

- [ ] **Step 3: Registry flip + docs test**

In `docs/vision/idea-registry.md`, SKY-20's row: status `raw` → `shipped`, and point **Where** at the star-chart spec (`../superpowers/specs/2026-07-09-star-chart-design.md`). Leave SKY-12 untouched.

Run: `cargo test -p hornvale --test docs_consistency` — Expected: PASS.

- [ ] **Step 4: Retrospective**

`docs/retrospectives/campaign-19.md`, one page, process-not-product (decision 0020), in the shape of `campaign-17.md`: what the ordering bought (fix before render), what surprised (if anything), what to repeat.

- [ ] **Step 5: Full CI artifact list locally, gate, commit**

Run every command in ci.yml's "Artifacts are current" step, then:

```bash
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
mdbook build book
```

Expected: no drift; clean build.

```bash
git add book/src/chronicle/19-the-star-chart.md book/src/SUMMARY.md book/src/domains/astronomy.md docs/vision/idea-registry.md docs/retrospectives/campaign-19.md
git commit -m "docs(book): chronicle 19 — the star chart; SKY-20 shipped; retro"
```

---

## Definition of Done

- [ ] `synodic_month` public, phase/months synodic, degenerate guard tested; almanac fixtures regenerated.
- [ ] `Neighbor` has drawn dec/RA from `astronomy/neighbor-positions`; pin-isolation property test green; manifest regenerated.
- [ ] `render.rs`: `chart_ascii` (72×24, digit stars, dashed equator), `moon_lines` (synodic strips), `chart_png` (planisphere pair, digit labels) — all byte-deterministic, all tested.
- [ ] `hornvale star-chart` in usage + dispatch; loud error on constant-sun worlds; gallery pair committed; CI line added; SUMMARY updated.
- [ ] Chronicle 19, astronomy-chapter freshness, SKY-20 → `shipped`, retrospective — docs_consistency green.
- [ ] Full gate green on every commit; final local run of the CI artifact list shows zero drift.
