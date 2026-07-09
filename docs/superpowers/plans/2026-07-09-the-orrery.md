# The Orrery — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship in-workspace, deterministic astronomy eyecandy — an ANSI color star map and an ANSI **orrery** (top-down orbital schematic), plus a kernel asciinema-`.cast` encoder so a render evaluated over a time range becomes a single Book-embeddable, drift-checked animation.

**Architecture:** A new `kernel/src/cast.rs` (sibling to `png.rs`) encodes a flip-book of ANSI frames as an asciinema-v2 document with **synthetic** timing — pure `serde_json`, byte-deterministic, no wall-clock. `domains/astronomy/src/render.rs` gains a spectral-class→ANSI-color palette, a colored star map, and `orrery_ansi(system, calendar, t)`. The CLI gains `hornvale orrery`, single-frame and `--cast` animated. The Book embeds `asciinema-player` (a committed static asset) and one drift-checked `.cast`.

**Tech Stack:** Rust edition 2024, std + `serde`/`serde_json` only (decision 0004). Crates touched: `hornvale-kernel`, `hornvale-astronomy`, `hornvale` (CLI), plus `book/`. No new dependency (`cli/tests/architecture.rs` enforces the two-name allowlist workspace-wide).

## Global Constraints

- **`serde` + `serde_json` only; no new crates** (decision 0004; `cli/tests/architecture.rs`). The `.cast` encoder uses `serde_json`; `asciinema-player` is a committed **static** JS/CSS asset under `book/`, never a Cargo dependency.
- **Determinism (constitutional):** same world + same `t` → byte-identical render; same frames+`dt` → byte-identical `.cast`. **No wall-clock** — cast timing is synthetic (`frame k` at `t = k·dt`). No `HashMap`/`HashSet`.
- **Sim emits data, clients render** (decision 0022): these renders read the world directly (the `render.rs` pattern), take no graphics dependency, and make their own palette choices; they do **not** go through scene JSON (that is the sequel campaign).
- **`#![warn(missing_docs)]`:** every public item/field gets a `///`.
- **Committed artifact:** the Book `.cast` joins CI's "Artifacts are current" drift check (Task 6).
- **The full gate before every commit:** `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`. Run `cargo fmt` (no `--check`) as the final step of each task. Never `--no-verify`.
- **ANSI convention:** colors are 256-color SGR escapes (`\x1b[38;5;Nm`), reset (`\x1b[0m`) after every colored glyph; portable and asciinema-player-renderable.
- **Baseline:** confirm counts before starting — `cargo test -p hornvale-kernel 2>&1 | tail -3`, `cargo test -p hornvale-astronomy 2>&1 | tail -3` — and after each task.

---

## File Structure

| File | Action | Responsibility |
| --- | --- | --- |
| `kernel/src/cast.rs` | Create | `asciinema_v2(width, height, dt, frames)` — deterministic `.cast` encoder. |
| `kernel/src/lib.rs` | Modify | `pub mod cast;` + re-export `asciinema_v2`. |
| `domains/astronomy/src/render.rs` | Modify | `spectral_color`/`star_color` palette; `chart_ansi`; `orrery_ansi`; `phase_glyph`. |
| `cli/src/main.rs` | Modify | `hornvale orrery` (single frame + `--cast` animated); help text. |
| `cli/tests/orrery.rs` | Create | Frame-count + determinism of the animated `--cast` output. |
| `book/theme/asciinema-player.min.js`, `.css` | Create | Committed static player asset (no build step). |
| `book/book.toml` | Modify | `additional-js`/`additional-css` for the player. |
| `book/src/gallery/orrery-seed-42.md` + `.cast` | Create | Embedded, drift-checked one-year time-lapse. |
| `.github/workflows/ci.yml` | Modify | Regenerate the committed `.cast` in the artifact step. |

---

### Task 1: The kernel `.cast` encoder

**Files:**
- Create: `kernel/src/cast.rs`
- Modify: `kernel/src/lib.rs`

**Interfaces:**
- Produces: `pub fn asciinema_v2(width: u16, height: u16, dt: f64, frames: &[String]) -> String`.

- [ ] **Step 1: Add the module.** In `kernel/src/lib.rs`, add `pub mod cast;` (alphabetical) and `pub use cast::asciinema_v2;` alongside the other re-exports.

- [ ] **Step 2: Write the failing tests + implementation.** Create `kernel/src/cast.rs`:

```rust
//! Deterministic asciinema-v2 cast encoding (The Orrery spec): a render
//! evaluated over a time range becomes one self-contained animation,
//! embeddable in the Book and convertible to mp4 by external tools
//! (`agg`/ffmpeg). Pure `serde_json`; **synthetic** frame timing (no
//! wall-clock), so the output is byte-deterministic and drift-checkable like
//! every gallery artifact.
#![warn(missing_docs)]

use serde::Serialize;

/// One asciinema output event, serialized as the array `[time, "o", data]`:
/// terminal output `data` shown at relative `time` seconds.
#[derive(Serialize)]
struct Event(f64, &'static str, String);

/// Encode a flip-book of terminal frames as an asciinema-v2 document: a JSON
/// header line, then one output event per frame at synthetic time `k * dt`.
/// Each frame string is emitted verbatim as terminal output — callers include
/// any screen-clear so playback redraws frame by frame. Deterministic: no
/// wall-clock, fixed header key order, `serde_json` escaping.
pub fn asciinema_v2(width: u16, height: u16, dt: f64, frames: &[String]) -> String {
    // Header with fixed key order (no wall-clock `timestamp`, for determinism).
    let mut out = format!(r#"{{"version": 2, "width": {width}, "height": {height}}}"#);
    out.push('\n');
    for (k, frame) in frames.iter().enumerate() {
        let event = Event(k as f64 * dt, "o", frame.clone());
        out.push_str(&serde_json::to_string(&event).expect("event serializes"));
        out.push('\n');
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn header_and_events_are_well_formed() {
        let frames = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        let cast = asciinema_v2(80, 24, 0.5, &frames);
        let mut lines = cast.lines();
        let header: serde_json::Value = serde_json::from_str(lines.next().unwrap()).unwrap();
        assert_eq!(header["version"], 2);
        assert_eq!(header["width"], 80);
        assert_eq!(header["height"], 24);
        let events: Vec<serde_json::Value> =
            lines.map(|l| serde_json::from_str(l).unwrap()).collect();
        assert_eq!(events.len(), 3);
        for (k, ev) in events.iter().enumerate() {
            assert_eq!(ev[0].as_f64().unwrap(), k as f64 * 0.5);
            assert_eq!(ev[1], "o");
        }
        assert_eq!(events[2][2], "c");
    }

    #[test]
    fn encoding_is_byte_deterministic() {
        let frames = vec!["x\ny".to_string(), "\u{1b}[2Jz".to_string()];
        assert_eq!(
            asciinema_v2(40, 10, 0.1, &frames),
            asciinema_v2(40, 10, 0.1, &frames)
        );
    }

    #[test]
    fn ansi_escapes_and_newlines_survive_the_round_trip() {
        let frame = "\u{1b}[38;5;39m*\u{1b}[0m\nline2\n".to_string();
        let cast = asciinema_v2(10, 2, 1.0, std::slice::from_ref(&frame));
        let ev: serde_json::Value = serde_json::from_str(cast.lines().nth(1).unwrap()).unwrap();
        assert_eq!(ev[2].as_str().unwrap(), frame);
    }
}
```

- [ ] **Step 3: Run + gate.** `cargo test -p hornvale-kernel cast && cargo clippy -p hornvale-kernel --all-targets -- -D warnings`. Expected PASS. `cargo fmt`.

- [ ] **Step 4: Commit.**

```bash
git add kernel/src/cast.rs kernel/src/lib.rs
git commit -m "feat(kernel): deterministic asciinema-v2 .cast encoder (The Orrery)

Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9"
```

---

### Task 2: The spectral-class palette

**Files:**
- Modify: `domains/astronomy/src/render.rs`

**Interfaces:**
- Produces: `pub fn spectral_color(class: NeighborClass) -> &'static str` (a 256-color SGR escape); `pub fn star_color(class_name: &str) -> &'static str` (anchor star, from its `class_name`). Both total.

- [ ] **Step 1: Write the failing test.** In `render.rs` tests, add:

```rust
    #[test]
    fn spectral_color_is_total_and_distinct_by_temperature() {
        use crate::pins::NeighborClass;
        // Every variant maps to a non-empty SGR escape.
        for class in [
            NeighborClass::BlueGiant,
            NeighborClass::WhiteDwarf,
            NeighborClass::SunLike,
            NeighborClass::OrangeGiant,
            NeighborClass::RedGiant,
            NeighborClass::RedDwarf,
        ] {
            let c = spectral_color(class);
            assert!(c.starts_with("\u{1b}[38;5;") && c.ends_with('m'), "{class:?}: {c:?}");
        }
        // Hot and cool ends differ.
        assert_ne!(spectral_color(NeighborClass::BlueGiant), spectral_color(NeighborClass::RedDwarf));
        // The anchor star classifies from its class_name letter.
        assert_eq!(star_color("yellow dwarf (G)"), spectral_color(NeighborClass::SunLike));
    }
```

**Note:** the `NeighborClass` variants (verified in `domains/astronomy/src/pins.rs`) are exactly `RedDwarf`, `SunLike`, `WhiteDwarf`, `OrangeGiant`, `RedGiant`, `BlueGiant` — used verbatim below.

- [ ] **Step 2: Implement.** In `render.rs`, add (adjust the match arms to the real variants):

```rust
use crate::pins::NeighborClass;

/// A star's terminal color from its spectral class — a **render** decision
/// (paint), not domain data: hot O/B blue-white through cool M red. 256-color
/// SGR escape; pair with a `\x1b[0m` reset. Total over `NeighborClass`.
pub fn spectral_color(class: NeighborClass) -> &'static str {
    match class {
        NeighborClass::BlueGiant => "\u{1b}[38;5;39m",   // blue-white
        NeighborClass::WhiteDwarf => "\u{1b}[38;5;255m", // white
        NeighborClass::SunLike => "\u{1b}[38;5;220m",    // yellow
        NeighborClass::OrangeGiant => "\u{1b}[38;5;208m", // orange
        NeighborClass::RedGiant => "\u{1b}[38;5;196m",   // red
        NeighborClass::RedDwarf => "\u{1b}[38;5;124m",   // dim red
    }
}

/// The anchor star's color from its human-readable `class_name` (which carries
/// the spectral letter, e.g. "yellow dwarf (G)"). Falls back to Sun-like.
pub fn star_color(class_name: &str) -> &'static str {
    let letter = class_name
        .rsplit('(')
        .next()
        .and_then(|s| s.chars().find(|c| c.is_ascii_alphabetic()))
        .unwrap_or('G');
    match letter.to_ascii_uppercase() {
        'O' | 'B' => spectral_color(NeighborClass::BlueGiant),
        'A' => spectral_color(NeighborClass::WhiteDwarf),
        'F' | 'G' => spectral_color(NeighborClass::SunLike),
        'K' => spectral_color(NeighborClass::OrangeGiant),
        'M' => spectral_color(NeighborClass::RedDwarf),
        _ => spectral_color(NeighborClass::SunLike),
    }
}
```

- [ ] **Step 3: Run + gate + commit.**

```bash
cargo test -p hornvale-astronomy spectral && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings
cargo fmt
git add domains/astronomy/src/render.rs
git commit -m "feat(astronomy): spectral-class ANSI palette for the star renders

Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9"
```

If a `NeighborClass` variant name differs from the guesses above, use the real names throughout — do not invent variants.

---

### Task 3: The color star map

**Files:**
- Modify: `domains/astronomy/src/render.rs`

**Interfaces:**
- Produces: `pub fn chart_ansi(neighbors: &[Neighbor]) -> String` — `chart_ascii`'s colored sibling.

- [ ] **Step 1: Write the failing test.** In `render.rs` tests:

```rust
    #[test]
    fn chart_ansi_is_deterministic_and_tinted() {
        let stars = sample_neighbors(); // reuse the existing test fixture builder
        let a = chart_ansi(&stars);
        assert_eq!(a, chart_ansi(&stars), "must be deterministic");
        assert!(a.contains("\u{1b}[38;5;"), "stars must be colored");
        assert!(a.contains("\u{1b}[0m"), "colors must reset");
        // Same 72×24 shape as the ASCII chart: 24 newline-terminated rows.
        assert_eq!(a.matches('\n').count(), ASCII_HEIGHT);
    }
```

Use the existing chart tests' neighbor fixture (grep the test module for how `chart_png`/`chart_ascii` tests build `stars`); mirror it.

- [ ] **Step 2: Implement.** In `render.rs`, add `chart_ansi` — the same plotting as `chart_ascii` but into a `(char, color)` grid, emitting SGR escapes:

```rust
/// The fixed night sky as a 72×24 ANSI chart: each star its brightness-rank
/// digit, tinted by spectral class; the celestial equator dashed. The colored
/// sibling of [`chart_ascii`]; same layout, same determinism.
pub fn chart_ansi(neighbors: &[Neighbor]) -> String {
    let mut grid: Vec<Vec<(char, &'static str)>> = vec![vec![(' ', ""); ASCII_WIDTH]; ASCII_HEIGHT];
    let equator = ASCII_HEIGHT / 2;
    for (col, cell) in grid[equator].iter_mut().enumerate() {
        if col % 2 == 0 {
            *cell = ('-', "");
        }
    }
    for (index, neighbor) in neighbors.iter().enumerate().rev() {
        let col = ((neighbor.right_ascension / 360.0) * ASCII_WIDTH as f64) as usize;
        let row = (((90.0 - neighbor.declination) / 180.0) * ASCII_HEIGHT as f64) as usize;
        let digit =
            char::from_digit(index as u32 + 1, 10).expect("digit glyphs run out past 9 neighbors");
        grid[row.min(ASCII_HEIGHT - 1)][col.min(ASCII_WIDTH - 1)] =
            (digit, spectral_color(neighbor.class));
    }
    emit_ansi_grid(&grid)
}

/// Emit a `(glyph, sgr)` grid as rows, resetting after every colored cell.
fn emit_ansi_grid(grid: &[Vec<(char, &'static str)>]) -> String {
    let mut out = String::new();
    for row in grid {
        for &(ch, color) in row {
            if color.is_empty() {
                out.push(ch);
            } else {
                out.push_str(color);
                out.push(ch);
                out.push_str("\u{1b}[0m");
            }
        }
        out.push('\n');
    }
    out
}
```

- [ ] **Step 3: Run + gate + commit.**

```bash
cargo test -p hornvale-astronomy chart_ansi && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings
cargo fmt
git add domains/astronomy/src/render.rs
git commit -m "feat(astronomy): ANSI color star map (chart_ansi)

Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9"
```

---

### Task 4: The orrery

**Files:**
- Modify: `domains/astronomy/src/render.rs`

**Interfaces:**
- Produces: `pub const ORRERY_WIDTH/HEIGHT`; `pub fn orrery_ansi(system: &StarSystem, calendar: &Calendar, t: StdDays) -> String`.

- [ ] **Step 1: Write the failing tests.** In `render.rs` tests (build a system via `generate` as the calendar tests do):

```rust
    #[test]
    fn orrery_is_deterministic_and_structured() {
        use crate::calendar::calendar_of;
        use crate::pins::{MoonsPin, RotationPin, SkyPins};
        use crate::system::generate;
        use crate::units::StdDays;
        use hornvale_kernel::Seed;
        let system = generate(
            Seed(42),
            &SkyPins {
                rotation: Some(RotationPin::PeriodHours(24.0)),
                moons: Some(MoonsPin::exact(2).unwrap()),
                ..SkyPins::default()
            },
        )
        .unwrap()
        .system;
        let cal = calendar_of(&system);
        let t = StdDays::new(10.0).unwrap();
        let a = orrery_ansi(&system, &cal, t);
        assert_eq!(a, orrery_ansi(&system, &cal, t), "deterministic in (system, t)");
        assert_eq!(a.matches('\n').count(), ORRERY_HEIGHT, "one row per line");
        assert!(a.contains('*'), "the star sits at center");
        assert!(a.contains('O'), "the world is drawn");
        // The world moves: a half-year later the frame differs.
        let half = StdDays::new(10.0 + system.anchor.year.get() / 2.0).unwrap();
        assert_ne!(a, orrery_ansi(&system, &cal, half), "the world orbits over time");
    }
```

- [ ] **Step 2: Implement.** In `render.rs` add (imports: `use crate::system::StarSystem; use crate::units::StdDays;`):

```rust
/// Orrery grid width in characters.
pub const ORRERY_WIDTH: usize = 61;
/// Orrery grid height in characters (terminal cells read ~2:1 tall, so the
/// x-axis is scaled ×2 for round orbits).
pub const ORRERY_HEIGHT: usize = 31;

/// A synodic-phase glyph: `o` new, `)` waxing, `O` full, `(` waning — the
/// provider's phase-word thresholds.
fn phase_glyph(phase: f64) -> char {
    if !(0.125..0.875).contains(&phase) {
        'o'
    } else if phase < 0.5 {
        ')'
    } else if phase < 0.625 {
        'O'
    } else {
        '('
    }
}

/// A top-down ANSI orbital schematic of the system at absolute time `t`
/// (standard days): the star at center (class-colored), the habitable zone as
/// a dotted ring, the world on its orbit at the year's phase, and each moon in
/// a tight ring around the world showing its synodic phase. Deterministic in
/// `(system, t)`. A schematic, not to scale beyond the orbit-to-grid fit.
pub fn orrery_ansi(system: &StarSystem, calendar: &Calendar, t: StdDays) -> String {
    let (w, h) = (ORRERY_WIDTH, ORRERY_HEIGHT);
    let (cx, cy) = (w as f64 / 2.0, h as f64 / 2.0);
    let aspect = 2.0; // char cells ~2:1 tall
    let mut grid: Vec<Vec<(char, &'static str)>> = vec![vec![(' ', ""); w]; h];

    let plot = |grid: &mut Vec<Vec<(char, &'static str)>>,
                r: f64,
                theta: f64,
                ch: char,
                color: &'static str| {
        let x = (cx + aspect * r * theta.cos()).round() as isize;
        let y = (cy + r * theta.sin()).round() as isize;
        if x >= 0 && (x as usize) < w && y >= 0 && (y as usize) < h {
            grid[y as usize][x as usize] = (ch, color);
        }
    };

    // Scale so the world's orbit sits at 80% of the half-height.
    let orbit = system.anchor.orbit.get();
    let scale = ((h as f64 / 2.0 - 1.0) * 0.8) / orbit;

    // Habitable-zone ring (dotted, uncolored).
    let hz_in = system.star.habitable_zone.inner().get() * scale;
    let hz_out = system.star.habitable_zone.outer().get() * scale;
    let mut a = 0.0_f64;
    while a < std::f64::consts::TAU {
        plot(&mut grid, hz_in, a, '.', "");
        plot(&mut grid, hz_out, a, '.', "");
        a += 0.15;
    }

    // The star at center.
    grid[cy as usize][cx as usize] = ('*', star_color(&system.star.class_name));

    // The world on its orbit (green), and its screen position.
    let theta = std::f64::consts::TAU * calendar.year_phase(t);
    let world_r = orbit * scale;
    plot(&mut grid, world_r, theta, 'O', "\u{1b}[38;5;42m");
    let wx = cx + aspect * world_r * theta.cos();
    let wy = cy + world_r * theta.sin();

    // Moons: sidereal orbital angle around the world, synodic-phase glyph.
    for (index, moon) in system.moons.iter().enumerate() {
        let ma = std::f64::consts::TAU * (t.0 / moon.period.get()).fract();
        let mr = 2.0 + index as f64;
        let glyph = calendar.moon_phase(t, index).map(phase_glyph).unwrap_or('o');
        let x = (wx + aspect * mr * ma.cos()).round() as isize;
        let y = (wy + mr * ma.sin()).round() as isize;
        if x >= 0 && (x as usize) < w && y >= 0 && (y as usize) < h {
            grid[y as usize][x as usize] = (glyph, "\u{1b}[38;5;250m");
        }
    }

    emit_ansi_grid(&grid)
}
```

- [ ] **Step 3: Run + gate + commit.**

```bash
cargo test -p hornvale-astronomy orrery && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings
cargo fmt
git add domains/astronomy/src/render.rs
git commit -m "feat(astronomy): the orrery — a top-down ANSI orbital schematic (function of t)

Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9"
```

---

### Task 5: The CLI — `hornvale orrery`, single-frame and animated

**Files:**
- Modify: `cli/src/main.rs`
- Create: `cli/tests/orrery.rs`

**Interfaces:**
- Consumes: `hornvale_astronomy::render::{orrery_ansi, chart_ansi}`, `hornvale_kernel::asciinema_v2`, the `sky_of`/`system()`/`calendar()` pattern of `cmd_star_chart`.
- Produces: `hornvale orrery [--world PATH] [--day D]` prints one frame; `hornvale orrery [--world PATH] --day A..B [--step k] --cast OUT.cast` writes an animation.

- [ ] **Step 1: Add the command + help.** In `cli/src/main.rs`, register `Some("orrery") => cmd_orrery(&args),` in the subcommand match, and add a help line near the `star-chart` one:

```
  hornvale orrery [--world <PATH>] [--day <D>]            print one orrery frame (ANSI)
  hornvale orrery [--world <PATH>] --day <A..B> [--step <k>] --cast <OUT>   animate to a .cast
```

- [ ] **Step 2: Implement `cmd_orrery`.** Mirror `cmd_star_chart`'s world/sky/calendar loading. Parse `--day` as either a single `f64` or an `A..B` range; `--step` (default 1.0); `--cast OUT` triggers animation. A single frame prints `orrery_ansi(system, calendar, t)`; the animated path collects a frame per step (each prefixed with a clear-and-home so playback redraws) and writes `asciinema_v2(...)`:

```rust
/// Render the orrery: one ANSI frame to stdout, or — with `--day A..B --cast
/// OUT` — a deterministic asciinema-v2 animation of the system over that span
/// (the moons cycling, the world orbiting, the sky drifting). Errors on a
/// world with no generated sky.
fn cmd_orrery(args: &[String]) -> Result<(), String> {
    use hornvale_astronomy::StdDays;
    use hornvale_astronomy::render::{ORRERY_HEIGHT, ORRERY_WIDTH, orrery_ansi};
    let world = load_world(args)?;
    let sky = world_builder::sky_of(&world).map_err(|e| e.to_string())?;
    let Some(system) = sky.system() else {
        return Err("this world has no generated sky; no orrery to draw".to_string());
    };
    let calendar = sky.calendar().expect("a generated sky always has a calendar");
    let day_arg = flag_value(args, "--day").unwrap_or("0");
    let mk = |d: f64| StdDays::new(d).map_err(|e| e.to_string());

    if let Some(cast_path) = flag_value(args, "--cast") {
        let (a, b) = day_arg
            .split_once("..")
            .ok_or("--cast needs a --day range, e.g. --day 0..365")?;
        let (a, b): (f64, f64) = (a.parse().map_err(|_| "bad --day start")?, b.parse().map_err(|_| "bad --day end")?);
        let step: f64 = flag_value(args, "--step").unwrap_or("1").parse().map_err(|_| "bad --step")?;
        if !(step > 0.0) || !(b > a) {
            return Err("--day A..B must have B>A and --step>0".to_string());
        }
        let mut frames = Vec::new();
        let mut d = a;
        while d < b {
            // Clear + home, then the frame, so playback redraws in place.
            frames.push(format!("\u{1b}[2J\u{1b}[H{}", orrery_ansi(system, calendar, mk(d)?)));
            d += step;
        }
        // 25 fps feels smooth; the value is synthetic, only the ratio matters.
        let cast = hornvale_kernel::asciinema_v2(ORRERY_WIDTH as u16, ORRERY_HEIGHT as u16, 0.04, &frames);
        std::fs::write(cast_path, cast).map_err(|e| format!("writing {cast_path}: {e}"))?;
        println!("wrote {} frames to {cast_path}", frames.len());
        Ok(())
    } else {
        let t = mk(day_arg.parse().map_err(|_| "bad --day")?)?;
        print!("{}", orrery_ansi(system, calendar, t));
        Ok(())
    }
}
```

(`StdDays` is re-exported at the astronomy crate root — verified in `domains/astronomy/src/lib.rs` — so `hornvale_astronomy::StdDays` is correct.)

- [ ] **Step 3: Write the CLI test.** Create `cli/tests/orrery.rs` (mirror `sky_exit_criterion.rs`'s `bin()`/temp-dir helpers):

```rust
//! The orrery CLI: single-frame print and deterministic .cast animation.
use std::process::Command;

fn bin() -> Command {
    Command::new(env!("CARGO_BIN_EXE_hornvale"))
}

#[test]
fn orrery_frame_and_cast_are_deterministic() {
    let dir = std::env::temp_dir().join(format!("hv-orrery-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let world = dir.join("w.json");
    assert!(bin().args(["new", "--seed", "42", "--out", world.to_str().unwrap()]).status().unwrap().success());

    // Single frame: deterministic, contains the star.
    let one = bin().args(["orrery", "--world", world.to_str().unwrap(), "--day", "10"]).output().unwrap();
    assert!(one.status.success());
    let frame = String::from_utf8(one.stdout).unwrap();
    assert!(frame.contains('*'), "frame has the star");

    // Animated: N frames for 0..10 step 2 => 5 events; deterministic bytes.
    let cast = dir.join("a.cast");
    let run = || {
        assert!(bin().args(["orrery", "--world", world.to_str().unwrap(), "--day", "0..10", "--step", "2", "--cast", cast.to_str().unwrap()]).status().unwrap().success());
        std::fs::read(&cast).unwrap()
    };
    let first = run();
    assert_eq!(first, run(), "the .cast is byte-deterministic");
    let text = String::from_utf8(first).unwrap();
    let events = text.lines().skip(1).count();
    assert_eq!(events, 5, "0..10 step 2 => 5 frames");
    std::fs::remove_dir_all(&dir).unwrap();
}
```

- [ ] **Step 4: Run the full gate + commit.**

```bash
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add cli/src/main.rs cli/tests/orrery.rs
git commit -m "feat(cli): hornvale orrery — single frame and deterministic .cast animation

Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9"
```

---

### Task 6: The Book — embed the player and a committed time-lapse

**Files:**
- Create: `book/theme/asciinema-player.min.js`, `book/theme/asciinema-player.min.css`
- Modify: `book/book.toml`
- Create: `book/src/gallery/orrery-seed-42.md`, `book/src/gallery/orrery-seed-42.cast`
- Modify: `book/src/SUMMARY.md` (link the gallery page), `.github/workflows/ci.yml`

- [ ] **Step 1: Vendor the player (static asset, no build).** Download the pinned `asciinema-player` standalone bundle (`.min.js` + `.min.css`, a released version) into `book/theme/`. These are committed static assets — **not** Cargo dependencies (the architecture test is unaffected). Record the version in a one-line comment at the top of a small `book/theme/asciinema-README.md`.

- [ ] **Step 2: Wire mdbook.** In `book/book.toml`, under `[output.html]`, add:

```toml
additional-css = ["theme/asciinema-player.min.css"]
additional-js = ["theme/asciinema-player.min.js"]
```

- [ ] **Step 3: Generate the committed `.cast`.** From the repo root:

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-orrery.json
cargo run -p hornvale -- orrery --world /tmp/hv-orrery.json --day 0..365 --step 5 --cast book/src/gallery/orrery-seed-42.cast
```

- [ ] **Step 4: Write the gallery page.** Create `book/src/gallery/orrery-seed-42.md` embedding the player against the committed `.cast` (an mdbook page can carry inline HTML/JS):

```markdown
# The Orrery of Seed 42

A year in the life of seed 42's system: the world circling its star, the moons
turning through their synodic phases. Generated deterministically — this seed
always yields this animation.

<div id="orrery"></div>
<script>
  AsciinemaPlayer.create('./orrery-seed-42.cast', document.getElementById('orrery'), { loop: true, autoPlay: true, cols: 61, rows: 31 });
</script>
```

Add a `- [The Orrery of Seed 42](./gallery/orrery-seed-42.md)` line to `book/src/SUMMARY.md` near the other gallery entries.

- [ ] **Step 5: Add regeneration to CI.** In `.github/workflows/ci.yml`'s "Artifacts are current" step, add (after the star-chart line, reusing its already-generated world if present, else the `new` above):

```bash
cargo run -p hornvale -- orrery --world /tmp/hv-ci-sky.json --day 0..365 --step 5 --cast book/src/gallery/orrery-seed-42.cast
```

Confirm the world path matches the one that step already builds (`/tmp/hv-ci-sky.json`).

- [ ] **Step 6: Verify determinism + build + gate.**

```bash
mdbook build book
# Idempotence: regenerating the .cast changes nothing.
cargo run -p hornvale -- orrery --world /tmp/hv-orrery.json --day 0..365 --step 5 --cast book/src/gallery/orrery-seed-42.cast
git diff --exit-code book/src/gallery/orrery-seed-42.cast && echo "cast idempotent"
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ && echo "artifact drift clean"
```

- [ ] **Step 7: Commit.**

```bash
git add book/theme/asciinema-player.min.js book/theme/asciinema-player.min.css book/theme/asciinema-README.md book/book.toml book/src/gallery/orrery-seed-42.md book/src/gallery/orrery-seed-42.cast book/src/SUMMARY.md .github/workflows/ci.yml
git commit -m "docs(book): embed the orrery time-lapse (asciinema-player + committed .cast)

Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9"
```

---

## Self-Review Notes

**Spec coverage (against `2026-07-09-the-orrery-design.md`):** §3.1 cast encoder → Task 1; §3.2 palette/star-map/orrery → Tasks 2–4; §3.3 CLI → Task 5; §3.4 Book → Task 6; §4 palette → Task 2; §5 determinism/drift → Tasks 1,4,6; §6 testing → each task's tests. §8 deferred (scene JSON, TUI, mp4/GIF) is untouched — no scene module, no interactive client, no video encoder.

**Read-the-engine notes:** `cast.rs` mirrors `kernel/src/png.rs` (encoder-in-kernel, deterministic bytes, structural round-trip test). The renders mirror `chart_ascii` (grid → String, RA/dec plotting, `ASCII_WIDTH/HEIGHT`). `cmd_orrery` mirrors `cmd_star_chart` (`load_world`/`sky_of`/`system()`/`calendar()`/`flag_value`). Two source facts were verified during plan-writing and used verbatim: the `NeighborClass` variants (`RedDwarf`/`SunLike`/`WhiteDwarf`/`OrangeGiant`/`RedGiant`/`BlueGiant`, in `pins.rs`) and that `StdDays` re-exports at the astronomy crate root.

**Determinism watch:** the `.cast` uses synthetic `k·dt` timing and omits the header `timestamp` — the two things that would otherwise inject wall-clock and break the drift check. Task 6 asserts idempotence explicitly. `serde_json`'s f64 formatting is deterministic for identical inputs, so the byte-identity tests are the guard.

**Dependency watch:** no crate is added. `asciinema-player` is a committed static asset under `book/theme/`, outside Cargo's view; `cli/tests/architecture.rs` stays green. The `.cast` encoder is `serde`/`serde_json` only.

**Concurrency watch (main is volatile):** Task 6 touches the CI artifact-command list and `book/` — the seam shared with other rendering campaigns. Run `git merge-base HEAD main` before Task 6 and reconcile the CI step if it moved.
```
