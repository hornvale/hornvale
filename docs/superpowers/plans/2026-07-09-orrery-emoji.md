# The Orrery in Emoji — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** An additive `--glyphs emoji` mode for `hornvale orrery`, on a grid where every cell is two display columns so emoji align — the Unicode orrery stays the default and byte-stable — plus a committed emoji `.cast` stacked with the Unicode one in the book.

**Architecture:** The orrery refactors from a `(char, color)` grid to a **semantic `Cell` grid** rendered by a chosen `GlyphSet` (`Unicode` = single-width, default, byte-identical to today; `Emoji` = two columns per cell). `orrery_ansi` gains a `glyphs: GlyphSet` parameter; `orrery_cols(glyphs)` reports a row's display width (`ORRERY_WIDTH` or `2×`). The CLI gains `--glyphs <unicode|emoji>`, sizing the `.cast` header accordingly. The book carries both `.cast`s, stacked.

**Tech Stack:** Rust edition 2024, `serde`/`serde_json` + std only (decision 0004). Crates touched: `hornvale-astronomy`, `hornvale` (CLI), `book/`. No new dependency. (Display width is *not* measured in tests — that needs a Unicode-width crate, which is banned; the grid guarantees two-column-*designed* cells and the rendered look is verified by watching the player.)

## Global Constraints

- **Additive, Unicode byte-stable:** the refactor must leave `orrery_ansi(..., GlyphSet::Unicode)` byte-identical to today, proven by regenerating the committed `orrery-seed-42.cast` and getting **no drift** (Task 1). Emoji is a new mode, never the default.
- **`serde`/`serde_json` only; no new crates** (`cli/tests/architecture.rs`).
- **Determinism (constitutional):** same seed + span + glyph set → byte-identical `.cast` (synthetic timing, no wall clock). No `HashMap`/`HashSet`.
- **The width invariant:** under `Emoji`, every cell renders to exactly two display columns (an emoji, or `·`+space, or two spaces), so a row is `2 × ORRERY_WIDTH` columns *by construction*. The `.cast` header `width` matches (`orrery_cols`).
- **`#![warn(missing_docs)]`** on public items.
- **Book DoD is explicit tasks here** (the lesson from The Orrery's retrospective): the gallery page, the CI line, and the chapter note are Task 3, not an assumed epilogue. This is an *increment* of Campaign 23 — **no new chronicle or retrospective**.
- **The full gate before every commit:** `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`, plus the artifact drift check for Task 3. `cargo fmt` is the final step of each task. Never `--no-verify`.
- **Baseline:** `cargo test -p hornvale-astronomy 2>&1 | tail -3` before starting and after each task.

---

## File Structure

| File | Action | Responsibility |
| --- | --- | --- |
| `domains/astronomy/src/render.rs` | Modify | `Cell`/`Phase`/`GlyphSet`; `orrery_ansi(..., glyphs)`; `orrery_cols`; both glyph sets; tests. |
| `cli/src/main.rs` | Modify | `--glyphs <unicode\|emoji>`; size the `.cast` header via `orrery_cols`; help text. |
| `cli/tests/orrery.rs` | Modify | Emoji `.cast` header width + determinism. |
| `book/src/gallery/orrery-emoji-seed-42.cast` | Create | The committed emoji time-lapse. |
| `book/src/gallery/orrery-seed-42.md` | Modify | Stack both players (Unicode + emoji), captioned. |
| `book/src/domains/astronomy.md` | Modify | The orrery paragraph names the emoji mode. |
| `.github/workflows/ci.yml` | Modify | Regenerate the emoji `.cast` in the artifact step. |

---

### Task 1: Refactor the orrery to semantic cells + glyph sets

**Files:** Modify `domains/astronomy/src/render.rs`.

**Interfaces:**
- Produces: `pub enum GlyphSet { Unicode, Emoji }`; `pub fn orrery_cols(glyphs: GlyphSet) -> usize`; `pub fn orrery_ansi(system: &StarSystem, calendar: &Calendar, t: StdDays, glyphs: GlyphSet) -> String` (the `t`-only signature gains `glyphs`).

- [ ] **Step 1: Add the types.** Above `orrery_ansi`, replacing `phase_glyph`:

```rust
/// Which glyphs the orrery draws with. `Unicode` is single-width (the default);
/// `Emoji` renders every cell as two display columns so emoji align.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GlyphSet {
    /// Single-width Unicode symbols (`★ ● · ○◐●◑`).
    Unicode,
    /// Two-column emoji (`🌞 🌍 🌑🌓🌕🌗`); each cell is two display columns.
    Emoji,
}

/// A moon's illumination phase, bucketed to one of four glyphs.
#[derive(Clone, Copy)]
enum Phase {
    /// Near new (dark).
    New,
    /// Waxing.
    Waxing,
    /// Near full.
    Full,
    /// Waning.
    Waning,
}

/// Bucket a synodic phase in `[0,1)` — the provider's phase-word thresholds.
fn phase_bucket(phase: f64) -> Phase {
    if !(0.125..0.875).contains(&phase) {
        Phase::New
    } else if phase < 0.5 {
        Phase::Waxing
    } else if phase < 0.625 {
        Phase::Full
    } else {
        Phase::Waning
    }
}

/// One orrery cell's semantic content; a `GlyphSet` renders it to a string.
#[derive(Clone, Copy)]
enum Cell {
    /// Nothing here.
    Empty,
    /// A habitable-zone ring dot.
    Ring,
    /// The central star.
    Star,
    /// The world.
    World,
    /// A moon at the given phase.
    Moon(Phase),
}

impl GlyphSet {
    /// Display columns per cell: 1 for `Unicode`, 2 for `Emoji`.
    fn cell_cols(self) -> usize {
        match self {
            GlyphSet::Unicode => 1,
            GlyphSet::Emoji => 2,
        }
    }

    /// This glyph set's display string for a cell. Every `Emoji` string is two
    /// columns wide by design (an emoji, `·`+space, or two spaces); every
    /// `Unicode` string is one.
    fn glyph(self, cell: Cell) -> &'static str {
        match (self, cell) {
            (GlyphSet::Unicode, Cell::Empty) => " ",
            (GlyphSet::Unicode, Cell::Ring) => "·",
            (GlyphSet::Unicode, Cell::Star) => "★",
            (GlyphSet::Unicode, Cell::World) => "●",
            (GlyphSet::Unicode, Cell::Moon(Phase::New)) => "○",
            (GlyphSet::Unicode, Cell::Moon(Phase::Waxing)) => "◐",
            (GlyphSet::Unicode, Cell::Moon(Phase::Full)) => "●",
            (GlyphSet::Unicode, Cell::Moon(Phase::Waning)) => "◑",
            (GlyphSet::Emoji, Cell::Empty) => "  ",
            (GlyphSet::Emoji, Cell::Ring) => "· ",
            (GlyphSet::Emoji, Cell::Star) => "🌞",
            (GlyphSet::Emoji, Cell::World) => "🌍",
            (GlyphSet::Emoji, Cell::Moon(Phase::New)) => "🌑",
            (GlyphSet::Emoji, Cell::Moon(Phase::Waxing)) => "🌓",
            (GlyphSet::Emoji, Cell::Moon(Phase::Full)) => "🌕",
            (GlyphSet::Emoji, Cell::Moon(Phase::Waning)) => "🌗",
        }
    }
}

/// The display width (columns) of one orrery row in this glyph set:
/// `ORRERY_WIDTH` for `Unicode`, twice that for `Emoji`.
pub fn orrery_cols(glyphs: GlyphSet) -> usize {
    ORRERY_WIDTH * glyphs.cell_cols()
}
```

- [ ] **Step 2: Rewrite `orrery_ansi` over the cell grid.** Replace the body with a `Vec<Vec<(Cell, &'static str)>>` grid and a glyph-set emit. Keep every geometric computation (scale, `plot`, ring loop, star/world/moon placement, colors) identical — only the cell type and the emit change:

```rust
pub fn orrery_ansi(
    system: &StarSystem,
    calendar: &Calendar,
    t: StdDays,
    glyphs: GlyphSet,
) -> String {
    let (w, h) = (ORRERY_WIDTH, ORRERY_HEIGHT);
    let (cx, cy) = (w as f64 / 2.0, h as f64 / 2.0);
    let aspect = 2.0;
    let mut grid: Vec<Vec<(Cell, &'static str)>> = vec![vec![(Cell::Empty, ""); w]; h];

    let plot = |grid: &mut Vec<Vec<(Cell, &'static str)>>,
                r: f64,
                theta: f64,
                cell: Cell,
                color: &'static str| {
        let x = (cx + aspect * r * theta.cos()).round() as isize;
        let y = (cy + r * theta.sin()).round() as isize;
        if x >= 0 && (x as usize) < w && y >= 0 && (y as usize) < h {
            grid[y as usize][x as usize] = (cell, color);
        }
    };

    let orbit = system.anchor.orbit.get();
    let scale = ((h as f64 / 2.0 - 1.0) * 0.8) / orbit;

    let hz_in = system.star.habitable_zone.inner().get() * scale;
    let hz_out = system.star.habitable_zone.outer().get() * scale;
    let mut a = 0.0_f64;
    while a < std::f64::consts::TAU {
        plot(&mut grid, hz_in, a, Cell::Ring, "");
        plot(&mut grid, hz_out, a, Cell::Ring, "");
        a += 0.15;
    }

    grid[cy as usize][cx as usize] = (Cell::Star, star_color(&system.star.class_name));

    let theta = std::f64::consts::TAU * calendar.year_phase(t);
    let world_r = orbit * scale;
    plot(&mut grid, world_r, theta, Cell::World, "\u{1b}[38;5;42m");
    let wx = cx + aspect * world_r * theta.cos();
    let wy = cy + world_r * theta.sin();

    for (index, moon) in system.moons.iter().enumerate() {
        let ma = std::f64::consts::TAU * (t.0 / moon.period.get()).fract();
        let mr = 2.0 + index as f64;
        // A degenerate moon (no synodic cycle) reads as new; unreachable at
        // genesis (the Hill cap) and present in no committed artifact.
        let cell = Cell::Moon(
            calendar
                .moon_phase(t, index)
                .map(phase_bucket)
                .unwrap_or(Phase::New),
        );
        let x = (wx + aspect * mr * ma.cos()).round() as isize;
        let y = (wy + mr * ma.sin()).round() as isize;
        if x >= 0 && (x as usize) < w && y >= 0 && (y as usize) < h {
            grid[y as usize][x as usize] = (cell, "\u{1b}[38;5;250m");
        }
    }

    // Emit: each cell → its glyph-set string, colored, one row per line.
    let mut out = String::new();
    for row in grid {
        for (cell, color) in row {
            let s = glyphs.glyph(cell);
            if color.is_empty() {
                out.push_str(s);
            } else {
                out.push_str(color);
                out.push_str(s);
                out.push_str("\u{1b}[0m");
            }
        }
        out.push('\n');
    }
    out
}
```

Note: `emit_ansi_grid` may still be used by `chart_ansi` — leave it; only the orrery stops using it. The old `phase_glyph` is deleted (replaced by `phase_bucket` + `Cell::Moon`).

- [ ] **Step 3: Update the orrery test to pass a glyph set + assert both.** In the render tests, change the existing `orrery_is_deterministic_and_structured` to call `orrery_ansi(&system, &cal, t, GlyphSet::Unicode)` and keep its assertions (`contains('★')`, `contains('●')`, row count via `ORRERY_HEIGHT`, time-variance). Add:

```rust
    #[test]
    fn orrery_glyph_sets_have_the_right_row_width_and_symbols() {
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
        assert_eq!(orrery_cols(GlyphSet::Unicode), ORRERY_WIDTH);
        assert_eq!(orrery_cols(GlyphSet::Emoji), 2 * ORRERY_WIDTH);
        let emoji = orrery_ansi(&system, &cal, t, GlyphSet::Emoji);
        // Emoji mode uses only the emoji set — none of the single-width Unicode
        // symbols leak in (which would break the two-column grid).
        for sym in ['★', '●', '○', '◐', '◑'] {
            assert!(!emoji.contains(sym), "unicode symbol {sym} leaked into emoji mode");
        }
        assert!(emoji.contains('🌞') && emoji.contains('🌍'), "emoji star + world present");
        assert_eq!(emoji, orrery_ansi(&system, &cal, t, GlyphSet::Emoji), "deterministic");
    }
```

- [ ] **Step 4: Run + gate + prove Unicode byte-stability.** `cargo test -p hornvale-astronomy orrery && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings`. Then prove the refactor didn't move the Unicode bytes — regenerate the committed `.cast` to a temp file and diff:

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv-u.json
cargo run -q -p hornvale -- orrery --world /tmp/hv-u.json --day 0..365 --step 5 --cast /tmp/hv-u.cast
diff -q book/src/gallery/orrery-seed-42.cast /tmp/hv-u.cast && echo "UNICODE BYTE-STABLE (no drift)"
```

(The CLI still calls `orrery_ansi` with two args here — Task 2 adds the `glyphs` argument at the call site. Until then, add `GlyphSet::Unicode` at the existing call site so the crate compiles; Task 2 replaces it with the parsed flag.) Expected: identical. `cargo fmt`.

- [ ] **Step 5: Commit.**

```bash
git add domains/astronomy/src/render.rs cli/src/main.rs
git commit -m "refactor(astronomy): orrery over a semantic cell grid + GlyphSet (unicode byte-stable, emoji added)

Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9"
```

---

### Task 2: The CLI `--glyphs` flag

**Files:** Modify `cli/src/main.rs`, `cli/tests/orrery.rs`.

**Interfaces:** Consumes `GlyphSet`, `orrery_cols`, `orrery_ansi(..., glyphs)`.

- [ ] **Step 1: Parse the flag.** In `cmd_orrery`, after loading the world, parse `--glyphs` (default `unicode`):

```rust
    use hornvale_astronomy::render::{GlyphSet, ORRERY_HEIGHT, orrery_ansi, orrery_cols};
    let glyphs = match flag_value(args, "--glyphs").unwrap_or("unicode") {
        "unicode" => GlyphSet::Unicode,
        "emoji" => GlyphSet::Emoji,
        other => return Err(format!("unknown --glyphs '{other}'; expected 'unicode' or 'emoji'")),
    };
```

(Drop the now-unneeded `ORRERY_WIDTH` import if `orrery_cols` replaces it; keep whatever the call sites use.)

- [ ] **Step 2: Thread it through both paths.** Single frame: `print!("{}", orrery_ansi(system, calendar, t, glyphs));`. Animated: each frame `orrery_ansi(system, calendar, mk(d)?, glyphs).replace('\n', "\r\n")` (keep the `\u{1b}[?25l\u{1b}[2J\u{1b}[H` prefix), and size the header by `orrery_cols`:

```rust
        let cast = hornvale_kernel::asciinema_v2(
            orrery_cols(glyphs) as u16,
            ORRERY_HEIGHT as u16,
            dt,
            &frames,
        );
```

- [ ] **Step 3: Help text.** Update the orrery help lines to mention `[--glyphs unicode|emoji]`.

- [ ] **Step 4: Test the emoji `.cast`.** In `cli/tests/orrery.rs`, add a check that `--glyphs emoji` yields a valid, deterministic `.cast` whose header width is `2 × 61 = 122`:

```rust
    // Emoji mode: header width doubles (two columns per cell), still deterministic.
    let ecast = dir.join("e.cast");
    let erun = || {
        assert!(bin().args(["orrery","--world",world.to_str().unwrap(),"--day","0..10","--step","2","--glyphs","emoji","--cast",ecast.to_str().unwrap()]).status().unwrap().success());
        std::fs::read(&ecast).unwrap()
    };
    let efirst = erun();
    assert_eq!(efirst, erun(), "emoji .cast is deterministic");
    let etext = String::from_utf8(efirst).unwrap();
    let header: serde_json::Value = serde_json::from_str(etext.lines().next().unwrap()).unwrap();
    assert_eq!(header["width"], 122, "emoji header is two columns per cell");
```

(`serde_json` is already a dev-usable dep of the CLI via the workspace; if the test crate lacks it, assert on the raw header string containing `"width": 122` instead.)

- [ ] **Step 5: Run the full gate + commit.**

```bash
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add cli/src/main.rs cli/tests/orrery.rs
git commit -m "feat(cli): orrery --glyphs unicode|emoji (emoji sizes the .cast to two columns per cell)

Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9"
```

---

### Task 3: The book — emoji `.cast`, stacked gallery, CI, chapter note

**Files:** Create `book/src/gallery/orrery-emoji-seed-42.cast`; modify `book/src/gallery/orrery-seed-42.md`, `book/src/domains/astronomy.md`, `.github/workflows/ci.yml`.

- [ ] **Step 1: Generate the emoji `.cast`.**

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv-orrery.json
cargo run -q -p hornvale -- orrery --world /tmp/hv-orrery.json --day 0..365 --step 5 --glyphs emoji --cast book/src/gallery/orrery-emoji-seed-42.cast
```

- [ ] **Step 2: Stack both players.** In `book/src/gallery/orrery-seed-42.md`, add the emoji player below the Unicode one, each with a caption, both deferred to `DOMContentLoaded` (the same pattern already there — the additional-js loads late), the emoji player sized `cols: 122, rows: 31`:

```markdown
### Single-width Unicode

<div id="orrery"></div>

### Emoji (experimental — needs an emoji-capable, two-column font)

<div id="orrery-emoji"></div>
<script>
  document.addEventListener('DOMContentLoaded', function () {
    AsciinemaPlayer.create('./orrery-seed-42.cast', document.getElementById('orrery'), { loop: true, autoPlay: true, cols: 61, rows: 31 });
    AsciinemaPlayer.create('./orrery-emoji-seed-42.cast', document.getElementById('orrery-emoji'), { loop: true, autoPlay: true, cols: 122, rows: 31 });
  });
</script>
```

(Replace the page's existing single `<div>`/`<script>` with this; keep the title, prose, and the `[Download the .cast]` fallback.)

- [ ] **Step 3: Chapter note.** In `book/src/domains/astronomy.md`, extend the orrery sentence to note the emoji mode — e.g. "`hornvale orrery` draws the system … (and, with `--glyphs emoji`, on a two-column grid whose moons are the phase emoji `🌑🌓🌕🌗`, an experiment that meets emoji's fixed double-width head-on)."

- [ ] **Step 4: CI.** In `.github/workflows/ci.yml`'s "Artifacts are current" step, after the existing orrery line, add:

```bash
cargo run -p hornvale -- orrery --world /tmp/hv-ci-sky.json --day 0..365 --step 5 --glyphs emoji --cast book/src/gallery/orrery-emoji-seed-42.cast
```

Confirm the world path matches the step's (`/tmp/hv-ci-sky.json`).

- [ ] **Step 5: Verify + gate.**

```bash
mdbook build book
# Idempotence: regenerating the emoji cast changes nothing.
cargo run -q -p hornvale -- orrery --world /tmp/hv-orrery.json --day 0..365 --step 5 --glyphs emoji --cast book/src/gallery/orrery-emoji-seed-42.cast
git diff --exit-code book/src/gallery/orrery-emoji-seed-42.cast && echo "emoji cast idempotent"
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ && echo "artifact drift clean"
```

Then **watch the rendered player** (the runtime check this campaign is about): open the built `book/book/gallery/orrery-seed-42.html` and confirm the emoji orrery aligns (the star, world, and phase-emoji moons sit on the grid, not smeared). Note the result in the commit or report; if it misaligns in a standard emoji font, that is the client-font fragility (§4 of the spec), not a workspace bug.

- [ ] **Step 6: Commit.**

```bash
git add book/src/gallery/orrery-emoji-seed-42.cast book/src/gallery/orrery-seed-42.md book/src/domains/astronomy.md .github/workflows/ci.yml
git commit -m "docs(book): stack the emoji orrery beside the Unicode one (committed .cast, CI drift-checked)

Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9"
```

---

## Self-Review Notes

**Spec coverage (against `2026-07-09-orrery-emoji-design.md`):** §2 semantic cell grid + glyph sets + `--glyphs` + Unicode byte-stability → Tasks 1–2. §3 two `.cast`s, stacked gallery, CI, chapter note, increment-not-campaign → Task 3. §4 determinism + the construction-not-display-width test + watch-the-player → Task 1 Step 3 and Task 3 Step 5. §5 testing → each task. §6 non-goals honored (orrery only; two named sets; emoji not default).

**Read-the-engine notes:** the refactor preserves every geometric line of the current `orrery_ansi` (scale, `plot`, ring loop, star/world/moon placement, colors) verbatim — only the cell type (`char` → `Cell`) and the emit (glyph-set lookup) change, which is what makes Unicode byte-stability achievable and testable (Task 1 Step 4's `.cast` diff). `emit_ansi_grid` stays for `chart_ansi`. The one intentional non-identity is the degenerate-moon fallback (`'o'` → `Cell::Moon(New)`), unreachable at genesis and absent from every committed artifact — called out inline.

**Determinism watch:** no draws, no wall-clock; both `.cast`s are pure functions of (seed, span, glyph set). The emoji header width is `orrery_cols(Emoji) = 122`, asserted in Task 2.

**Fragility watch:** the test asserts construction (emoji-only glyphs, doubled header width), never terminal display width (no Unicode-width crate — decision 0004). The real render is verified by watching the player (Task 3 Step 5), the discipline The Orrery's retro established.
