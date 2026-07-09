# The Orrery — Design

**Date:** 2026-07-09
**Status:** Approved (brainstorming session)
**Kind:** Campaign spec.
**Parent spec:** `2026-07-09-rendering-strategy-design.md` (the observation stack;
decisions 0022, 0018, 0004 govern). This campaign executes and extends Rings 0–1.
**Provenance:** A request for "serious eyecandy" — ANSI/ASCII star maps, a
roguelike solar-system display with star colors — and a devlog/outreach thread
(screenshots and video on BlueSky, narrating the "invent the universe" pipeline).
Placed against the rendering strategy, that vision splits at the `cli → clients`
seam: the *interactive* roguelike is an external client (Ring 3, a later
campaign), but *static, deterministic* ANSI renders and animations live
in-workspace (Rings 0–1). This campaign is the in-workspace half, and the
in-process reference client the situated **scene** pole (a sequel campaign) will
later formalize.

---

## 1. Goal

Two things the workspace does not yet have, both deterministic and dependency-free:

1. **A solar system you can see** — an ANSI-color **orrery** (top-down orbital
   schematic: the star, its habitable zone, the world on its orbit, the moons at
   their phases) and a **color star map** (the neighbor sky tinted by spectral
   class), rendered directly from the world like the star chart.
2. **Motion you can share** — a deterministic **animation** format so a render
   evaluated over a time range becomes a single self-contained clip: the moons
   cycling, the seasons turning, and — over deep time — the sky drifting (the
   payoff of Firm Ground II's moving sky).

The animation format is chosen to honor decision 0022 without kludge: the
workspace emits an **asciinema-v2 `.cast`** (JSON-lines, already inside the
`serde_json` allowlist), which embeds directly in the Book and converts to mp4
by an external open-source pipeline (`agg`/ffmpeg). No GIF/APNG, no in-workspace
video dependency, no loose-PNG-plus-ffmpeg pipeline.

## 2. Design principles

1. **Sim emits data, clients render — and the in-process render is a client
   too.** These renders read the world directly (the `render.rs` pattern), make
   their own palette choices (§4), and emit deterministic bytes. They take no
   graphics dependency; decision 0004's two-name allowlist is never bent
   (`cli/tests/architecture.rs` enforces it workspace-wide, so an isolating
   "video window crate" is not a loophole — it was considered and rejected on
   exactly this ground).
2. **Animation is a deterministic artifact, not a recording.** A committed
   `.cast` uses **synthetic** frame timing (frame *k* at `t = k·dt`), never
   wall-clock (constitutionally banned, and drift-checking needs determinism).
   Live captures of a human driving a future interactive client use real
   `asciinema rec` out-of-workspace; those are not committed artifacts.
3. **Paint belongs to the renderer, semantics to the domain.** The domain keeps
   its prose `color` strings (for the almanac); the renderer owns the
   spectral-class → ANSI-color mapping. Nothing about a body's *meaning* moves;
   only how this particular client draws it.
4. **This is the reference client, not the seam.** The renders consume the world,
   not scene JSON. The situated-scene pole (`hornvale scene sky`) that an external
   roguelike will consume is a **sequel campaign** (§8); this campaign proves the
   picture is worth serializing before formalizing the serialization.

## 3. Components

Four units, each with one responsibility and its own tests.

**3.1 `kernel/src/cast.rs` — the animation encoder.** Sibling to `png.rs`; the
kernel is the encoder home (rendering strategy §Ring 1). Given screen dimensions,
an ordered list of ANSI frame strings, and a fixed interval `dt`, it produces an
asciinema-v2 document: a header line `{"version":2,"width":W,"height":H}` (no
`timestamp` field — its omission is what keeps the artifact deterministic) then
one event line `[k·dt, "o", frame]` per frame. Each frame string is a full-screen
redraw (clear + home + body) so playback is a flipbook. Pure `serde_json`; no
wall-clock; byte-deterministic.

**3.2 `domains/astronomy/src/render.rs` — the two renders.**
- `spectral_color(class) -> AnsiColor` (and an anchor-star variant deriving class
  from `class_name`/luminosity): the render's palette, a total mapping from
  spectral class to an ANSI color (O/B blue-white → G yellow → M red). A render
  decision, not domain data.
- A **color star map**: `chart_ascii`'s sibling, each neighbor tinted by
  `spectral_color(neighbor.class)`, glyph weight by `apparent_brightness`, at the
  chart's existing celestial-position layout.
- The **orrery**: `orrery_ansi(&system, t) -> String` — a top-down schematic with
  the star at center (class-colored), the habitable-zone band, the world at
  orbital angle `2π·year_phase(t)` on its orbit (`anchor.orbit`), and each moon at
  its orbital angle around the world showing its synodic phase glyph. A pure
  function of the system and `t`, so it animates by evaluation over a range.

**3.3 CLI.** `hornvale orrery [--world PATH] [--day D]` renders one frame to
stdout; `hornvale orrery [--day A..B --step k] --cast OUT.cast` renders the frame
sequence through `cast.rs`. The same `--day A..B --step k --cast` mode is
available to the color star map. Wiring follows `cmd_star_chart`.

**3.4 The Book.** Commit `asciinema-player` (a self-contained JS/CSS bundle — a
static asset, **not** a Cargo dependency) under `book/`, wired via mdbook's
`additional-js`/`additional-css`. Commit one drift-checked `.cast`: a one-year
orrery time-lapse of seed 42. It is the first "invent the universe" devlog panel,
and it lives in the Book, playable in the browser.

## 4. Palette (a render decision, recorded for consistency, not binding elsewhere)

Spectral class → ANSI color, terminal-portable (256-color or truecolor):
O/B blue-white, A white, F pale yellow, G yellow, K orange, M red; giants
brighter/saturated, dwarfs dimmer. The mapping is a table in `render.rs`, total
over `NeighborClass`, with the anchor star classified from its `class_name`
letter (fallback: mass/luminosity band). Per the rendering strategy §6, palettes
are per-render decisions — this one is documented so the star map and orrery
agree, and imposes nothing on any other render or on the future scene pole (which
emits *class*, never color).

## 5. Determinism, contracts, and the gate

- **Deterministic renders:** same world + same `t` → identical ANSI string,
  asserted in the `chart_*_deterministic` pattern.
- **Deterministic animation:** the committed `.cast` is byte-identical per seed
  (synthetic `dt`, no wall-clock) and joins CI's "Artifacts are current" drift
  check — a silently changed animation fails the build like any gallery artifact.
  The `.cast` schema is treated as an output format, versioned by convention if it
  ever changes.
- **Totality:** `spectral_color` covers every `NeighborClass` variant (test
  enumerates them) and every anchor `class_name` the generator can emit.
- **No new dependency:** asserted by the existing `cli/tests/architecture.rs`
  allowlist test, unchanged.

## 6. Testing

- Render determinism (star map, orrery) at fixed `t`.
- `cast.rs`: a structure/round-trip test — valid header, exactly *N* events at
  timestamps `k·dt`, each frame recoverable; and a determinism test (same frames
  → same bytes).
- Frame-count correctness for a `--day A..B --step k` range (off-by-one guard).
- `spectral_color` totality over `NeighborClass` and the anchor class bands.
- The committed Book `.cast` under the artifact drift gate (a second regeneration
  is byte-identical).

## 7. Success criteria

- `hornvale orrery --world <w>` prints a legible, class-colored ANSI orbital
  schematic; the color star map likewise.
- `hornvale orrery --day 0..365 --step 1 --cast out.cast` emits a valid,
  deterministic asciinema-v2 clip that `asciinema play`/`agg` renders and that
  embeds in the Book.
- A committed Book `.cast` time-lapse plays in the browser and is drift-checked.
- The full gate passes: `cargo test --workspace && cargo fmt --check && cargo
  clippy --workspace --all-targets -- -D warnings`, plus the artifact drift check.
- No new workspace dependency.

## 8. Explicitly deferred (decisions, not omissions)

- **The situated scene pole** — `hornvale scene sky`, semantic sky+system JSON
  (positions, magnitudes, spectral class, phases) for an external client. The
  "both, sequenced" sequel; this campaign is its in-process reference client.
- **The interactive roguelike TUI** — an external repo (ratatui) driving
  `hornvale scene` as a subprocess (Ring 3, RENDER-3). Owns its own colors,
  glyphs, and — eventually — tilemaps and custom bitmap fonts (the pixel track,
  where `.cast` no longer suffices and PNG-frames → external mp4 takes over).
- **mp4 / GIF / APNG in the workspace** — mp4 is always an external transcode of
  the `.cast` or PNG frames; GIF/APNG are not built.
- **The cartographic (tilemap) pole and the pixel animation track** — the map
  side of the scene protocol (in flight elsewhere) and its PNG-sequence videos.

## 9. Consequences for existing documents

- **Rendering strategy (§Ring 1 members):** gains **deterministic animation
  (asciinema `.cast`)** as a committed-artifact member, alongside the ASCII/PNG
  renders — a small amendment noted at merge, consistent with "encoder home is
  the kernel."
- **Rendering strategy (§Ring 3, in-book JS):** its "hand-written vanilla JS"
  note is read to admit a **self-contained, build-free third-party player
  bundle** (`asciinema-player`) as a committed static asset — no npm, no build
  step, no Cargo dependency, which is the property that note protects. Recorded
  so the extension is deliberate, not accidental.
- **`docs/vision/idea-registry.md`:** the animated-`.cast` capability and the
  orrery are concrete render work (like the star chart, which took no row); no
  speculative row is added. The situated-scene sequel is already RENDER-1
  (spec'd); the external TUI is RENDER-3.
