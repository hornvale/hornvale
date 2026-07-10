# The Live Orrery — Design

**Date:** 2026-07-09
**Status:** Approved (brainstorming session)
**Kind:** Campaign spec — a new scene kind plus a new in-repo client. Advances
the RENDER cluster (a sibling to the Atlas, RENDER-2) along the rendering
strategy's rings; follows decisions 0022 (sim emits data, clients render) and
0023 (in-repo clients carry their own toolchains) without amending either.
**Parent specs:** `2026-07-09-rendering-strategy-design.md`,
`2026-07-09-scene-protocol-design.md` (the cartographic pole and the scene-window
pattern), `2026-07-09-atlas-viewer-design.md` (the client pattern this mirrors),
and `2026-07-09-the-orrery-design.md` (the terminal orrery this supersedes in the
book).
**Provenance:** The terminal orrery (Campaign 23) and its recent moon-phase fixes
established that the sim can render its system faithfully, but the asciinema
`.cast` is a frame-baked, discrete animation with only four moon glyphs. The owner
asked for a **pure-JS, smoothly animated** orrery that **computes everything** —
a glowing star, computed-terminator moons, and the world drawn as its *real,
spinning globe* from the committed terrain — and that is interactive
(play/pause, scrub, speed). The scene protocol already anticipated this: "time
arrives with the situated pole … a future sibling kind." This is that sibling —
the **system (top-down, exocentric) pole**, distinct from and not foreclosing the
observer's-sky situated pole the Atlas still awaits.

---

## 1. Goal

An interactive, smoothly-animated top-down orrery of a world's system, rendered
in the browser from committed, sim-emitted data. The simulation emits a new
semantic scene kind (`scene/system/v1`); a new TypeScript client
(`clients/orrery/`) reads it — plus the existing `scene/tiles/v1` terrain — and
animates the system on a 2D canvas: the star as a glowing disc, each moon as a
computed lit disc whose terminator faces the star, and the world as an
orthographic globe of its *actual* generated continents, spinning at its day
rate, tilted at its obliquity, with a day/night terminator. It replaces the two
asciinema orrery players as the gallery's centerpiece.

The division of labor is decision 0022's, exactly: **the sim emits semantic
physical elements; the client owns all presentation** (positions over time,
colors, glyphs, projection, animation timing).

## 2. The scene kind: `scene/system/v1`

A new kind emitted by the scene window (`windows/scene`), a sibling to
`scene/tiles/v1`. Semantic-only: physical orbital *elements*, never positions
over time, never colors or glyphs, never anything revealing which provider
produced a value. The client evaluates body positions and phases from these
elements — the same posture as a field (emit the function's parameters; the
consumer samples).

**Why elements, not pre-sampled positions.** Elements are the semantic truth
(genesis produces orbital parameters, not a trajectory), they are tiny and
fixed-size, and they let the client scrub *continuously* to any instant rather
than interpolating between samples. The one risk — the client re-implementing
the position/phase math — is closed by a golden-sample test (§5): Rust emits a
handful of `(t → positions, phases)` samples the TypeScript evaluator must
reproduce, locking the two implementations together.

**Document shape** (field order fixed, like `TilesScene`):

```json
{
  "schema": "scene/system/v1",
  "seed": 42,
  "star": {
    "class_name": "yellow dwarf (G)",
    "luminosity_rel": 1.0,
    "hz_inner_au": 0.90,
    "hz_outer_au": 1.40
  },
  "world": {
    "orbit_au": 1.0,
    "year_days": 372.4,
    "day_length_days": 1.0,      // omitted when tidally locked (no spin)
    "obliquity_deg": 23.1,        // mean obliquity; time-varying forcing is out of scope for v1
    "year_phase_offset": 0.137
  },
  "moons": [
    { "sidereal_days": 15.99, "phase_offset": 0.42, "distance_mm": 384.4, "size_rel": 1.0 }
  ]
}
```

- All values come straight from the generated `StarSystem` / `Calendar` /
  `OrbitalForcing` the terminal orrery already reads (`class_name`,
  `habitable_zone`, `anchor.orbit/year/obliquity`, `Rotation`,
  `forcing.year_phase_offset`, per-moon `period`, `moon_phase_offsets`,
  `distance`, `angular_diameter_rel`). No new physics.
- `day_length_days` is absent for a tidally-locked world (the globe does not
  spin; it keeps one face to the star).
- `obliquity_deg` is the **mean** obliquity (`forcing.obliquity_mean`-derived
  value the calendar already exposes); v1 does not animate the Milankovitch
  obliquity/eccentricity drift (§7).
- Moons carry their **real** `distance_mm` and `sidereal_days`; the client keeps
  their relative ordering/ratios but maps radius schematically for visibility
  (§4). `size_rel` is the angular-diameter ratio the terminal orrery's size-word
  uses.

**CLI.** `hornvale scene system [--world <PATH>]` — JSON to stdout, mirroring
`hornvale scene tiles`. Unknown kinds already error loudly naming the known
kinds; the list gains `system`. Usage text gains the command.

**Committed example.** `book/src/gallery/scene-system-seed-42.json`, generated by
the CI sky world (`/tmp/hv-ci-sky.json`), riding the existing
`git diff --exit-code book/src/gallery/` drift check via a new line in CI's
"Artifacts are current" list. Data, not a chapter — no SUMMARY entry.

## 3. The client: `clients/orrery/`

A second citizen under `clients/`, built exactly like `clients/atlas/` (decision
0023): Deno, native TypeScript, `deno task` for check/test/build, bundled to a
single committed `book/src/gallery/orrery.js`, verified by its own CI job beside
the atlas's — never inside the Rust gate.

**Modules** (`clients/orrery/src/`), each unit-tested where it carries logic:

- `scene.ts` — parse and validate both inputs: the `scene/system/v1` document
  and the `scene/tiles/v1` terrain (reuse the atlas's `TilesScene` parsing shape;
  the elevation grid and `sea_level_m` are all the globe needs).
- `ephemeris.ts` — the pure evaluator: given the system elements and a time `t`
  (standard days), return the world's orbital angle, each moon's orbital angle
  and illumination phase, and the world's rotation angle. This is the one place
  that re-implements sim math (`angle = τ·(t/period + offset)`; synodic phase
  `τ·(t/synodic + offset)` with `synodic = P·Y/(Y−P)`), and the golden-sample
  test pins it to Rust.
- `globe.ts` — orthographic projection of the equirectangular `elevation_m` grid
  onto a disc at a given rotation (longitude offset) and axial tilt, shading each
  visible pixel land/sea by `sea_level_m` and by elevation via the palette, and
  darkening the night side past the day/night terminator (a great circle whose
  pole is the sub-solar point).
- `moon.ts` — the computed phase disc: a circle with a terminator ellipse sized
  by illuminated fraction and rotated so the lit limb faces the on-screen star
  direction (the correct, continuous form of the terminal orrery's four-glyph
  bucketing).
- `palette.ts` — elevation→color and star-class→tint. Borrow the atlas's
  elevation palette (copy into this client; a shared module is more toolchain
  than one function warrants — decision 0023 clients "carry their own stacks").
- `main.ts` — the canvas render loop and controls: play/pause, a **year
  scrubber** (drag through one year), and a **speed** control.
  `requestAnimationFrame` advances a playback clock that maps to sim-time `t`;
  the scrubber sets `t` directly.

**Embedding.** The gallery orrery page replaces its two `AsciinemaPlayer.create`
calls with a canvas, the controls, and `<script type="module"
src="./orrery.js">`, reading `./scene-system-seed-42.json` and
`./scene-tiles-seed-42.json` — the same "every pixel is read in your browser from
the committed document the sim emitted" framing the atlas page uses.

## 4. Rendering — compute everything

- **Star.** A radial-gradient glow at center, tinted by spectral class (the
  client's palette maps `class_name` → hue). The habitable zone is two faint
  rings at `hz_inner_au`/`hz_outer_au` (scaled to the world's orbit), and the
  world's orbit a third — the terminal orrery's faint rings, smooth.
- **Moons.** Each moon is a computed lit disc: draw the dark disc, light the
  sunward semicircle, then carve (crescent) or extend (gibbous) with a terminator
  ellipse whose x-radius is `R·|1−2k|` for illuminated fraction `k`, the whole
  rotated so the lit limb faces the **on-screen** direction to the star. Fraction
  `k` follows from the phase; orientation follows from geometry — continuous, at
  any angle. Moons orbit the world at their real relative distances, mapped to a
  schematic display radius (real astronomical scale would hide them inside the
  world's disc), and at their real sidereal periods and synodic phases.
- **World.** An orthographic globe of the real terrain: for each pixel of the
  world's disc, invert the projection to a (lat, lon) on the sphere — offset by
  the current rotation angle (day spin) and tilted by obliquity — sample
  `elevation_m`, color land/sea and elevation via the palette, then darken it if
  it lies on the night side of the terminator (dot product of the surface normal
  with the sub-solar direction < 0). The continents are seed 42's own; the
  terminator sweeps them as the world spins and orbits. A tidally-locked world
  (`day_length_days` absent) does not spin — one face holds toward the star.

The result is deterministic in `(scene documents, t)`; only playback *speed* is
wall-clock, which is presentation a client owns.

## 5. Determinism, testing, and CI

- **No wall-clock in the sim.** `scene/system/v1` is a pure function of the world;
  same seed → byte-identical document (drift-checked). The client's
  `requestAnimationFrame` drives only playback speed, never sim state.
- **Rust side:** `windows/scene` gains `SystemScene` + `system_scene(&World)` +
  `SYSTEM_SCHEMA`, unit-tested for shape and determinism; the CLI `scene system`
  path tested like `scene tiles`; the committed example drift-checked. A small
  **golden-samples** artifact (or inline test constants) records
  `(t → world angle, per-moon angle & phase, world rotation)` for the seed-42
  system, emitted by Rust — the contract the TS evaluator must match.
- **TypeScript side (own CI job):** `deno fmt --check`, `deno lint`,
  `deno task check`, `deno task test`, `deno task build` + `git diff --exit-code
  book/src/gallery/orrery.js`. Unit tests: `ephemeris` reproduces the Rust golden
  samples within a tight epsilon; `globe` projection round-trips and respects the
  terminator; `moon` terminator geometry (new toward star, full opposite, half
  lit toward the star); `scene` parsing/validation.
- **Display cannot be pixel-tested** (canvas in a browser) — verified the way the
  atlas and the terminal orrery's bugs taught: build the book and watch it. Tests
  assert the *logic* (ephemeris, projection, phase geometry), not the pixels.

## 6. The book, and the terminal orrery

- **Gallery.** `book/src/gallery/orrery-seed-42.md` swaps the two cast players for
  the live client (canvas + controls + `orrery.js`), reading the two committed
  JSONs. Caption reframed: the system, computed live in your browser from the
  data the sim emitted.
- **The casts go.** `orrery-seed-42.cast` and `orrery-emoji-seed-42.cast` are
  deleted, and their two CI generation lines removed — nothing embeds them.
- **The CLI stays.** `hornvale orrery` (terminal ANSI/emoji render) and its
  `render.rs` unit tests remain — a legitimate text-first artifact, no longer the
  book's centerpiece. Its recent correctness (moon placement, lit-limb
  orientation) is untouched.
- **Astronomy chapter.** The orrery paragraph gains a sentence on the live client
  (the computed globe of the real terrain), and points the gallery link at it.
- **Chronicle + retro.** A new chronicle entry and a one-page retrospective, per
  the Definition of Done; a freshness sweep of the rendering/orrery chapters.
- **Registry.** A new RENDER-cluster row for `scene/system/v1` + the orrery client
  (the situated/system pole sibling); no new decision (follows 0022/0023).

## 7. Non-goals (v1)

- **The observer/situated-pole sky** — an observer at a place looking up. That is
  the Atlas's separate await; `scene/system/v1` is the exocentric sibling and must
  not foreclose a future `scene/sky/v1` (it doesn't — it is a distinct kind).
- **Time-varying Milankovitch drift** in the orrery — obliquity/eccentricity are
  shown at their mean; deep-time neighbor-star drift is not animated. Year-scrub
  only.
- **Physical radial scale for moons** — the radial axis is schematic (proportional
  ordering, not true astronomical distance); periods and phases are real.
- **WebGL / 3D** — a 2D canvas suffices for an orthographic globe and flat orrery.
- **Retiring the terminal orrery** — the CLI render stays; only its committed
  casts and book embed are replaced.

## 8. Consequences for existing documents

- `windows/scene/src/lib.rs` — gains `SYSTEM_SCHEMA`, `SystemScene` (+ nested
  structs), `system_scene(&World)`, `scene_json` reuse; tests.
- `cli/src/main.rs` — `cmd_scene` gains a `"system"` arm; usage/help text.
- `clients/orrery/` — new client (Deno config, `src/`, tests, `deno task build`).
- `.github/workflows/ci.yml` — add `scene system` to "Artifacts are current";
  remove the two `orrery … --cast` lines; add an `orrery` client CI job mirroring
  `atlas`.
- `book/src/gallery/orrery-seed-42.md` — live client replaces the cast players.
- `book/src/gallery/{orrery-seed-42,orrery-emoji-seed-42}.cast` — deleted.
- `book/src/domains/astronomy.md` — orrery paragraph names the live client.
- `book/src/SUMMARY.md` — new chronicle entry.
- `docs/vision/idea-registry.md` — new RENDER row.
- `docs/decisions/` — none (follows 0022/0023).
