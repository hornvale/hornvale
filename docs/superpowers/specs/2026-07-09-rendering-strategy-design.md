# Rendering Strategy — The Observation Stack

**Date:** 2026-07-09
**Status:** Approved (brainstorming session)
**Kind:** Standing design spec (like the long-term plan), not a campaign spec.
Campaigns and tasks that execute pieces of it cite it as parent.
**Provenance:** A survey of rendering as actually built (three ASCII/PPM map
renders, the First Light BMP, the almanac's markdown, espeak audio clips)
against rendering as promised (Constitution §3.4's star charts; decision 0018's
PNG migration; the frontier's four-media engine) showed no single document
answering "where does a new render live, and what rules govern it?" This spec
is that document.

---

## 1. Purpose and governing principle

Rendering exists to serve four observers:

- the **book reader** — public, in a browser, seeing committed artifacts;
- the **REPL user** — at a terminal, interrogating a live world;
- the **lab scientist** — comparing worlds through studies and metrics;
- the **eventual player** — situated, restricted, sensory (Constitution §3.5).

One principle governs every ring of the stack, to be ratified as **decision
0022 — sim emits data, clients render**:

> The simulation emits deterministic data; clients render pixels. Every
> in-workspace render is deterministic bytes — text, ASCII, PNG, PCM, scene
> JSON — reproducible from seed and query alone. The workspace never takes a
> rendering or graphics dependency (decision 0004 never bends). Interactive
> clients live **outside** the workspace and consume the CLI's output across
> a **process boundary** — they drive `hornvale` as a subprocess or read its
> committed artifacts; they never link the crates.

This is "sim first, game as lens" applied to output, and it extends the
constitutional layering one ring further:

```
kernel → domains → windows → cli → clients
                                    ↑ the seam (process boundary)
```

**Constitutional posture.** §7's deferral of graphical clients stands — this
spec plans the road without opening the gate. Rings 0–1 are binding and
near-term; Ring 2 is a binding principle with a sketched design; Ring 3 is
explicitly non-binding vision. When a graphical client does arrive, its shape
is already settled: it is a subprocess-driving consumer of scene JSON, and
nothing in the workspace changes to accommodate it.

## 2. The four rings

Rendering questions are placement questions. Each ring names its members, its
contracts, and its current state.

### Ring 0 — Text in process

**Members.** The almanac window's one-page markdown document; the REPL's
reports (`sky`, `calendar`, `why`, …); the content→render prose seam
(`hornvale_language::render_line`, the text arm of the frontier's four-media
engine, shipped in Campaign Y2-3).

**Contracts.** Deterministic strings: same world + same query → identical
output, asserted by tests (`render_is_deterministic` and kin). Markdown is the
document format; prose passes through the content→render seam so that *what is
said* and *how it is voiced* stay separable.

**State.** Healthy. This ring is documented, not reformed. New textual reports
follow the existing patterns; nothing here changes shape under this spec.

### Ring 1 — Committed artifacts

**Members.** The gallery's deterministic artifacts: three ASCII map renders
(elevation, biome, settlement — 72×24 equirectangular, one `render.rs` per
domain); their 256×128 raster siblings (P6 PPM today); the First Light 512×512
BMP; the content-addressed espeak audio clips. Future members: the star chart
(§3), PCM music (frontier, expressive-culture cluster), higher-resolution
renders, diegetic documents.

**Contracts.** Same seed + same pins → byte-identical artifacts, enforced by
CI's "Artifacts are current" drift check. Encoders are hand-rolled and
std-only (decisions 0004, 0018). Shared encoding utilities live in the
**kernel** — a domain's `render.rs` may depend on nothing else, so the kernel
is the only possible home (0018 anticipated this).

**State.** Two debts, both scheduled in §5:

1. **The 0018 debt.** The ratified PNG migration is unimplemented: no PNG
   encoder exists and the gallery still commits PPM/BMP. The encoder (stored
   deflate + table-driven CRC32, ≈120 lines of std) lands in the kernel; the
   three domain renders gain `*_png` siblings; the gallery migrates; the CI
   artifact command list updates. PPM may survive as an internal
   intermediate; what the gallery commits is PNG.
2. **The star chart.** Constitution §3.4 promises the almanac "star charts,
   calendars, maps" — calendars and maps exist; the star chart does not. It
   is built by the established pattern: `domains/astronomy/src/render.rs`
   producing an ASCII chart and a PNG sibling from the generated sky, joining
   the almanac page, the CLI (`hornvale star-chart`), and the gallery. Not a
   registry idea — promised work.

### Ring 2 — Scene descriptions

The boundary artifact: machine-readable, deterministic, render-ready JSON
emitted over the query surface. The seam of decision 0022 made concrete.

**The rule of the ring: semantic content only.** A scene says *what an
observer can see* — "this tile is tundra," "this star sits at this altitude
with this magnitude" — never *how to draw it*. No colors, no glyphs, no
projection choices; clients own palettes and paint. Scene JSON also preserves
the kernel's trace discipline: observations carry no producer identity —
clients, like the religion domain, never learn which system produced a
phenomenon.

**Two poles, one query surface:**

- **The cartographic pole** — grids over the globe. A scene is a tile lattice
  at a requested resolution, each tile carrying semantic layers (elevation
  band, biome, settlement presence, plate id, …). This is where "tilemaps"
  actually lives: the sim describes tiles, a client draws them. The existing
  ASCII and raster renders become, retroactively, the first two in-process
  clients of this data.
- **The situated pole** — an observer at a place and time. A sky scene (star
  positions, magnitudes, moon phases for a given night) and, eventually, a
  locale scene for the game: entities and features ranked by salience. This
  pole is mostly already designed — it is the phenomena protocol serialized.

**Architecture.** Scene emission is presentation, so it is a window:
`windows/scene` (crate `hornvale-scene`), building worlds through
`hornvale-worldgen` like every window, surfaced as
`hornvale scene <kind> [--world PATH] [--at …] [--day …]` emitting JSON on
stdout. External clients consume that stdout or the committed artifacts —
the process boundary is the seam.

**Contracts.** Same world + same query → byte-identical JSON. The schema is a
**save-format-class contract**: versioned by epoch suffix (`scene/tiles/v2`),
never renamed, exactly as seed labels are handled. Committed example scenes
join the gallery drift check so schema drift is caught the way artifact drift
is.

**State.** Unbuilt. First cut is roadmap item 3 (§5) — cartographic pole
first, built when the first client (§4) needs it, not before. This spec fixes
the contracts so earlier rings don't foreclose it.

### Ring 3 — External clients (vision; non-binding)

Charted so the inner rings serve them; committed to nowhere. Ordered by
plausibility:

- **The in-book web viewer.** The book publishes to hornvale.github.io and
  mdbook supports custom JS. Hand-written vanilla JS (no npm, no build step,
  no framework) committed in `book/`, reading committed scene JSON: pan a
  biome map, scrub the sky through a year. Static assets are not workspace
  dependencies, so the first client costs nothing constitutionally, and the
  book stays the public window. The viewer itself is not sim output and is
  not drift-checked; the data it reads is.
- **The TUI world viewer.** A separate repository with its own dependencies
  (ratatui et al.), driving `hornvale scene` as a subprocess. Browse maps,
  walk the almanac, watch the sky animate in ASCII. The bridge between REPL
  and game.
- **The tilemap game view.** When the game arrives (a campaign like any
  other, per Constitution §3.5), **focalized prose remains the primary
  render**; a tilemap
  view is secondary. What this requires of Ring 2, so the protocol design
  must not foreclose it: observer-relative knowledge (the fog of war is the
  player-knowledge ledger, TOOL-2 — a scene for a situated observer contains
  what they *know and perceive*, not what is true); per-species senses (the
  frontier's perception ideas — a scene is filtered through an articulation
  of eyes, not a camera); salience-ranked detail (the phenomena protocol's
  native ordering). No schedule, no commitment — requirements only.

## 3. What this spec settles

- **Placement:** any proposed render is placed on the ring axis, and its ring
  determines its rules. "Where does X live?" stops being a per-case debate.
- **The seam:** decision 0022 (§1). Clients are subprocess consumers;
  decision 0004 is permanent, not provisional.
- **Encoder home:** shared raster/audio encoding lives in the kernel.
- **Scene schema class:** scene JSON schemas are save-format contracts with
  epoch-suffix versioning.
- **Game render order:** prose primary, tilemap secondary — restating
  Constitution §3.5 so no client work inverts it.

## 4. Registry rows

A new `RENDER-` category in `docs/vision/idea-registry.md`:

| ID | Idea | Status | Where |
|----|------|--------|-------|
| RENDER-1 | Scene-description protocol — `windows/scene`, semantic-only JSON over the query surface; cartographic (tile lattice) and situated (observer at place/time) poles | spec'd | this spec |
| RENDER-2 | In-book web viewer — vanilla JS in `book/`, reads committed scene JSON; the book as interactive public window | raw | this spec §2 Ring 3 |
| RENDER-3 | TUI world viewer — external repo, own deps, drives `hornvale scene` as subprocess | raw | this spec §2 Ring 3 |
| RENDER-4 | Tilemap game view — secondary render of the game lens; prose stays primary; requires fog (TOOL-2), species senses, salience | raw | this spec §2 Ring 3 |

TOOL-6 (hand-rolled PNG) stays where it is — already ratified as 0018. The
star chart takes no row: constitutionally promised work, not speculation.

## 5. Roadmap

Ordered, undated. Items 1–3 are each a normal task or campaign with its own
plan cycle; this spec fixes only the order and the contracts.

1. **Pay the 0018 debt.** PNG encoder in the kernel; `*_png` siblings in the
   three domain `render.rs` modules; gallery migrates off PPM/BMP; CI
   artifact list updates.
2. **The star chart.** `domains/astronomy/src/render.rs`, ASCII + PNG, by the
   established render pattern; joins almanac, CLI, and gallery.
3. **Scene protocol, cartographic pole first.** `windows/scene` +
   `hornvale scene`, with a committed, drift-checked example scene.
4. **First client.** The in-book web viewer's first increment, once scene
   JSON exists to feed it. (Vision until then; the first Ring-3 item to
   become real.)

## 6. Non-goals

- **No graphics dependencies, ever, in the workspace** — not deferred,
  settled (0004 + 0022).
- **No rendering style guidance.** Palettes, glyph choices, and projections
  are per-render decisions (or client decisions, beyond the seam); this spec
  governs placement and contracts only.
- **No game design.** §2's Ring 3 records what the game *requires of the
  protocol*, nothing about the game itself.
- **No audio expansion.** Existing espeak clips and the frontier's PCM-music
  idea are placed on the ring axis (Ring 1) and otherwise untouched.

## 7. Consequences for existing documents

- `docs/decisions/0022-sim-emits-data-clients-render.md` — new, ratifying §1.
- `docs/vision/idea-registry.md` — the `RENDER-` rows of §4.
- Constitution §7 — unchanged; this spec is consistent with the deferral and
  cites it.
- Decision 0018 — unchanged; item 1 of §5 executes it.
