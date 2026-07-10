# The Atlas Viewer — Design

**Date:** 2026-07-09
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-09-rendering-strategy-design.md` (Ring 3; roadmap item 4; decision 0022 governs the seam). Registry: RENDER-2. Consumes `scene/tiles/v1` (`2026-07-09-scene-protocol-design.md`).
**Provenance:** The scene protocol shipped with its committed example scene explicitly laid out as "the future in-book web viewer's test data." This spec builds that viewer — the first client beyond decision 0022's seam — and, with it, settles how in-repo clients are tooled: the owner chose TypeScript with real automated tests over the registry row's vanilla-JS sketch, which this spec supersedes (ratified as decision 0023).

---

## 1. Goal

An interactive atlas in the published book: a new gallery page rendering the committed `scene-tiles-seed-42.json` on a canvas — five switchable layers, hover inspection, settlement markers, pan and zoom — implemented in TypeScript under `clients/atlas/`, unit-tested, bundled to one committed `atlas.js`, and verified by a dedicated CI job that never touches the Rust workspace's dependency surface.

## 2. Decision 0023 — in-repo clients carry their own toolchains

To be ratified alongside this spec:

> Ring-3 clients that live in this repository sit under `clients/` (a new top-level layer outside the Cargo workspace), carry their own dependency stacks and toolchains (per decision 0022 — clients never link the crates), commit their **built artifacts** into the book, and are verified by **dedicated CI jobs**. The Rust workspace's dependency rules (0004) and its CI job are untouched. This supersedes the "no npm, no build step" sketch in the registry's viewer row: the binding posture was always *the workspace takes no client dependencies*, not *clients take none*.

The committed-artifact pattern is decision 0009's ("models author, dice roll") applied to clients: build offline, commit the output, drift-check its freshness.

## 3. The client — `clients/atlas/`

**Toolchain: Deno** (one pinned binary — native TypeScript, built-in test runner/formatter/linter, esbuild-backed `deno bundle`). No `node_modules`, no `package.json`; `deno.json` declares tasks:

- `check` — `deno check` over the sources
- `test` — `deno test` (unit tests beside their modules)
- `fmt` / `lint` — the built-in tools, checked in CI
- `build` — `deno bundle` producing the single committed `book/src/gallery/atlas.js`

**Modules, split at the testable seams** (everything except `main.ts` is pure and unit-tested):

- `scene.ts` — parse + validate a `scene/tiles/v1` document: schema string, width/height/layer-length consistency, biome indices within legend bounds, feature shape. Fails loudly with a message naming what's wrong — the client half of the schema contract, and the test bed for it.
- `projection.ts` — lattice↔canvas transforms; the pan/zoom viewport (scale clamped to 1×–16×; translation clamped so the lattice never fully leaves view); tile-under-cursor inverse transform.
- `palette.ts` — per-layer color functions: **biome and elevation reuse the raster renders' exact colors** (the 22 biome RGBs and the ocean-blue→green→tan→white elevation ramp, copied into TS as the client's own palette — decision 0022: the schema carries no colors; this client *chooses* continuity with the committed PNGs). Plate (categorical) and unrest (sequential) get new palettes, chosen at plan time under the dataviz guidance; ocean is a two-value layer.
- `hittest.ts` — cursor→tile readout assembly (lat/lon from tile coords, elevation, biome name via legend, plate, unrest, ocean) and cursor→feature proximity for marker hover.
- `main.ts` — thin canvas/DOM glue only: fetch the sibling JSON, wire controls, `requestAnimationFrame` redraw on interaction. No logic worth unit-testing lives here; the browser screenshot verifies it.

**Determinism note:** the committed bundle is a build artifact drift-checked in CI, so the Deno version is pinned (in `deno.json` and the CI action) — same sources + same version → same bundle bytes.

## 4. The page — `book/src/gallery/atlas.md`

"The Atlas of Seed 42": brief prose at book altitude (what the atlas is; that everything on screen is read from the same committed scene document a reader can open raw), then raw-HTML canvas + controls, with the bundle loaded as `<script type="module" src="./atlas.js">`:

- **Layer switcher:** biome / elevation / plate / unrest / ocean (radio buttons; biome default).
- **Hover readout:** a fixed line under the canvas — latitude, longitude, elevation (m), biome name, plate id, unrest, ocean — updating as the cursor moves; feature names shown when hovering a marker.
- **Markers:** the features array as points over every layer; the flagship visually distinct; names on hover.
- **Pan + zoom:** drag and wheel, clamped per `projection.ts`.
- **Theme awareness:** page chrome and controls inherit mdbook's light/dark themes (CSS variables); the canvas is self-colored by the palettes.
- **Graceful failure:** if the fetch fails (e.g. `file://` viewing), the canvas region shows a plain message linking the committed JSON and the static PNGs — never a blank box.

SUMMARY gains the gallery entry. The scene-schema reference page gains one cross-link to the atlas as a live consumer.

## 5. CI — a dedicated job

A new job in `.github/workflows/ci.yml` (alongside, not inside, the Rust job): pinned `setup-deno`, then `deno fmt --check`, `deno lint`, `deno task check`, `deno task test`, `deno task build`, and `git diff --exit-code book/src/gallery/atlas.js` — the client's own gate plus the bundle freshness check. The Rust job's steps and dependency surface are unchanged; the committed `atlas.js` and `atlas.md` are static book content to it.

## 6. Testing

- **Unit (deno test):** scene validation accepts the committed gallery scene and the crate's golden fixture verbatim; rejects wrong schema string, mismatched layer lengths, out-of-range biome indices. Projection round-trips (canvas→tile→canvas), zoom clamping at both bounds, pan clamping at edges. Palette totality (every legend index and every layer value maps to a color). Hit-test correctness at corners and under transform.
- **Integration:** `mdbook build` clean; the built page's asset references resolve (covered by docs_consistency's link checking where applicable; the plan confirms and extends only if the check ignores non-md assets).
- **Visual:** during implementation, serve the built book and screenshot the atlas — layer switcher, hover, markers, zoom each eyeballed once (I render the screenshots; the merged artifact is also just *there* in the published book for continuous human verification).
- **CI:** the new job green; the Rust job untouched and green.

## 7. Consequences for existing documents

- `docs/decisions/0023-in-repo-clients-carry-their-own-toolchains.md` — new (§2), plus README index row.
- Registry: RENDER-2 flips `raw → shipped` on merge, Where → this spec, row text updated to match reality (TypeScript client under `clients/`, committed bundle — superseding the vanilla-JS sketch per this spec; the old wording survives in git history and the strategy spec).
- Chronicle + retrospective for the campaign (number verified against main at merge time — the parallel-campaign lesson).
- The rendering strategy's roadmap item 4: satisfied on merge. All four roadmap items done; the strategy's near-term program completes.

## 8. Non-goals

- **No sky scrubbing, no time axis** — the registry row's "scrub the sky through a year" sketch awaits a situated-pole scene kind; the atlas is the cartographic client only.
- **No server, no dynamic data** — the atlas reads one committed document; world selection, live generation, and multi-seed browsing are future work.
- **No mobile-touch gestures** in v1 (mouse/trackpad interactions only; the page still renders statically on touch devices).
- **No framework adoption** — Deno's standard library and the DOM; no React/Svelte/etc.
- **No second client** — the TUI viewer (RENDER-3) and tilemap view (RENDER-4) stay vision.
