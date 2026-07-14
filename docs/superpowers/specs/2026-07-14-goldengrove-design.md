# Goldengrove — Design

**Date:** 2026-07-14
**Status:** Draft (autopilot brainstorm; awaiting G3 review)
**Kind:** Campaign spec.
**Parent spec:** `2026-07-09-rendering-strategy-design.md` (the observation
stack; decisions 0022, 0023, 0052 govern). This campaign opens Ring 3 —
the first external client — and builds the seam it stands on.
**Provenance:** Nathan's request to bring the Orrery "into the 21st
century with a glamorous 3D presentation" in the manner of
bitterbridge/goldengrove, and his ruling (in-session) that the client is a
new repository named **`hornvale/goldengrove`**, checked out at
`~/Projects/hornvale/goldengrove`, a sibling of the hornvale checkout.

---

## 1. Goal

Two deliverables, one on each side of a new seam:

1. **The catalog** (in `hornvale/hornvale`): a versioned wasm library —
   `clients/world-wasm/`, crate `hornvale-world-wasm` — that turns a seed
   into scene JSON entirely in the browser: genesis through
   `hornvale-worldgen`, then `scene/system/v1` and `scene/tiles/v1`
   documents through `hornvale-scene`. Built and released by hornvale CI;
   never committed (decision 0052's regime).
2. **The planetarium** (`hornvale/goldengrove`, new repo): a Vite +
   TypeScript + three.js application that consumes the catalog and
   renders the world glamorously — v1 is the zoom ladder's first two
   rungs, a 3D **system view** (the orrery, grown up) and a **globe view**
   (the planet with its real elevation, biomes, and terminator) — for any
   seed given in the URL.

And one removal, ruled by Nathan at G3: **the old orrery dies in this
campaign.** The terminal orrery (the `hornvale orrery` CLI verb, the ANSI
renderer and its unicode/emoji glyph sets, the `.cast` animations) and the
in-book live orrery (`clients/orrery/`, the committed `orrery.js` bundle,
the gallery pages and committed scene example, the CI `orrery` job and its
drift-check entries) are all removed — "it was never a great fit."
Goldengrove is the orrery now. Survivors, explicitly: the **star chart**
(a different artifact serving the sky chapters), the **atlas** (a
different instrument), and the **`scene/system/v1` kind itself** (built
for the old client, but it is exactly the catalog's food — the schema
outlives its first consumer). The kernel cast encoder goes only if the
removal orphans it (the repl may consume it; plan-level check). The
removal is render-layer only: no streams, facts, or save-format surface
is touched, so no determinism contract moves.

## 2. Design principles

1. **The repo boundary is the determinism boundary.** Everything in
   `hornvale/hornvale` stays deterministic, serde-only, drift-checked.
   Everything in `hornvale/goldengrove` is presentation: free camera,
   shader sparkle, client-side randomness that never re-enters the ledger
   (decisions 0022/0023; MAP-23's waiver framing). No CI carve-outs on
   either side, because nothing crosses dressed as the other thing.
2. **The observatory publishes a catalog; the planetarium pins a
   version.** The client never builds Hornvale from source. It downloads
   a tagged release asset (`world-wasm-vN`) and pins it by URL +
   checksum. One catalog can feed many future planetaria (a TUI viewer,
   diptych casements, RENDER-3/-4).
3. **Harvest, don't transfer.** bitterbridge/goldengrove's view code
   (cubeSphere, sky, starfield, observer, ground, HUD, clock, URL state,
   vitest harness) is copied into the new repo and rewired; its gg-*
   generation crates are not carried — Hornvale is the engine. The old
   repo's afterlife (archive vs. continue) is Nathan's call, outside this
   campaign.
4. **Mirror the vessel, don't invent.** The wasm crate is a standalone
   out-of-workspace cdylib with a hand-rolled `extern "C"` ABI — no
   wasm-bindgen, no new dependency regime — exactly the shape
   `clients/vessel/wasm` proved (decision 0052), including its size-tuned
   release profile.

## 3. The catalog: `clients/world-wasm/`

**Crate.** `hornvale-world-wasm`, standalone workspace (root `Cargo.toml`
`exclude`s it, like vessel and tools/type-audit). Path-deps only:
`hornvale-kernel`, `hornvale-worldgen`, `hornvale-scene`. Release profile
copied from vessel (`opt-level = "z"`, LTO, panic=abort, strip).

**ABI.** `hw_*` prefix (vessel owns `hv_`); JSON strings across a
caller-visible in/out buffer, vessel's exact memory shape:

- `hw_new(seed: u64) -> i32` — genesis with default pins. 0 on success;
  nonzero means the out-buffer holds a `GenesisError` JSON envelope.
- `hw_new_pinned(len: usize) -> i32` — pins as a JSON object in the
  in-buffer whose keys are the CLI's pin flag names (`sky`, `moons`,
  `rotation`, `neighbor`, `plates`, `ocean-fraction`, `supercontinent`,
  …) with the same value vocabulary. Pins fail loudly
  with the physical reason, exactly as the CLI does; no retry across
  seeds.
- `hw_scene_system() -> i32` — `scene/system/v1` JSON in the out-buffer.
- `hw_scene_tiles(width: u32) -> i32` — `scene/tiles/v1` JSON;
  `SceneError` (width odd / out of range) surfaces as the error envelope.
- `hw_in_ptr() / hw_out_ptr() / hw_out_len()` — vessel's buffer protocol.

**Gates.**

- *Golden contract:* a node smoke driver (vessel's `drive.mjs` pattern)
  generates seed 42 through the wasm and asserts both scene documents are
  **byte-identical** to the native `hornvale scene system|tiles` output —
  the two-language golden contract extended to the wasm boundary.
- *Size:* ≤ 1 MB raw, vessel's gate.
- `make wasm-world` builds it locally; a CI job (sibling of the vessel
  job) runs build + golden smoke on every push. Neither joins `make
  gate` — out-of-workspace crates keep their own gates (0052 precedent).

**Versioning and release.** Tag `world-wasm-vN` on `hornvale/hornvale` →
CI attaches `hornvale_world.wasm` + a SHA-256 checksum file to the GitHub
release. Scene schemas are already versioned (`scene/*/v1`); a schema
widening bumps the scene version per the existing scene-protocol
discipline, and the catalog version communicates it downstream. Note:
scene schemas become **cross-repo contracts** at this merge — widening
stays additive-or-versioned, never in-place (flagged at G3).

## 4. The planetarium: `hornvale/goldengrove`

**Bootstrap.** New repo in the `hornvale` org (creation happens at
execution, after G3). Local checkout `~/Projects/hornvale/goldengrove`.
Seeded by harvesting bitterbridge/goldengrove's `web/` tree — three.js
views, HUD, clock, URL state, vitest + happy-dom harness — with a README
crediting the lineage (both are Nathan's, MIT). The gg-* crates, the old
`build:wasm` script, and wasm-bindgen glue are not carried.

**Stack.** Vite + TypeScript + three.js (the harvest's stack, unchanged).
The repo owns its toolchain entirely (decision 0023); nothing here is
drift-checked by hornvale CI.

**Data layer.** `src/sim/wasm.ts` is rewritten against the `hw_*` ABI: a
web worker loads the pinned catalog, runs genesis off the main thread
(vessel's worker pattern — genesis is ~5 s), and hands parsed
`scene/system/v1` / `scene/tiles/v1` documents to the views.
`src/sim/parse.ts` validates against the scene schemas and is the one
place schema versions are checked. Dev mode (`VITE_WASM_PATH` or
equivalent) loads a locally built wasm from the sibling hornvale
checkout.

**v1 views — the zoom ladder's first two rungs** (ORRERY-semantic-zoom):

- **System view.** The 3D orrery: class-tinted star, the habitable-zone
  band, the world and each moon on their orbits at ephemeris-true
  positions, a year scrubber with speed control. Client-side
  interpolation between scene states is free-hand (non-deterministic
  glamour is licensed).
- **Globe view.** The cube-sphere planet with real relief from
  `elevation_m`, ocean mask, biome palette keyed by `biome_legend`,
  day/night terminator driven by the scrubbed world time, and `features`
  markers (settlements, the flagship).
- **The zoom itself** is the transition between rungs — one continuous
  camera move, system ↔ globe.
- **Seed in the URL** (`#seed=1337`, plus view/time state): any world,
  deep-linkable — the instrument, not the exhibit (ORRERY-any-world,
  ORRERY-deep-link).

**Deploy.** GitHub Pages from the repo's own CI: fetch the pinned
catalog release, `vite build`, publish. The hornvale book's gallery
chapter gains a link to the live instrument as part of the campaign's
close (the book never lags merged reality — the link lands only once the
deploy is real).

## 4½. Are the existing APIs sufficient?

Asked directly at G3; answered concretely. **For v1 as scoped: yes,
nothing new is required.** The evidence, per rung:

- **Genesis in the browser** is proven: the Casement's vessel wasm runs
  full worldgen (kernel + worldgen + terrain) at 474 KiB / ~5.6 s for
  seed 42 (decision 0052's measurements). The catalog adds only
  `hornvale-scene` on top.
- **The system view** is fully payable from `scene/system/v1`: star class
  and luminosity (color derivable client-side), the HZ band, the world's
  `orbit_au` / `year_days` / `year_phase_offset`, and per-moon periods,
  phase offsets, distances, and angular sizes. Orbits render as circles
  traversed uniformly — which is the *scene kind's* lie, not the sim's
  (`forcing.rs` holds live eccentricity that the schema omits;
  ORRERY-ellipse-truth). v1 inherits it knowingly.
- **The terminator** — the subtle one — is already payable and *correct*:
  `obliquity_deg` + `year_phase_offset` + `day_length_days` locate the
  subsolar point for any scrubbed world time. Locked worlds
  (`day_length_days: null`) get a frozen terminator, truthfully.
- **The globe** is payable from `scene/tiles/v1` at up to 1024×512
  (~40 km/tile at the equator): elevation, ocean mask, biome + legend,
  plates, unrest, features. Ample for an orbital view; it is not
  ground-level fidelity and does not pretend to be.

Where the APIs run out — the honest list, all pre-registered or noted,
none blocking v1:

1. **Eccentricity** (ORRERY-ellipse-truth): the first widening worth
   making; additive field, but it extends the two-language golden
   contract, so it is a deliberate sequel, not a v1 rider.
2. **Neighbor stars** absent from the scene (ORRERY-starfield-backdrop).
3. **Planet radius** is not exposed anywhere, so true-scale relief is
   unpayable; v1 uses declared relief exaggeration (which every real
   globe render does anyway — true-scale relief is invisible).
4. **Resolution ceiling** at `MAX_WIDTH = 1024`: a close-zoom globe or
   regional sampling API is future widening territory.
5. **Moon appearance** is a gray disc of `size_rel`
   (ORRERY-moon-character awaits moon individuation).

## 5. Error handling

- Genesis and pin failures cross the ABI as the `GenesisError` JSON
  envelope with the physical reason; the client renders it as a visible
  state ("this seed's pins are unsatisfiable: …"), never a silent retry —
  the seed is a world's identity on both sides of the seam.
- Catalog load failures (missing wasm, checksum mismatch, worker crash)
  get an explicit UI state with the pinned version named.
- Schema-version mismatch (client meets a scene kind it doesn't know) is
  detected in `parse.ts` and reported with both versions printed.

## 6. Testing

**hornvale/hornvale:** the golden-contract smoke (wasm seed-42 scenes
byte-identical to native), the ≤ 1 MB size gate, and the existing scene
drift-checks — all in the world-wasm CI job; `make gate` is untouched.

**hornvale/goldengrove:** the harvested vitest suite adapted — unit tests
for `parse.ts` (schema validation, error envelopes), palette and
cube-sphere math, URL state round-trips — plus one node smoke that loads
the real catalog and generates seed 42. Rendering itself (three.js
output) is not golden-tested; it is licensed glamour.

## 7. Non-goals (this campaign)

- **Ground/sky situated view** — harvested code exists but Hornvale does
  not emit a situated scene pole yet (ORRERY-situated-jump; v2).
- **Neighbor starfield backdrop** — `scene/system/v1` carries no
  neighbors today (ORRERY-starfield-backdrop).
- **Living-globe animation** — winds/currents/migrations stay MAP-23
  (raw).
- **npm publication of the catalog** — GitHub release assets only.
- **Removing the star chart or atlas** — only the orrery dies; its
  removal is in scope (§1), their survival is explicit.
- **The old mythopoetic goldengrove scope** — the name returns; that
  charter does not.

## 8. Decisions to promote at merge

- **External clients consume Hornvale as a versioned wasm catalog**
  (repo boundary = determinism boundary; observatory/planetarium
  pattern) — candidate `docs/decisions/` record at close.
- Scene schemas as cross-repo contracts (may fold into the same record).

## 9. Process notes

Two-repo campaign: the hornvale side runs the normal campaign process
(worktree `goldengrove`, `make gate`, preflight absorptions). The client
repo is new and owns its CI from the first commit. Repo creation in the
`hornvale` org is externally visible and happens at execution, per
Nathan's in-session authorization. The decision ledger for this brainstorm
lives at `.superpowers/sdd/decision-ledger.md` in the worktree.
