# The Cistern — design

**Campaign:** The Cistern
**Date:** 2026-07-28
**Status:** **COMPLETE — shipped and merged 2026-07-29.** All three tasks
shipped. The preregistered hypothesis of §5 (~11× on region tiles) **held**:
902.3 → 81.4 ms/tile within one process, **11.1×**, with byte-identity intact
across all nine scene documents. Chronicle: `book/src/chronicle/the-cistern.md`.
Retrospective: `docs/retrospectives/the-cistern.md`. **No decisions were
minted** and no epoch was declared — nothing in the derivation moved (0084's
test). Spec approved at G3.

A vessel that holds what was drawn. The scene window derives the planet once
per world instead of once per call, and the guard that makes the old
behaviour unwritable lands with it.

## 1. The problem

The Sextant measured it and left the instrument behind. Every terrain-facing
`windows/scene` entry point opens by re-deriving the whole planet:

```rust
let terrain = hornvale_worldgen::terrain_of(world)?;   // 543.8 ms, no memoization
let climate = hornvale_worldgen::climate_from(world, &terrain)?;  // 94.0 ms
```

**638 ms of fixed overhead per call.** `tiles_region_scene` costs 702 ms/tile,
flat in tile count; a flamegraph puts 91.6% of each call in that
re-derivation, leaving ~64 ms of actual sampling. The Orrery requests one
region patch per LOD tile, so a camera move touching 24 tiles costs ~17 s, of
which ~15.3 s is the same planet built 24 times.

Full measurement and method: `docs/superpowers/specs/2026-07-28-the-sextant-design.md`.
Two levers were measured and ruled out there and are not reopened here:
`BuildDepth` (37 ms) and `opt-level="z"` (~23%).

### 1.1 The fix's shape already ships in this crate

`windows/scene/src/surrounds.rs:174` is the pattern, in the same crate:

```rust
pub fn surrounds_scene_in(
    world: &World,
    ctx: &LocaleContext,   // the artifact, passed in
    room: &RoomAddr, radius: u32, at: WorldTime,
) -> Result<SurroundsScene, SceneError>
```

Its doc comment makes this campaign's argument and carries its own
measurement: `LocaleContext::build` ~1.19 s against ~2 ms of per-cell work,
"so building a fresh context per call would make a radius-0 chart cost the
same as a radius-8 one." `windows/locale`'s `LocaleContext::build`
(`windows/locale/src/lib.rs:183`) does the derive-once itself.

So this is not a new seam. It is the `x_scene` / `x_scene_in` pair extended
to the four entry points that lack it.

## 2. Non-goals

- **No change to any scene document's bytes.** This is a pure performance
  change; byte-identity is the acceptance criterion, not a hoped-for
  side effect (§4).
- **No new seeded draw, stream label, or serialized shape.** No epoch.
- **Not the JSON size problem.** `tiles_scene(512)` emits 17.3 MB and spends
  567 ms serializing it. That survives this campaign untouched and becomes
  the dominant remaining cost — a candidate for the next one, not this one.
- **Not `opt-level`, not `BuildDepth`** — measured and ruled out by The
  Sextant.
- **No client-side (Orrery repo) work.**

## 3. Design

### Item 1 — `SceneContext`, built once

A new public struct in `windows/scene`, modelled directly on `LocaleContext`
(`windows/locale/src/lib.rs:173-199`) — same shape, same doc register:

```rust
pub struct SceneContext {
    seed: Seed,
    terrain: GeneratedTerrain,
    climate: GeneratedClimate,
    terrain_index: NearestCellIndex,
    climate_index: NearestCellIndex,
    biomes: CellMap<Biome>,
}

impl SceneContext {
    pub fn build(world: &World) -> Result<SceneContext, SceneError> { … }
}
```

**It must hold the indices and the biome map, not merely terrain and
climate.** `tiles_scene` and `tiles_region_scene` each construct two
`NearestCellIndex::new` values and call `climate.biome_map()` — which
returns a `CellMap<Biome>` **by value** (`domains/climate/src/provider.rs:424`),
i.e. builds a fresh map per call. `NearestCellIndex::new` was 1.57% of self
time in the region flamegraph. Caching only the two providers would leave
those rebuilt per call and forfeit part of the win for no reason.

`Biome::catalog()` stays a per-call value: it is a `&'static` catalog lookup,
not derived state.

### Item 2 — `_in` variants on the terrain-facing entry points

| existing | new |
|---|---|
| `tiles_scene(world, width)` | `tiles_scene_in(world, ctx, width)` |
| `tiles_region_scene(world, face, level, ix, iy, samples)` | `tiles_region_scene_in(world, ctx, …)` |
| `temperature_grid(world, width, day)` | `temperature_grid_in(world, ctx, width, day)` |
| `temperature_grid_region(world, face, level, ix, iy, samples, day)` | `temperature_grid_region_in(world, ctx, …)` |

**Amended after the Task 1 review (2026-07-28):** `temperature_grid_region`
(`windows/scene/src/region.rs:456`) is a **fourth** terrain-facing entry
point and was missed when this table was drafted — it opens with
`climate_of` + `NearestCellIndex::new` per call, the full 638 ms, and
`windows/scene/examples/region_temperature_golden.rs:35` calls it in a day
loop. It is in scope: §1's claim is "every terrain-facing entry point", and
leaving one out would make Item 4's guard narrower than the rule it
enforces.

The `&World`-only forms **stay and keep their signatures**, each delegating:

```rust
pub fn tiles_scene(world: &World, width: u32) -> Result<TilesScene, SceneError> {
    tiles_scene_in(world, &SceneContext::build(world)?, width)
}
```

That is exactly `surrounds_scene` / `surrounds_scene_in`'s relationship, it
keeps every existing caller compiling unchanged, and it makes the delegation
the byte-identity argument: the wrapper's behaviour is the old behaviour by
construction.

`temperature_grid` is included even though the Orrery evaluates the seasonal
curve client-side (`orrery src/sim/climate.ts`, golden-pinned) and no wasm
export reaches it. It carries the identical defect on the lab and book paths,
and leaving any of them unfixed would make the structural guard in Item 4
narrower than the rule it is meant to enforce.

The four astronomical documents (`system_scene`, `moons_scene`,
`neighbors_scene`, `eclipses_scene`) are **not** touched: they read
`sky_of(world)` and derive no terrain, which is why they cost 0.3 ms
together.

### Item 3 — the catalog holds the cistern

`clients/world-wasm/src/lib.rs` keeps `static mut WORLD: Option<World>`
(`:16`) and the `&raw mut` access pattern; a sibling static joins it:

```rust
static mut SCENE_CTX: Option<SceneContext> = None;
```

`hw_scene_tiles` and `hw_scene_tiles_region` build it on first use and reuse
it thereafter.

**The staleness hazard is the one real bug risk in this campaign, and the
discipline for it already exists in prose.** `hw_new_pinned`'s doc comment
says:

> Clears the prior world *before* parsing, even on the -1/-2/-3 early
> returns: any `hw_new*` call invalidates the prior world, full stop — a
> caller must never be able to observe a stale world surviving a refused
> pinned call.

That invariant now covers two statics rather than one. `SCENE_CTX` must be
cleared **in the same statement region as `WORLD`, before the early
returns** — never in a later branch, never only on the success path. A
context surviving a refused `hw_new_pinned` would serve the previous world's
terrain under the new world's seed: silent, plausible, and wrong. `SceneContext`
carries `seed` so a debug assertion can catch it, but placement is the real
guard.

**Enforcement shape, settled at the Task 1 review:**
`debug_assert_eq!(ctx.seed(), world.seed, …)` as the first line of each
`_in` variant — **not** a new `SceneError` variant. A `Result` variant would
widen the error enum with a case the `&World` wrapper path can never take,
forcing every caller to handle an impossible branch. A `debug_assert` costs
nothing in the `opt-level="z"` catalog build (decision 0052), fires in every
test run, and is precisely the layer that catches a misplaced `SCENE_CTX`
invalidation. This is also why `temperature_grid_in` keeps its otherwise-
unused `world` parameter.

### Item 4 — the structural guard The Sextant deferred

Now writable, because Item 2 creates the seam it needs. Two layers, per the
lesson that multi-layer guards need one shared source of truth:

1. **A source-scan test** in `cli/tests/` (the home of workspace-wide
   enforcement tests, alongside `architecture.rs` and `heavy_tier.rs`, and
   the same technique): assert that every scene entry point in
   `clients/world-wasm/src/lib.rs` reaches the `_in` variant, never a
   `&World`-only form. Deterministic, no timing, cannot flake.
2. **A behavioural test** in `windows/scene`: build one `SceneContext`, call
   `tiles_region_scene_in` across several addresses, and assert the
   documents are byte-identical to the same addresses through the
   `&World`-only path. This is the guard that the delegation is honest.

Layer 1 alone would pass if the `_in` variant internally re-derived; layer 2
alone would pass if the wasm never used it. Neither is worth much without
the other.

### Item 5 — ratchet the ceilings down

`cli/tests/scene_cost.rs`'s budgets exist to be tightened, and this is the
first campaign entitled to do it. Lower each to ~2× the newly measured
value, re-record measured value / date / host / profile in each constant's
doc comment, and note in the module doc that the values descend from The
Sextant's. **Lowering needs no ceremony** — that is the ratchet's designed
direction (The Sextant §3.3).

The `SMALL_DOCS_BUDGET_MS` ceiling is untouched: those four documents are
outside this campaign's scope and their cost should not move.

## 4. Verification — byte-identity is the acceptance criterion

Determinism is constitutional here and this campaign touches the path that
produces committed artifacts. Ordered cheapest-first:

1. **Whole-route byte comparison before any optimisation is trusted.** Build
   seed 42 and emit **every** scene document — system, moons, neighbors,
   eclipses, tiles(512), and a fan of region patches — to files on
   `origin/main`, then again on the branch, and `cmp` each. Checking only the
   region path would verify one route and claim the total.
2. `cargo test -p hornvale-scene` — the crate's own suite, including the new
   equivalence test (Item 4 layer 2).
3. `cargo test -p hornvale --test scene_cost -- --ignored --nocapture` — the
   measured improvement, and the basis for Item 5's new ceilings.
4. `make gate` green; `cli/tests/heavy_tier.rs` still green.
5. `make world-check` — the catalog's own lint + golden byte-identity smoke +
   size gate. `SceneContext` adds a live struct to a size-critical
   `opt-level="z"` binary; report the size delta rather than assume it.
6. `make rebaseline` then `git diff --exit-code` on the artifact directories:
   worldgen output must not move.

## 5. Expected result

From The Sextant's measurements, per-call cost loses the 638 ms derive:

| | now | expected |
|---|---|---|
| region patch | 702 ms/tile | ~64 ms/tile (~11×) |
| `tiles_scene(512)` build | 1069 ms | ~431 ms |
| a 24-tile camera move | ~16.8 s | ~1.5 s |
| cold load + one camera move | ~20.5 s | ~4.3 s (~4.7×) |

**The ~64 ms residual is measured by difference (702 − 638), not directly**,
and it still contains the two `NearestCellIndex::new` builds and the
`biome_map()` copy that Item 1 also caches — so the true figure should come
in *under* it. Treat the table as the hypothesis this campaign tests, not as
a promise; the profiler prints the answer.

## 6. Flagged for review (G3)

1. **`SceneContext` is new public API on a window crate**, and the
   `_in` variants widen the scene surface from 6 entry points to 9. That is
   the precedented shape (`surrounds_scene_in`), but it is API growth and the
   type audit will want tags on the new boundary.
2. **A stale `SCENE_CTX` is the failure mode with real consequences** — it
   would serve the wrong world's terrain silently. §3.3 places the
   invalidation and explains why; it deserves specific attention at review.
3. **Binary size.** The wasm is size-critical (decision 0052 ties go to the
   smaller binary). Holding a second live structure costs bytes. Measured at
   §4.5; if the delta is material, that is a real trade to put to you rather
   than absorb silently.
4. **No save-format, epoch, schema, or determinism-contract change intended**
   — but this campaign touches the code path that *produces* the scene
   documents, so §4.1's byte comparison is what turns that intent into
   evidence.
