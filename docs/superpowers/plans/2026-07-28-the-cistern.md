# The Cistern Implementation Plan

> **EXECUTED AND MERGED 2026-07-29. This file is now history.**
>
> All three tasks shipped. The central prediction — ~11× on region tiles with
> not one scene document byte moved — **was measured and held** (11.1×, and
> 10.8× on the preceding run). No decisions were minted; no epoch was declared.
>
> Kept unedited below as the record of what was planned, including the parts
> that were wrong. Four of this plan's own assumptions failed at execution and
> each is named in the retrospective: the byte-equivalence test it opens with
> went **tautological** the moment the `&World` forms became delegations; the
> "compare against The Sextant's recorded reference" measurement method would
> have measured the box rather than the code; "ratchet the ceilings down to
> ~2×" would have left the relocated 638 ms under no ceiling at all; and the
> binary-size delta it asked for is not measurable from a worktree. A plan
> rewritten after the fact to look prescient teaches nothing.
>
> ---
>
> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Derive the planet once per world instead of once per scene call, without moving a single byte of any scene document.

**Architecture:** A `SceneContext` in `windows/scene` holds terrain, climate, both nearest-cell indices and the biome map. Each terrain-facing entry point gains an `_in` variant taking that context; the existing `&World` form delegates to it, so old behaviour is preserved by construction. The wasm catalog holds one context beside its `WORLD` static, invalidated with it. A two-layer structural guard makes the old shape unwritable.

**Tech Stack:** Rust 2024, workspace crates only. No new dependencies.

## Global Constraints

- **No new dependencies.** `windows/scene` already depends on `hornvale-worldgen`, `-astronomy`, `-terrain`, `-climate`, `-settlement`, `-locale`, `-kernel` (`windows/scene/Cargo.toml`). Add nothing.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (`clippy.toml` `disallowed-types`).
- **No wall-clock in the sim.** `std::time::Instant` needs a scoped `#[allow(clippy::disallowed_types)]` with a justifying comment at each use site.
- **`#![warn(missing_docs)]`** — every public item, field and variant gets a one-line doc comment. `SceneContext` is new public API; the type audit is default-deny on untagged pub-boundary primitives.
- **Byte-identity is constitutional.** Same seed + same pins → byte-identical documents. This campaign must move **zero** bytes of scene output. That is the acceptance criterion, verified in Task 1 Step 2 and again in Task 3.
- **`cargo fmt` as the final step before every commit.**
- **Seed 42** is the canonical fixture seed.
- **Type homes** (verified in-tree — do not guess, and do not route these through `hornvale_worldgen`, where some re-exports are private; The Sextant lost a build cycle to exactly that):
  - `hornvale_kernel::{World, Seed, CellId, CellMap, NearestCellIndex, Geosphere}` (`kernel/src/geosphere.rs:14,47,339`)
  - `hornvale_terrain::GeneratedTerrain`
  - `hornvale_climate::{GeneratedClimate, Biome}` (`domains/climate/src/provider.rs:62`)
  - `Biome::catalog() -> &'static [Biome]` (`domains/climate/src/biome.rs:135`) — a static lookup, **not** cached in the context.
  - `GeneratedClimate::biome_map() -> CellMap<Biome>` (`domains/climate/src/provider.rs:424`) — returns **by value**, which is why it must be cached.

---

### Task 1: `SceneContext` and the `_in` variants

**Files:**
- Modify: `windows/scene/src/lib.rs` (add `SceneContext`; refactor `tiles_scene:276`, `temperature_grid:433`)
- Modify: `windows/scene/src/region.rs` (refactor `tiles_region_scene:300`)
- Test: `windows/scene/src/lib.rs` test module (byte-equivalence)

**Interfaces:**
- Produces, for Task 2: `hornvale_scene::SceneContext`, `SceneContext::build(&World) -> Result<SceneContext, SceneError>`, `tiles_scene_in(&World, &SceneContext, u32)`, `tiles_region_scene_in(&World, &SceneContext, u32, u32, u32, u32, u32)`, `temperature_grid_in(&World, &SceneContext, u32, f64)`.
- Consumes: nothing from earlier tasks.

- [ ] **Step 1: Capture the byte baseline BEFORE touching anything**

This is the campaign's safety net; it must exist before the refactor, not after.

```bash
mkdir -p /tmp/cistern-baseline
cargo run --release -p hornvale -- new --seed 42 --out /tmp/cistern-baseline/world.json
```

Then write a throwaway example or use the CLI to emit **every** scene document for seed 42 — system, moons, neighbors, eclipses, tiles(512), and region patches at `(face 0, level 3, ix 0..3, iy 0)` with `samples=64` — into `/tmp/cistern-baseline/`. The simplest route is a scratch example in `windows/scene/examples/` that you delete before committing; `windows/scene/examples/profile_scene.rs` already shows every call's exact signature.

Record the file sizes and a checksum of each:
```bash
sha256sum /tmp/cistern-baseline/*.json > /tmp/cistern-baseline/SUMS
cat /tmp/cistern-baseline/SUMS
```

Paste `SUMS` into your report. Every later verification compares against it.

- [ ] **Step 2: Write the failing byte-equivalence test**

Add to `windows/scene/src/lib.rs`'s test module. It will not compile yet — that is the point.

```rust
#[test]
fn the_context_path_is_byte_identical_to_the_world_path() {
    let world = gen42();
    let ctx = SceneContext::build(&world).expect("context builds");

    // Tiles: the big document.
    let via_world = scene_json(&tiles_scene(&world, 64).expect("tiles"));
    let via_ctx = scene_json(&tiles_scene_in(&world, &ctx, 64).expect("tiles_in"));
    assert_eq!(via_world, via_ctx, "tiles_scene diverged from tiles_scene_in");

    // Region: the hot path, across several addresses on one face.
    for ix in 0..3u32 {
        let via_world =
            region_json(&tiles_region_scene(&world, 0, 3, ix, 0, 8).expect("region"));
        let via_ctx =
            region_json(&tiles_region_scene_in(&world, &ctx, 0, 3, ix, 0, 8).expect("region_in"));
        assert_eq!(via_world, via_ctx, "tiles_region_scene diverged at ix={ix}");
    }

    // Temperature: the third terrain-facing entry point.
    let via_world = temperature_grid(&world, 64, 100.0).expect("temps");
    let via_ctx = temperature_grid_in(&world, &ctx, 64, 100.0).expect("temps_in");
    assert_eq!(via_world, via_ctx, "temperature_grid diverged from temperature_grid_in");
}
```

`gen42()` already exists as a test helper in `windows/scene/src/region.rs`'s test module (`region.rs:591`) — reuse it, or lift it to a shared test helper if the module boundary makes that awkward. Small `width`/`samples` values keep the test inside the commit gate's budget; the full-size check is Step 1's baseline plus Task 3.

- [ ] **Step 3: Run it and watch it fail to compile**

Run: `cargo test -p hornvale-scene the_context_path_is_byte_identical`
Expected: compile error — `SceneContext`, `tiles_scene_in`, `tiles_region_scene_in`, `temperature_grid_in` do not exist.

- [ ] **Step 4: Add `SceneContext`**

In `windows/scene/src/lib.rs`, modelled on `LocaleContext` (`windows/locale/src/lib.rs:173-199`) — read that first and match its register:

```rust
/// The reusable coarse-world build for scene documents. Constructed once and
/// reused across every scene call, so a document stays a cheap derived view.
///
/// Every terrain-facing entry point used to rebuild all of this per call —
/// 638 ms of terrain and climate derivation, 91.6% of a region patch (The
/// Sextant). The `x_scene` / `x_scene_in` pair here is the same one
/// [`surrounds_scene_in`] already uses with a `LocaleContext`.
pub struct SceneContext {
    /// The world this context was built from; guards against reuse across worlds.
    seed: Seed,
    /// The sculpted terrain, derived once.
    terrain: GeneratedTerrain,
    /// The derived climate, derived once.
    climate: GeneratedClimate,
    /// Nearest-cell index over the terrain geosphere.
    terrain_index: NearestCellIndex,
    /// Nearest-cell index over the climate geosphere.
    climate_index: NearestCellIndex,
    /// The per-cell biome map (`biome_map()` returns by value, so it is built once).
    biomes: CellMap<Biome>,
}

impl SceneContext {
    /// Derive terrain, climate, both nearest-cell indices and the biome map once.
    pub fn build(world: &World) -> Result<SceneContext, SceneError> {
        let terrain =
            hornvale_worldgen::terrain_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
        let climate = hornvale_worldgen::climate_from(world, &terrain)
            .map_err(|e| SceneError::Build(e.to_string()))?;
        let terrain_index = NearestCellIndex::new(terrain.geosphere());
        let climate_index = NearestCellIndex::new(climate.geosphere());
        let biomes = climate.biome_map();
        Ok(SceneContext {
            seed: world.seed,
            terrain,
            climate,
            terrain_index,
            climate_index,
            biomes,
        })
    }

    /// The seed this context was built from.
    pub fn seed(&self) -> Seed {
        self.seed
    }
}
```

Keep the two indices separate. `tiles_scene:288-293` carries a comment explaining why (terrain and climate each own a geosphere; they happen to agree today, and keeping them separate is deliberate defensiveness) — preserve that reasoning, moved to the struct.

- [ ] **Step 5: Refactor the three entry points**

For each of `tiles_scene` (`lib.rs:276`), `temperature_grid` (`lib.rs:433`), `tiles_region_scene` (`region.rs:300`), mechanically:

1. Rename the function to `<name>_in` and give it a `ctx: &SceneContext` parameter after `world`.
2. Delete its derivation preamble (the `terrain_of` / `climate_from` / `climate_of` calls and the `NearestCellIndex::new` / `biome_map()` lines) and read those from `ctx` instead.
3. **Change nothing else in the body.** The sampling loops, the field order, and the quantization must be untouched — that is what makes byte-identity structural rather than hoped-for.
4. Add the `&World`-only wrapper that delegates:

```rust
/// Build the `scene/tiles/v1` scene, deriving a fresh [`SceneContext`].
///
/// Prefer [`tiles_scene_in`] whenever a context is already in hand: the
/// derivation this performs costs ~638 ms against far less per-call work.
pub fn tiles_scene(world: &World, width: u32) -> Result<TilesScene, SceneError> {
    tiles_scene_in(world, &SceneContext::build(world)?, width)
}
```

Note `temperature_grid` currently calls `climate_of` (one call, `lib.rs:441`), which is `terrain_of` then `climate_from` (`windows/worldgen/src/lib.rs:1459-1462`) — so it reads `ctx.climate` and `ctx.climate_index` only.

Keep each wrapper's validation (the `width` range and parity checks) in the **wrapper and the `_in` form both**, or in the `_in` form alone — but be consistent across all three, and make sure an invalid `width` still errors before any expensive derivation in the `&World` path.

- [ ] **Step 6: Run the equivalence test**

Run: `cargo test -p hornvale-scene the_context_path_is_byte_identical`
Expected: PASS.

If it fails, **stop and report** — a divergence here is the campaign's core risk materialising, not a detail to patch. Report the first differing byte offset and both surrounding excerpts.

- [ ] **Step 7: Run the crate's full suite and re-verify against the Step 1 baseline**

Run: `cargo test -p hornvale-scene`
Expected: all pass, including the existing fixture-backed tests (`windows/scene/tests/fixtures/`).

Then re-emit the same documents as Step 1 and diff the checksums:
```bash
sha256sum -c /tmp/cistern-baseline/SUMS
```
Expected: every file `OK`. Paste the output.

- [ ] **Step 8: fmt, clippy, type audit, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/scene/src/lib.rs windows/scene/src/region.rs
git commit -m "feat(the-cistern): SceneContext and the _in variants

The three terrain-facing scene entry points take a prebuilt context
instead of re-deriving terrain and climate per call. The &World forms
delegate, so the old behaviour is preserved by construction and the
byte-equivalence test proves it."
```

If the type audit fails, it is telling you a new pub-boundary primitive needs a verdict tag — fix the tag, do not weaken the audit.

---

### Task 2: The catalog holds the cistern, and the guard closes

**Files:**
- Modify: `windows/scene/src/region.rs` (the fourth entry point; the assertions)
- Modify: `windows/scene/src/lib.rs` (the assertions)
- Modify: `clients/world-wasm/src/lib.rs`
- Create: `cli/tests/scene_context_discipline.rs`
- Create: `windows/scene/tests/fixtures/region-seed-1-f0-l3.json` (+ its golden test)

**Interfaces:**
- Consumes from Task 1: `SceneContext::build`, `tiles_scene_in`, `tiles_region_scene_in`.
- Produces: nothing importable.

**Read first:** `clients/world-wasm/src/lib.rs` in full — it is 328 lines, it is a **standalone workspace** (its own `Cargo.toml`, `opt-level="z"`), and its module doc says "statics are the whole state model."

- [ ] **Step 0a: Close the fourth terrain-facing entry point**

The Task 1 review found `temperature_grid_region` (`windows/scene/src/region.rs:456`) still opens with `hornvale_worldgen::climate_of(world)` + `NearestCellIndex::new` per call — the full 638 ms. It was missed when the spec's Item 2 table was drafted; the spec is amended and it is in scope.

Give it a `temperature_grid_region_in(world, ctx, face, level, ix, iy, samples, day)` and delegate from the `&World` form, exactly as Task 1 did for the other three. Same rule: **the body changes only where a derivation becomes a context read.** `windows/scene/examples/region_temperature_golden.rs:35` calls it in a day loop and must keep producing identical output.

- [ ] **Step 0b: Enforce the context/world match**

Add as the first line of **each** of the four `_in` variants:

```rust
debug_assert_eq!(
    ctx.seed(),
    world.seed,
    "SceneContext was built for a different world than this call's"
);
```

Not a `SceneError` variant — that would widen the enum with a case the `&World` wrapper path can never take, forcing every caller to handle an impossible branch. A `debug_assert` costs nothing in the `opt-level="z"` catalog (decision 0052), fires in every test run, and is the layer that catches a misplaced `SCENE_CTX` invalidation in Step 1. This is also what `temperature_grid_in`'s otherwise-unused `world` parameter is for — un-prefix it from `_world` where the assert now reads it.

- [ ] **Step 0c: Commit a region golden**

Today the region path's only in-repo byte evidence is the *mutual* equivalence test — it would pass if both paths moved together. The absolute evidence lives in `/tmp` and dies with a reboot. `windows/scene/tests/golden.rs` pins `tiles_scene` (`tiles-seed-1-w16.json`) and the surrounds set, but nothing pins a region patch.

Add one, following `golden.rs`'s existing pattern exactly: a small seed-1 region patch (`face 0, level 3, ix 0, iy 0, samples 8`), its bytes committed under `tests/fixtures/`, and a `#[test]` asserting they are unchanged. Small enough for the commit gate. This is what makes Task 3's ratchet safe to repeat.

Run `cargo test -p hornvale-scene` after 0a-0c and paste the summary before moving on.

- [ ] **Step 1: Add the static and the invalidation**

Beside `static mut WORLD: Option<World> = None;` (`:16`):

```rust
/// The scene context for the live world, built on first scene call and
/// reused. Cleared with `WORLD` — a context outliving its world would serve
/// the previous planet's terrain under the new seed.
static mut SCENE_CTX: Option<SceneContext> = None;
```

**The invalidation is the critical edit.** `hw_new_pinned`'s doc comment already states the invariant:

> Clears the prior world *before* parsing, even on the -1/-2/-3 early returns: any `hw_new*` call invalidates the prior world, full stop — a caller must never be able to observe a stale world surviving a refused pinned call.

Extend that to both statics. In `genesis()` (`:114`) and in `hw_new_pinned` (`:157`), wherever `*world_ptr = None` happens, clear `SCENE_CTX` in the **same statement region, before the early returns** — not in a later branch, not only on success. Update that doc comment to say "the prior world and its scene context".

- [ ] **Step 2: Use the context in the two scene entry points**

In `hw_scene_tiles` (`:244`) and `hw_scene_tiles_region` (`:265`): build the context if absent, then call the `_in` variant. Follow the file's existing `&raw mut` / `&raw const` idiom exactly — do not introduce a different unsafe pattern.

The other four scene exports (`hw_scene_system`, `hw_scene_moons`, `hw_scene_neighbors`, `hw_scene_eclipses`) derive no terrain and are **not** touched.

- [ ] **Step 3: Write the structural guard (layer 1)**

Create `cli/tests/scene_context_discipline.rs`, a source-scanning test in the same technique as `cli/tests/heavy_tier.rs` (read it — it shows the repo-root resolution and file-walking helpers to copy).

The assertion: in `clients/world-wasm/src/lib.rs`, the terrain-facing scene calls go through the `_in` variants. Concretely — parse the file's text and assert it contains `tiles_scene_in` and `tiles_region_scene_in`, and does **not** call `hornvale_scene::tiles_scene(` or `hornvale_scene::tiles_region_scene(` (the `&World`-only forms).

Give the failure message a real explanation, not just "assertion failed": say that the catalog must reuse one `SceneContext` per world, that calling the `&World` form re-derives the planet per call, and cite The Cistern's spec. A guard whose failure message does not teach is a guard someone will delete.

This test is **not** `#[ignore]`d — it scans source, costs milliseconds, and belongs in the commit gate.

- [ ] **Step 4: Verify the guard can fire**

Temporarily change one call site in `clients/world-wasm/src/lib.rs` back to the `&World` form, run the test, and confirm it FAILS with your explanatory message. Then restore and confirm it passes.

Run: `cargo test -p hornvale --test scene_context_discipline`
Paste both the failing and the restored-passing output. **Do not commit the mutated call site** — verify with `git diff` before committing.

- [ ] **Step 5: Build the wasm and check the size gate**

Run: `make world-check`
Expected: pass. This runs the catalog's lint plus a golden byte-identity smoke test plus a size gate.

**Record the `.wasm` byte size before and after** (`git stash` the branch's wasm changes, build, unstash, build — or build once on `origin/main` for the baseline). Spec §6.3 flags binary size as a real trade: the catalog is size-critical (decision 0052). Report the delta in bytes and percent. If it exceeds ~5%, stop and report rather than absorbing it.

- [ ] **Step 6: fmt, clippy, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add clients/world-wasm/src/lib.rs cli/tests/scene_context_discipline.rs
git commit -m "feat(the-cistern): the catalog holds one context per world

SCENE_CTX joins WORLD as the catalog's state, cleared with it before
hw_new_pinned's early returns so a refused pinned call cannot leave a
context describing the previous planet. A source-scan guard keeps the
&World forms out of the catalog's scene path."
```

---

### Task 3: Measure, ratchet, close

**Files:**
- Modify: `cli/tests/scene_cost.rs` (lower the ceilings)
- Create: `book/src/chronicle/the-cistern.md`, `docs/retrospectives/the-cistern.md`
- Modify: `book/src/SUMMARY.md`, `docs/timings.md`

- [ ] **Step 1: Measure the improvement**

Run: `cargo run --release -p hornvale-scene --example profile_scene -- 8`

This is The Sextant's profiler, unchanged, measuring the same workload. Compare against its recorded reference (release, seed 42, 8 tiles): `hw_new` 1848.9 ms, tiles build 930.6 ms, tiles json 553.4 ms, small docs 0.3 ms, **per tile 581.5 ms**, TOTAL 7984.7 ms.

Paste both. The per-tile figure is the campaign's headline.

**Note the profiler calls the `&World` forms.** After Task 1 those still derive once per call, so the example measures *no improvement* unless it is updated to build a context once and use the `_in` variants — which is exactly what a client does. Update `windows/scene/examples/profile_scene.rs` to do that, and have it print **both** paths so the profiler shows the before and after side by side in one run. That is the more honest instrument anyway.

- [ ] **Step 2: Re-measure in the dev profile and lower the ceilings**

The ceilings in `cli/tests/scene_cost.rs` are **dev-profile** numbers, because `make gate-full` runs the heavy tier via `cargo nextest run --workspace --run-ignored only` with no `--release` (`scripts/gate-full-heavy.sh:47`). Do not set them from Step 1's release figures.

Run three times, take the slowest per metric:
`cargo test -p hornvale --test scene_cost -- --ignored --nocapture`

Set each ceiling to ~2× the slowest, and in each constant's doc comment record the new measured value, the date, the host, "dev profile, as `gate-full` runs it", **and that this is a ratchet-down from The Sextant's value, with the old value named**. Lowering needs no ceremony (The Sextant §3.3) — but the provenance chain must stay readable.

Leave `SMALL_DOCS_BUDGET_MS` alone: those four documents are out of scope and their cost should not have moved. If it did, that is a finding — report it.

- [ ] **Step 3: Full byte-identity re-verification**

Run: `sha256sum -c /tmp/cistern-baseline/SUMS`
Expected: every file `OK`.

Then: `make rebaseline` and `git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/`
Expected: no drift. Worldgen output must not have moved.

- [ ] **Step 4: The gates**

Run `make gate` (`timeout: 3600000`). Expected green.
Run `make gate-full` (`timeout: 3600000`). Expected green including `scene_api_cost_is_bounded_on_seed_42` under its new, lower ceilings.

If a heavy test unrelated to this campaign fails, report it as inherited debt with its name and output — do not fix it.

- [ ] **Step 5: Timing ledger**

```bash
bash scripts/timed.sh scene-profile -- cargo run --release -p hornvale-scene --example profile_scene -- 8
```
(The `--` separator is required — `scripts/timed.sh:16-17`.) Confirm with `make timings LABEL=scene-profile`.

- [ ] **Step 6: Chronicle, retrospective, freshness sweep**

Chronicle at `book/src/chronicle/the-cistern.md`, wired into `book/src/SUMMARY.md` **after The Sextant** (the list is merge-chronological). Prose altitude: technical and mathematical, comprehensible without reading the code (root `CLAUDE.md`).

It should carry the measured before/after, and the point that the instrument built one campaign earlier is what made this one's claim checkable — the ceilings moved down for the first time, in the direction the ratchet was designed for.

Freshness sweep: `grep -rn "tiles_region_scene\|scene API\|638\|91.6\|702" book/src/ --include=*.md` and fix every claim this campaign falsified. **The Sextant's own chronicle now describes a solved problem** — it must read as history, not as current state, without rewriting what it measured. Re-score `book/src/open-questions.md` if a Confidence Gradient bet moved (decision 0030), and flip any idea-registry row this shipped.

Retrospective at `docs/retrospectives/the-cistern.md` — process lessons, not product (decision 0020).

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add -A
git commit -m "docs(the-cistern): close — chronicle, retro, ratcheted ceilings, timing ledger"
```

---

## Self-review

**Spec coverage.** §3.1 `SceneContext` → Task 1 Step 4. §3.2 `_in` variants → Task 1 Step 5. §3.3 the catalog's static and its invalidation → Task 2 Steps 1-2. §3.4 the two-layer guard → Task 2 Step 3 (layer 1, source scan) and Task 1 Step 2 (layer 2, byte-equivalence). §3.5 ratchet → Task 3 Step 2. §4 verification → Task 1 Steps 1/7, Task 2 Step 5, Task 3 Steps 3-4. §5's expected result → Task 3 Step 1, framed as a hypothesis to measure. §6.3 binary size → Task 2 Step 5.

**Placeholders.** None. Task 3 Step 2's ceiling values are outputs of Step 2's own measurement, and the rule for deriving them is stated.

**Type consistency.** `SceneContext::build(&World) -> Result<SceneContext, SceneError>` is used identically in Tasks 1 and 2. `tiles_region_scene_in(&World, &SceneContext, u32, u32, u32, u32, u32)` matches `tiles_region_scene`'s existing five address parameters (`windows/scene/src/region.rs:300`) with the context inserted second. Type homes are pinned in Global Constraints against the real tree.

**Known risk.** Task 1 Step 5 says "change nothing else in the body", which is what makes byte-identity structural — but nothing mechanically enforces it. The byte-equivalence test (Step 2) and the checksum re-verification (Step 7) are the two independent nets, and they check different things: the test compares the two code paths against each other, the checksums compare the new code against pre-campaign output. Both are needed — the test alone would pass if the refactor changed both paths identically.
