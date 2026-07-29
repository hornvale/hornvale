# The Sextant — design

**Campaign:** The Sextant
**Date:** 2026-07-28
**Status:** spec, awaiting G3 review

A committed instrument for the client-facing scene APIs: measure what the
Orrery actually asks for, on the shape it actually asks for it, and leave a
ceiling behind that a future regression trips over.

## 1. The problem

The Orrery feels slow, and it is. Measured on `lefford` (40 cores, native
`profiling` profile, seed 42) with a scratch harness mirroring the client's
`hw_*` call sequence:

```
hw_new                     2065.2 ms
hw_scene_tiles(512)        1068.6 ms build +  567.0 ms json (17313 KB)
hw_scene_tiles_region x1    687.3 ms total,  687.3 ms/tile
hw_scene_tiles_region x8   5604.2 ms total,  700.5 ms/tile
hw_scene_tiles_region x24 16842.6 ms total,  701.8 ms/tile
hw_scene_system/moons/neighbors/eclipses   < 0.3 ms each
```

Per-tile cost is flat in tile count: nothing is cached anywhere. Every
`windows/scene` entry point opens with the same two lines
(`region.rs:316`, `lib.rs:284`, `lib.rs:441`):

```rust
let terrain = hornvale_worldgen::terrain_of(world)?;   // → hornvale_terrain::generate, no memoization
let climate = hornvale_worldgen::climate_from(world, &terrain)?;
```

Isolated: `terrain_of` 543.8 ms, `climate_from` 94.0 ms — **638 ms of fixed
overhead on every scene call.** A frame-pointer flamegraph over
`hw_scene_tiles_region × 12` reports inclusive shares of *total process*
time: `terrain_of` 61.36 %, `climate_from` 9.84 %, and the region calls
themselves 77.77 % (the remainder is the one-time genesis). Normalized into a
region call, that is **(61.36 + 9.84) / 77.77 = 91.6 % redundant
re-derivation**, leaving ~64 ms of actual sampling — and it agrees with the
wall clock independently, 638 of 702 ms/tile = 90.9 %. `Fbm::sample` alone is
24.9 % of self time.

The Orrery requests one region patch **per LOD tile** (`TILE_QUADS = 64`,
`REGION_MIN_LEVEL = 3`, `LOD_CDLOD_MAX_LEVEL = 4`) on every camera move, so a
move touching 24 tiles costs ~17 s, of which ~15.3 s is rebuilding the same
planet 24 times.

### 1.1 Two things measured and ruled out

Both were checked before proposing anything, so neither is re-litigated later:

- **`BuildDepth` is not the lever.** Astronomy 1.1 ms, Terrain 562.1 ms,
  Settlements 1763.3 ms, Full 1800.7 ms. The Orrery needs climate, which
  first appears at `Settlements`; dropping `Full → Settlements` saves 37 ms.
- **`opt-level = "z"` is a secondary lever.** The wasm profile (decision
  0052, size-first) costs ~23 % on this path natively (866.4 vs 702.0
  ms/tile). Real, an order of magnitude smaller than the rebuild, and it
  trades against binary size.

### 1.2 Why nothing caught this

The gate ladder has no instrument pointed here. `windows/worldgen`'s
committed profiler measures the **build**, not the scene reads; The Frame
Budget's harness measures the **client**, in a different repo, and blamed
`buildTiles` — correctly, for what it could see. The 91 % lives exactly in
the seam neither instrument covers: the producer's cost *under the
consumer's call pattern*. That seam is this campaign's territory.

### 1.3 The defect is a signature, not a hot loop

Nothing here is slow code. `tiles_region_scene(world, face, level, ix, iy,
samples)` is a clean pure function of the world; `terrain_of` is the honest
way to get terrain from a `World`. The cost is emergent — each scene
entry point opens the same way because its neighbour did. A fixture that
measures only *time* will keep rediscovering the symptom; the durable guard
has to be able to see the *shape*. See §3.5.

## 2. Non-goals

- **The fix itself.** Caching the build artifacts is a separate campaign
  (owner decision, this session: fixture first). This spec deliberately
  lands the instrument against today's numbers.
- **Changing any scene API signature.** Consequence: see §5, flagged.
- **The wasm/browser side.** Native measurement only; the ~23 % `opt-level
  = "z"` multiplier from §1.1 is recorded as a documented constant, not
  re-measured per run.
- **`opt-level` or `BuildDepth` changes** — measured, ruled out, §1.1.
- **Any client-side (Orrery repo) work.** Different repo, different gate.

## 3. Design

### Item 1 — `windows/scene/examples/profile_scene.rs`, a consumer-shaped workload

Follows the committed-profiler convention exactly
(`windows/worldgen/examples/profile_build.rs`,
`domains/terrain/examples/profile_terrain.rs`): an `examples/` binary in the
crate that owns the code, `SAMPLE`-style positional arg, prints a share
table to stdout, commits nothing.

Verified convention (real output, this box):

```
$ cargo run -q -p hornvale-worldgen --example profile_build -- 2
build profile over 2 seeds (total 5.871s):
  astronomy                       0.004s    0.1%
  terrain                         2.341s   39.9%
  climate+settlements             2.886s   49.1%
  ...
```

The departure from precedent is the **workload**: not "each scene function
once" but *the Orrery's session shape* — `hw_new`, then the six scene
documents, then a fan of N region patches at the client's real
`TILE_QUADS`/`REGION_MIN_LEVEL` constants. The workload is named after the
consumer because that is where the defect lives (§1.2). Constants are
mirrored from the Orrery with a source comment (`cubeSphere.ts:11`,
`globe.ts:346`) so a client-side change that invalidates them is greppable.

`Instant` needs the scoped carve-out the no-wall-clock rule requires, with
the justifying comment — copy `profile_terrain.rs`'s exactly:

```rust
// The profiler measures wall-clock durations for a committed diagnostic
// only — it never touches WorldTime or facts, so it is exempt from the
// no-wall-clock-in-the-sim rule (the sanctioned Instant use for this crate).
#[allow(clippy::disallowed_types)]
use std::time::Instant;
```

No new dependencies: `windows/scene` already depends on `hornvale-worldgen`,
`-astronomy`, `-terrain`, `-climate` (verified in its `Cargo.toml`), which
covers `SkyPins` / `TerrainPins` / `SettlementPins` / `build_world`.

### Item 2 — `cli/tests/scene_cost.rs`, falsification ceilings

Modelled on `cli/tests/graph_cost.rs`, including its framing: a budget is
"set comfortably above the measured value (a falsification ceiling, not a
target)". Same mechanics:

- `#[ignore]`d with the **verbatim** canonical heavy-tier reason string
  enforced by `cli/tests/heavy_tier.rs`:
  `"heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"`
- Runs in `make gate-full` (`scripts/gate-full-heavy.sh` — verified present).
- Measured numbers recorded in the module doc comment, as `graph_cost.rs`
  does, so the ceiling's provenance travels with it.

Ceilings, one per client-visible operation: `hw_new`, `tiles_scene(512)`
build and JSON separately (they are 1069 / 567 ms and regress for different
reasons), the small scene documents as one aggregate, and **per-tile**
region cost — per-tile, not per-fan, so the ceiling is independent of how
many tiles the fixture happens to request.

### Item 3 — ceilings ratchet down, never quietly up

`graph_cost.rs`'s own history is the argument: its wall-time budget was
re-baselined from 2.6 s to ~31 s as the world grew (129 → 344 settlements).
That was the right call each time and it is documented — but the mechanism
has only one direction of travel, and a ceiling that rises whenever it is
hit stops being a guard.

So each budget constant carries, in its doc comment: the measured value, the
date, the host, and — if it has ever been raised — the decision that raised
it. Lowering needs no ceremony. **Raising is an explicit, reviewed act**,
and the constant's comment is where that shows. This is a documentation-and-
review discipline, not new machinery.

### Item 4 — campaign-cadence runs enter the timing ledger

`scripts/timed.sh` already appends `wall_s / user_s / sys_s / cpu_ratio /
commit / branch / host / cores` to `docs/timings.md`, and `make timings`
reads it back. The profiler run at a campaign close goes through it under a
`scene-profile` label — reusing the ledger rather than starting a second
timing story.

**Nothing timing-shaped becomes a committed, drift-checked artifact.**
`the-sounding`'s `rows.csv` carries `bake_ns` / `read_ns_per_op` and its
churn is a known nuisance; that trap is avoided by construction here. The
profiler prints to stdout; ceilings live as constants in a test; history
lives in the append-only ledger.

### Item 5 — the structural guard, and why it is not in this campaign

The guard that would actually pin §1.3 is structural, not temporal: *the
scene layer derives terrain at most once per world per session.* It is
deterministic, cannot flake, and targets the signature rather than the
symptom — the same character as the drift check, the type audit, and the
architecture test, and unlike a wall-clock ceiling (which is the only
member of that family that is neither structural nor statistical).

**It cannot be written against today's signature.** Every scene entry point
takes `&World` and derives internally; there is no seam at which a test can
observe "derived once" without either the artifact-taking API the fix
introduces, or invasive counter instrumentation in `worldgen` that would
exist solely for the test. So the structural guard is specified here and
**deferred to the fix campaign**, which is the first point it can be written
honestly. Recorded in the followup register; flagged in §5.

## 4. Verification

- `cargo run -p hornvale-scene --example profile_scene -- 8` prints a share
  table; the numbers land within noise of §1's scratch-harness figures
  (that agreement is the fixture's own acceptance test — the scratch
  harness is the reference implementation it replaces).
- `cargo test -p hornvale --test scene_cost -- --ignored --nocapture` passes
  with the ceilings set from that run.
- `make gate` stays green and its wall time is unchanged (the new test is
  `#[ignore]`d out of it; the example is not run by any gate).
- `cli/tests/heavy_tier.rs` passes — proving the new test's ignore-reason
  string matches the canonical one verbatim.
- `make gate-full` runs the new battery.
- `cargo fmt` + `cargo clippy --workspace --all-targets -- -D warnings`
  clean, including the scoped `disallowed_types` allow.
- Type audit: `cargo run --manifest-path tools/type-audit/Cargo.toml --
  check` — the example and test add no `pub` boundary primitives, so this
  should be a no-op; run it to confirm rather than assert it.

## 5. Flagged for review (G3)

1. **The strongest guard is deferred (§3.5).** "Fixture first" was the owner
   call, and this spec honours it — but the consequence surfaced during
   design: the structural, unflakeable guard is *coupled to the fix* and
   cannot land in this campaign. What lands here is a profiler, wall-clock
   ceilings, and a ledger entry. If the goal is to make the 91 % impossible
   to reintroduce, this campaign alone does not achieve it; the pair does.
   **Nathan's call:** accept the split, or fold the two campaigns together.
2. **The ceilings are set above a known-bad number.** They will be ~700
   ms/tile — locking in "no worse than today's bad", which is real
   regression protection but reads oddly in the tree until the fix ratchets
   them. Alternative considered: withhold the region ceiling until the fix.
   Recommended against — a missing ceiling is worse than a loose one, and
   §3.3's ratchet discipline is exactly the mechanism for tightening it.
3. **Cross-repo constant duplication.** `TILE_QUADS` / `REGION_MIN_LEVEL`
   are the Orrery's, mirrored here with a source comment. They can drift
   silently; nothing enforces the mirror. Judged acceptable (the alternative
   is a shared manifest for two constants), but it is a new, deliberate
   duplication across a repo boundary and should be seen.
4. **No save-format, epoch, schema, or determinism-contract change.** This
   campaign adds an example and a test; it touches no seeded draw, no stream
   label, and no serialized shape. Noted explicitly because that is the
   section that normally leads here.

## 6. Acceptance criteria

- [ ] `windows/scene/examples/profile_scene.rs` exists, runs, and prints a
      per-operation table over a `SAMPLE` argument.
- [ ] Its workload mirrors the Orrery's call sequence and LOD constants,
      with source comments naming the client files.
- [ ] `cli/tests/scene_cost.rs` exists, `#[ignore]`d with the verbatim
      canonical heavy-tier reason, with measured numbers in its module doc.
- [ ] Every budget constant documents measured value, date, and host.
- [ ] `make gate` green and unchanged in wall time; `make gate-full` runs
      the new battery green.
- [ ] `docs/timings.md` has a `scene-profile` row from the close run.
- [ ] The structural guard is recorded in the followup register with §3.5's
      reasoning.
- [ ] Book: chronicle entry + freshness sweep. The Frame Budget's chapter
      says the halo fix "lives in the world's generator, off-limits to a
      client-only campaign" — this campaign's findings are the
      producer-side counterpart to that chapter and should cross-reference
      it.
