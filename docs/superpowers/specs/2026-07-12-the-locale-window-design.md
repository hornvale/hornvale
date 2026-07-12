# The Locale Window — Design (P2, campaign 2)

**Status:** Design, approved 2026-07-12. Follows *The Room Mesh* (campaign 1,
`kernel/src/room.rs`, registry **MAP-28** shipped). This is the **first
consumer** of the room mesh: the window that turns a `RoomAddr` into an
observable place.

**Registry home:** rides on **MAP-28** (the substrate) and is the near-term,
constitution-fitted slice of **MAP-29** (room-scale variety) — it stands up
the composition path (coarse-field inheritance → a described room) that the
biome bestiary later fills. It is also **The Walk §4.1's Local-space
interface**: the game layer's first walkable surface.

**Governing prior art (read before implementing):**
- `kernel/src/room.rs` — the shipped substrate (addressing, geometry,
  `neighbors`, vertical verbs, `corner_weights`, per-room `seed`).
- `docs/design/room-scale/synthesis.md` §4 — the target architecture (the
  `locale` window as composition consumer over worldgen).
- `docs/design/room-scale/p2-subdivision-design.md` §14 Q4 — the categorical
  field-blend question this spec settles.
- Decision `identity-computes-on-the-canonical-grid` and registry **MAP-30**
  — refine fields, inherit mesh-bound truth unchanged; a finer re-quantization
  of a category is an epoch, not a tier.

---

## 1. What this is

A new window crate, `windows/locale` (`hornvale-locale`), that answers one
question deterministically: **given a world and a `RoomAddr`, what is it like
to stand there?** The answer is a plain, serializable `Locale` value —
inherited biome, blended continuous fields, seed-derived sub-cell texture, and
exits — plus a CLI `locale` subcommand that renders it as prose ("look", not
yet "walk").

The window constructs **no providers of its own**. It reads the coarse world
through `worldgen` (`climate_of`, `terrain_of`) exactly as the almanac window
does, and composes room-scale detail on top. It is the concrete instantiation
of the synthesis's P1 *inheritance* step, minus the full field-combinator
algebra (deferred — see §9).

## 2. Scope

**In scope (v1):**
- The `windows/locale` crate, the `locale/room/v1` schema, and the reusable
  `LocaleContext` (§3).
- Coarse-field inheritance and blend (§4) — settling P2 §14 Q4.
- Seed-derived sub-cell texture as an explicit P3 placeholder (§5).
- The exit model: base (lateral) + vertical (§6).
- One small **presentation-side** kernel addition: `RoomAddr::containing`
  (§7), so a coordinate can name a room.
- The `hornvale locale` CLI "look" surface + a drift-checked artifact (§8).

**Out of scope (deferred, with homes — §9):** the P1 field-combinator
framework, the P3 weighted-cross-product grammar and its descriptor pools, the
`ecology`/`society` domains, the palimpsest time axis (P4/MAP-30), overlay
exits and passability, adaptive-depth T-junction neighbours, and any "walk"
(movement) REPL.

## 3. The `Locale` data model and the `LocaleContext`

`Locale` is a **versioned semantic schema**, `locale/room/v1`, in the manner of
the `scene` window (`scene/tiles/v1`): a save-format-class contract, additive
changes stay in-version, changed meaning mints `locale/room/v2` alongside. It
is **ground truth** — the derived, re-derivable, never-stored *view* a designer
or the sim reads (UNI-20's derived-view architecture) — explicitly **not** the
perceived/belief view (UNI-16 / The Walk's projection); a later belief layer
reduces this, it does not replace it.

```rust
pub const ROOM_SCHEMA: &str = "locale/room/v1";

pub struct Locale {
    pub addr: RoomAddr,
    pub id: RoomId,
    pub depth: u32,
    pub coord: GeoCoord,              // centroid (presentation)
    pub biome: Biome,                // inherited, max-weight (§4)
    pub fields: LocaleFields,        // weighted blend (§4)
    pub corners: [(CellId, u64); 3], // provenance, from corner_weights
    pub texture: SubCellTexture,     // seed-derived (§5)
    pub exits: Vec<Exit>,            // base + vertical (§6)
}

pub struct LocaleFields {
    pub temperature_c: f64,
    pub moisture: f64,
    pub elevation_m: f64,
}
```

**The `LocaleContext` — the reusable coarse-world build.** Describing a room
needs the whole planet's coarse world (geosphere + `NearestCellIndex` +
`climate_of` + `terrain_of`), which is expensive to build. That build is
therefore a **context object**, constructed once and reused, never rebuilt
inside `describe`:

```rust
pub struct LocaleContext { /* geosphere, index, climate, terrain, globe_level */ }

impl LocaleContext {
    pub fn build(world: &World) -> Result<LocaleContext, LocaleError>;
    /// A room's ground-truth locale at observation time `at`. Pure over
    /// (context, addr, at): same inputs → byte-identical `Locale`.
    pub fn describe(&self, addr: &RoomAddr, at: WorldTime) -> Result<Locale, LocaleError>;
}
```

This keeps `Locale` a genuinely cheap derived view: the v1 CLI builds the
context once per invocation, and the deferred walk loop (§9) reuses one context
across every step for free — no signature churn when it lands. `describe`
threads `WorldTime` so the P8 temporal-phase layer (season/moon/succession) is
an in-version refinement, not a breaking change; **v1 samples the
time-independent annual mean and does not yet vary with `at`** (§4).
`LocaleError` fails loudly (`AboveGrid` when the room is coarser than the
canonical grid; the physical reason, per the fail-loud convention).

## 4. Inheritance & blend — settling §14 Q4

From `addr.corner_weights(geo, index) -> Option<[(CellId, u64); 3]>`: the three
canonical globe-level corner cells and their integer weights (numerators over
`D = 3 << (depth − globe_level)`, summing to `D`).

**Categorical biome (mesh-bound truth — inherited, never re-quantized):**
the room's `Biome` is the biome of the **maximum-weight** corner cell. Ties
break by **lowest `CellId`** (deterministic, platform-exact — all integer). No
blend-then-reclassify: re-quantizing a category at finer resolution would
contradict the canonical grid, which decision
`identity-computes-on-the-canonical-grid` reserves for an *epoch*, not a tier.

**Continuous fields (resolution-free — refined by weighted blend):**
`temperature_c`, `moisture`, and `elevation_m` are the integer-weighted average
of the three corner cells' `CellMap` values:
`field = Σ (weight_i · value_i) / D`. This is the constitutionally-sanctioned
half of refinement — a room may sample coarse fields as finely as it likes.

**Above the grid:** if `corner_weights` returns `None` (`depth < globe_level`),
`describe` returns `LocaleError::AboveGrid`. The default depth (§8) guarantees
this never happens on the CLI happy path; the error exists for library callers
passing arbitrary addresses.

The three source fields come from `worldgen`: biome from
`climate_of(world).biome_map()`, temperature from the climate's **annual mean**
(`mean_temperature_at` — time-independent, the right static field for v1; the
time-varying `temperature_at(cell, day)` is what P8 will read through the
threaded `WorldTime`), moisture from `moisture_at`, elevation from
`terrain_of(world).globe().elevation`.

**Known aesthetic limit (deliberate).** Max-weight inheritance makes the biome
a *step* function of position: it flips wherever the dominant corner changes, so
adjacent rooms can show a **hard biome edge mid-triangle**. This is honest —
the canonical grid itself has hard cell boundaries — and correct under
"inherit, never re-quantize." v1 is intentionally the **low-variety inheritance
spine**: it interpolates smoothly and offers no exceptions, so its bare "look"
is close to uniform-by-design. Softening those seams (ecotones, frontiers) is
P5's job; strangeness and true variety are P3/P7 — all deferred (§9). The one
thread of non-sameness in v1 is the seed-derived texture (§5).

## 5. Sub-cell texture (seed-derived)

`addr.seed(world)` gives a per-room, integer-derived, platform-exact `Seed`.
From it, v1 derives a deliberately **small** `SubCellTexture` — an honest
placeholder for the future P3 grammar, **not** premature content authoring:

```rust
pub struct SubCellTexture {
    pub aspect: &'static str,  // one draw from a tiny biome-keyed pool
    pub relief_jitter: f64,    // bounded ± perturbation, sibling-distinguishing
}
```

- **`aspect`**: a single weighted draw (one `Stream` off the room seed, a
  declared seed-label) from a **tiny fixed pool keyed by the inherited biome**
  (e.g. TemperateForest → {"dense understory", "old growth", "windthrow
  clearing"}). Explicitly marked in code as the P3 descriptor-grammar
  stand-in; pools stay ≤ ~4 entries per biome so this does not pre-empt
  MAP-29's combinatorial engine.
- **`relief_jitter`**: a bounded scalar (a few percent of a field-appropriate
  range) so sibling rooms under one corner cell read differently. It textures
  the *presented* fields; it does not alter inherited categorical truth.

The seed-label(s) join the frozen save-format set (declared as constants in the
crate's `streams` module, published via `stream_labels()` — the existing
discipline). Stream-consumption order is a contract.

## 6. Exits — base + vertical

```rust
pub struct Exit {
    pub direction: Direction, // compass bucket, or Enter(digit) / Exit
    pub kind: ExitKind,       // Edge | Vertical  (extensible)
    pub to: RoomAddr,
}
```

- **Lateral (3):** `addr.neighbors()` — the three geometric base-mesh edges —
  each named by `addr.bearing_to(&neighbor)` bucketed to a compass point
  (N/NE/E/SE/S/SW/W/NW). `neighbor[i]` is across the edge opposite corner `i`.
- **Vertical:** `child(0..4)` → `Enter(digit)` (descend into a finer sub-room),
  `parent()` → `Exit` (step back out to the containing room). Deliberately
  **not** `Up`/`Down`: the vertical verb is a change of *scale* (zoom), and
  `Down` would collide with the future Underdark's real *stratum* descent
  (subterranean rooms, 1.16/1.17). `Enter`/`Exit` keeps that semantic space
  clean for when strata land.

`ExitKind` is an **open enum** shaped so overlay kinds (river / road / tunnel /
portal, keyed by `RoomId`) and passability deltas compose as additive variants
later — they get homes in the deferred layers (§9), not stubs now. v1 emits no
overlay or passability edges.

## 7. Addressing a room from a coordinate — one small kernel addition

The substrate has no public coordinate→room locator (`decode` is private;
`NearestCellIndex::nearest` yields only a `CellId`). v1 adds one:

```rust
// kernel/src/room.rs, beside the other presentation helpers (coord, corners…)
impl RoomAddr {
    /// The room at `depth` containing sphere `position`. PRESENTATION-SIDE:
    /// resolves a float coordinate to an integer address (float→address, the
    /// same determinism class as `NearestCellIndex::nearest`). NOT an identity
    /// path — a boundary-straddling coordinate may resolve to adjacent rooms on
    /// different platforms; the room's *content* stays integer-exact once
    /// addressed. type-audit: pending(wave-1)
    pub fn containing(geo: &Geosphere, position: [f64; 3], depth: u32) -> RoomAddr;
}
```

Implementation: identify the base icosahedron face containing `position`
(point-in-spherical-triangle over the 20 faces), project into that face's
plane, quantize the barycentric coordinate to `scale = 2^depth`, and reuse the
existing private `decode` for top-down containment. This lives in the kernel
because it needs `decode`/`bary_triple` and belongs beside `coord`/`corners`,
which are already float presentation helpers (tagged `pending(wave-1)`). It
introduces **no new identity or save-format surface** — addressing math is
unchanged; this is a query.

## 8. The CLI "look" and the artifact

```
hornvale locale --world w.json [--at LAT,LON | --room ID] [--depth D]
```

- `--at LAT,LON` → `RoomAddr::containing` at the chosen depth (the friendly
  path). `--room ID` → `RoomId::unpack` (round-trips a printed id). Exactly one
  is required.
- `--depth D` default = **`globe_level + 6`** (six refinements below the
  canonical cell — a sub-cell patch on the order of a few km, always below the
  grid so inheritance is defined). Depth is validated against `MAX_DEPTH`.
- The command builds one `LocaleContext` (§3), then describes the addressed
  room. Default output is prose (std-only manual formatting, matching the other
  `cmd_*`): the coordinate, inherited biome, blended fields, texture aspect, and
  the exit list with bearings/enter-exit and destination room ids. `--json`
  emits the `locale/room/v1` schema instead — the machine contract, exactly as
  the CLI's cartographic commands render from `scene` JSON.

**Artifact:** a drift-checked `locale/room/v1` JSON render for **seed 42** at a
fixed room (address chosen at implementation, over land) written under
`book/src/reference/` (the schema-emitting home, beside `scene`'s), and
regenerated in CI's "Artifacts are current" step. All `f64` in the serialized
form pass through `hornvale_kernel::quantize` at the emit boundary, so the
artifact is cross-platform byte-stable.

## 9. Deliberate non-goals (deferred, with homes)

| Deferred | Home |
| --- | --- |
| P1 field-combinator algebra (`Layered`/`Masked`/`Blended`/`Conditioned`) | synthesis P1; built when a second consumer needs it |
| P3 weighted-cross-product grammar + real descriptor pools | MAP-29 / synthesis P3 |
| `ecology` / `society` domains (inhabitants, culture) | frontier PSY-6 / SOC-9 / UNI-20; cycle-03 |
| Palimpsest / place-history time axis | MAP-30 / synthesis P4 |
| Overlay exits (river/road/tunnel/portal) + passability | P2 §9 / the extensible-exit design; additive `ExitKind` |
| Adaptive-depth T-junction neighbours (`neighbors_adaptive`) | P2 deferred layer (spike-validated) |
| A "walk" (movement) REPL over exits | The Walk §4.1; clean fast-follow on this crate |

None of these are stubbed in v1 — each is a named, additive layer on the
interfaces v1 fixes.

## 10. Constitutional fit

- **Layering:** `kernel → domains → windows → cli`. `hornvale-locale` is a
  window depending on kernel + worldgen + climate + terrain; it presents
  domains, constructs no providers. Cross-domain reads go through worldgen, the
  composition root. Enforced by `cli/tests/architecture.rs`.
- **Determinism:** `Locale` is a pure function of `(seed, addr)`. Categorical
  inheritance and seeding are integer-only (platform-exact). Blended fields and
  the coordinate locator are float presentation; emitted floats are quantized.
- **No new crates / std-only:** compass bucketing, weighted draws, and
  point-in-spherical-triangle are hand-rolled. `serde` + `serde_json` only.
- **Save-format contracts:** the texture seed-label(s) and their consumption
  order join the frozen set via `streams`/`stream_labels()`. No change to the
  `RoomId` addressing contract.
- **No `HashMap`/`HashSet`, no wall-clock:** `BTreeMap`/`Vec` only; observation
  time is `WorldTime` (threaded, not read from a clock).
- **Derived view, versioned schema:** `Locale` is a UNI-20 derived view — cheap,
  re-derivable from the seed, never stored — emitted as the save-format-class
  schema `locale/room/v1` in `scene`'s manner (additive-in-version; changed
  meaning mints `v2`). The `LocaleContext` isolates the one expensive build so
  the view stays cheap per room.

## 11. Testing

- **Determinism:** `ctx.describe(addr, at)` byte-identical across repeated
  calls and across two independently-built contexts for the same world;
  `locale/room/v1` artifact drift-checked in CI.
- **Blend:** weights sum to `D`; blended field = weighted mean of corners;
  max-weight biome selection with lowest-`CellId` tie-break pinned.
- **Above-grid:** `depth < globe_level` → `LocaleError::AboveGrid`.
- **`containing` round-trip:** a room's own centroid re-locates to that room
  for interior rooms (exhaustive at a small depth on one face), with the
  boundary-straddle caveat documented, not asserted globally.
- **Exits:** exactly 3 lateral (+ vertical where present); bearings in
  `[0,360)`; lateral reciprocity where the substrate guarantees it; each `to`
  shares two corners with `addr` (the substrate's edge contract).
- **CLI:** `--at`/`--room`/`--depth` parse and error handling; a golden render
  test for the artifact seed/room.

## 12. Definition of Done

The full gate (`cargo test --workspace`, `cargo fmt --check`, `cargo clippy
--workspace --all-targets -D warnings`), the type-audit `check`, the artifact
drift-check green, and the campaign book duties: a chronicle entry
(`book/src/chronicle/`), a freshness sweep of stale chapters (the room-scale /
frontier chapters that name the locale window move from "planned" to
"shipped"; MAP-29's row notes the inheritance path is now real), a re-score of
any Confidence-Gradient bet this resolves, and a one-page retrospective in
`docs/retrospectives/`.

## 13. Implementation shape (the plan follows via writing-plans)

A TDD sequence, each step compiling and gated:
1. `RoomAddr::containing` in the kernel (+ round-trip tests) — the one new
   substrate surface, first because §8 depends on it.
2. The `hornvale-locale` crate skeleton, the `locale/room/v1` types
   (`Locale`/`LocaleFields`/`Exit`/`SubCellTexture`), the `LocaleContext`
   (build-once coarse world), and `describe` inheritance + blend (§4).
3. Sub-cell texture (§5) + the `streams` seed-labels.
4. Exits (§6) — lateral compass + `Enter`/`Exit`.
5. The `hornvale locale` CLI subcommand (§8), `--json` schema emit + prose, and
   the drift-checked `locale/room/v1` artifact.
6. Campaign DoD: book chronicle, freshness sweep, registry/CI wiring,
   retrospective.
