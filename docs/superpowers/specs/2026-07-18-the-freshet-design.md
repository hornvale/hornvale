# The Freshet — the salt/fresh water distinction (DOM-5 first slice)

**Campaign:** The Freshet (fresh-water classification)
**Registry:** DOM-5 (hydrology exposure — first slice) · SEQ-2 (rivers as
infrastructure) · B/C → CLIM-endorheic + named-water-entities (deferred)
**Status:** G3 draft (awaiting Nathan)

---

## 1. Summary

The world computes rivers and interior basins but never says which water is
*drinkable*. Terrain's drainage substrate already produces, per cell, the
upstream flow accumulation (`drainage`) and an endorheic mask (`endorheic` —
land that never drains to the sea) — it uses them to carve waterfalls and
condense settlements. But no consumer can ask "is the water here salt or fresh?",
so "where's water?" has meant "below sea level" = *the salt ocean*. For any
living thing that distinction is existential, and its absence is a first-order
failure (it is what stranded The Surmise's thirsty agents beside an unreachable
sea).

The Freshet surfaces the distinction as a **pure derived classification** over
the fields terrain already computes — no new physics, no new stored state, no
epoch. A `WaterKind` per cell (ocean / salt basin / river / dry land), with a
`is_fresh` (drinkable) query, exposed to room-scale consumers through
`LocaleFields`. It realizes the first slice of DOM-5 (the hydrology exposure)
and unblocks The Surmise, whose `is_water` becomes `is_fresh_water`.

This follows **The Ground (MAP-39)** exactly: a no-epoch classification layer
over globe fields the tectonic pipeline already derives — zero stream draws,
zero moved bytes.

## 2. Background: the substrate already exists

`domains/terrain/src/globe.rs` (`GlobeSummary`) exposes, all *recomputed at
genesis, never serialized*:

- `elevation: CellMap<ReferenceElevation>` and `sea_level: ReferenceElevation`;
- `drainage: CellMap<f64>` — upstream land-cell accumulation (0 on ocean);
- `endorheic: CellMap<bool>` — land whose downhill path never reaches the sea
  (`domains/terrain/src/drainage.rs`, "the banked salt-basin hook").

Precedent threshold: `carve.rs::WATERFALL_MIN_DRAINAGE = 80.0` (the accumulation
above which a watercourse is significant). Consumers read terrain through the
locale surface: `LocaleFields { temperature_c, moisture, elevation_m }`
(`windows/locale/src/lib.rs`), continuous fields blended by integer-weighted mean,
**discrete** fields (biome, substrate, regime) taken from the max-weight cell
(the canonical-grid rule, MAP-30 / The Locale Window — never blend-then-classify).

## 3. The design

### 3.1 The classification (terrain domain)

A pure per-cell `classify` over existing bits, **materialized once at genesis**
into a `GlobeSummary.water_kind: CellMap<WaterKind>` (recomputed at genesis, never
serialized — exactly like `drainage`/`endorheic`), so consumers read a map and no
one threads the geosphere at query time:

```
pub enum WaterKind { Ocean, SaltBasin, River, DryLand }

// pure + unit-testable on synthetic inputs (no globe needed):
fn classify(elevation: f64, sea_level: f64, drainage: f64,
            endorheic: bool, is_terminal_sink: bool) -> WaterKind {
    if elevation < sea_level                       -> Ocean       // salt
    else if endorheic && is_terminal_sink          -> SaltBasin   // salt
    else if drainage >= RIVER_MIN_DRAINAGE         -> River       // FRESH
    else                                           -> DryLand
}

impl WaterKind { pub fn is_fresh(self) -> bool { matches!(self, WaterKind::River) } }

// built where drainage/endorheic already are (geo + downhill in hand):
pub fn water_field(geo, elevation, sea_level, drainage, endorheic) -> CellMap<WaterKind>
```

`is_terminal_sink(cell)` is computed in the field builder, where the downhill
graph is available: a cell is a terminal sink iff `endorheic[cell]` **and** it is
a local elevation minimum (`downhill_targets` gives `None`, or no strictly-lower
neighbour — `total_cmp`). The genesis assembly (`globe.rs`) calls `water_field`
right after it finalizes `drainage`/`endorheic` and stores the result on
`GlobeSummary`.

- **Ocean** — below sea level; salt.
- **SaltBasin** — a *terminal* endorheic sink: `endorheic[cell]` **and** a local
  elevation minimum (no strictly-lower neighbour). The evaporative salt lake /
  playa (the Dead Sea, the Great Salt Lake). Salt.
- **River** — accumulated runoff at or above `RIVER_MIN_DRAINAGE` that is not a
  salt sink. **Fresh** — this deliberately includes *endorheic feeder rivers*
  (fresh on their way to a terminal lake — the Jordan) and *freshwater
  through-flow lakes* (high-drainage, exorheic). Potability, not geometry, is the
  axis here.
- **DryLand** — everything else.

`is_terminal_sink` = `endorheic[cell] && geo.neighbors(cell).all(|n|
elevation(n) >= elevation(cell))` — deterministic, `total_cmp`. Correctness
priority: an endorheic *river* must read fresh; only the terminal evaporative
sink reads salt (the exact salt/fresh confusion this campaign kills).

### 3.2 The exposure (locale seam)

`LocaleFields` gains a **discrete** field `water: WaterKind`, taken from the
max-weight cell (as biome/substrate/regime are — never blended):
`water: *globe.water_kind.get(best.0)`. `WaterKind` is re-exported from terrain
through the window layer so consumers name one type. This is the seam The Surmise
reads (`LocaleFields.water.is_fresh()` replaces its sea-level `is_water`), and
the same query life and settlement will use.

### 3.3 Optional surfacing (include if it fits, no regen)

Like The Ground's lithology map + almanac section: a **water map lens** (ocean /
salt basin / river / dry, a PPM like the elevation map) and an **almanac "The
Waters" line** (fresh-water land fraction for a seed) — both drift-checked
artifacts, **no census**. Included if cheap; not load-bearing. Census *metrics*
(fresh-water fractions across 1000 seeds) are **deferred** — they need an AWS
regen (carve-out).

## 4. Architecture & files

Terrain owns the classification (a pure projection, like MAP-39 lithology);
locale exposes it. No new crate (DOM-5's hydrology-crate split waits until
hydrology becomes people-infrastructure — B/SEQ-2).

- **Create** `domains/terrain/src/water.rs` — `WaterKind`, `classify` (pure),
  `water_field` (builds the `CellMap<WaterKind>`), `RIVER_MIN_DRAINAGE`; unit
  tests on synthetic inputs/globes.
- **Modify** `domains/terrain/src/globe.rs` — `GlobeSummary.water_kind:
  CellMap<WaterKind>`, populated right after `drainage`/`endorheic` finalize
  (doc: recomputed at genesis, never serialized).
- **Modify** `domains/terrain/src/lib.rs` — `pub mod water; pub use`.
- **Modify** `windows/locale/src/lib.rs` — `LocaleFields.water`, re-export
  `WaterKind`, read `globe.water_kind` at `best.0`.
- **Optional** a `water` map subcommand + almanac line (CLI/almanac window).
- Domains depend only on the kernel (terrain already does); locale already
  depends on terrain. No new workspace dependency.

## 5. Testing & acceptance

The classification is the deliverable; the tests prove it is **correct and
mechanical** (derived, never authored — the anti-lookup-table discipline, 0021):

1. **Ocean is salt** — a below-sea-level cell → `Ocean`, `!is_fresh`.
2. **A high-drainage exorheic cell is a fresh River** — `drainage >= threshold`,
   reaches the sea → `River`, `is_fresh`.
3. **An endorheic feeder river is still FRESH** (the correctness keystone) — a
   high-drainage cell that is `endorheic` but **not** a local minimum → `River`,
   `is_fresh`. (Mutation: "all endorheic = salt" fails this.)
4. **The terminal endorheic sink is a SaltBasin** — an `endorheic` local minimum
   → `SaltBasin`, `!is_fresh`. (Mutation: dropping the `is_terminal_sink` guard
   marks the salt lake fresh — fails.)
5. **Dry land** — low-drainage exorheic land → `DryLand`.
6. **Determinism** — `water_kind` is pure/total, `total_cmp` throughout, no
   `HashMap`, no RNG; same globe+cell → same kind, reload-stable (globe re-derives
   from seed).
7. **Locale exposure** — `LocaleFields.water` equals `water_kind` at the room's
   max-weight cell; a real seed-42 world classifies a plausible fresh-water
   fraction (a sanity assert, not a pinned magic number).
8. **Genesis byte-identity** — the committed ledger/almanac/census artifacts that
   do *not* render water are byte-identical (no new fact, no seed draw); only the
   locale/room artifacts that now carry `water` re-baseline.

Synthetic globes (planted elevation/drainage/endorheic) for 1–6, exactly as
`drainage`/`carve` tests build them — no reliance on a particular seed's geography.

## 6. Determinism & save-format (leads the G3 review)

**Fresh water at zero epoch cost.** No new committed ledger fact, no seed draw,
no epoch — `water_kind` is a pure projection over globe fields that are
themselves recomputed-at-genesis and never serialized (the MAP-39 shape). The
one drift: `LocaleFields` gains a discrete `water` field, so committed artifacts
that render LocaleFields **re-baseline** (drift-check, not a save-format event) —
re-pinned in this campaign's commits, never deferred to close. **No census
regen** this slice (no census metric reads water; adding one is a deferred
follow-on needing AWS authorization). All ordering `total_cmp`; the classification
is total.

## 7. Non-goals (deferred — the follow-ons)

- **B — named river/lake entities + settlement water-access facts** (genesis
  change → census + epoch). The people-infrastructure layer (SEQ-2): rivers as
  toponymy/trade/borders, settlements sited by fresh water.
- **C — closed-basin lake physics** (CLIM-endorheic): precipitation-routed
  lake-filling (real freshwater lakes vs salt playas by aridity), flat multi-cell
  salt lakes, brackish estuaries, lake-effect climate feedback.
- **The `hydrology` crate split** (DOM-5 full) — waits until hydrology is
  people-infrastructure, not a land projection.
- **The scene/render river layer** (rivers drawn on the Orrery globe) — a scene
  contract addition (world-wasm version bump, cross-repo); its own follow-on.
- **Census metrics** for water (need an AWS regen).
- **The Surmise's rewiring** — this campaign only *exposes* the truth; The
  Surmise consumes it in its own (parked) T5.

## 8. Judgment calls

- **`RIVER_MIN_DRAINAGE`** — the accumulation above which fresh flowing water
  exists. Tuned so seed-42 settlements have fresh water within a short walk (the
  reachability The Surmise needs); starting reference is the existing
  `WATERFALL_MIN_DRAINAGE = 80.0` (rivers are more common than waterfalls, so a
  lower value is likely). An authored constant with a documented basis.
- **`is_terminal_sink`** — a single-cell local minimum. Flat multi-cell salt
  lakes are a C refinement.
- **`WaterKind` variant set** — four classes; freshwater-lake vs river geometry
  and brackish estuaries are deferred (potability is captured; geometry is not).

## 9. Task shape (for the plan)

Roughly three TDD tasks: (T1) `water.rs` — `WaterKind` + pure `classify` +
`water_field` + `RIVER_MIN_DRAINAGE`, the six classification/determinism tests on
synthetic inputs (incl. the endorheic-feeder-is-fresh and terminal-sink-is-salt
keystones), and the `GlobeSummary.water_kind` field wired in `globe.rs`; (T2)
`LocaleFields.water` exposure + the seed-42 sanity + genesis byte-identity pins +
artifact re-baselines; (T3) optional water map lens + almanac "The Waters" line +
book chronicle + close (registry: flip DOM-5 to first-slice-shipped, advance
SEQ-2).
