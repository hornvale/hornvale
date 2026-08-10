# The Commons — design

**Working name** (blessed at G6). A determinism-safe genesis-performance
campaign, follow-on to The Lookup: the geosphere — the shared spatial substrate
every domain computes over — is cached per level but handed to each provider as
a **deep clone**. Share it by `Arc<Geosphere>` instead, at the terrain ownership
point.

## Goal

Finish the geosphere cache's own job. `geosphere_for(level)` already builds each
level's mesh **once per process** (a `OnceLock<Mutex<BTreeMap<u32, Geosphere>>>`),
but returns `.clone()` — so every world build deep-copies the whole mesh
(`positions` + `coords` + `neighbors: Vec<Vec<CellId>>`, the last being N inner
Vec allocations). Return `Arc<Geosphere>` and share the one cached instance.
Byte-identical (sharing immutable data changes no value); measured ~3% off the
census, and it trims allocator lock contention that compounds under census
parallelism.

## Background — the measurement

samply on current `origin/main` (post-Lookup). The census path (40-world,
all-metric sweep, 7 threads) spends ~25% in allocation + allocator-contention
syscalls; within it, `Vec::clone` (~2.2%) and `drop_in_place<Geosphere>`
(~2.0%) are the geosphere clone + teardown. Prototyped `Arc<Geosphere>` on the
terrain ownership point:

- seed-42 world **byte-identical** (sha unchanged).
- census sweep (40 worlds, all metrics), min of 3: **11.07 s → 10.71 s (~3%)**.
- genesis single-build: ~0 (one world clones the mesh ~once; the win is
  per-world and *accumulates* across the census).

## The change

`Geosphere` is immutable after construction and **seed-independent** (the same
mesh for every world at a level) — the ideal shared immutable.

- `worldgen::geosphere_for(level) -> Arc<Geosphere>`: cache stores
  `Arc<Geosphere>`, returns a cheap `Arc` clone (refcount bump) instead of a
  deep `Geosphere` clone.
- `GeneratedTerrain`: field `geosphere: Arc<Geosphere>`; `new` takes
  `Arc<Geosphere>`; `geosphere() -> &Geosphere` unchanged (derefs the Arc via
  `as_ref()`), so **every consumer that borrows `&Geosphere` is untouched**.
- The two non-test `geosphere_for` callers (worldgen) pass the `Arc` straight
  into `GeneratedTerrain::new`; `hornvale_terrain::generate(seed, &geo, …)`
  borrows through the Arc unchanged (deref coercion).

`Arc`, not `Arc<Mutex>`: the mesh never mutates after `new`, so there is no
aliasing hazard. `Arc`, not `Rc`: the census is multi-threaded (`Send` needed).

## Byte-safety

Sharing vs copying an immutable changes no value or order. Verified in the
spike: seed-42 sha unchanged; the census sweep produced identical results.
Determinism gates for the campaign: `lens_purity`, `graph_byte_identity`, the
terrain property batteries, a ≥40-seed world-hash sweep, and — the merge gate —
the 1000-seed census drift (Nathan's out-of-band regen). Not an epoch: no draw,
no seed label, no serialized-byte change (`Geosphere` is never serialized).

## Scope

**In:** the terrain-side `Arc<Geosphere>` (cache + `GeneratedTerrain`) and the
~9 `#[cfg(test)]` `GeneratedTerrain::new` sites updated to pass `Arc::new(geo)`.

**Out (with reasons):**
- **The climate provider's clone is a deliberate NON-goal of this campaign, and
  it is the larger second half — flagged for G3.** `GeneratedClimate` also owns
  a `Geosphere`, cloned at `climate/provider.rs:213`, hot per-world. Sharing
  terrain's `Arc` into it requires `ClimateInputs.geosphere: &'a Geosphere` →
  `Arc<Geosphere>` *and* threading the Arc through the era/history climate-build
  path (`worldgen/lib.rs:1513`), which today holds only a `&Geosphere`. That is
  a separate redesign; captured as a followup expected to roughly double this
  win. (This scope line corrects an ideonomy conclusion — "single owner" — that
  the verify-claims check falsified before it reached the spec; see ledger #2.)
- Seed-*dependent* structures (globes, `CellMap`s) are per-world and correctly
  owned — not the shared-immutable pattern.
- Diffuse allocation (`finish_grow` Vec growth, scattered collects) — a
  different, non-surgical lever; followup.

## Non-goals

No new dependency. No physics/format/draw change. No cross-platform claim
beyond quantization. The `[profile.profiling]` infra already landed with The
Lookup.

## Decisions (promoted from the ledger)

- **Terrain-side `Arc<Geosphere>`**, sharing the per-level cached mesh instead
  of cloning it (ledger #1).
- **The climate clone is deferred** — a real second instance blocked by the
  `ClimateInputs` borrow-struct + era-path threading; followup, not this
  campaign (ledger #2, a verify-caught scope correction).
- **Not an epoch**; the 1000-seed census drift is the merge gate.

## Definition of Done

The terrain-side Arc landed with the test sites updated; `Arc<Geosphere>`/
`Arc<Mutex>`-not-needed rationale documented; full `make gate` + census-drift
verification on the merged tree; chronicle + retrospective; followups
(climate half; diffuse allocation) promoted into the retro.
