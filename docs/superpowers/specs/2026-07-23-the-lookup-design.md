# The Lookup — design

**Working name** (final blessed at G6). A determinism-safe genesis-performance
campaign: replace two "re-derive on every access, keyed by a dense index"
hotspots with precomputed flat storage, aligning them with the project's
existing `CellMap`/`ComponentStore` dense-storage convention.

## Goal

Cut world-genesis CPU without moving a single serialized byte. A samply
flamegraph of `hornvale new --seed 42` on current `origin/main` found two
hotspots that both reduce to one shape — *a value determined by a dense,
immutable `0..N` index is re-derived on every access* — and both are
byte-identical to fix. Prototyped and measured in a spike this session:
genesis median **998 ms → 622 ms (~38%)**, seed-42 world byte-identical.

## Background — the measurement

Method: `samply` (0.13.1), a non-invasive `[profile.profiling]` (inherits
`release` + debug symbols; separate `target/profiling/`), symbolicated with
`atos`. Genesis self-time on current `origin/main`, before:

- `hornvale_topology::graph::ConnectionGraph::add_edge` — **21.9%**
- `libm::math` (transcendentals) — 22.6% aggregate, of which
  `Geosphere::coord`'s `asin`+`atan2` were a distinct, avoidable slice.

Both are pure functions of a dense `CellId` (`0..cell_count`) index.

## The two optimizations

### 1. Cache `Geosphere::coord` per cell

`Geosphere` stores `positions: Vec<[f64;3]>` and `coord(id)` recomputes
`GeoCoord { latitude: asin(z).to_degrees(), longitude: atan2(y,x).to_degrees() }`
on **every call**. Precompute a `coords: Vec<GeoCoord>` once in
`Geosphere::new()` (the sole constructor; `Geosphere` is never serialized) and
return the cached value.

**Byte-safety (by construction):** the cache is populated by the *identical
expression* on the *identical stored position*, computed once. `coord(id)`
therefore returns a bit-for-bit unchanged `GeoCoord` for every id.

**Measured:** genesis 998 → 907 ms median; `libm` self-time 22.6% → 15.7%.

### 2. `ConnectionGraph` adjacency: `BTreeMap<CellId, Vec<Edge>>` → `Vec<Vec<Edge>>`

`ConnectionGraph::new(node_count)` inserts `CellId(0..node_count)` — a **dense,
complete** key set — into a `BTreeMap`, then `add_edge` does two O(log N)
`entry()` traversals per edge. A `Vec<Vec<Edge>>` indexed by `CellId.0` is O(1),
allocation-free per node, and iterates in ascending-`CellId` order — exactly
the order a `BTreeMap<CellId, _>` gave.

**Byte-safety:** `ConnectionGraph` derives only `Clone, Debug` — it is never
serialized and appears in no committed artifact. Its three observable methods
keep identical output: `edges(from)` (same slice), `nodes()` (ascending
`CellId`, now Vec-index order), `reachable_regions()` (`BTreeSet` components,
sorted by min `CellId` — unchanged). This aligns `ConnectionGraph` with the
`CellMap`/`ComponentStore` (UNI-22) dense-`Vec`-storage convention the project
already uses everywhere else.

**Measured:** genesis 907 → 622 ms median; `add_edge` self-time **21.9% →
2.2%**.

## Determinism verification (the campaign's spine)

Neither change is an **epoch**: no new stream draws, no changed seed labels, no
changed serialized bytes (the kernel `streams.rs` / `noise.rs` / `seed.rs`
constants are untouched; `ConnectionGraph` is not serialized). Verification,
in ascending strength:

1. `graph_byte_identity` (exists) + topology unit tests + a new kernel unit
   test asserting cached `coord(id)` bit-equals the recomputed expression for
   every cell across several levels.
2. `lens_purity` — seed-42 world matches the committed fixture byte-for-byte.
3. A multi-seed world-hash sweep (≥40 seeds, cached vs baseline) — **all
   identical** in the spike.
4. **The merge gate: full census-drift across 1000 seeds** (`make gate-full`
   + the census regen). Single-seed identity is necessary, not sufficient, for
   a kernel change (kernel `CLAUDE.md`); the census is the real net. The census
   regen is Nathan's out-of-band carve-out.

## Scope

**In:** the two fixes above; a **bounded** grep for sibling instances of the
dense-index shape (other `BTreeMap`/`BTreeSet` keyed by a dense `CellId`/`0..N`
index; other pure per-cell functions recomputed per call) — include a found
instance **only** if it is provably byte-safe by the same argument; a short
convention note (dense-index storage uses `Vec`, per `CellMap`).

**Out (with reasons):**
- Sparse-key maps — `subdivide`'s midpoint `BTreeMap<(u32,u32),_>` genuinely
  needs a map (the key is an edge pair, not dense). Excluded.
- Loop recomputes — `annual_mean_insolation`'s per-(cell,day) solar trig is a
  loop-hoist lever, not an index cache. Followup.
- Allocation churn / the `.collect()` pattern (`[malloc]` leads the post-fix
  profile). Different lever. Followup.
- Removing the `asin` band-select in `NearestCellIndex::nearest_to_position` —
  **rejected**: a one-ULP band-boundary shift would change which cells are
  searched → different world bytes. Its cost was the dot loop, not the asin.

## Non-goals

No new dependency (samply is a dev tool installed like nextest, not a workspace
dep; the `[profile.profiling]` addition is optional campaign infra). No change
to any physics formula, draw, or serialized format. No cross-platform claim
beyond what quantization already provides.

## Decisions (promoted from the ledger)

- **Scope is the two proven fixes + a bounded byte-safe sibling grep + a
  convention note**, not an open-ended sweep (ledger #1).
- **`asin`-removal in `nearest_to_position` is rejected** as
  determinism-unsafe (ledger #2).
- **Neither fix is an epoch**; the census-drift check is the merge gate.

## Definition of Done

Standard campaign DoD: the two fixes landed with regression tests; the bounded
sibling grep run and its findings recorded (fixed-if-safe, else followup); the
convention note; full `make gate` + census-drift verification on the merged
tree; chronicle + retrospective; the `[profile.profiling]` infra decision
(keep or drop) recorded; followups register promoted into the retro.
