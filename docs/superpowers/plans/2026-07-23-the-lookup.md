# The Lookup — implementation plan

> **For agentic workers:** the two code changes already exist as a validated,
> byte-identical prototype in this worktree (spike this session). Execution
> **formalizes** them into clean task-commits with regression tests and
> independent review — it does NOT re-derive them (re-derivation risks a
> byte-diverging variant; the prototype is the correct implementation).

**Goal:** two determinism-safe genesis-perf fixes (cache `Geosphere::coord`;
`ConnectionGraph` `BTreeMap`→`Vec`), landed with regression tests, a bounded
sibling grep, a convention note, and full census-drift verification.

**Architecture:** replace "value keyed by a dense `0..N` index, re-derived per
access" with precomputed flat `Vec` storage, per the `CellMap`/`ComponentStore`
convention.

**Tech stack:** Rust, std-only. No new workspace dependency.

## Global Constraints (verbatim from the spec)

- **No epoch:** no new stream draws, no changed seed labels
  (`kernel/src/streams.rs`), no changed hash/noise constants
  (`seed.rs`/`noise.rs`), no changed serialized bytes.
- **Byte-identity is the whole point:** every fix must be byte-identical to
  baseline. `lens_purity`, `graph_byte_identity`, topology tests, and a
  ≥40-seed world-hash sweep must stay green; the census-drift check across
  1000 seeds is the merge gate (Nathan's out-of-band regen).
- No `HashMap`/`HashSet`; `BTreeMap`/`BTreeSet`/`Vec` only. No wall-clock.
- `#![warn(missing_docs)]`; type-audit tags on pub-boundary primitives;
  `cargo fmt`; `clippy -D warnings`.

---

### Task 1: Cache `Geosphere::coord` per cell

**Files:** Modify `kernel/src/geosphere.rs` (add `coords: Vec<GeoCoord>` field;
populate in `new()`; `coord()` returns the cached value). Test:
`kernel/src/geosphere.rs` `#[cfg(test)]`.

**Interfaces:** `coord(&self, CellId) -> GeoCoord` unchanged (public signature
and return value both preserved).

- [ ] **Step 1 — regression test (red without the cache):** assert, for every
  cell across levels 0..=5, that `geo.coord(id)` bit-equals a freshly recomputed
  `GeoCoord { latitude: math::asin(z).to_degrees(), longitude:
  math::atan2(y,x).to_degrees() }` from `geo.position(id)` — compared via
  `f64::to_bits` on both fields. This pins the cache to the exact expression.
- [ ] **Step 2 — the change** (already prototyped): `coords` field, populated in
  `new()` by mapping `positions` through the identical expression; `coord()`
  returns `self.coords[id.0 as usize]`.
- [ ] **Step 3 — verify:** `cargo test -p hornvale-kernel` (geosphere tests +
  new test) green; `cargo test -p hornvale --test lens_purity` green.
- [ ] **Step 4 — commit:** `perf(kernel): cache Geosphere::coord per cell (The
  Lookup)`.

### Task 2: `ConnectionGraph` adjacency `BTreeMap`→`Vec<Vec<Edge>>`

**Files:** Modify `domains/topology/src/graph.rs`. Tests: existing
`domains/topology/tests/graph.rs` +
`windows/worldgen/tests/graph_byte_identity.rs` (both already exercise
`nodes`/`edges`/`reachable_regions`).

**Interfaces:** `new`, `add_edge`, `edges`, `nodes`, `reachable_regions` — all
signatures and observable output preserved (ascending-`CellId` order).

- [ ] **Step 1 — confirm coverage:** the existing topology tests +
  `graph_byte_identity` assert node set, per-node edge lists, and component
  structure. Add one test asserting `nodes()` yields `CellId(0..n)` ascending
  and `edges()` on an out-of-range id returns `&[]` (the preserved defensive
  behavior). Run: expect green on the prototype.
- [ ] **Step 2 — the change** (already prototyped): `adjacency: Vec<Vec<Edge>>`;
  `new` = `vec![Vec::new(); node_count]`; `add_edge` indexes directly; `edges`
  uses `.get(idx).map(as_slice).unwrap_or(&[])`; `nodes` = `(0..len).map(CellId)`;
  `reachable_regions` uses `self.nodes()`. Drop the now-unused `BTreeMap` import.
  Update the struct doc to name the `CellMap` convention.
- [ ] **Step 3 — verify:** `cargo test -p hornvale-topology`,
  `cargo test -p hornvale-worldgen --test graph_byte_identity`, `lens_purity` —
  all green.
- [ ] **Step 4 — commit:** `perf(topology): ConnectionGraph adjacency as a
  dense Vec, not a BTreeMap (The Lookup)`.

### Task 3: Bounded sibling grep

**Files:** none necessarily; findings → `.superpowers/sdd/followups.md` or a
fix commit if a find is provably byte-safe.

- [ ] **Step 1:** grep the workspace for the dense-index shape — `BTreeMap<CellId`,
  `BTreeMap<.*, ` keyed by a dense `0..N` index, and pure per-cell functions
  recomputed per call (e.g. other `asin`/`atan2`-of-position conversions). List
  candidates.
- [ ] **Step 2:** for each candidate, decide: (a) provably byte-safe dense-index
  fix → apply + test + fold into a commit; (b) sparse-key / loop-recompute /
  not-hot → record in `followups.md` with the reason it's excluded.
- [ ] **Step 3 — commit** (only if a fix was applied): `perf(...): <site> dense
  Vec (The Lookup sibling)`. Otherwise record findings; no commit.

### Task 4: Convention note + profiling infra

**Files:** `kernel/CLAUDE.md` (or a short decision record) for the convention;
`Cargo.toml` (`[profile.profiling]` keep/drop decision).

- [ ] **Step 1:** add a one-paragraph convention note where the `CellMap`
  guidance lives (kernel `CLAUDE.md`): a collection keyed by a dense `0..N`
  `CellId`/index uses a `Vec`, not a `BTreeMap` — the map buys nothing over a
  dense key and costs a tree traversal + per-key allocation. Cite `CellMap`.
- [ ] **Step 2:** decide the `[profile.profiling]` Cargo.toml addition — keep
  (useful, non-invasive profiling infra) or drop. Record the call in the ledger.
- [ ] **Step 3 — commit:** `docs(the-lookup): dense-index-uses-Vec convention +
  profiling profile`.

### Task 5: Close

**Files:** `book/src/chronicle/the-lookup.md` (+ SUMMARY);
`docs/retrospectives/the-lookup.md`; spec/plan completion notes.

- [ ] **Step 1:** chronicle (book altitude — the one-shape insight, the measured
  wins, byte-identity discipline) + SUMMARY entry.
- [ ] **Step 2:** retrospective (process: spike-then-formalize; the stale-checkout
  profiling trap; formalize-not-re-derive for validated byte-critical code;
  ideonomy's periodic-grid confirming scope). Promote followups.
- [ ] **Step 3 — verify on the merged tree:** absorb origin/main; full `make
  gate`; artifact-drift (`connections-seed-42.md` etc. regenerate byte-clean);
  the census-drift check across 1000 seeds is Nathan's out-of-band carve-out.
- [ ] **Step 4:** G6 hard-stop package.

## Self-Review

**Spec coverage:** §"two optimizations" → T1/T2; §"determinism verification" →
T1/T2 tests + T5 gate; §scope bounded grep → T3; §scope convention note + infra
→ T4; §DoD → T5. Complete.

**Placeholder scan:** the code steps reference the already-prototyped diffs (in
this worktree) rather than repeating them — deliberate, since re-typing a
byte-critical diff from prose is the exact re-derivation risk this plan
forbids. Every test step names its assertion and command.

**Type consistency:** `coords`/`GeoCoord`/`coord`, `adjacency`/`Vec<Vec<Edge>>`/
`nodes`/`edges`/`reachable_regions` — names match the spec and the prototype.
