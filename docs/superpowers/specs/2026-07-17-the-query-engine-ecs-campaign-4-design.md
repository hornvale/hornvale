# The Query Engine (ECS Campaign 4) — Design

> **STATUS: SHIPPED** — *The Concordance*, merged to main 2026-07-17 (byte-identical; full gate green). Chronicle: `book/src/chronicle/the-concordance.md`.

Program: **The Entity-Component Substrate** (metaplan
`docs/superpowers/specs/2026-07-14-ecs-program-metaplan-design.md`, campaign 4;
registry **UNI-22**). Base: local main `@2eb570f` (campaigns 1–3 shipped: The
Menagerie, The True Name, The Dissolution). Chronicle-name candidate: **The
Concordance** (a concordance is an index of every occurrence and where it
appears — precisely the permutation indexes); confirmed at close, per the
one-evocative-noun convention.

## 1. What this is

The Fact ledger is a flat `Vec<Fact>` with no index (`kernel/src/ledger.rs`).
So `commit` is O(n) — it runs a full-fact idempotency scan
(`facts.contains(&fact)`) **and** a functional-contradiction scan — which makes
building a world of n facts O(n²), and `facts_about`/`find` are O(n) linear
filters (`find` also allocates a `String` per call). This is the metaplan §12
**commit-index dependency**: the sequencing constraint that campaign 4 must land
the permutation indexes **before any census-scale work**, or the census slows
super-linearly.

This campaign reinstates the proven forms — the triple store's permutation
indexes and the compiler's symbol table — over the ledger, entirely as
**derived views**. The world is unchanged: same seed + same pins → the same
byte-identical worlds, almanacs, and artifacts. **The perf gain is the entire
deliverable**, and byte-identity is the proof it was behaviour-preserving.

Target complexity after this campaign:

```
operation                     | today          | after
------------------------------+----------------+---------------------------
commit (dedup + contradiction)| O(n) => O(n^2) | O(log n + k) => build O(n log n)
                              |   per world    |
facts_about(subject)          | O(n)           | O(log n + k)
find(predicate)               | O(n) + a String| O(log n + k), no alloc
query by object               | (none)         | O(log n + k)
kinds_with(component)          | O(k)          | O(k) (reflection, formalized)
```

## 2. Scope

**In (the metaplan §5 campaign-4 row, exactly):**

1. Three permutation indexes over the ledger — **SPO / PSO / OSP**, the rotations
   of (subject, predicate, object) — three ordered `BTreeMap`s, each answering
   one two-known-one-unknown query in O(log n).
2. **Predicate interning** (`String` → `Symbol(u32)`) — the query-speed half:
   Symbols key the indexes for compact keys and O(1) comparison.
3. The **commit fix** — both O(n) scans in `commit` route through the index (the
   actual §12 blocker).
4. **`kinds_with(component)` / the reflection query** — the kind-side backend
   (UNI-21's capability query; the GOAP action-set read), over the campaign-3
   component registries. A separate, small deliverable from the fact index.
5. The **benchmark slice** (§7): the INDEX≡SCAN property, deterministic budgets
   as lab studies / const-asserts, wall-time micros in the heavy tier.

**Out (deferred; captured in `followups.md` with measured triggers):**

- The **Fact-shrink** half of interning (`Fact.predicate: String → Symbol`) —
  it fights serde-with-context on the determinism-critical struct and is a
  measured *space* budget, not load-bearing. c4 ships the query-speed half only.
- **`NonZeroU64` EntityId** niche (`Option<EntityId>` 16 → 8 bytes) → campaign 5,
  which already re-touches `EntityId` for instance-keying. Orthogonal to queries;
  breaks every `EntityId(n)` literal workspace-wide.
- **Phantom `Id<T>`** ids — already declined (decision-ledger #53): `KindId` and
  `EntityId` are distinct newtypes, so cross-indexing is already a compile error.
- **AoS → SoA** — already delivered by The Dissolution (per-component registries;
  fauna already carry no inert `PeopledTraits`).
- The **Phenomenon** ranked-join and **Field** query backends (metaplan §4.4) →
  campaign 5+. c4 is the **Fact backend only**.
- **Place / day / provenance** indexing — the indexes key on (S, P, O) only;
  place/day filters post-filter the retrieved postings O(k). A spatial/temporal
  index is the Field / chunk-ledger backend (campaign 7), out of scope.

## 3. Determinism contract — the save-format line (leads any G3)

**The serialized ledger does not change, so the campaign is byte-identical.**

- `Ledger`'s only serialized fields stay `facts: Vec<Fact>` and
  `next_entity: u64`. The three permutation indexes and the intern table are new
  `#[serde(skip)]` derived fields — omitted from JSON entirely, `Default` (empty)
  after deserialize.
- `Fact` is **unchanged**: `predicate` stays a `String` on disk and in memory
  this campaign; objects stay `Value`. `EntityId` stays `u64` on disk.
- Therefore every committed artifact — worlds, the three seed-42 almanacs, the
  elevation map, the census, the lab studies — is **byte-identical**, and the
  existing drift-check *is* the proof (the master oracle, metaplan §6). A
  behaviour-preserving refactor passes it unchanged.
- Indexes are pure functions of the ledger prefix (store-irreversible-derive-
  reversible, decision 0037), so they can never corrupt a world.

The one derived-order subtlety, made a contract: **index-backed
`facts_about`/`find` preserve commit order.** Postings are stored as ascending
fact-*positions* (indices into `facts`), so iterating an index yields facts in
commit order exactly as today — the byte-identity of any artifact that renders
these results is untouched. (Position order is itself a total deterministic
order; the metaplan §3.5 "(subject, predicate, object) total-key order" rule is
for future *parallel* query consumers and is deferred to whichever campaign 5/6
consumer first materializes one — it sorts postings then.)

Symbol integers **never serialize**, so their values are byte-irrelevant; the
interner may be registry-ordered or first-seen-ordered without any determinism
consequence. INDEX≡SCAN does not depend on the mapping's numeric choice.

## 4. Architecture

### 4.1 Extend `Ledger` in place

The query engine **extends `Ledger`** (metaplan §4.7), it does not wrap it. The
existing method signatures — `commit`, `check`, `facts_about`, `find`,
`value_of`, `text_of`, `iter` — are preserved, so the **303 call-sites across 62
files change by zero** and byte-identity is trivial. A wrapping
`IndexedLedger`/`QueryEngine` is rejected: `commit`'s contradiction check lives
*inside* `Ledger`, so the index must live there too; a wrapper would either
duplicate the whole API or force a 303-site migration.

```
Ledger {
    facts: Vec<Fact>,                 // serialized (unchanged)
    next_entity: u64,                 // serialized (unchanged)
    #[serde(skip)]
    index: Option<FactIndex>,         // derived; absent-or-complete, never partial
}

struct FactIndex {
    interner: Interner,               // String <-> Symbol(u32), build-state
    spo: BTreeMap<SpoKey, Postings>,  // subject, predicate  -> positions
    pso: BTreeMap<PsoKey, Postings>,  // predicate           -> positions
    osp: BTreeMap<OspKey, Postings>,  // object, subject     -> positions
}
type Postings = Vec<usize>;           // ascending fact positions == commit order
```

The exact key granularity (how much of the triple is in the key vs. read back
from `facts[pos]`) is an implementation detail for the plan; the contract is the
three query shapes and O(log n) lookups. The object participates in the OSP key,
so `Value` needs a **total order** — derived via `total_cmp` for the `Number(f64)`
variant (consistent with the workspace float-sort rule; byte-irrelevant since
the index is not serialized, but must be deterministic; objects are already
quantized at commit, so equal quantized floats compare equal).

Storage is `BTreeMap` (not a sorted-`Vec` + binary search: O(n) insertion would
kill the incremental write path; and `BTreeMap` is mandated by the no-`HashMap`
rule).

### 4.2 The commit fix — both scans

`commit` today does two O(n) passes; both collapse through the SPO index:

- **Idempotency** (`facts.contains(&fact)`): look up the `(subject, predicate)`
  postings, compare the full fact against the (usually few) matches → O(log n + k).
- **Functional contradiction** (subject already holds a different object for a
  functional predicate): the same `(subject, predicate)` postings; check for a
  differing object → O(log n + k), where k ≈ 1 for a functional predicate.

Together these make `commit` O(log n + k) and world-build O(n log n) — the
complete fix for the §12 blocker.

`check(&self, …)` (the refinement engine's commit-less test, also called inside
`commit`) is `&self`, so it **reads the index if present and falls back to the
naive scan if absent**. A `&self` reader never builds the index — that dodges
interior mutability and preserves the metaplan §3.6 `&self`-query /
`&mut self`-commit split. INDEX≡SCAN guarantees the two paths return identical
answers, so the fallback is correct, not merely tolerable. During a world build
the index is present throughout (the interleaved commits maintain it), so
`check` is fast exactly when it matters.

### 4.3 Index lifecycle — incremental on write, lazy rebuild on load

Not an either/or; each path is forced by its role.

- **Write path — incremental.** Every `commit` that appends a fact also inserts
  its position into all three indexes. This is what makes `commit` O(log n) — the
  campaign's whole point.
- **Read / load path — lazy rebuild-if-absent.** After deserialize the
  `#[serde(skip)]` index is `None`; the first index-consuming operation rebuilds
  it once (O(n log n)) from `facts`, then maintains it incrementally. (Matches
  the metaplan's "lazy-rebuild-until-measured", in-memory-until-scale, 0037.)

Byte-identical either way: a `BTreeMap` iterates in key order regardless of
insertion order, so incremental-insert and batch-rebuild produce the same index
— the INDEX≡SCAN safety. **Invariant: the index is absent or complete-and-current,
never partial** (append-only, monotonic commit means it is never stale between
ops).

`World::from_json` already runs a post-deserialize hook (`kernel/src/world.rs:52`,
the `minting_is_valid` check) — the natural site to *eagerly* reindex on load if
load-then-query paths ever prove hot. Deferred as a measured trigger; the
`&self`-scan-if-absent fallback keeps a read-only loaded world correct without it.

### 4.4 Predicate interning

The intern table (`String` ↔ `Symbol(u32)`) lives in `FactIndex` as build-state,
`#[serde(skip)]`, rebuilt with the indexes. Predicates come from the fixed
concept registry (120 registered today), so the table is small and ordered.
**Symbols key only the indexes** — compact keys and O(1) comparison during
`BTreeMap` traversal, replacing per-node `String` comparison. `Fact.predicate`
stays a `String`; the query-speed half of interning ships, the Fact-shrink space
half is deferred (§2).

### 4.5 Reflection — `kinds_with(component)`

The kind-side backend, distinct from the fact index: a uniform reflection query
over the campaign-3 component registries (`WorldComponents`) —
`kinds_with(component) -> impl Iterator<Item = KindId>` — the "what kinds have
component X" read that is UNI-21's capability query and the GOAP available-action
set (PSY-6). Each registry is already a `ComponentStore<KindId, C>` iterable by
`KindId`; this formalizes the reflection vantage the metaplan §4.6 names (the
schedule, the query engine, and reflection are three views over the same
capability schema). Small and self-contained; it does not touch the ledger index.

## 5. Correctness — the keystone and the ladder

Per the metaplan §6 ladder; the load-bearing new property is **INDEX ≡ SCAN**.

1. **Type-level.** No-`HashMap` (clippy), `&self`/`&mut self` borrow split, the
   open `Component` marker — all already in force.
2. **Property — INDEX ≡ SCAN (the keystone).** Every permutation index returns
   *exactly* what the naive O(n) linear scan returns, over random ledgers. The
   whole perf story rests on this one property: the index is only safe if it is a
   provable refinement of the scan it replaces. Realized with the project's
   hand-rolled seeded-loop style (`for seed in 0..N { … }`, as in
   `domains/astronomy/tests/genesis_properties.rs`; **no proptest dependency** —
   deps stay serde-only): generate a random-but-seeded ledger, then for every
   query shape assert `index_result == naive_scan_result` (same facts, same
   order). Plus intern round-trip (`resolve(intern(s)) == s`) and serialization
   round-trip (serialize → deserialize → rebuild → identical query results).
3. **Equivalence / shadow.** The index-backed `facts_about`/`find`/`value_of`/
   `text_of` return byte-identical results to their pre-index implementations —
   the shadow each replacement ships behind.
4. **The master oracle.** The committed-artifact byte-identity drift-check (CI),
   which a behaviour-preserving refactor passes unchanged.

**The `naive_*` scan reference impls are kept** (test-scope, over the raw `Vec`).
They serve double duty: the INDEX≡SCAN oracle *and* the heavy-tier micro-bench
"before" baseline — the O(n) path we replace stays available so before/after
numbers are always measurable.

## 6. Perf budgets — benchmarks (rolled-our-own; no criterion, decision-ledger #37)

- **Deterministic budgets** (byte-identical → gate-able): interned `Symbol`
  size, index fact-counts / entry-counts on a fixed synthetic ledger — as
  drift-checked lab studies and `const`/`static` asserts.
- **Wall-time micros** (not byte-identical → never gated; heavy `heavy:` ignore
  tier, `std::hint::black_box` + `Instant`): `commit` throughput before vs. after
  the index (proves the §12 blocker fix); `facts_about`/`find`/query-by-object
  latency vs. n (proves the indexes). The 1000-world census is the standing
  macro-oracle for end-to-end world-build time.
- **Not** asserting `size_of::<Option<EntityId>>() == 8` this campaign — that is
  the deferred `NonZeroU64` niche (it is 16 now); the assert lands with it.

## 7. Stages (strangler-fig; each byte-identical, each independently testable)

Ordered by the load-bearing kernel first (the `complexity` dimension: the
simplest thing that resolves the §12 blocker is the SPO index feeding `commit`;
PSO/OSP and the new query methods are the completeness layer).

1. **SPO index + the commit fix.** Add `FactIndex` (SPO only) as a `#[serde(skip)]`
   field; incremental maintenance on `commit`; route both `commit`/`check` scans
   through it; lazy rebuild-if-absent. `facts_about` switches to the SPO index
   (position postings → commit order). INDEX≡SCAN for the S-shape. **Proves the
   perf win** (world-build O(n log n)); everything after is completeness.
2. **Predicate interning + PSO.** The intern table; `find` switches to the PSO
   index, no per-call `String` alloc. INDEX≡SCAN for the P-shape; intern
   round-trip.
3. **OSP + the object query.** The object total order (`total_cmp`); the
   `query-by-object` method; INDEX≡SCAN for the O-shape and the two-known shapes.
4. **Reflection.** `kinds_with(component)` over `WorldComponents`.
5. **Benchmark slice + verify + close.** The lab studies / const-asserts, the
   heavy-tier micros, the full INDEX≡SCAN battery, byte-identity confirmation,
   and the book (chronicle + freshness sweep + UNI-22 re-score).

## 8. Risks and open items

- **The 303 call-sites are the reward, not a risk** — preserving signatures and
  observable order means they are untouched, and the drift-check catches any
  order regression loudly. The single delicate point is that `find`/`facts_about`
  keep commit order (position postings, §3); the INDEX≡SCAN order assertion
  guards it directly.
- **Clone cost.** `Ledger` derives `Clone`; cloning a live index doubles the
  clone cost. No correctness issue; a benchmark note if any hot path clones large
  ledgers (measure, don't pre-optimize).
- **Boundedness (metaplan §12).** Postings accumulate with the (unbounded) facts;
  no new unbounded structure is introduced. The release valves (MEM-1, 0037
  compaction) remain campaign 7; no data structure here assumes boundedness.

## 9. Decisions (promoted from the campaign decision ledger #66–#70)

- **#66 — save-format line (leads G3):** the serialized ledger is unchanged;
  indexes + interner are `#[serde(skip)]` derived views; `Fact` and `EntityId`
  untouched on disk; the drift-check is the byte-identity proof.
- **#67 — extend `Ledger` in place:** preserve all signatures (303 call-sites
  untouched); postings are ascending fact positions (commit order preserved);
  both `commit` scans route through the index; `check(&self)` reads-if-present-
  scans-if-absent.
- **#68 — lifecycle:** incremental on write (the O(log n) commit), lazy rebuild-
  if-absent on load; absent-or-complete invariant; `Value` total order via
  `total_cmp`.
- **#69 — interning + scope:** query-speed half in c4 (Symbols key the indexes,
  `Fact.predicate` stays `String`); defer Fact-shrink and `NonZeroU64` (c5);
  AoS→SoA already done (c3); phantom ids declined (#53).
- **#70 — boundaries + keystone:** (S,P,O) only (place/day post-filtered); Fact
  backend only (not Phenomena/Field); INDEX≡SCAN via kept `naive_*` references,
  which double as the micro-bench baseline.

## 10. Definition of Done

- The five stages land byte-identical; INDEX≡SCAN green across all three
  permutations; the perf budgets recorded (deterministic as gate-able studies,
  wall-time in the heavy tier).
- Book: a chronicle entry (candidate *The Concordance*), a freshness sweep of any
  stale chapter, and a UNI-22 re-score noting campaign 4 shipped and campaign 5
  (instance⋈ledger) is next. Cross-ref UNI-20 (derived views), UNI-21
  (reflection / capability query), UNI-23 (magic-as-graph-grammar, whose matcher
  *is* this SPO/PSO/OSP engine).
- A one-page retrospective in `docs/retrospectives/`.
- Census fixtures ride the shared deferred AWS regen; **no local regen** (and a
  byte-identical refactor cannot drift the census regardless).
