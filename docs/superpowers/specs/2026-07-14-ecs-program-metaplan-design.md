# The Entity-Component Substrate: A Program Metaplan — Design

**Status:** Draft for G3 review (2026-07-14) · **Author:** Claude (campaign-
autopilot) · **Decider:** Nathan

> This is a **program metaplan**, not a campaign spec — the shape of
> `2026-07-11-the-walk-metaplan-design.md`. It enumerates a sequence of
> shippable campaigns, fixes the commitments they share, and sets the standing
> gate. Each campaign then gets its own spec → plan → execute cycle off this
> document. Nothing here is greenfield: the program *unifies and grows*
> substrate Hornvale already runs.

## 1. What this is

`SpeciesDef` is a god-struct. It fuses peoples-only concerns (psychology,
perception, phonology, the social lexicon, the family proto) with biosphere
traits (mass, resource niche, condition niche, potency). A dragon or a fungus
is forced to carry an inert psychology vector and a proto-language. The Niche's
menagerie exposed it: authoring a dozen creatures from autotroph to dragon meant
asking a fungus to carry the words for its chieftain. This is the
inheritance-versus-composition problem that motivates entity-component systems,
and it does not belong to one campaign.

The fix is to restructure entity data into **components grouped by domain, keyed
by identity, optional per entity, composed only at worldgen** — an entity *is*
the set of components it carries. But this is not a foreign ECS bolted on. It is
the entity-space instance of the constitution's Fact/Field/Phenomenon trace
protocol, and a **convergence of four things Hornvale already runs**:

- the append-only **Fact ledger** (the log),
- the **derived-view / tuple query engine** (UNI-20/21 — "entities with
  component X" *is* a tuple query),
- the **chunk-partitioned room ledger** (decision 0037 — the LSM idea at small
  scale, disk/compaction deferred),
- the **component split** The Niche forced.

The program's job is to make these one coherent substrate. It is migrated
**strangler-fig** across seven shippable campaigns; the first folds into The
Niche's unfinished menagerie and unblocks it today.

## 2. The spine: an entity is its component-set, sorted by rate

Two claims carry the whole design.

**An entity is its component-set.** Two entities with the same component-set are
the same *kind*. Border-zone entities a fat struct cannot express — a culture
(no body), a deity (biosphere + religion), an awakened beast (biosphere +
nascent psyche), a corpse (biosphere, animacy flipped) — are just unusual
component-sets: a *data* change, never a new god-struct. This is
row-polymorphism, or an orchestral score — each domain a staff, each entity a
measure, silence where a component is absent.

**Every boundary in the substrate is a rate boundary.** This is the load-bearing
lens, and it is not new — it is the constitution's existing build-state /
world-state / derived-view split (The Walk §3.4; decision 0037
"store-irreversible-derive-reversible"; UNI-21) applied to entities:

| tier | changes… | example | storage | in the save? |
|------|----------|---------|---------|--------------|
| **build-static** | never (authored) | red dragon = 4000 kg | typed registry | **no** (re-derived) |
| **sim-dynamic** | over sim time | dragon-7 perches here | Fact ledger | **yes** (only truth) |
| **derived** | per query (ephemeral) | "mighty things, north" | index / view | **no** (re-derived) |

The ECS inherits this rate model; it does not add one.

## 3. Core commitments

These are fixed for every campaign. They are the "optimize-for / give-up" axes.

1. **Determinism over speed.** `BTreeMap`/`BTreeSet`/`Vec` only, never
   `HashMap` (clippy-enforced); same seed + pins → byte-identical worlds and
   artifacts. We give up HashMap throughput, background compaction, and foreign
   iteration order. (A dividend: a `BTree` index yields sorted iteration for
   free — see §4.5.)
2. **Data, not code** (decision 0011). Components are authored data in domain
   registries, not compiled archetypes or per-species Rust types. We give up
   columnar-archetype raw throughput.
3. **Open extension** (constitutional). Adding a domain adds a registry and a
   system and edits *nothing* existing. There is no central `enum Component`.
   We give up closed-enum exhaustiveness.
4. **In-memory until measured scale demands otherwise** (decision 0037). The
   store is in-memory `Vec` + derived indexes; disk paging and LSM compaction
   are campaign 7, triggered by measurement, not guessed.
5. **Concurrency by direction, not by lock** (decision-ledger #35, ratified).
   The write path is single-threaded and deterministic; reads are pure over an
   immutable ledger prefix and freely parallel, enforced by Rust ownership
   (`&self` queries are `Send + Sync`; `&mut self` commits are exclusive — a
   data race is a compile error). No lock, no atomic, no `unsafe`. The real
   parallelism budget is across worlds (the census), across queries, and
   client-side (the Living Globe, 0022/0023) — never inside one sim tick.
6. **The sim tick is bulk-synchronous** (decision-ledger #40, ratified). Every
   system reads the frozen tick-N snapshot (parallel, pure); writes accumulate
   and commit into tick-N+1 in stable-label order (single-threaded,
   deterministic). Cross-system effects have one tick of latency by default;
   same-tick immediate effects are the opt-in that needs the topological
   schedule (§4.6). This is the deterministic-parallel form of agent-based
   modeling — synchronous updating, cellular-automata-style (§8).

## 4. The architecture

### 4.1 Identity: kind vs instance

Two identities at two rates, kept distinct but related.

- **`KindId` is a stable label** (`"red-dragon"`), not a position. Today species
  identity is a positional `u32` from `.enumerate()` over the alphabetical
  registry (`windows/worldgen/src/lib.rs`) — deterministic only because it is
  never serialized. The moment a kind reference is serialized it must be the
  label. Precedent: decision 0015 (predicate names are their own registry keys),
  decision 0006 (`CellId`/`RoomId` are the frozen id contracts). The packer
  keeps a **build-local dense index** for hot-loop array math, derived from the
  label registry each build and never serialized — the same durable/ephemeral
  split as `EntityId` vs an array offset.
- **`EntityId` is a minted instance handle** (`kernel::ledger`, `u64`, never
  reused). It becomes `NonZeroU64` (0 is already reserved), which is free and
  shrinks `Option<EntityId>` from 16 to 8 bytes.
- **`instance_of(EntityId) -> KindId` is a Fact**, because a kind *can* change
  over sim time (awakened beast, corpse, lich — UNI-7/BIO-11), and that change
  is exactly the observable, provenanced event the ledger exists for. Border-
  zone and transformed entities are kind-change facts, never struct edits.

Both ids are phantom-typed — `Id<Kind>` and `Id<Entity>` over a shared newtype —
so indexing a kind registry with an entity id is a compile error, at zero cost.

### 4.2 Component storage: the API already ships as `CellMap`

`kernel::geosphere::CellMap<T>` is a dense `Vec<T>` built in ascending-id order:
a deterministic, hot-loop-proven typed store keyed by a dense id. The component
store is `CellMap` with the key generalized — `ComponentStore<K, C>`,
monomorphized per use, no dynamic dispatch. Storage splits by key density:

| key | density | store | precedent |
|-----|---------|-------|-----------|
| `CellId`, `KindId` | dense, few | `Vec<C>` (`CellMap`) | ships today |
| `EntityId` | sparse, minted | `BTreeMap<EntityId, C>` | BTreeMap-only rule |

Operations are small: `get`, `insert`, `iter`, `contains`, `ids`. Each domain
publishes its own registry type (`BiosphereRegistry`, `PsycheRegistry`, …) and
constructor; worldgen (the composition root) holds the struct-of-registries and
is the only place they meet. A `Component` **marker trait** (open, *not* sealed —
sealing would break open-extension) plus per-component validating constructors
gives both open extension and invariant safety: anyone adds a component, nobody
constructs an invalid one.

### 4.3 The Facts-vs-registries line

The line is rate, and it is the build-state / world-state boundary:

- **run-static, authored, per-kind → typed registry** (build-state, re-derived,
  not saved). A dragon's mass did not "happen on a day," has no provenance, and
  cannot be contradicted; Fact machinery is pure overhead, and putting build-
  state in the save is the cross-version hazard UNI-21 names.
- **sim-mutable, observed, per-instance → Fact** in the one ledger (world-state,
  serialized, the only stored truth — UNI-20, decision 0037).

An **instance component** (a typed `InstanceBiosphere`) is therefore a **derived
lens** materialized from the entity's fact-cluster on demand and cached within a
tick — never serialized, exactly like an index. The kind ⋈ instance join is
**prototype inheritance**: an instance's effective trait is its instance fact-
override *else* its kind's authored default. That is the "archetype/prefab".

### 4.4 The query engine: two derived-view backends, joined by Phenomena

The trace protocol has three reads; all three fit the derived-view engine.

| read | store / index | query shape |
|------|---------------|-------------|
| **Fact** | ledger + `SPO`/`PSO`/`OSP` permutation indexes | point / pattern (symbolic) |
| **Field** | `CellMap` / `RoomMap` (spatial, pure functions) | range / sample (spatial) |
| **Phenomenon** | none (derived) | ranked join of the two, for an observer |

The three permutation indexes are the rotations of (subject, predicate, object)
— three ordered `BTreeMap`s, each answering one two-known-one-unknown query in
O(log n). All indexes are **derived views**: not in the save, rebuilt from the
log, byte-identical whether built lazily or incrementally. A **Phenomenon** is
the ranked join — gather the observer's neighborhood-bounded facts plus the
field samples at their place, rank by salience, and project away the producing-
system tag (the constitution's provenance-hiding). The unification is at the
derived-view level (all are lossy reductions; only the ledger is stored), not a
single uniform query language — facts and fields keep different index shapes.

### 4.5 Performance: reinstate the proven forms

Today the ledger is a flat `Vec<Fact>` with no index, so `commit` is O(n) (a
`contains` scan plus a functional-contradiction scan) and building a world is
O(n²); `facts_about`/`find` are O(n) and `find` allocates a `String` per call.
The fixes are textbook:

- **Triple-store permutation indexes** → `commit` and all queries O(log n),
  world build O(n log n).
- **Predicate interning** (`String` → `Symbol(u32)`, compiler-symbol-table
  style; predicates come from the fixed registry, so the intern table is
  build-state and ordered) → O(1) comparison and one copy per predicate instead
  of one per fact.
- **AoS → SoA** (data-oriented) → an absent component is zero bytes; a fauna
  kind stops carrying inert `PeopledTraits`.
- **`NonZeroU64` niche** → `Option<EntityId>` halves.

Ordering is free: a `BTree` index yields sorted iteration with no separate sort,
so §3.5's "results in total-key order" rule costs nothing and joins are lazy
ordered merges. This is a second, independent reason for the no-`HashMap` rule.

### 4.6 The schedule: a derived view over the capability schema

A deterministic sim needs a total order over systems, but open extension forbids
a central hand-edited schedule. Resolve it the way build systems and spreadsheet
recalculation do: each system **declares the predicates it reads and writes**
(this is UNI-21's capability schema); "reads P" must run after "writes P"; the
resulting data-dependency DAG is **topologically sorted, tie-broken by stable
label** (never registration position — that would be the positional-tag bug one
level up). Adding a domain adds its declarations; the schedule re-derives;
nothing central is edited. The schedule, the query engine, and reflection are
thus all derived views over the same capability schema — one self-describing
substrate, three vantages.

### 4.7 Kernel placement

Follows the existing mechanism/population split (kernel defines, worldgen
composes — the provider pattern). The **kernel** gets the domain-agnostic
mechanisms: `ComponentStore<K, C>`, the fact index and query engine (extending
`Ledger`), the capability-schema *type* and the topological-sort function, and
the `KindId` newtype. **Domains** own their component registries. **Worldgen**
composes the population and derives the schedule. The kernel grows but stays
domain-agnostic; no new layer is introduced.

## 5. The campaign carve (strangler-fig; each shippable, reversible, shadowed)

Each step ships value, is reversible, and lands behind an equivalence shadow
(§6) before any cutover — the discipline the coexistence stack and The Niche
already used.

| # | campaign | delivers | depends on |
|---|----------|----------|------------|
| **1** | **The Seam** (in The Niche) | `SpeciesDef` → `{ biosphere, peopled: Option<…> }`; the packer reads `.biosphere`; **the menagerie ships** (fauna carry `peopled: None`); genesis cutover onto the differentiated stack | — |
| 2 | **Kinds get identity** | `KindId` = stable label; the biosphere registry keyed by `KindId`; the packer's positional `u32` becomes a build-local dense index; no serialized position | 1 |
| 3 | **Components by domain** | each concern → a domain-owned registry keyed by `KindId`; `SpeciesDef` dissolves; the ownership matrix enforced by construction | 2 |
| 4 | **The query engine** | the `SPO`/`PSO`/`OSP` permutation indexes; predicate interning; `kinds_with(component)`; the reflection / GOAP action-set query | 3 |
| 5 | **Instance ⋈ ledger** | `EntityId`-keyed instances; `instance_of` facts; the typed instance-component lens; prototype-inheritance join; non-species entities (deity, culture, material, awakened) | 3, 4 |
| 6 | **Systems & schedule** | domains formalized as systems with read/write declarations; the capability-schema-derived topological schedule; the BSP tick made concrete | 4, 5 |
| 7 | **Spatial partition** (deferred) | the chunk ledger (decision 0037) as storage partition *and* parallel-agent-update partition — only if measured scale demands it | 5 |

Campaign 1 is The Niche's remaining work (the menagerie + genesis cutover it
deferred, BIO-25/26/27); it folds forward here and unblocks the mighty roster.

## 6. The standing gate (determinism · correctness · perf)

Not a phase — a cross-cutting gate every campaign passes.

**Determinism.** Before/after byte-identity of committed artifacts (the master
test — CI already runs the drift-check; a behavior-preserving refactor is
byte-identical, so every step passes it unchanged). A `KindId`-label-stability
regression (insert a dummy kind; assert every other kind's serialized reference
is byte-identical). No-`HashMap` / ordered-iteration (clippy).

**Correctness — a guarantee ladder, strongest first.**
1. *Type-level* (the state cannot be written): phantom ids, `NonZero`,
   validating constructors, borrow-checker data-race freedom, the open marker
   trait.
2. *Property* — the keystone is **INDEX ≡ SCAN**: every permutation index
   provably returns exactly what the naive O(n) scan returns, over random
   ledgers. The whole perf story rests on this one property. Plus intern round-
   trip, join laws, serialization round-trip.
3. *Equivalence / shadow*: `biosphere_of(kind)` equals the old `SpeciesDef`
   fields, every kind, every step — each migration step ships behind this.
4. *The master oracle*: the census byte-identity drift-check.

**Perf — a budget per campaign, measured, rolled-our-own (no criterion; decision-
ledger #37).** Deterministic budgets (space, fact counts, index size,
`size_of`) are drift-checked lab studies and `const`/`static` asserts — gate-
able. Wall-time micros (throughput, latency) use a tiny `std::hint::black_box`
+ `Instant` harness in the `heavy:` ignore tier (ungated — wall time is not
byte-identical). The 1000-world census is the standing macro-oracle. The point
lazy indexing stops sufficing is campaign 7's measured trigger.

## 7. Determinism contracts (these lead any G3 for a campaign spec)

- **Serialized kind references are the label, never the position.** A deliberate
  label change takes an epoch suffix (`red-dragon/v2`), never a rename — the
  discipline of decision 0006 and the deity-name /v2 epoch (0050/0051).
- **Single writer per functional predicate.** Each functional predicate is
  written by exactly one system, declared in the capability schema and checked
  at load (like the orphan-predicate check), making same-tick write conflicts
  *unrepresentable* rather than resolved after the fact.

## 8. Agent-based modeling: the frame, placed honestly

Lifted above its NetLogo surface, ABM is two claims — emergence (author local
rules; the macro pattern emerges) and locality (agents act on local perception,
affect neighbors). At that altitude the liveness layer already *is* an ABM
(PSY-6 GOAP, UNI-16 belief, SOC-9, PSY-7, UNI-1). ABM's celebrated concurrency
is the naive-parallel form of its own well-known simultaneity hazard; the
rigorous fix is **synchronous updating**, which is exactly the BSP tick (§3.6).
So ABM adds no new concurrency mechanism — it justifies and names the one we
chose. We adopt its emergence and locality and synchronous updating; we reject
(as features) its stateful mutable agents (ours are stateless derived views over
the one ledger), its asynchronous/continuous time (we are discrete-synchronous),
its full-population eager sim (ours is lazy and observer-relative — "game as
lens"), and its direct messaging (interaction is via the ledger). Its one new
gift: **locality is the parallel-decomposition key** — agents in different
chunks share no same-tick dependency, so the deferred spatial partition
(campaign 7) is simultaneously the storage partition, the derived-view locality,
and the parallel-agent-update decomposition. The parallelism budget is spatial
and measured.

## 9. In / out boundary

**In:** the sim's entity / component / query / system substrate — kinds,
components, tuple queries, domains-as-systems, the deterministic BSP schedule,
the spatial partition. **Out:** the game / render / vessel layer; the
possess/game seam; the client-side Living Globe (determinism waived client-side,
0022/0023). The ECS is the sim core's skeleton, not the game.

## 10. Frontier bookkeeping

- **UNI-22** (the entity-component substrate) is this program's registry home;
  on the first campaign's merge it moves `raw` → `elaborated` and points here.
- Cross-refs to keep live: UNI-20 (derived-view / tuple engine), UNI-21 (the
  self-reflective capability schema — the schedule and reflection derive from
  it), decision 0037 (the chunk ledger = the spatial + parallel partition),
  PSY-6 (GOAP = the agent core), BIO-25/26/27 (The Niche traits the menagerie
  exercises), decisions 0022/0023 (client-side parallel liveness).
- No new registry row for ABM — it is a lens over the existing liveness cluster,
  not a new bet.

## 11. Process

This metaplan governs; each campaign is its own spec → plan → execute → merge
cycle, and each merge carries the book (a chronicle entry, a freshness sweep, a
Confidence-Gradient re-score if it moves a bet) and a retrospective, per the
project's Definition of Done. Campaign 1 resumes The Niche's paused menagerie on
the split model. Census fixtures ride the shared deferred AWS regen; no local
regen. The two open determinism contracts (§7) lead the G3 of whichever campaign
spec first encounters them.

## 12. Open questions (measured triggers, not design gaps)

- **Boundedness.** `EntityId` (monotonic, never reused) and facts (never
  deleted) both grow unbounded. MEM-1 (the diegetic melt) and decision 0037
  (compaction, campaign 7) are the release valves; no data structure may *assume*
  boundedness (instances stay `BTreeMap`; a generational slotmap only if
  measured).
- **Commit-index dependency.** The O(n²) commit means campaign 4 must land the
  permutation indexes before any census-scale work — a sequencing constraint.
- **Load-time re-derivation cost.** Re-deriving all indexes and fields on load
  is the correct discipline (0033/0037); if it is too slow at scale the fix is
  lazy/incremental derivation, never snapshotting derived state. A perf budget,
  not a design change.
- **Campaign 2/3 merge.** Whether `KindId` (campaign 2) and domain registries
  (campaign 3) are one campaign or two is a sequencing call for the first
  campaign spec, once the seam's shape is concrete.
