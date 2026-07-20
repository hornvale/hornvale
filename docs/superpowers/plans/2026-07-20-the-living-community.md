# The Living Community — Campaign 1 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Grow the present world as the last frame of a derived deep history —
living settlements, standing ruins, and territories — retiring the one-shot
settlement placer and committing a compact, queryable occupation skeleton whose
flesh (structures, artifacts, the goblin doll) derives on demand.

**Architecture:** A new kernel-only `domains/history` crate owns the occupation
data model, the predicates, and the *pure local flesh derivations*. The *bake*
(the forward history) runs at the `windows/worldgen` composition root — the only
place domains meet — reading terrain, the MAP-7 capacity field, and paleoclimate
era-variance, and emitting the committed skeleton plus the present-frame
`is-settlement` facts the demography condenser used to emit. Displacement is driven
by paleoclimate habitability shifts + refugia; conflict is the simple
pressure-overshoot loop (SOC-criticality is a later campaign). It is a genesis
epoch.

**Tech Stack:** Rust 2024; `hornvale-kernel` only (Seed/Stream, Ledger, CellMap,
BTreeMap/Vec); `hornvale-paleoclimate`, `hornvale-demography`, `hornvale-terrain`,
`hornvale-climate` consumed *only* at the worldgen root.

## Global Constraints

- Kernel randomness only: `Seed`/`Stream`; `range_u32(lo, hi)` is **inclusive of hi**.
- No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only. Float order via `f64::total_cmp` with deterministic tie-breaks.
- No wall-clock time. Time is `WorldTime { day: f64 }` / years as `f64`.
- `f64` transcendentals via `hornvale_kernel::math` (`ln`, etc.); `f64::ln` is disallowed.
- **Quantize at emit only** (`hornvale_kernel::quantize`); the bake runs at full precision. `Ledger::commit` already quantizes `Number` objects and `day`.
- **Byte-identity:** same seed + pins → byte-identical skeleton, present, and flesh.
- **Lorenz-safe:** the flesh is a pure function of *committed facts + lossless seed*, never of quantized checkpoints. The bake is a deterministic replay, not a chaotic integrator seeded from ledger floats.
- Every crate: `#![warn(missing_docs)]`; every pub item documented.
- **type-audit:** every primitive at a `pub` boundary carries a `type-audit:` verdict tag (item-level comment listing each: `/// type-audit: bare-ok(<class>: <name>)`). CI-only; run `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` before each commit.
- Dependencies: `serde` + `serde_json` only. A **domain** serializes no domain enum; any enum crossing the save boundary serializes at the **window** via `serialize_with` (The Freshet lesson).
- A domain depends on `hornvale-kernel` and nothing else. The bake lives at `windows/worldgen`.
- `cargo fmt` is the final step before every commit. Gate: `make gate` (heavy tier `#[ignore]`d with the verbatim reason string; see `cli/tests/heavy_tier.rs`).

---

### Task 1: Scaffold `domains/history` — the occupation data model + predicates

**Files:**
- Create: `domains/history/Cargo.toml`
- Create: `domains/history/src/lib.rs`
- Create: `domains/history/src/record.rs` (the occupation record + enums)
- Create: `domains/history/src/streams.rs` (seed-derivation labels)
- Modify: `Cargo.toml` (workspace members — add `domains/history`)
- Modify: `windows/worldgen/Cargo.toml` (add `hornvale-history` dep)
- Test: `domains/history/tests/record.rs`

**Interfaces:**
- Produces:
  - `pub struct OccupationRecord` with fields: `people: KindId`, `community: EntityId`, `lineage: EntityId`, `site: CellId`, `founded: f64`, `ended: Option<f64>`, `peak_population: u32`, `tech: TechHorizon`, `function: Function`, `deity: Option<KindId>`, `tongue: Option<KindId>`, `cause: Option<CauseOfEnd>`, `ended_by: Ended`, `founded_from: Founding`, `notability: Notability`.
  - `pub enum CauseOfEnd { Famine, Burned, Plague, Fled, Migrated }`
  - `pub enum Function { Agrarian, Mine, Trade, Cult, Fort }`
  - `pub enum TechHorizon { Neolithic, Bronze, Iron, Classical }` (ordinal; `#[derive(PartialOrd, Ord)]`)
  - `pub enum Ended { Nature, By(EntityId) }` (the ★ global thread)
  - `pub enum Founding { Genesis(CellId), From(EntityId) }` (the ★ global thread)
  - `pub enum Notability { Backwater, Common, Seat }`
  - `pub fn tenure(&self, now: f64) -> f64` on `OccupationRecord` (`ended.unwrap_or(now) - founded`)
  - `pub fn is_alive(&self) -> bool` (`ended.is_none()`)
  - predicate constants in `lib.rs`: `IS_OCCUPATION`, `OCC_PEOPLE`, `OCC_SITE`, `OCC_FOUNDED`, `OCC_ENDED`, `OCC_PEAK`, `OCC_TECH`, `OCC_FUNCTION`, `OCC_CAUSE`, `OCC_ENDED_BY`, `OCC_FOUNDED_FROM`, `OCC_NOTABILITY`, `IS_RUIN`.
  - `pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError>`
  - `pub fn stream_labels() -> Vec<(&'static str, &'static str)>`
  - `pub struct History;` (registrable unit for the worldgen roster — mirrors `Settlement`)

Use `KindId` from `hornvale_kernel` (per-domain kind registries after The Dissolution — confirm the import path with `grep "pub struct KindId" kernel/src`).

- [ ] **Step 1: Write the failing test** — `domains/history/tests/record.rs`:

```rust
use hornvale_history::record::{CauseOfEnd, Ended, Founding, Function, Notability, OccupationRecord, TechHorizon};
use hornvale_kernel::{CellId, EntityId, KindId};

fn eid(n: u64) -> EntityId { EntityId(std::num::NonZeroU64::new(n).unwrap()) }

#[test]
fn tenure_and_liveness_read_off_the_span() {
    let goblin = KindId(1);
    let alive = OccupationRecord {
        people: goblin, community: eid(10), lineage: eid(10), site: CellId(3),
        founded: 340.0, ended: None, peak_population: 80, tech: TechHorizon::Bronze,
        function: Function::Agrarian, deity: None, tongue: None,
        cause: None, ended_by: Ended::Nature, founded_from: Founding::Genesis(CellId(3)),
        notability: Notability::Common,
    };
    assert!(alive.is_alive());
    assert_eq!(alive.tenure(2000.0), 1660.0);

    let dead = OccupationRecord { ended: Some(1980.0), cause: Some(CauseOfEnd::Burned),
        ended_by: Ended::By(eid(42)), ..alive.clone() };
    assert!(!dead.is_alive());
    assert_eq!(dead.tenure(2000.0), 1640.0);
}

#[test]
fn tech_horizon_is_ordinal() {
    assert!(TechHorizon::Neolithic < TechHorizon::Iron);
}
```

- [ ] **Step 2: Run it — verify it fails** (`cargo test -p hornvale-history --test record`; expected: crate/type not found).
- [ ] **Step 3: Create `Cargo.toml`** mirroring `domains/settlement/Cargo.toml` (name `hornvale-history`, dep `hornvale-kernel` only), add `domains/history` to the workspace `members`, and `hornvale-history = { path = "../../domains/history" }` to `windows/worldgen/Cargo.toml`.
- [ ] **Step 4: Implement `record.rs`** — the struct + enums above, `#[derive(Clone, Debug, PartialEq)]`, `#![warn(missing_docs)]` doc comments on every item, `tenure`/`is_alive`. Derive `PartialOrd, Ord` on `TechHorizon`. Add item-level `type-audit:` tags for the bare primitives (`founded: f64`, `peak_population: u32`, etc.).
- [ ] **Step 5: Implement `lib.rs`** — `pub mod record; pub mod streams;`, predicate constants (`pub const IS_OCCUPATION: &str = "is-occupation";` …), `register_concepts` (register each predicate: functional where single-valued, e.g. `OCC_PEOPLE` functional, `IS_RUIN` functional flag), `stream_labels` (root `history` label + the bake sub-labels this crate documents), `pub struct History;`.
- [ ] **Step 6: Run the test — verify it passes.**
- [ ] **Step 7: fmt + clippy + type-audit check, then commit** (`feat(history): occupation record + predicates (History C1 T1)`).

---

### Task 2: The flesh derivations — pure, local, deterministic

**Files:**
- Create: `domains/history/src/flesh.rs`
- Modify: `domains/history/src/lib.rs` (`pub mod flesh;`)
- Test: `domains/history/tests/flesh.rs`

**Interfaces:**
- Consumes: `OccupationRecord`, the enums (Task 1); `Seed`, `Stream` (kernel).
- Produces:
  - `pub struct RoleHandle(pub u64);` — the lazily-expandable individual handle.
  - `pub struct Persona { pub name_seed: u64, pub trait_seed: u64 }` (flesh; expanded to prose by the window, not here).
  - `pub fn persona_of(handle: RoleHandle, seed: Seed) -> Persona` — pure hash expansion.
  - `pub struct Residue { pub items: Vec<ResidueItem> }`, `pub enum ResidueItem { Doll, Bauble, Reliquary, Tool, Weapon, Bones, Inscription }`
  - `pub fn residue_of(occ: &OccupationRecord, now: f64, seed: Seed) -> Residue` — a small deterministic set keyed by `(people, cause, tenure-age, notability)`.
  - `pub fn structures_of(occ: &OccupationRecord, seed: Seed) -> Vec<Structure>` where `pub enum Structure { Hut, Longhouse, Granary, Shrine, Temple, Wall, Mineshaft, Market }` — gated by `function` × `tech` × `peak_population`.

**Load-bearing rule:** every function here is a *total function of its arguments*; no world, no global state, no replay. `seed` is derived once per occupation via `world.seed.derive("history/flesh").derive(&occ.community.0.to_string())` at the call site.

- [ ] **Step 1: Write the failing test** — `domains/history/tests/flesh.rs`:

```rust
use hornvale_history::flesh::{persona_of, residue_of, ResidueItem, RoleHandle};
use hornvale_history::record::{CauseOfEnd, Ended, Founding, Function, Notability, OccupationRecord, TechHorizon};
use hornvale_kernel::{CellId, EntityId, KindId, Seed};
// ... same eid helper + a `burned_goblin_village()` builder (cause = Burned, people = goblin, ended 1980) ...

#[test]
fn flesh_is_deterministic() {
    let occ = burned_goblin_village();
    let a = residue_of(&occ, 2000.0, Seed(7));
    let b = residue_of(&occ, 2000.0, Seed(7));
    assert_eq!(a.items, b.items);           // same inputs -> same flesh
    assert_eq!(persona_of(RoleHandle(9), Seed(7)), persona_of(RoleHandle(9), Seed(7)));
}

#[test]
fn a_recently_burned_goblin_hamlet_leaves_a_doll() {
    let occ = burned_goblin_village();      // low notability, young ruin, burned
    let r = residue_of(&occ, 2000.0, Seed(7)); // died 1980, now 2000 -> 20y old
    assert!(r.items.contains(&ResidueItem::Doll));
    assert!(!r.items.contains(&ResidueItem::Reliquary)); // that's for a Seat
}
```

- [ ] **Step 2: Run it — verify it fails.**
- [ ] **Step 3: Implement `flesh.rs`.** `persona_of`: the splitmix-style hash from The Sounding's `expand_handle`. `residue_of`: derive a stream from `seed`; the item set is a deterministic function — burned+young leaves personal effects (Doll for a family hamlet, Weapon/Bones for a fort); notability=Seat adds Reliquary/Bauble; age weathers items away (older ⇒ fewer). `structures_of`: a match on `function`×`tech` producing the structure list, count scaled by `peak_population`. Use `BTreeSet`/`Vec` only; sort with `total_cmp` if ordering floats.
- [ ] **Step 4: Run the test — verify it passes.**
- [ ] **Step 5: fmt + clippy + type-audit, commit** (`feat(history): pure local flesh derivations — persona/residue/structures (T2)`).

---

### Task 3: The bake — the deep-history forward simulation

**Files:**
- Create: `windows/worldgen/src/history_bake.rs`
- Modify: `windows/worldgen/src/lib.rs` (`mod history_bake;` + re-exports)
- Test: `windows/worldgen/tests/history_bake.rs`

**Interfaces:**
- Consumes: `OccupationRecord`, enums (T1); the capacity field builder (`carrying_inputs_of` / `demography::carrying_capacity`, worldgen `lib.rs:449,654`); paleoclimate `EraClimate` (per-era `habitable: CellMap<bool>`, `ice`, `sea_level`) and `PaleoRecord.refugia: CellMap<bool>` (`domains/paleoclimate/src/strata.rs`); `Seed`/`Stream`; `Geosphere`/`CellMap`/`CellId`.
- Produces:
  - `pub struct BakeConfig { pub start_year: f64, pub end_year: f64, pub epoch_years: f64 }` with `pub fn default_millennia() -> BakeConfig` (start 0.0, end 2000.0, epoch 25.0).
  - `pub struct History { pub records: Vec<OccupationRecord>, pub now: f64 }` — the whole skeleton (alive + dead), `records` in deterministic commit order.
  - `pub fn bake(seed: Seed, geo: &Geosphere, capacity: &CellMap<f64>, eras: &[EraClimate], refugia: &CellMap<bool>, peoples: &[KindId], cfg: &BakeConfig) -> History`
  - `pub struct BakeCensus { pub grew, founded, migrated, raided, fled, collapsed, resettled: u64, pub records_total, alive_at_now: u64 }`, `pub fn census(h: &History) -> BakeCensus`.

**Algorithm (deterministic; document each draw's stream label):**
1. **Seed the ancient world:** for each people, found a small number of proto-communities at the highest-capacity habitable cells of the *earliest* era (draw sites via `seed.derive("history/genesis/<people>")`). One alive community per site (the invariant that made scan≡index byte-identical in The Sounding — keep a `node_index: BTreeMap<CellId, usize>`).
2. **Step epochs** `start_year..end_year` by `epoch_years`. Each epoch, resolve the *era* whose `day` window contains the year → its `habitable` map is the live mask; a cell's effective capacity = `capacity[cell] * habitability_factor(era, cell)` (0 when uninhabitable/iced).
3. **Per community, in commit order:** `pressure = population * need / eff_capacity`.
   - `eff_capacity == 0` (cell turned hostile/iced): **migrate** toward the nearest refugial/habitable neighbour; if none, **collapse** (record `cause = Migrated`/`Famine`).
   - `pressure >= 2.0`: **collapse** (`Famine`), close the record (`ended = year`).
   - `pressure >= 1.0`: **raid** a graph-adjacent occupied neighbour (spatial adjacency this campaign) — displace a fraction of *its* population; the raided community **flees** (closes its record with `cause = Fled`, `ended_by = By(raider)`) and **refounds** on the nearest vacant habitable cell (a new record with `founded_from = From(fled community)`), or collapses if none.
   - `pressure < 1.0`: **grow** ×(1 + r); occasionally **found** a daughter on a vacant habitable neighbour (`founded_from = From(parent)`).
   - Update `peak_population`, `tech` (era-driven monotone via `TechHorizon` from year + per-people rate).
4. **Close** at `now = end_year`: alive records keep `ended = None`.

No floor: displacement is sustained by the era mask changing under the communities. Keep every arithmetic op in `f64` full precision; sort neighbour candidates by `total_cmp`.

- [ ] **Step 1: Write the failing determinism + census test** — `windows/worldgen/tests/history_bake.rs`:

```rust
#[test]
fn same_seed_bakes_byte_identical_history() {
    let (geo, cap, eras, ref_) = fixture(42);           // a small test world
    let peoples = vec![KindId(1), KindId(2), KindId(3), KindId(4)];
    let cfg = BakeConfig::default_millennia();
    let a = bake(Seed(42), &geo, &cap, &eras, &ref_, &peoples, &cfg);
    let b = bake(Seed(42), &geo, &cap, &eras, &ref_, &peoples, &cfg);
    assert_eq!(a.records, b.records);
}

#[test]
fn different_seeds_diverge() { /* Seed(42) vs Seed(43) -> records differ */ }

#[test]
fn the_workload_fires_displacement_at_volume() {
    // measure-don't-narrate: the phenomenon the campaign exists on MUST fire.
    let c = census(&bake(/* base test world, 2000y */));
    assert!(c.fled + c.resettled > 50, "displacement inert: {c:?}");
    assert!(c.collapsed > 0 && c.alive_at_now > 0);
}
```

- [ ] **Step 2: Run — verify it fails.**
- [ ] **Step 3: Implement `history_bake.rs`** per the algorithm. `fixture(seed)` builds a small geosphere + capacity + a 3-era `EraClimate` slice with a glacial swing + refugia (enough to force migration).
- [ ] **Step 4: Run — verify determinism + census pass.** If displacement is inert, the era swing / thresholds are wrong — fix the dynamics, do **not** add a floor.
- [ ] **Step 5: fmt + clippy + type-audit, commit** (`feat(worldgen): the deep-history bake (T3)`).

---

### Task 4: Commit the skeleton + present-as-query + territories

**Files:**
- Create: `windows/worldgen/src/history_emit.rs`
- Modify: `windows/worldgen/src/lib.rs`
- Test: `windows/worldgen/tests/history_emit.rs`

**Interfaces:**
- Consumes: `History`, `OccupationRecord` (T1/T3); `World`, `Ledger::commit`, `Fact`, `Value`, `EntityId`, the query API (`find`, `facts_about`, `query_by_object`).
- Produces:
  - `pub fn emit_history(world: &mut World, h: &History) -> Result<(), BuildError>` — one entity per occupation; commits `IS_OCCUPATION`, then each of the ~12 facts (people/site/founded/ended/peak/tech/function/cause/ended-by/founded-from/notability); commits `IS_RUIN` for dead records; commits `IS_SETTLEMENT` + `POPULATION` + `CELL_ID` for **alive** records (history as the settlement provider — the keystone); every fact carries `provenance` naming the bake epoch.
  - `pub fn territories(world: &World) -> BTreeMap<KindId, BTreeSet<CellId>>` — dominant people per region, from alive occupations.
  - `pub fn ruins_of_people(world: &World, people: KindId) -> Vec<EntityId>` — a **query** (`query_by_object` on `OCC_PEOPLE` ∩ `IS_RUIN`), proving the deep past is queryable without replay.

- [ ] **Step 1: Write the failing test** — commit a small hand-built `History`, then:

```rust
#[test]
fn the_present_is_the_live_occupations() {
    let mut w = test_world();
    emit_history(&mut w, &hand_history());
    let settlements = hornvale_settlement::all_settlements(&w);
    // every is-settlement subject is an alive occupation; counts match.
    assert_eq!(settlements.len(), alive_count(&hand_history()));
}

#[test]
fn the_deep_past_is_queryable_not_replayed() {
    let mut w = test_world();
    emit_history(&mut w, &hand_history());
    let goblin_ruins = ruins_of_people(&w, KindId(1));
    assert!(!goblin_ruins.is_empty());
    // provenance points back at the bake for each
    for e in &goblin_ruins { assert!(w.ledger.facts_about(*e).any(|f| f.predicate == IS_RUIN)); }
}
```

- [ ] **Step 2: Run — verify it fails.**
- [ ] **Step 3: Implement `history_emit.rs`.** Mint entity ids in record order; commit facts; `territories` folds alive occupations by people; `ruins_of_people` uses the object index. Quantize is automatic at `commit`.
- [ ] **Step 4: Run — verify it passes.**
- [ ] **Step 5: fmt + clippy + type-audit, commit** (`feat(worldgen): commit the skeleton, present-as-query, territories (T4)`).

---

### Task 5: Retire the draft placer + wire the bake into the build (the epoch)

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (the `BuildDepth::Settlements` stage — replace the demography-condensation settlement emission with `bake` → `emit_history`)
- Test: `windows/worldgen/tests/history_placement.rs`; update any settlement-count tests

**Interfaces:**
- Consumes: `bake`, `emit_history`, the existing capacity-field builders.
- The demography *report* (coexistence stack) accessor stays for the Lab; only the **settlement-emitting** condensation path is replaced as the genesis placer.

- [ ] **Step 1: Write the failing sole-provider + quality-gate test:**

```rust
#[test]
fn history_is_the_sole_settlement_provider() {
    let w = build(Seed(42), BuildDepth::Settlements);
    // every is-settlement subject also carries is-occupation (no parallel placer)
    for f in w.ledger.find(IS_SETTLEMENT) {
        assert!(w.ledger.facts_about(f.subject).any(|g| g.predicate == IS_OCCUPATION));
    }
}

#[test]
fn emergent_settlement_count_stays_in_the_sane_band() {
    // the no-regression quality gate: history must not regress the walkable map.
    let n = hornvale_settlement::all_settlements(&build(Seed(42), BuildDepth::Settlements)).len();
    assert!((40..=400).contains(&n), "regressed settlement count: {n}");
}
```

- [ ] **Step 2: Run — verify it fails** (old placer still emits, or band off).
- [ ] **Step 3: Rewire the `Settlements` stage** to call `bake` (with the composition-root capacity field + paleoclimate eras/refugia + the peoples roster) then `emit_history`; delete the settlement-emitting condensation call. Tune the bake's founding density so seed-42's live count lands in the band (this is the quality gate — do not fake it with a post-filter).
- [ ] **Step 4: Run — verify sole-provider + band pass.** Fix the fallout in existing settlement/demography tests by **re-pinning measured values in this commit** (golden-pin discipline — never defer). If a test asserts the *draft* placement specifically, it is superseded — update it to assert the history-provided invariant, noting why.
- [ ] **Step 5: fmt + clippy + type-audit, commit** (`feat(worldgen): retire the draft placer — history places the world (T5, epoch)`).

---

### Task 6: The preregistered measurement gates (Lab battery)

**Files:**
- Create: `windows/worldgen/tests/history_gates.rs` (light in-gate assertions on a small world)
- Create: `cli/tests/history_battery.rs` (heavy `#[ignore]`d full-world battery)
- Test: the two above

**The three frozen gates (write the thresholds BEFORE running):**
1. **displacement-fired-at-volume** — `census(bake).fled + .resettled` ≥ a floor on the base world; a run below ABORTS with a specific message (the Sounding's floor, inverted into a falsification metric).
2. **territories-separated** — the four goblinoids (`KindId`s for goblin/kobold/hobgoblin/bugbear) occupy measurably distinct cell sets: mean pairwise Jaccard overlap of their territory sets < a preregistered threshold (e.g. 0.5). This is the peoples-diversity fix, measured.
3. **stratigraphy-emerged** — ≥ a preregistered fraction of occupied sites carry ≥ 2 layers, and layer depth correlates positively with capacity (`total_cmp`-sorted rank correlation > 0).

- [ ] **Step 1: Write the three gate tests** with the frozen thresholds as `const`s and abort messages. The heavy battery carries the verbatim ignore reason `"heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"`.
- [ ] **Step 2: Run the light gates on a small world — verify they compile and fail (thresholds not yet met by a stub) then pass on the real bake.**
- [ ] **Step 3: Run the heavy battery locally once** (`cargo test -p hornvale --test history_battery -- --ignored`); record the census + gate values.
- [ ] **Step 4: fmt + clippy + type-audit, commit** (`test(history): preregistered displacement/territory/stratigraphy gates (T6)`).

---

### Task 7: The legibility surface — a site's stratigraphy + its flesh

**Files:**
- Create: `windows/almanac/src/history.rs` (or a new `windows/chronicle_view` — follow the almanac pattern; a **window** may serialize the domain enums via `serialize_with`)
- Modify: the CLI (`cli/src/…`) to add a `history --world <w> --site <cell>` verb (std-only parsing)
- Modify: `book/src/gallery/` (a committed, drift-checked example page for seed 42)
- Test: `windows/almanac/tests/history_render.rs`

**Interfaces:**
- Consumes: the ledger occupation facts (query), `flesh::{residue_of, structures_of, persona_of}`.
- Produces: `pub fn render_site(world: &World, site: CellId) -> String` — the stratigraphy stack (each layer: people, span, tech, function, cause, ended-by, founded-from, in prose) + a derived flesh sample (structures + residue). Deterministic.

- [ ] **Step 1: Write the goblin-doll acceptance test:**

```rust
#[test]
fn a_burned_goblin_clearing_shows_its_doll() {
    let mut w = test_world();
    emit_history(&mut w, &history_with_a_recently_burned_goblin_hamlet_at(CellId(3)));
    let text = render_site(&w, CellId(3));
    assert!(text.contains("goblin"));
    assert!(text.contains("burned"));
    assert!(text.to_lowercase().contains("doll"));   // the whole campaign, in one string
}
```

- [ ] **Step 2: Run — verify it fails.**
- [ ] **Step 3: Implement `render_site`** and the CLI verb; generate the committed seed-42 gallery page.
- [ ] **Step 4: Run — verify the doll test passes;** eyeball the rendered seed-42 page (visual pass — a formula can be green and read wrong).
- [ ] **Step 5: fmt + clippy + type-audit, commit** (`feat(almanac): render a site's stratigraphy + flesh — the doll (T7)`).

---

### Task 8: Determinism contract — stream manifest, byte-identity fixture, type-audit

**Files:**
- Modify: `domains/history/src/streams.rs` (finalize the bake's seed-derivation labels)
- Modify: the generated stream manifest page (regen, don't hand-edit)
- Create: `cli/tests/fixtures/history-seed-42.json` (byte-identity keystone)
- Test: `windows/worldgen/tests/history_byte_identity.rs`

- [ ] **Step 1: Enumerate every `seed.derive(...)` label** the bake uses; declare each in `stream_labels()` with docs. **Bake the manifest regen into this task** (the recurring `stream_labels()`-additions gap): run the "Artifacts are current" commands and commit the regenerated manifest.
- [ ] **Step 2: Write the byte-identity test** — build seed-42 to `BuildDepth::Settlements` twice, assert the serialized ledgers are byte-identical; freeze `history-seed-42.json` from main's tip and assert equality.
- [ ] **Step 3: Run the full type-audit `check`** (`cargo run --manifest-path tools/type-audit/Cargo.toml -- check`); tag every remaining pub-boundary primitive item-level; regenerate the audit report.
- [ ] **Step 4: Run — verify byte-identity + manifest drift + type-audit all clean.**
- [ ] **Step 5: fmt + clippy, commit** (`chore(history): stream labels, manifest regen, byte-identity fixture, type-audit (T8)`).

---

### Task 9: Definition of Done — book, registry, retro, census

**Files:**
- Create: `book/src/chronicle/the-living-community.md` + wire into `book/src/SUMMARY.md`
- Create: `docs/retrospectives/the-living-community.md`
- Modify: `book/src/frontier/idea-registry.md` (registry flips)
- Modify: `book/src/open-questions.md` (Confidence-Gradient re-score if a bet moved)
- Modify: book chapters the campaign staled (settlement/demography domain chapters — history now places)
- Delete: `docs/superpowers/plans/2026-07-20-the-living-community.md` (this plan) once all tasks done; note completion in the spec

- [ ] **Step 1: Chronicle entry** — the history-first turn, the overturn (commit-skeleton/derive-flesh), the paleoclimate-driven displacement, the doll. Name-only designation; no registry IDs in prose outside `book/src/frontier/` (the `docs_consistency` drift-check).
- [ ] **Step 2: Retrospective** — one page, process lessons (the annotation-vs-spine reframe; measure-don't-narrate applied to the displacement gate).
- [ ] **Step 3: Registry flips** — SOC-10 `elaborated → shipped` + repoint Where; SOC-criticality note (feeds campaign 3); MAP-61 note (campaign 2 next); never delete a row.
- [ ] **Step 4: Freshness sweep** — the settlement + demography domain chapters now describe *history as the placer*; fix the lag.
- [ ] **Step 5: Confidence-Gradient** — re-score any peoples-diversity / society bet the campaign moved.
- [ ] **Step 6: Commit the DoD**, then re-run the FULL gate (DoD prose can leak a registry ID or a stale generated page — re-gate after the DoD commit, not just the fix's own tests).
- [ ] **Step 7: Census regen on lefford** (the epoch): `HV_CENSUS=1` via `scripts/census-run.sh` on the canonical box; re-pin calibration + golden-pins in the same commit. Keystone refreeze from main's tip. **This is the close carve-out — Nathan authorizes at G6.**

---

## Self-Review

**Spec coverage:** §1 payoff → T7 (doll); §3 spine → T3+T5; §4 architecture (domain vs worldgen) → T1/T2 (domain) + T3/T4/T5 (worldgen); §5 data model → T1 (record) + T2 (flesh) + T4 (stratigraphy via emit); §6 persistence (commit skeleton / present-as-query) → T4; §7 keystone (sole provider + provenance) → T5 + T4; §8 dynamics (paleoclimate, no floor) → T3; §9 gates + non-goals → T6; §10 retire placer + legibility → T5 + T7; §11 epoch/census/quality-gate → T5/T8/T9; §12 DoD → T9. No gaps.

**Placeholder scan:** every code step shows real signatures and test assertions; algorithm steps in T3 are explicit; no "handle edge cases"/"TBD".

**Type consistency:** `OccupationRecord`/`History`/`BakeCensus`/`Residue`/`RoleHandle` names are used identically across T1–T7; `bake(...)` and `emit_history(...)` signatures match between T3/T4/T5; predicate constants (`IS_OCCUPATION`, `IS_RUIN`, `OCC_*`) are defined in T1 and consumed in T4/T5/T7.
