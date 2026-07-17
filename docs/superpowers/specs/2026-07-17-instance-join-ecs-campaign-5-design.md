# Instance ⋈ Ledger (ECS Campaign 5) — Design

Program: **The Entity-Component Substrate** (metaplan
`docs/superpowers/specs/2026-07-14-ecs-program-metaplan-design.md`, campaign 5;
registry **UNI-22**). Base: main `@37ae360` (campaigns 1–4 shipped: The
Menagerie, The True Name, The Dissolution, The Concordance). Chronicle-name
candidate: **The Individuation** — instances individuating from their kinds:
identity emerging from collective defaults via per-instance overrides, the
awakened beast as individuation in-fiction; confirmed at close, per the
one-evocative-noun convention.

## 1. What this is

Campaigns 1–4 built the *kind* half of the substrate: kinds are stable labels
(`KindId`), their authored traits live in per-domain component registries, and
the query engine answers "which kinds carry component X" plus every
two-known-one-unknown fact query in O(log n). What does not exist yet is the
*instance* half — the thing the whole program is for. Today:

- `EntityId`s are minted ad hoc (settlements, belief entities, sky neighbors,
  the world entity, the per-kind species representatives) with **no canonical
  record of what kind of thing each one is**. `SPECIES_NAME` names a
  kind-representative; `PEOPLED_BY` relates a settlement to a species; nothing
  says "entity 17 *is a* settlement".
- There is no way to give one individual a trait that differs from its kind
  ("this owlbear is unusually large") — the god-struct's inheritance problem,
  solved for kinds in c3, is unsolved for individuals.
- The keystone query the program promised — **"mighty things in the cold
  north"**: filter instances by their *kind's* components crossed with their
  *own* ledger facts — has no join to run on.

This campaign lands that join. It is the entity-space instance of prototype
inheritance (JS prototypes, Self, DB column defaults): **an instance's
effective trait is its own fact-override, else its kind's authored default.**
An entity *is* its component-set; the instance is the individuated view of it.

Also folded in: the c4-deferred `NonZeroU64` niche for `EntityId`
(`Option<EntityId>` 16 → 8 bytes; c4 spec §2/§6 parked it here).

## 2. Scope

**In (the metaplan §5 campaign-5 row, exactly, plus the c4 deferral):**

1. **`NonZeroU64` `EntityId`** — the niche fold-in, first task, byte-identical;
   includes the `const`-assert `size_of::<Option<EntityId>>() == 8`.
2. **`instance-of` facts** — a canonical kernel-core predicate; a kernel
   `mint_instance` mechanism (the sole writer); a `kind_of` latest-wins read;
   kind *change* over sim time as an ordinary fact commit (awakened / corpse /
   lich, UNI-7 / BIO-11).
3. **The typed instance-component lens** — the effective `BiosphereTraits` of
   an instance, materialized on demand from its override facts else its kind's
   registry default; species-owned; never serialized, never cached (this
   campaign — §4.3).
4. **The prototype-inheritance join** — the override-else-default rule, with
   level-agnostic trait predicates (mass, potency) registered for the
   overridable scalars.
5. **The first non-species kinds** — deity, culture, material (terrain
   lithology classes), and the awakened beast — each a distinct component-set
   shape (§4.4), plus the union kind-roster and validated minting.
6. The **keystone integration test**: "mighty things in the cold north" — the
   kind ⋈ instance join answered by c4's query engine over a test roster.

**Out (deferred; captured in `followups.md`):**

- **Genesis instantiation in shipped worlds.** No shipped world mints an
  instance this campaign; the ledger bytes of every world are unchanged (§3).
  The first campaign whose consumer *renders* instances mints them at genesis
  and owns that artifact-drift / census-regen call.
- **The per-tick lens cache** (metaplan §4.3 "cached within a tick") — c6: the
  tick is the cache's invalidation boundary and does not exist yet.
- **Kind-transition guards** — c6's capability schema (a system declaring
  `writes instance-of` can carry guards); c5's machine is deliberately open.
- **Structured-trait overrides** — c5 overrides numeric scalars only;
  niche/vector overrides need a Value-envelope story.
- **Phenomenon / Field query backends** (metaplan §4.4) — unchanged from c4's
  deferral.
- **Phantom `Id<T>` unification** — declined at decision-ledger #53; two
  distinct newtypes already make cross-indexing a compile error.
- **Generational / slotmap ids** — metaplan §12: instances stay `BTreeMap`;
  nothing may assume boundedness.

## 3. Determinism contract — the save-format line (leads G3)

**Every shipped world's ledger is byte-identical. The only serialized change
is concept-registry rows.**

- Genesis commits **zero** new facts: no instance is minted in a shipped world
  this campaign (the strangler-fig shadow discipline, metaplan §5 — the
  mechanism lands and is proven before any cutover writes production data).
  A pinned test asserts a genesis world contains no `instance-of` facts.
- `World::new` registers `instance-of` (kernel-core, precedent: `NAME`), and
  the species domain registers the two override trait predicates (mass,
  potency). `World { registry }` serializes, so **`world.json` fixtures and
  the concepts reference page drift by these additive registry rows only** —
  regenerated locally via `scripts/regenerate-artifacts.sh` (never the live
  census; the c4-retro trap). Almanacs render from the ledger and providers:
  byte-identical. Census `rows.csv`: no metric reads the registry — expected
  untouched, verified by diff at the gate. **No census regen. No epoch.**
- **Kind references serialize as labels** (metaplan §7): the `instance-of`
  object is `Value::Text(kind label)`. `KindId` itself remains build-state and
  is never serialized.
- **`instance-of` is non-functional.** A kind change (owlbear →
  awakened-owlbear) is a new fact with day + provenance — the observable,
  provenanced event the ledger exists for. A functional predicate would
  reject it as a Contradiction and block the UNI-7/BIO-11 arc. The current
  kind is the **last** committed `instance-of` fact (`kind_of`, §4.2); commit
  order is deterministic time order, so the read is total.
- **No stream draws.** Minting consumes no randomness; stream consumption
  order and all pin-isolation contracts are untouched.
- **`NonZeroU64` changes no bytes.** Ids were always minted from 1; JSON
  serialization of `NonZeroU64` is the plain number. Deserializing a forged
  id 0 now fails loudly — a tightening of the existing "0 is reserved"
  contract, not a change to it.

## 4. Architecture

### 4.1 `EntityId` becomes `NonZeroU64` (first, mechanical)

Measured blast radius: 40 `EntityId(` sites in 14 files. The two
non-sentinel `EntityId(0)` uses are test observer dummies (astronomy); the
`fact_index` range sentinels rework to `NonZeroU64::MIN` / `MAX` (all index
ranges are inclusive, so MIN = 1 is a valid bottom — no real id is excluded).
`mint_entity` mints `next_entity + 1`, which is never 0. A `const fn`
constructor (`EntityId::new(u64) -> Option<EntityId>` or equivalent) keeps
test literals readable; exact shape is the plan's call. The
`size_of::<Option<EntityId>>() == 8` `const`-assert lands with it (c4 §6).

### 4.2 `instance-of`: the kernel mechanism

Following the mechanism/population split (metaplan §4.7 — kernel defines,
worldgen composes):

- **Kernel** (`ledger.rs`): the `INSTANCE_OF` predicate constant, registered
  in `World::new`; `mint_instance(kind_label, day, provenance, &registry)` —
  mint + commit the `instance-of` fact in one operation, making the mint API
  the predicate's **sole writer** (the single-writer discipline of
  decision-ledger #39, applied by construction); `kind_of(&self, e) ->
  Option<&str>` — the label in the **latest** `instance-of` fact (via the SPO
  postings' maximum position). `kind_of` is deliberately distinct from
  `value_of` (first-wins, the functional-predicate read): the two reads
  encode the two predicate temporalities.
- **Worldgen**: the validated composition — minting against the union kind
  roster (§4.4); the kernel stays roster-blind.
- A kind *change* is just a second `mint`-less commit of `instance-of` on an
  existing subject, by whatever system owns the transition (tests, this
  campaign; systems, c6). An entity's kind history is thereby a state
  machine — states = kinds, transitions = day-stamped, provenanced facts —
  readable today via `facts_about` + filter; no named history read until a
  consumer wants one.

### 4.3 The lens and the join: override, else default

The typed instance component is a **derived lens, materialized per call,
never serialized, never cached** (this campaign):

```
effective trait T of instance e:
  1. value_of(e, T's predicate)            — the instance's own override fact
  2. else registry[kind_of(e)].T           — the kind's authored default
  3. else None                             — no kind fact, or dangling label
```

- First lens: `hornvale_species::instance_biosphere(&ledger, e, &biosphere)
  -> Option<BiosphereTraits>` — the kind's `BiosphereTraits` with the
  instance's numeric overrides (mass, potency) applied. It returns the plain
  effective `BiosphereTraits`; a distinct wrapper type waits for a consumer
  that needs to distinguish effective from authored (YAGNI).
- **Trait predicates are level-agnostic** — one predicate per trait; the
  subject (kind-representative entity vs instance) is the level. Precedent:
  genesis already commits kind-level trait facts (`THREAT_RESPONSE` et al.)
  under level-agnostic names. c5 registers only the two predicates its
  demonstrations override; names follow the concept-registry naming
  conventions (plan detail).
- **Overrides are per-instance, not per-(instance, kind): they survive kind
  change.** The unusually-large owlbear stays unusually large when awakened —
  its mass override outlives the transition. This is stated as a contract and
  exercised by the property battery (§5).
- **No cache.** Reads are O(log n) point lookups against c4's index; the
  metaplan's within-tick cache is c6's work, because the tick is its
  invalidation boundary. When it lands, `FactIndex`'s absent-or-complete
  `#[serde(skip)]` lifecycle (c4 #68) is the model.
- Label → registry lookup: `kind_of` returns `&str`; `ComponentStore` gains a
  by-label read (`get_by_label(&str)` or equivalent — `KindId`'s `&'static
  str` cannot be constructed from ledger text; comparison is by content).
  Plan detail.

### 4.4 The four non-species kinds: the component-set matrix

The constraint that shapes the whole section: **genesis mints a species
entity for every canonical `biosphere.ids()` row** (and the placement engine
reads the same roster). So no new kind may enter the canonical biosphere
registry this campaign, or worlds drift and deities get placed as fauna.

| kind | components (owner) | demonstrates | where it lives |
|------|--------------------|--------------|----------------|
| `deity` | `DeityTraits { manifest: bool }` (religion) | mind-adjacent kind, no body | canonical registries |
| `culture` | `CultureTraits { transmission: Oral \| Written }` (culture) | social aggregate — no body, no psyche | canonical registries |
| `granite`, `limestone` | `MaterialTraits { hardness_mohs: f64, sedimentary: bool }` (terrain) | body-stuff without agency; multiple kinds per component | canonical registries |
| `awakened-owlbear` | biosphere + the full peopled cluster | kind reached **by transition** (owlbear → awakened-owlbear fact) | **test/lab roster only** (`from_stores`) |

- The three new component types live in their owning domains; canonical
  `WorldComponents` gains three stores + `ComponentTag` variants (a
  composition-root edit — the one legal place domains meet). None of the new
  stores is genesis-iterated: **zero ledger drift**.
- `awakened-owlbear` needs biosphere + psyche, and `check_integrity` forces
  the full peopled cluster (psyche ⇒ perception / articulation / lexicon /
  family — it speaks; the c3 invariant is kept, not relaxed for "nascent").
  Canonical registries would place it as fauna, so it is composed in the
  test/lab roster via `from_stores` — the established Lab synthetic-kind
  path (serpent / goblin-twin).
- **The canonical kind roster becomes the union of all component stores'
  key-sets** (`WorldComponents::kinds()`), retiring "the biosphere store is
  the canonical entity set" (doc updated; genesis still iterates biosphere
  for species entities, unchanged). `kinds_with` is unchanged per tag.
  Worldgen's validated mint checks the label against this union.
- Deviations from metaplan §2's illustrative parentheticals, ledgered (#77):
  deity carries no canonical biosphere row (the genesis/placement constraint
  above; the mighty-instance composition is demonstrated in the test roster
  instead), and "nascent psyche" is the full peopled cluster (the integrity
  invariant outranks flavor). Component fields are deliberately thin and
  honest — `manifest`, `transmission`, Mohs hardness — enriching them is
  content work for consuming campaigns; **fidelity is Nathan's call at G3.**

## 5. Correctness — the ladder

Per the metaplan §6 ladder; the load-bearing new property is **JOIN ≡ SCAN**.

1. **Type-level.** The `NonZeroU64` niche (+ the `const`-assert); distinct
   `EntityId` / `KindId` newtypes; validating component constructors; the
   mint API as sole `instance-of` writer by construction.
2. **Property — JOIN ≡ SCAN (the keystone).** The lens over the index equals
   the naive computation (linear ledger scan + registry lookup), over random
   seeded ledgers, in the project's hand-rolled `for seed in 0..N` style.
   **The generator's value-space coverage is part of the review** (the c4
   lesson — an Entity-only generator exempted signed zero): it must cover
   override present / absent; kind changed 0, 1, and 2 times; overrides
   committed before *and* after a kind change (the survive-kind-change
   contract); numeric override values including both zero spellings; a
   dangling kind label (lens total: `None`, no panic); an entity with no
   `instance-of` fact at all. Plus `kind_of` ≡ last-fact-scan, and a
   serialization round-trip (world with instances → JSON → reload →
   identical `kind_of` / lens answers, exercising index rebuild).
3. **Equivalence / shadow.** A genesis world contains zero `instance-of`
   facts (the pinned no-cutover assertion); every committed artifact except
   the registry-bearing fixtures is byte-identical; the CI drift-check over
   the regenerated fixtures is the proof.
4. **The master oracle.** The census byte-identity drift-check — untouched
   fixtures, no regen (§3).

Every new test is mutation-verified before it counts (the standing
measure-don't-narrate discipline): break the join rule (swap override/default
precedence), drop `kind_of`'s latest-wins, drop the survive-kind-change
behavior — each must redden its test.

## 6. Perf budgets (rolled-our-own; no criterion)

- **Deterministic, gated:** `size_of::<Option<EntityId>>() == 8` (const);
  `size_of::<crate::fact_index::Symbol>() == 4` stays; lens fact-read count
  on a fixed synthetic world (a drift-checked count, not wall time).
- **Wall-time (heavy tier, ungated):** none new — the lens is O(k log n)
  point reads over an index c4 already benchmarked; `kind_of` is one
  postings lookup. No new unbounded structure (instances stay in the one
  ledger; metaplan §12 stands).

## 7. Stages (strangler-fig; each byte-identical, each independently testable)

1. **`NonZeroU64` `EntityId`** + sentinel rework + `const`-assert. Mechanical;
   proves the niche; everything downstream builds on the new API.
2. **`instance-of` + `mint_instance` + `kind_of`** (kernel), the pinned
   zero-instances genesis assertion, and the kind-change state-machine tests
   (the kernel is roster-blind, so these use bare labels; the composed
   owlbear → awakened-owlbear transition is stage 4's).
3. **The join + `instance_biosphere` lens** + the two override predicates +
   the JOIN ≡ SCAN battery (generator coverage per §5.2).
4. **The four kinds**: three component types / stores / `ComponentTag`
   variants, the union roster + validated mint, the `from_stores` test
   roster with `awakened-owlbear`, integrity tests.
5. **The keystone query test** (Drill 4: mighty things ⋈ cold-north place
   facts, answered by the c4 engine) + local artifact regen
   (`regenerate-artifacts.sh` — never the live census) + book (chronicle,
   freshness sweep, UNI-22 re-score) + retrospective + close.

## 8. Risks and open items

- **The registry rows touch every `world.json` fixture.** Additive and
  deterministic, but the regen must be complete (all three seed-42 almanacs
  are ledger-derived and should NOT drift — a partial regen that drifts an
  almanac is a signal, not noise). The census diff at the gate is the
  backstop.
- **Overrides-survive-kind-change** is the one semantic subtlety; it is
  stated as a contract (§4.3) and generator-covered (§5.2) rather than left
  to interpretation.
- **The union roster retires an assumption.** Any code that treated
  `biosphere.ids()` as "all kinds" is audited at plan time (grep the
  `kinds_with` / `biosphere.ids()` consumers); genesis's use is deliberate
  and unchanged.
- **`type-audit` now runs in `make gate`** (The Presiding): every new pub
  boundary (mint/kind_of signatures, the component fields) carries its
  verdict tag from the first commit.

## 9. Decisions (promoted from the campaign decision ledger #71–#79)

- **#72 — the save-format line (leads G3):** canonical `instance-of`
  predicate; shipped-world ledgers byte-identical (zero genesis facts);
  registry-rows-only drift, regenerated locally; genesis instantiation
  deferred to the first consuming campaign; no census regen, no epoch.
- **#73 — kind change:** `instance-of` non-functional; `kind_of` latest-wins;
  the kind history is an open state machine (no transition guards until c6's
  capability schema).
- **#74 — no lens cache in c5:** per-call materialization; the tick (c6) is
  the cache's invalidation boundary; FactIndex's lifecycle is the model.
- **#75 — level-agnostic trait predicates:** one predicate per trait, the
  subject is the level; overrides survive kind change.
- **#76 — `NonZeroU64` as task 1:** measured 40-site radius; sentinels to
  `NonZeroU64::MIN/MAX`; the `const`-assert lands here.
- **#77 — the component-set matrix:** deity/culture/material canonical but
  never biosphere-keyed (the genesis/placement constraint);
  `awakened-owlbear` in the test roster via `from_stores`; the union kind
  roster; thin honest fields (fidelity flagged to Nathan).
- **#78 — demonstration surface:** keystone join as integration tests; no
  lab metric, no almanac section, until instances ship in worlds.

## 10. Definition of Done

- All five stages green under `make gate` (which now includes type-audit);
  the full `cargo nextest run --workspace` before any FF; census fixtures
  untouched (verified by diff against the merge-base — distinguish any red
  from pre-existing debt).
- Byte-identity holds everywhere except the enumerated registry-bearing
  fixtures, regenerated locally in the same commit that registers the
  predicates (re-baseline in the drifting commit, never deferred to close).
- Book: a chronicle entry (candidate *The Individuation*), a freshness sweep
  (the components chapter's "biosphere is the entity set" line, the
  concept-registry chapter's new predicates), and a UNI-22 re-score noting
  campaign 5 shipped and campaign 6 (systems & schedule) is next.
- A one-page retrospective in `docs/retrospectives/` (including the
  scratch-ledger-lost-with-worktree process lesson, #79).
- The followup register's six rows promoted into the retrospective.
