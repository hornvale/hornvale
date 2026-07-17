# Systems & Schedule (ECS Campaign 6) — Design

Program: **The Entity-Component Substrate** (metaplan
`docs/superpowers/specs/2026-07-14-ecs-program-metaplan-design.md`, campaign 6;
registry **UNI-22**). Base: main `@1acbe98` (campaigns 1–5 shipped: The
Menagerie, The True Name, The Dissolution, The Concordance, The Individuation).
Chronicle-name candidate: **The Ordination** — order *ordained by the
declarations*, not decreed by a hand-written sequence; confirmed at close.

## 1. What this is

The substrate can now say what a kind is (c1–c3), answer any fact query in
O(log n) (c4), and give an entity an individuated, kind-changing identity (c5).
What it cannot yet say is **in what order its systems run, and why that order
is what it is.** Today that order is a hand-written sequence of genesis calls
in `windows/worldgen/src/lib.rs::build_to` — sky, then terrain, then
paleoclimate, then settlement, then culture, then religion, then species —
correct only because a human placed each call after the ones it depends on.
Nothing declares those dependencies; nothing checks them; adding a domain means
editing the central sequence by hand. That is precisely the positional,
hand-authored, anti-modular shape every prior campaign in this program replaced
with a **derived view over declared data.**

This campaign does it for order. Each system **declares the predicates it reads
and writes**; the schedule is the topological sort of the resulting
data-dependency DAG, tie-broken by stable label (metaplan §4.6). The schedule,
the query engine (c4), and reflection are then three derived views over one
capability schema — the self-describing substrate UNI-21 names.

**The keystone framing (the campaign's core insight): genesis is tick 0.** A
running simulation would rotate a bulk-synchronous cycle — read the frozen
tick-N snapshot, accumulate writes, commit tick-N+1 in stable-label order
(decision-ledger #40, ratified). Genesis is the once-run, from-seed **first
half-turn** of exactly that cycle. So c6 builds the cycle's machinery — the
schedule and the BSP step mechanism, as kernel types — and runs exactly its
first turn (the existing genesis pipeline, formalized as declared systems),
proving the mechanism **without the ongoing rotation.** The running liveness
tick (ticks 1..N — GOAP, belief, social systems generating new sim-time facts)
is a future arc (§2 Out).

Every prior ECS campaign shipped byte-identical (c4) or in shadow (c5). This
one is **byte-identical**: it adds build-state declarations, a derived
schedule, a tested tick mechanism, and a load-time check — and commits no fact,
registers no predicate, changes no serialized byte.

## 2. Scope

**In (the metaplan §5 campaign-6 row, exactly):**

1. **Systems as declarations.** A kernel `System` type: a stable-label id plus
   the predicate sets it reads and writes. Worldgen composes one declaration
   per genesis stage (the composition root; mechanism/population split §4.7).
   NOT a `Domain`-trait member (its doc forbids new members — "future per-domain
   behaviors get their own traits").
2. **The capability schema + the derived schedule.** A kernel type holding the
   systems' declarations, and a `schedule()` that returns the topological order
   of the read/write DAG, tie-broken by stable label. A pure function of the
   declarations; a derived view, never serialized.
3. **The keystone — SCHEDULE ≡ HAND-ORDER.** A property/assertion that the
   derived schedule is a valid linearization **consistent with the genesis
   pipeline's existing hand-order** — the c-series X≡reference keystone (c4
   INDEX≡SCAN, c5 JOIN≡SCAN). Genesis keeps executing through `build_to`; the
   schedule *validates* that sequence (shadow), it does not replace it.
4. **The single-writer-per-functional-predicate check** (metaplan §7), at load,
   fails-loudly (mirroring worldgen's orphan-family `check_integrity`). **Run
   against the real declarations and measure** — its most valuable outcome is a
   violation it surfaces (§5).
5. **The BSP tick mechanism made concrete.** A kernel type for one
   bulk-synchronous step — read a frozen snapshot, accumulate writes, commit in
   stable-label order — exercised by a **toy** system in a kernel test that
   proves the machinery (determinism, one-tick latency, stable-label commit
   order). No real domain implements a runnable step this campaign.

**Out (deferred; captured in `followups.md`):**

- **The running liveness tick (ticks 1..N).** The actual liveness systems
  (PSY-6 GOAP, UNI-16 belief, SOC-9 social) generating new sim-time facts —
  §9's game/liveness layer, world-changing, no consumer yet. c6 gives them the
  schedule and the mechanism to eventually run on; it does not build them.
- **Genesis executes through the derived schedule** (the cutover). Needs a
  uniform runnable `step()` — genesis stages have domain-specific signatures
  and cannot run in a derived order until then; rides the running-tick arc.
- **Same-tick immediate effects** (§3.6's opt-in) — a running-tick concern; the
  schedule that orders such a chain ships here, the opt-in itself does not.
- **The `systems` reflection page** (a `concepts`/`streams`-style CLI/book dump
  of the derived schedule) — additive, deferred (followup 4).
- **Spatial-partition parallelism** (metaplan campaign 7) — unchanged deferral.

## 3. Determinism contract — the save-format line (leads G3)

**Byte-identical. No new fact, no new predicate, no serialized change. No
epoch, no census regen.**

- System declarations, the capability schema, the schedule, and the tick
  mechanism are **build-state derived views and load-time validation**. The
  schedule is a pure function of the declarations (store-irreducible-
  derive-reversible, decision 0037) — never serialized.
- Genesis is **unchanged**: it still runs through `build_to`'s hand-order and
  commits exactly the facts it commits today. The schedule *observes and
  validates* that order; it does not reorder execution (§2, #84). So every
  committed artifact — worlds, almanacs, the census, lab studies — is
  byte-identical, and the existing drift-check is the proof (the master oracle,
  metaplan §6).
- The single-writer check reads declarations; it commits nothing.
- **The one determinism risk, stated plainly:** if the single-writer check (§5)
  surfaces a functional predicate with two writers whose *reconciliation*
  changes genesis commit order (e.g. splitting a predicate, moving a write),
  **that** would break byte-identity and would take an epoch or a re-baseline.
  The drift-check catches it; if it materializes it leads the G6 digest and is
  Nathan's call. The expected case is no reconciliation needed — Hornvale's
  domain-prefixed predicate naming (`species-name`, `deity-name`,
  settlement-owned `name`, …) is *already* largely the single-writer discipline;
  the check formalizes and enforces a convention mostly in place.

## 4. Architecture

### 4.1 The `System` declaration (kernel)

```
struct System {
    label: &'static str,          // stable id; the schedule tie-break key
    reads:  BTreeSet<&'static str>,  // predicate names this system reads
    writes: BTreeSet<&'static str>,  // predicate names this system writes
}
```

A declaration, not a behavior. `BTreeSet` for deterministic iteration
(no-`HashMap` rule). Predicate names are the existing registry keys (the same
`&'static str` predicate constants domains already export). This is the unit
UNI-21 calls the capability schema; the mechanism/population split (§4.7) puts
the *type* in the kernel and the *instances* in worldgen.

### 4.2 The capability schema + `schedule()` (kernel)

```
struct CapabilitySchema { systems: Vec<System> }   // build-state, never serialized

impl CapabilitySchema {
    fn schedule(&self) -> Result<Vec<&'static str>, ScheduleError>;
    fn single_writer_check(&self, registry: &ConceptRegistry) -> Result<(), ScheduleError>;
}
```

- **`schedule()`** builds the dependency DAG — an edge `A → B` whenever a
  predicate in `A.writes` is in `B.reads` — and returns the systems' labels in
  topological order, **ties broken by ascending stable label** (never
  registration position — the positional-tag bug one level up, metaplan §4.6).
  A cycle is a `ScheduleError::Cycle` naming the systems involved (a genuine
  design error to fix, not to resolve silently). Determinism: a `BTreeMap`-
  backed Kahn's algorithm with a stable-label ready-queue yields one canonical
  order.
- **`single_writer_check()`** (§4.4).

Both are pure functions of the declarations — the third view over the capability
schema, beside c4's query engine and reflection.

### 4.3 The BSP tick mechanism (kernel, toy-tested)

The bulk-synchronous step made concrete (decision-ledger #40):

```
trait TickSystem {                      // the runnable interface (mechanism)
    fn label(&self) -> &'static str;
    fn step(&self, frozen: &Ledger) -> Vec<Fact>;   // read frozen tick-N, return writes
}

fn tick(frozen: &Ledger, systems: &[&dyn TickSystem],
        order: &[&'static str], registry: &ConceptRegistry) -> Result<Ledger, LedgerError>;
```

`tick` runs each system's `step` against the **frozen** tick-N ledger (reads see
no mid-tick writes — the simultaneity guarantee), gathers all writes, and
commits them into a tick-N+1 ledger **in `order`** (the schedule) then by
stable-label — deterministic, single-threaded commit. Cross-system effects have
one tick of latency by default (a reader sees a writer's fact next tick), which
the test asserts. **Exercised only by a toy `TickSystem` in a kernel test** —
it proves the machinery (determinism across runs, frozen-snapshot isolation,
stable-label commit order, one-tick latency). No real domain implements
`TickSystem` this campaign; genesis is not rerouted through `tick` (#84).

### 4.4 The single-writer contract (kernel + worldgen)

`single_writer_check` scans the declarations: for every predicate that is
**functional** (per the `ConceptRegistry`), assert **at most one** system
declares it in `writes`; a second writer is `ScheduleError::MultipleWriters {
predicate, systems }`. Called at worldgen load beside the existing
`check_integrity` (the orphan-family fails-loudly precedent). Non-functional
predicates (e.g. `instance-of`, `located-in`) may have many writers — the
contract is functional-only (§7).

**This is a measurement (§5).** The expected result on today's declarations is
a pass (domain-prefixed naming already separates writers), but the check is run
for real and its outcome reported — if it finds a violation, that is the
campaign's most valuable finding, reconciled per §5 / the §3 risk note.

### 4.5 Worldgen composition — one declaration per genesis stage

Worldgen (the composition root) builds the `CapabilitySchema` from one `System`
declaration per genesis stage in `build_to`: `sky`, `terrain`, `paleoclimate`,
`settlement`, `culture`, `religion`, `species` (plus the seed-only `world-entity`
source). Each declaration's `reads`/`writes` are the predicate constants that
stage actually reads and commits — derived by reading the genesis code, and
**pinned by the SCHEDULE ≡ HAND-ORDER keystone**: if a declaration is wrong
(omits a real dependency), the derived schedule diverges from the hand-order and
the keystone test reddens. The declarations are thus self-checking against the
one ground truth (the working pipeline).

## 5. Correctness — the ladder

Per metaplan §6; the load-bearing new property is **SCHEDULE ≡ HAND-ORDER**.

1. **Type-level.** `BTreeSet`/`BTreeMap`/`Vec` only; `ScheduleError` makes a
   cycle and a multi-writer unrepresentable-as-success (they are `Err`, checked
   at load); stable-label tie-break (never position).
2. **Property — SCHEDULE ≡ HAND-ORDER (keystone).** The derived schedule is a
   valid topological linearization of the declared DAG **and** is consistent
   with the genesis hand-order: every dependency edge the hand-order respects,
   the schedule respects, and the stable-label tie-break resolves the remaining
   freedom to one canonical order. Realized in the project's seeded-loop style
   over the real worldgen declarations plus randomized synthetic schemas
   (generator coverage per the c4/c5 lesson: cover a DAG with a diamond, a
   multi-root source set, a chain, ties requiring the label break, and a
   deliberate cycle → `Err`). Plus: the single-writer check is exercised with a
   deliberate two-writer schema (→ `Err`) and the real declarations (→ measured
   outcome).
3. **The BSP mechanism** (§4.3): a toy two-system test asserting frozen-snapshot
   isolation (a reader does NOT see a same-tick write), stable-label commit
   order, and run-to-run determinism.
4. **Equivalence / master oracle.** The committed-artifact byte-identity
   drift-check — c6 commits nothing, so it passes unchanged, the proof the
   campaign is behaviour-preserving.

Every new test is **mutation-verified** before it counts (the standing
measure-don't-narrate discipline): break the tie-break (use registration
position) → the keystone must redden; drop a real dependency edge from a
declaration → the keystone must redden; give two systems the same functional
write → the single-writer check must redden. Report each.

## 6. Perf budgets (rolled-our-own; no criterion)

- **Deterministic, gated:** the derived schedule of the real worldgen
  declarations is a fixed, drift-checkable sequence (a `const`/study assert on
  the system order); `size_of` of `System`/`CapabilitySchema` if a niche is
  claimed (none expected).
- **Wall-time:** none new — `schedule()` runs once at load over ~8 systems;
  Kahn's algorithm over a handful of nodes is not a measured hot path. No new
  unbounded structure.

## 7. Stages (each byte-identical, each independently testable)

1. **The `System` declaration + `CapabilitySchema` + `schedule()`** (kernel):
   the type, the DAG build, Kahn's topological sort with stable-label tie-break,
   `ScheduleError::Cycle`. Property tests over synthetic schemas (diamond, chain,
   multi-root, tie, cycle→Err). Mutation-verify the tie-break.
2. **The single-writer check** (kernel): `single_writer_check`; the two-writer
   synthetic → `Err`; functional-only (non-functional multi-writer allowed).
3. **Worldgen declarations + the SCHEDULE ≡ HAND-ORDER keystone**: one `System`
   per genesis stage; the keystone test (derived schedule consistent with
   `build_to`'s order); wire `single_writer_check` into the worldgen load path
   beside `check_integrity` and **report its measured outcome on the real
   declarations**. Mutation-verify (drop a dependency edge → keystone reddens).
4. **The BSP tick mechanism** (kernel): `TickSystem` + `tick`; the toy
   two-system test (frozen isolation, one-tick latency, stable-label commit
   order, determinism).
5. **Verify + close**: full `cargo nextest run --workspace`; the artifact
   drift-check (byte-identical — commits nothing); the schedule `const`/study
   assert; book (chronicle, freshness sweep, UNI-22 re-score); retrospective;
   close.

## 8. Risks and open items

- **The declarations must match reality.** A wrong `reads`/`writes` set is
  caught by the keystone (schedule diverges from hand-order) — the declarations
  are self-checking against the working pipeline, which is why the keystone is
  the campaign's spine, not a nicety.
- **The single-writer check may surface a real multi-writer** (§3 risk note).
  Expected pass; if it fails, reconcile per §5 and — only if the fix moves
  genesis commit order — treat as a byte-identity event for Nathan (G6).
- **Genesis order has freedom the hand-order fixed arbitrarily.** Where two
  stages are truly independent (no read/write edge between them), the schedule's
  stable-label tie-break may pick a different order than `build_to` did. If
  those two stages both draw from the seed or both commit facts, a different
  order could change draw/commit order → the keystone would catch it. The
  keystone test's job is exactly to confirm the tie-break's order matches the
  hand-order for the *independent* pairs too; if it does not, either the
  hand-order encodes a hidden dependency (add the declaration edge) or the two
  are genuinely commutative (byte-identical either way — assert it). This is the
  delicate case and stage 3's real work.
- **`type-audit` runs in `make gate`** (The Presiding): every new pub boundary
  (`System`, `CapabilitySchema`, `schedule`, `TickSystem`, `tick`) carries its
  verdict tag from the first commit.

## 9. Decisions (promoted from the campaign decision ledger #82–#87)

- **#83 — scope (leads G3):** genesis is tick 0; c6 builds the schedule + BSP
  mechanism and runs its first turn (genesis-as-declared-systems), byte-
  identical; the running liveness tick is deferred.
- **#84 — shadow, not cutover:** the keystone is SCHEDULE ≡ HAND-ORDER; genesis
  keeps executing via `build_to`, the schedule validates it; execution-cutover
  deferred (needs uniform `step()`).
- **#85 — `System` is a declaration, not a `Domain` member;** the runnable
  `TickSystem`/`tick` is the mechanism, toy-tested, not domain-implemented.
- **#86 — single-writer check is a measurement:** run it for real, report the
  outcome, reconcile a violation if found.
- **#87 — byte-identical, no epoch/census;** the one risk is a single-writer
  reconciliation that moves commit order (drift-check catches it, Nathan's call).

## 10. Definition of Done

- All five stages green under `make gate` (incl. type-audit); the full
  `cargo nextest run --workspace` before any FF; census fixtures untouched
  (byte-identical — c6 commits nothing; verified by diff against the merge-base).
- The keystone SCHEDULE ≡ HAND-ORDER green; the single-writer check's measured
  outcome on the real declarations reported in the close.
- Book: a chronicle entry (candidate *The Ordination*), a freshness sweep (any
  chapter describing the genesis pipeline order as hand-authored), and a UNI-22
  re-score noting campaign 6 shipped and campaign 7 (spatial partition) is the
  last substrate campaign.
- A one-page retrospective in `docs/retrospectives/`.
- The followup register's rows promoted into the retrospective.
