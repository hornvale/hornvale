# The Kerf: One Index Fewer — A Campaign Design

**Status:** draft (G3) · **Date:** 2026-09-04 · **Branch:** `campaign/the-kerf`
· **Decision block:** 0726–0735

A kerf is the slot a saw takes out. This campaign's whole deliverable is
subtraction: one of the resident fold store's five tenants is a strict
projection of another, and it goes. Nothing a world does changes.

---

## 0. What this campaign is, and the thing it is not

It is the redemption of a debt The Pawl recorded rather than paid. Its
chronicle says so in as many words:

> **And one redundancy shipped knowingly.** Three per-entity indexes now fold
> the same stream of sightings: the trail, the per-room visit lists, and the
> water belief — where the second is derivable from the first and the third
> from the second. Merging them is recorded as owed rather than optional. The
> cost of leaving it is memory and catch-up work, never a wrong answer, which
> is why it was allowed to ship and why it is written down.

The registry row is `TOOL-known-water-is-latest-visit`; the retrospective's
item 7 says the same thing.

**It is not the other two merges that sentence invites.** `LatestVisit` is
NOT merged into `Trail`, and §2.2 gives the criterion that refuses it rather
than an opinion. `is_water` is not indexed per room
(`TOOL-place-predicate-index`), and `plan_to_room`-per-known-water-room —
which The Detent measured as the belief read's actual cost — is not touched
(`TOOL-known-water-plan-per-water-room`). Those are named here so that a
reader who arrives at this file looking for them finds the refusal and its
reason instead of an absence.

**It is not a behaviour change.** The output of every read is byte-identical
by construction (§5) and the campaign-time hash witness is the empirical
check.

---

## 1. The finding, restated

`windows/vessel/src/resident.rs` holds five tenants. Three of them fold the
same predicate, `agent-at`, and they form a chain in which each is a strict
projection of the one before it:

```
Ledger  >  Trail  >  LatestVisit  >  KnownWater  >  prefix_len
```

- `Trail` keeps every sighting as `(day, room)`, ascending by `(day, room)`.
- `LatestVisit` keeps the same pairs keyed the other way round: per room, the
  ascending list of instants that room was visited.
- `KnownWater` keeps, per room, one instant — the FIRST visit.

`LatestVisit`'s per-room list is kept ascending by a sorted insert
(`days.partition_point(|d| *d <= day)`), so a room's first visit **is**
`days[0]`. `KnownWater` holds no bit that `LatestVisit` does not already
hold, and its one read has the same complexity on either.

### 1.1 The reads, against the indexes that can serve them

```
                prefix_len   latest_at   rooms_at    water_at
  Trail           O(log h)     O(h)        O(h)        O(h)
  LatestVisit       --      O(r log v)   O(r)        O(r)
  KnownWater        --      IMPOSSIBLE   O(r)        O(r)
```

`h` = the entity's history length; `r` = its distinct rooms; `v` = visits per
room.

Two things are visible in that table and neither is visible in the prose the
campaign inherited. First, `KnownWater`'s row is a strict subset of
`LatestVisit`'s: every read it can serve, `LatestVisit` serves in the same
complexity class. Second, the reverse is not true — a first-visit map
**cannot** answer `latest_at` at all, because the information is gone. So the
collapse has exactly one legal direction, and it is the one this campaign
takes.

### 1.2 The criterion, so the next campaign does not have to re-derive it

> **A resident index earns its keep only if some read it serves is
> asymptotically cheaper on it than on its parent in the projection chain.**

Applied to the chain above:

| index | over its parent | the read that pays for it | verdict |
|---|---|---|---|
| `Trail` | the ledger | `prefix_len`: O(log h) vs O(h) | earns it |
| `LatestVisit` | `Trail` | `latest_at`: O(r log v) vs O(h) | earns it |
| `KnownWater` | `LatestVisit` | `water_at`: O(r) vs O(r) | **earns nothing** |

That third row is this campaign. The second row is why `LatestVisit` is not
merged into `Trail`: the emitter path's per-room `partition_point` is what
keeps `hazard_memory_memo` O(rooms) rather than O(history), which is the
entire result The Pawl's Task 5 shipped. Merging it back would undo a
measured win to save a copy of a structure whose per-entry cost §4 measures.
The criterion says so without appeal to anyone's judgement, which is the
point of writing it down.

---

## 2. The architecture

### 2.1 `water_at` moves onto `LatestVisit`; `KnownWater` is deleted

`KnownWater`'s only read is `water_at(entity, t, terrain)` — the water rooms
the entity had stood in at or before `t`, ascending by room. On `LatestVisit`
that is the same predicate over the same key set with `days.first()` in place
of the stored first-visit instant.

`LatestVisit` already has a read of exactly that shape: `rooms_at`, the alarm
scan's domain, which is `water_at` with the terrain filter removed. The two
are one question — *the distinct rooms whose first visit is at or before `t`,
filtered by a static predicate over places* — and this campaign states the
membership rule once rather than twice:

```rust
/// Private. The rooms whose FIRST visit is at or before `t`, filtered.
fn rooms_at_where(&self, entity: EntityId, t: WorldTime,
                  keep: impl Fn(&Facet) -> bool) -> Vec<Facet>;

pub fn rooms_at(&self, entity: EntityId, t: WorldTime) -> Vec<Facet>;
pub fn water_at(&self, entity: EntityId, t: WorldTime,
                terrain: &dyn Terrain) -> Vec<Facet>;
```

Writing `water_at` as a second, independent copy of the first-visit test
would re-create at the method level exactly the redundancy this campaign
exists to remove, one scale down. It would also miss what the shape is: this
is the `TOOL-place-predicate-index` form — a fold over visits crossed with a
static predicate over places — and naming it that way costs nothing and
connects the type to a row that is already in the registry.

**`first()`, never `[0]`.** An empty `days` vector cannot occur (the entry is
created by the insert that fills it), but the read has no panic path and the
spec says so rather than relying on the invariant.

**`water_at`'s correctness depends on `LatestVisit`'s ascending invariant**,
and that dependency is new: today only `rooms_at` leans on it. A future
tenant that stored visits in commit order would break `water_at` silently on
any world where a room's sightings commit out of day order. §4 pins it with a
fixture that commits one room's sightings in DESCENDING day order, so
`days[0]` differs from the first fact absorbed.

### 2.2 What else moves

| site | today | after |
|---|---|---|
| `ResidentFolds::advance` | five `advance_to` calls | four |
| `ResidentFolds::position` | four equality asserts | three |
| `ResidentFolds::known_water_and_trail` | `(&KnownWater, &Trail, &mut ReadWitness)` | renamed `latest_visit_trail_and_witness`, `(&LatestVisit, &Trail, &mut ReadWitness)` |
| `ResidentFolds::known_water` | plain accessor, test-only reader | deleted; tests take `latest_visit` |
| `liveness::believed_water` | `known.water_at(..)` | `latest.water_at(..)` — nothing else in the function moves |
| `ReadWitness::note_belief` | unchanged | unchanged — the belief read still runs at past instants and rule 6 still holds |

The `ReadWitness` belief counters are untouched on purpose: the rule-6
question ("does a production belief read run before a committed sighting?")
is a question about the read, not about which tenant serves it, and its
answer — measured, fired — is what makes the `first_visit <= t` filter
load-bearing in the first place.

### 2.3 What is deliberately not done

- **`LatestVisit` into `Trail`** — refused by §1.2's criterion, on a measured
  number rather than a preference.
- **An `is_water` index per room** (`TOOL-place-predicate-index`) — The
  Detent measured the 121 samples per call NOT to be the belief read's cost.
- **`plan_to_room` per known water room**
  (`TOOL-known-water-plan-per-water-room`) — the belief read's actual cost,
  99.08% of the six timed folds at the final band. It is the next quarry and
  it is a behaviour-risk campaign of its own; this one must not disturb it,
  which is why §4 measures the belief read's own time rather than assuming a
  subtraction cannot slow it down.
- **A memo over `water_at`'s result** — considered and rejected. Its key
  would have to carry the terrain as well as `(entity, t)`, `t` varies per
  read, and the answer is already O(distinct rooms). Recorded so that the
  absence is a decision.

---

## 3. Decision rules, not predictions

1. **The hash constants' positive control comes FIRST.** Task 1 mutates
   `KnownWater::absorb`'s min-keeping branch (`if day < *first` →
   `if day > *first`) and runs the candidate scripts BEFORE any constant is
   written down. *The hash moves on a script* → mint the constant on that
   script. *It does not move on the seed-42 fixed script* → that script does
   not reach the fold under migration; mint on `WALKING_SEED` and/or
   `EMITTER_SEED` instead, and record which scripts were blind and why. *It
   moves on none of them* → STOP: there is no identity proof available and
   the campaign has no way to know it changed nothing.
2. **The chaos-schedule witnesses' cost.** The standing rule is that a
   witness costs at most 60 s. *Under 60 s* → keep the script and record the
   measured number at the test. *Over* → shorten the script (waits, ticks,
   agents), re-measure, and record BOTH numbers and what reach was lost.
   Never drop a floor to buy time; a shorter witness is a smaller sample, a
   floorless one is not a witness.
3. **The non-vacuity floors.** Each shape must show (a) at least one entity
   with a non-empty `water_at` at the final instant, and (b) at least one
   comparison at an instant strictly before that entity's last committed
   sighting. *Both met* → proceed. *(a) unmet* → the `is_water` filter is
   vacuous on this shape; choose a shape where it is not (the possession
   shape's seed is the lever). *(b) unmet* → the first-visit prefix is not
   exercised; drive the comparison at explicitly chosen past instants rather
   than only at the ledger end. *Unmeetable at any script length* → STOP and
   re-read which shape reaches `believed_water` before building more.
4. **The retargeted oracle still discriminates.** Stage 2 changes what the
   Stage 1 tests compare against. Before the stage is called done, mutate
   `LatestVisit::water_at`'s membership test (`days.first()` →
   `days.last()`) and watch every one of those tests go RED. *Red* → the
   retarget kept the discrimination. *Green* → the test was pinned to the
   type, not to the behaviour; fix the test, not the mutation.
5. **The belief read's own time.** §4 measures µs/call before and after.
   *Within run-to-run spread* → report it as unmoved, with the spread.
   *Materially slower* → the larger value type is costing cache locality;
   the finding goes in the chronicle and the campaign reconsiders whether
   `water_at` should copy the first-visit instants into a read-side vector.
   *Faster* → report it; one fewer tenant advancing on every read is a
   plausible mechanism and the number, not the mechanism, is the claim.
6. **Sibling campaigns in `windows/vessel`.** Several branches are live in
   this crate. Absorb `main` at the stage boundary; if an absorption moves
   `resident.rs`, re-record every hash constant MAIN-FIRST (decision 0541)
   and re-take the BEFORE reading — a constant minted against a merge base
   that has moved is not a control.

---

## 4. Preregistration

Frozen before the code that would move it (decision 0016). All three
quantities are taken on `windows/vessel/examples/session_length_scaling.rs`,
seed 42, 50 agents, 200 ticks, bands of 20 — the shape The Detent's M1 used,
so the numbers are comparable to the ones already in that spec's §12.4 table.
BEFORE readings are taken in Stage 1 on the merge base; AFTER readings in
Stage 2.

- **K1 — held bytes.** New `entries()` and `held_bytes()` on `Trail`,
  `KnownWater` and `LatestVisit`, summed exactly the way
  `GroundHazards::held_bytes` and `FrighteningGround::held_bytes` already sum
  theirs, and carrying the identical caveat: an ESTIMATE of held data, not an
  allocator measurement — it counts neither `BTreeMap`/`Vec` overhead nor
  allocator slack. Reported per band. **Prediction: `KnownWater`'s held bytes
  and entries at band 10 are the campaign's saving, exactly, and nothing
  else's row moves.** A moved `Trail` or `LatestVisit` row is a finding, not
  a rounding error.
- **K2 — advance work per fact.** A cold `ResidentFolds` advanced over the
  band's whole ledger, timed, divided by `ledger.len()`: ns/fact. This
  isolates `absorb` from every other term in a tick, its denominator is exact,
  and it is the quantity the registry row calls "one tenant's advance from
  every read". **Prediction: it falls.** The size of the fall is the
  measurement; no number is predicted, because the three folds do different
  amounts of work per fact and this spec has not measured their split.
- **K3 — the belief read's own time.** µs/call for `believed_water`. **The
  instrument already exists and this campaign builds none**: the bench's
  `probe_believed_water_us` and `probe_shared_believed_water_us` already
  report `believed_water_us` and `shared_believed_water_us` per band, and
  each carries its own vacuity check. **That check is a `println!`, not a
  panic, and this line said "panic" until Task 2 read it** — it fails nothing,
  and it has been firing at every band since The Detent. What it reports is
  that the bench's PROBE agent (the roster's max-history member) holds an
  empty belief set; a roster sweep at Task 2 found 11 of 50 members holding
  real sets, up to 46 rooms, so the probe is unrepresentative and the column
  must never be read as a statement about `believed_water` across the roster.
  For THIS campaign that is a gift rather than a defect: with the probe's set
  empty, `plan_to_room` contributes nothing and the column isolates exactly
  the function Task 4 rewrites. Checked in the source before this
  line was written, rather than assumed from the fact that The Detent quotes
  a per-call figure. The BEFORE and AFTER readings simply record the column.
  `water_at` moves from walking a `BTreeMap<Facet, WorldTime>` to walking a
  `BTreeMap<Facet, Vec<WorldTime>>`: the same key count, a larger value, so
  the class is identical and the constant may move. **This is measured rather
  than argued**, because "a subtraction cannot make anything slower" is
  precisely the shape of confident claim this project has been wrong about
  before.
- **FOLD-equals-SCAN on real shapes.** `known_water_scan_oracle` — the
  verbatim copy of the pre-Pawl `believed_water` set-building loop, and the
  only statement of it that survives anywhere — is kept VERBATIM and its
  comparand becomes `LatestVisit::water_at`. Equality is proven under both
  chaos schedules (discard and rebuild at every position, and at every third
  position) on the **possession shape** (a real `Session` at `WALKING_SEED`,
  a short fixed script) and the **lab shape**
  (`the_detent::bench_shape`, the in-test reconstruction of a `windows/lab`
  run), at every prefix, for every roster entity, at instants that include
  past ones. With §3 rule 3's two floors.
- **The ascending-invariant fixture.** A hand-built ledger commits one room's
  sightings in DESCENDING day order, so the first fact absorbed for that room
  is not its first visit. `water_at` must admit that room at the EARLIEST of
  its instants and not at the first one committed.
- **Determinism.** No `f64` is combined differently; no float is added,
  removed or reordered. The read's output is a `Vec<Facet>` in ascending
  `Facet` order on both sides (§5).
- **No `HashMap`, no wall clock** outside the `--release` examples that
  already carry the scoped `#[allow(clippy::disallowed_types)] // benchmark
  harness`.

---

## 5. Determinism contracts (lead the G3 flagged section)

**No save-format surface is touched.** Nothing here is serialized: the
resident store has no `Serialize`, is not reachable from `World`, and holds
only values the ledger re-determines (decision 0536). No seed label, no
stream, no draw, no predicate, no concept.

**The output is byte-identical by construction**, and the argument has four
steps, each checkable against the code rather than believed:

1. **The key sets are identical.** `KnownWater::absorb` and
   `LatestVisit::absorb` guard on the same three conditions in the same order
   — `predicate == AGENT_AT`, `Value::Text`, `fact.day.is_some()` — and key
   by the same `room_from_text(s)`. Every fact one admits, the other admits.
2. **`days[0]` is the minimum.** `LatestVisit::absorb` inserts at
   `days.partition_point(|d| *d <= day)`, so the list is ascending at every
   position; the minimum is its first element. `KnownWater::absorb` keeps a
   running minimum. The two agree at every prefix.
3. **The iteration order is the same.** Both are `BTreeMap<Facet, _>` and
   both reads yield rooms ascending by `Facet`. The `Vec<Facet>`
   `believed_water` receives is the same sequence.
4. **Nothing downstream moves.** `believed_water`'s `plan_to_room` ranking,
   its `(hops, Facet)` tie-break and the guard-drop before the ranking are
   untouched.

**The drift check cannot see this.** No committed artifact carries a ticked
session ledger (The Pawl §5, verbatim; The Detent §5, verbatim). So the proof
is the campaign-time hash witness under decision 0541: Task 1 mints
constants from the merge base with a positive control run FIRST (§3 rule 1),
re-records them MAIN-FIRST after every absorption, and retires them at close
— leaving the constant-free two-fresh-runs-agree witnesses with their floors,
and the constants plus their control recorded as dated history in the module
doc and the chronicle.

---

## 6. The stage carve

**Stage 1 — the instrument, minted before the cut.** Everything that must
exist while the old code still does.

- Task 1: hash constants + their positive control (§3 rule 1).
- Task 2: `entries()`/`held_bytes()` on all three indexes; the three new
  bench readouts (K1, K2, K3); the BEFORE reading, recorded.
- Task 3: the chaos-schedule equality tests on the possession and lab shapes,
  and the descending-order fixture — all against TODAY's `KnownWater`. They
  must be green before the change or they are not an oracle.

→ **stage gate** (`make sluice-stage`).

**Stage 2 — the cut.**

- Task 4: `rooms_at_where`; `water_at` onto `LatestVisit`; delete
  `KnownWater`; retarget the accessor, `advance`, `position` and every test.
  §3 rule 4's mutation proof.
- Task 5: the AFTER reading; the hash constants unchanged; retire them; DoD.

→ **merge** (`make sluice`).

---

## 7. In / out

**In.** `windows/vessel/src/resident.rs`; `believed_water`'s one line in
`windows/vessel/src/liveness.rs`; `windows/vessel/tests/suite/resident_folds.rs`;
`windows/vessel/examples/session_length_scaling.rs`; a doc reference in
`windows/vessel/src/session.rs`; the DoD artifacts.

**Out.** Every item in §2.3. Any change to what a world does. `windows/lab`,
`windows/worldgen`, the kernel, the clients, the census.

---

## 8. Decisions this campaign will need (block 0726–0735)

- **0726.** A resident index earns its keep only if a read it serves is
  asymptotically cheaper on it than on its parent (§1.2). The durable half of
  this campaign: it is the rule that both deletes `KnownWater` and refuses the
  `Trail` merge, and it binds future tenants.
- **0727.** (provisional) The place-predicate reads of one index state their
  membership rule once (§2.1) — i.e. `rooms_at`/`water_at` share
  `rooms_at_where`. Minted only if the implementation finds a reason the rule
  is worth binding beyond this type.

Further numbers in the block stay unminted unless execution produces a
question worth settling; an unused reservation is cheaper than a collision.

---

## 9. Frontier bookkeeping

- `TOOL-known-water-is-latest-visit` → **shipped**, with the measured saving
  and a **Where** cell pointing at this spec's §11.
- `TOOL-place-predicate-index` → its `KnownWater::water_at` clause is
  rewritten to name `LatestVisit::water_at`, and the row gains the note that
  `rooms_at`/`water_at` are now one form with two predicates.
- `TOOL-known-water-plan-per-water-room` → untouched except to record that it
  is now the ONLY remaining item on this axis, and that K3 measured whether
  this campaign moved it.

---

## 10. Operational notes for the implementer

- `windows/vessel` is busy: absorb `main` at the stage boundary and re-record
  MAIN-FIRST (§3 rule 6).
- Adding `pub` methods drifts `docs/audits/type-audit-report.md`; every new
  pub-boundary primitive needs its `type-audit:` verdict tag and
  `make rebaseline` runs before the commit that adds them.
- `bench_shape` is `pub` in the `the_detent` test module and reachable as
  `crate::the_detent::bench_shape` from `resident_folds.rs` — the same
  cross-module path `the_detent.rs` already uses for
  `crate::ledger_hash_witness::*`.
- The bench is a `--release` example; time it there, never in a test.
- Cost-order the iteration, but a crate-scoped green is not a branch green:
  the workspace enforcement tests live in `cli/`.

---

## 11. What shipped, measured

`KnownWater` is gone. `LatestVisit::rooms_at_where` now owns the one
first-visit membership rule; `rooms_at` supplies an always-true predicate and
`water_at` supplies `is_water`. The independent fold-equals-scan witnesses
cover both real shapes and the descending-order writer shape, so the equality
is not merely an argument from sorted insertion.

The three AFTER release runs were on this Mac and worktree, seed 42, 50 agents,
200 ticks, bands of 20. K1 was deterministic: Trail = 6,219 entries / 329,607
bytes and LatestVisit = 6,219 / 259,677 in every run. The absent BEFORE
KnownWater row — 4,665 / 247,245 — is the exact saving; neither surviving row
moved. K2 was 60.98, 61.14, and 62.63 ns/fact. Band-10 K3
`believed_water` / `shared_believed_water` was 10,340.62 / 10,413.11,
16,513.75 / 8,731.01, and 8,489.31 / 8,848.43 µs/call; the endpoint loads and
wall times are in the ledger. The K3 probe is deliberately a single
empty-belief agent, as Task 2 established, so it isolates `water_at` rather
than representing the whole roster.

Decision 0726 binds the criterion exposed by this cut: a resident index earns
its state only where it changes a served read's asymptotic class. 0727 is
unminted: the private shared predicate has no independent repository site and
does not yet deserve a repository-wide interface rule. The temporary hash
constants retired under 0541; fresh-run agreement, floors, and independent
fold-equals-scan witnesses remain.
