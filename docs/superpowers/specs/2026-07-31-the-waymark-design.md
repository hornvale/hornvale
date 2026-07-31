# The Waymark — the sim loop stops re-deriving its geometry

**Campaign:** the-waymark
**Date:** 2026-07-31
**Status:** Draft — awaiting G3 review
**Thesis:** With the derivation weir closed, the suite's cost lives in the
sim loop: A* and room-mesh geometry are 45–50 % of every liveness test, and
almost all of it is re-derivation of pure functions of stable inputs — a
full home-replan per NPC per action pop, and the same three-corner
cell lookup recomputed per field read per room. A waymark is the marker
that makes the next traversal cheap: memoize the geometry at the scope its
inputs actually change, and stop replanning what nobody moved.

**The scaling stake (Nathan, G3):** today's numbers are for TEN creatures
— `HEALTH_NPCS = 6` + `HEALTH_WILD = 4` over 40 ticks costs 60–90 s,
i.e. **~400+ full budget-1000 searches and ~150–200 ms of navigation
re-derivation per creature-tick**. The tests are affordable only because
the population is ten. Any future that populates a settlement with real
walkers inherits this cost multiplied. The campaign's bar is therefore not
"the tests get faster" but "**the marginal cost of a stationary,
unchanged-belief creature approaches zero**" — sim scalability is the
goal; test time is the readout.

## 1. Evidence (measured 2026-07-31 on lefford; FP + DWARF profiles, code map)

Post-Weir baseline: suite 7 696.5 s; the sim batteries dominate — lab
1 753.6 s (7 of the top 8 tests), vessel 2 110.3 s. In the health battery:
`astar` 32.1 % inclusive, `NavSpace::successors` 12.5 %,
`RoomAddr::neighbors` 11.6 %, `corner_weights` 9.9 %, `decode` 10.4 %;
DWARF self-times: `child_at_scale` 5.6 %, `SliceOrd/memcmp/RoomAddr::cmp/
Vec::cmp` ≈ 12.7 % (comparison churn on `(u8, Vec<u8>)` addresses used as
BTree keys), `NearestCellIndex::scan_at` 7.2 % self — the largest single
self-time frame.

The code map (file:line evidence, verified by an exploration pass):

- **The dominant waste:** `decide_step` calls
  `plan_to_room(pos, &npc.home, PLAN_BUDGET, &view.believed_hazard)`
  **unconditionally** (windows/vessel/src/liveness.rs:3846) to derive the
  Social drive's loneliness/home-step feature — a budget-1000
  Dijkstra-mode search re-run from scratch on every decision step of every
  NPC, even when `pos` did not change. Pure function of
  `(pos, home, avoid)`.
- **The second waste:** `corner_weights` (kernel/src/room.rs:526) runs 3×
  `NearestCellIndex::nearest_to_position` per call, and the locale field
  reads (`temperature_at`/`productivity_at`/`blend_at`/`hazards_at`/
  `describe_at`, windows/locale/src/lib.rs:357–515) each independently
  recompute the same three-corner lookup for the same room. Pure function
  of `(RoomAddr, Geosphere, NearestCellIndex)` — the latter two immutable
  per world.
- **The third:** `RoomAddr::neighbors` (room.rs:490) costs up to 3×
  `decode`, each `O(depth)` × `child_at_scale`/`orient` — pure integer
  function of the address alone.
- **Precedents already in the tree:** `Geosphere::coords` — a
  precomputed-once cache with a bit-equality pin test
  (`coord_cache_bit_equals_recomputation_at_every_cell`,
  geosphere.rs:594) — and `PrimaryAfraidMemo`
  (liveness.rs:937), a caller-owned per-tick memo already threaded through
  the exact call path `decide_step` sits on. Nothing analogous exists for
  plans or geometry; no comment claims that is deliberate.
- **Determinism constraints located:** A*'s tie-break IS its
  `BTreeSet<(f, g, state)>` order (astar.rs:30, pinned by test);
  `NearestCellIndex`'s exact-float-equality tie-break (geosphere.rs:446,
  pinned) must not be touched by any optimization; `move_cost`'s empty-
  avoid byte-identity note (liveness.rs:5091). All candidate memos are
  pure-function result caches that leave every tie-break untouched.

## 2. Design

Two memo tiers, matched to how fast the inputs age, plus the ratified
tier-split and a re-profile gate before any deeper surgery.

### Stage 1 — the ratified sweep tier-split (ledger #1, Nathan 2026-07-31)

`the_null_control_holds_across_a_seed_sweep` splits: the commit gate keeps
the seed-42 control (already a separate test) and the synthetic
sensitivity scenarios (`stranded_from_known_water` asserts
`stuck == 1.0`); seeds 0/1/2/7 move to a `heavy:`-tagged sibling that runs
in `make gate-full` (campaign-close cadence). Coverage unchanged globally;
~347 s leaves the commit gate and the ci-profile suite. A silence-armed
alarm keeps one negative control and the test button in the fast loop; the
negative-control panel runs on a schedule (the clinical-QC cadence).
Independent of Stages 2–3; lands first.

### Stage 2 — session-lived geometry memos (the waymarks)

- **`corner_weights` memo:** a caller-owned
  `BTreeMap<RoomAddr, [(CellId, u64); 3]>` (workspace HashMap ban) living
  with the objects that own the `Geosphere`/`NearestCellIndex` pair for a
  session — the locale context / sim setup — so the three-corner lookup
  runs once per room per session instead of once per field read.
  `scan_at`'s cost folds into it. The memo is write-once per key
  (append-only, The Phantom's monotonic-memo precedent) and its correctness
  bar is the `Geosphere::coords` one: a pin test asserting memoized ==
  recomputed, bit-for-bit, over the rooms a real walk visits.
- **`neighbors` memo:** same shape, `BTreeMap<RoomAddr, [RoomAddr; 3]>`,
  pure integer. Placement decided at plan time (kernel-side struct the
  caller owns, or vessel-side alongside the NavSpace) — NOT a global: no
  hidden state in the kernel.
- Where exactly these live (locale vs vessel vs a small kernel
  `RoomMeshMemo` struct the windows own) is a plan-time decision with one
  constraint: **no `RefCell`/global; `&mut` threading like
  `PrimaryAfraidMemo`.**

### Stage 3 — the plan cache (the dominant waste, now with a scaling bar)

First, a plan-time verification: is the loneliness/home-step feature
consumed on every `decide_step`, or only when the Social drive is live?
If conditionally consumed, laziness composes with the cache below (compute
only when read, cache what was computed).

**The mechanism (revised under the G3 scaling directive — supersedes G1's
per-tick-scope choice, ledger #4):** a **cross-tick, per-entity plan
cache** — the cached home-plan result lives with the NPC's sim state and
is invalidated by exactly two events, both of which have identifiable
write points: (a) the entity's `pos` changed (a Step resolution), and
(b) the entity's believed-hazard set mutated (the belief-update path
bumps a per-entity **avoid-epoch counter**; the cache key stores the
epoch it was computed at). A stationary creature with unchanged beliefs
therefore pays **zero searches** on every tick after its first — the
scaling property Nathan set as the bar — while per-tick scope would have
left one search per creature per tick standing.

Determinism: the cache changes when a search runs, never what it returns;
the epoch counter is deterministic (bumped at the same committed
belief-update points in every run). **The stale-plan hazard is the
campaign's one real correctness risk and gets two deterministic tests:**
(1) the adversarial invalidation scenario — the avoid set changes and the
plan must change with it (red-run-proven against a deliberately
non-invalidating cache); (2) a **search-count assertion** — a synthetic
scenario instruments the search counter and asserts a stationary,
unchanged-belief creature triggers zero recomputations after warm-up, and
a moved creature triggers exactly one. Search counts are deterministic
integers, so these pin the scaling property itself, not a wall-clock
proxy.

### Stage 4 — re-profile gate

After Stages 1–3: one FP flamegraph of the health battery and one vessel
walk. If comparison churn (`RoomAddr` BTree keys, ~12.7 % today) is then
the dominant remainder, the packed-address idea (a fixed-width encoding
preserving `(face, path)` lexicographic order) is **specced as a follow-up
campaign, not implemented here** — it touches the address type every
domain sees, and this campaign does not open that surface without its own
evidence and its own G3. If the remainder is diffuse, the campaign ships
what it has.

### Out of scope (followups / registry)

The census-proxy breadth metric (static blockage preconditions across
2 000 worlds) — additive, speculative, gets an idea-registry row in this
campaign's close. The packed `RoomAddr` (Stage 4's conditional spec-out).
Scene threading, the lens readouts, the build-volume audit — unchanged
from the-weir's register.

## 3. Determinism and save-format analysis

Zero new draws, zero stream-order changes, zero serialization changes,
zero ledger writes. Every memo is a pure-function result cache: same
inputs, same bits, computed once instead of N times — the
`Geosphere::coords` class, each with a bit-equality pin test. No tie-break
logic is touched (A*'s frontier order, `scan_at`'s float-equality, the
`containing` seam fallback all stay byte-for-byte). The plan memo changes
*when* a search runs, never what it returns; its adversarial invalidation
test plus the vessel byte-pin battery (`session_snapshot`, named
explicitly this time) plus a scripted possession transcript diffed across
binaries are the acceptance evidence. The sim's committed outputs
(AGENT_AT facts, affect traces, health metrics) must be byte-identical —
staged references first, as both prior campaigns.

## 4. Success criteria (testable)

1. **Commit gate loses the sweep** (Stage 1): gate/ci suite total drops by
   the sweep's cost; the heavy-tier sibling runs green in `gate-full` with
   a `heavy:` reason naming the measured cost.
2. **Per-test (lefford, solo, dev, staged before/after):** health
   null-control 63.2 s → **< 45 s**; `heat_hastens_thirst_end_to_end`
   ~90 s → **< 60 s**; `the_sky_follows_the_walker` 34.8 s → **< 25 s**;
   the two synthetic band tests (~100 s each) → **< 70 s**.
3. **Suite total:** ≥ 15 % below 7 696.5 s on quiet-lefford `make ci`,
   ledgered (Stage 1 contributes ~5 %; the memos carry the rest).
3b. **The scaling criterion (deterministic, not wall-clock):** the
   search-count pins hold — zero recomputations for a stationary,
   unchanged-belief creature after warm-up; exactly one on movement or
   belief change. Plus one measured scaling probe in the readout: a
   synthetic scenario at N=10 vs N=30 mostly-stationary creatures, with
   per-creature-tick nav cost reported for both (the marginal-cost claim,
   measured once, recorded in the chronicle — not a pass/fail gate, since
   wall-clock scaling is load-sensitive; the deterministic search counts
   are the gate).
4. **Byte-identity:** §3's evidence set, all IDENTICAL; the memo pin tests
   green; the adversarial invalidation test red-run-proven (it must fail
   against a deliberately stale plan).
5. Misses ship un-retuned with the post-fix flamegraph (standing rule).

## 5. Testing strategy

Existing pins are the harness (session_snapshot's byte pins named as the
primary battery for vessel changes; health pins; possession transcripts).
New tests: one bit-equality pin per memo (the `coords` pattern); the
adversarial stale-plan test (red-run-proven); the heavy-tier sweep sibling
with its non-vacuity guards carried over verbatim.

## 6. Risks

- **Stale-plan correctness** (Stage 3): mitigated by scope choice
  (per-tick), the plan-time belief-stability verification, and the
  adversarial test. This is the campaign's real risk; the geometry memos
  are mechanically safe.
- **Memory growth of session memos:** bounded by rooms actually visited
  (the working set of a walk), not the 20×4^depth address space; the plan
  states the bound and the memo is append-only.
- **The memo plumbing touches `liveness.rs` (7 000 lines, sim-critical):**
  the PrimaryAfraidMemo threading is the template; changes ride existing
  parameter paths.
- Parallel campaigns: the usual absorb-per-task discipline; `liveness.rs`
  and `session.rs` are the churn surfaces.

## 7. Decisions (promoted from the autopilot ledger)

- **Ratified (Nathan, pre-G1):** the sweep tier-split (ledger #1), with
  the ideonomy trail (lattice; clinical-QC convergence) recorded there.
- **G1 — two-tier memos on existing threading:** geometry memos
  session-lived, plan memo tick-lived riding the PrimaryAfraidMemo path;
  lazy-vs-memo resolved at plan time by whether the feature is
  conditionally consumed; packed-address parked behind the Stage-4
  re-profile gate. 2 ideonomy passes, 0 overturns (pass 1: negation
  surfaced the lazy alternative and the rate-scoped memo tiers; pass 2:
  the side-effect check — `&mut` threading, no RefCell, no globals).
- **Q — siting: lefford** (ledger #2, trivial by precedent).
