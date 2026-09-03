# The Detent — the fear path's terrain verdicts are held, not re-sampled

**Date:** 2026-09-02 · **Registry rows:** `TOOL-hazard-affect-cross-tick-memo`
(corrected by this campaign — its mechanism was read, not measured),
`UNI-ecs-is-the-adaptive-cache` (`raw`, high — the layer this campaign adds
a second concrete instance to), `TOOL-emitter-scan-tests-out-of-liveness`,
`TOOL-believed-hazard-memo-is-dead` (both closed by this campaign),
`TOOL-place-predicate-index` (opened by this campaign) · **Program:** The
Penstock metaplan (`docs/superpowers/specs/2026-08-22-the-penstock-metaplan.md`),
stage 3 — the derived-component layer — entered for the fear path's
world-derived half (metaplan §6.6's left column, decision 0206's `Pure`
class). · **Ledger:** `docs/superpowers/ledgers/2026-09-02-the-detent.md` ·
**Decision block:** 0626–0635.

A detent is the catch that holds a mechanism in a position until something
deliberately releases it. A verdict about the ground — *is this room
frightening to this creature* — is a pure function of a terrain that never
changes within a session. Today it is re-taken on every visited room of every
roster member on every tick. This campaign takes it once and holds it.

## 0. What this campaign is, and the thing it is not

**Byte-identical.** No fact is added, removed or reworded; no predicate,
stream label or epoch. The commit site is not touched. The value the fear
path computes for every `(creature, room, day)` is the same `f64` in the same
summation order; what changes is how many times `terrain.hazards(room)` is
evaluated to reach it. §5 says how that is proven, and why the drift check is
not the instrument.

**It is the second concrete instance of the adaptive-cache layer, on the
other class.** The Pawl built the ledger-derived half: a resident store of
what the ledger determines, advancing as facts commit. This campaign adds the
world-derived half to the same session: a memo of what the *terrain*
determines, keyed by room, never invalidated (decision 0206's `Pure`), owned
beside the store and the mesh memo. Nathan's standing direction (Penstock
§6.6) applies: keep it general. The tenant is the hazard field because the
hazard field is what is hot; `is_fresh_water` and `forage_value` have the same
shape and are named in §9, not built here.

**It is not the cross-tick affect memo.** `TOOL-hazard-affect-cross-tick-memo`
proposed memoising the emitter's *affect* at a past visit day across ticks,
keyed by the emitter's reset partition. §1 measures that path reached **zero
times** on every shape the criteria run on. It is not built; §3 rule 2 says
under what measurement it would be.

**7b is out**, for The Pawl's reason (an epoch, bundled with a nothing-moved
campaign, destroys both campaigns' legibility). **Hysteresis is out.**
**`KnownWater` into `LatestVisit` is out** — it moves no criterion and its
cost is memory, never an answer; the row stands.

## 1. The finding, measured

The Pawl's second readout (its §12.4) left three criteria failing on one
fold: `hazard_memory_memo` at **93,153 µs/call** at the final band, 84.2% of
the six timed reads' total, elasticity **0.92**, moved 1.57× by the resident
store and, against the frozen 73–97 ms/call, not at all. Its chronicle then
named the mechanism "legible from the code rather than merely suspected":
per visited room × per emitter, the emitter's affect at the room's
latest-visit day, memoised only within the tick.

That mechanism was read, not counted, and the same campaign's retrospective
names that defect shape as the one it kept producing. So before this campaign
was chosen, the fold was **counted** on the instrument H4 is measured on —
`session_length_scaling.rs`'s own construction (seed 42, `derive_npcs(50)`,
`DriveMovements::step_with_occupancy` per tick, one resident store), with a
`Terrain` wrapper that counts every `hazards()`, `temperature()`,
`is_fresh_water()` and `elevation()` call, and the store's own `ReadWitness`
read before and after each probe. Source parked at the session scratchpad
(`hazard_attribution_spike.rs`); Task 1 rebuilds it as the committed witness.

```
seed 42, 50 derived agents, one hazard_memory_memo call on the max-history probe

                                tick 15   tick 30   tick 60   tick 100  tick 200
FRESH PrimaryAfraidMemo per call (the bench's exact shape)
  terrain.hazards()             14,004    16,758    22,302    29,097    43,164
  alarm_replays (affect at a past day)  0         0         0         0         0
  emitter scans with an emitter      0         0         0         0         0
  wall, loaded box                42 ms     35 ms     46 ms     59 ms     87 ms
WARM memo, second call (production's per-creature read after the tick's scan)
  terrain.hazards()                684       819     1,089     1,449       729
  wall                           1.4 ms    1.8 ms    2.3 ms    3.1 ms    1.5 ms
probe: agent-at facts               76        91       121       161       279
probe: distinct rooms               76        91       121       161        81
roster: distinct rooms, sum      1,430     1,721     2,307     3,022     4,665
roster: rooms ∪ one-hop halo     7,006     8,357    10,832         —         —
WHOLE TICK (50 walks + their reads)
  terrain.hazards()             29,250    34,146    44,694         —         —
  temperature()                  3,570     1,378     1,141
  is_fresh_water()               1,575     1,869     2,454
  facts committed                  181        35        31

seed 6 (the emitter seed The Pawl's second witness searched to), 50 agents,
tick 60: every scan finds ONE emitter; alarm_replays 0; hazards() 22,671
fresh / 1,152 warm.
```

Five things follow, and each is the kind of claim this document is required
to attach a command to (the table is the command's output):

1. **The 87 ms at tick 200 is §12's 93 ms.** Same construction, same band,
   a loaded box against a quiet one. The counts are what that cost is made
   of, and they contain no affect replay.
2. **Every `hazards()` sample is a pure function of the room.**
   `Terrain::hazards(&self, room)` takes no `day` by contract ("a slow
   field, so it takes no `day`", `liveness.rs`); `LocaleContext::hazards_at`
   reads the climate's geosphere, the nearest-vertex index and the regime
   budget; the predator axis is "computed once at" session start
   (`session.rs`, `Session.predator`). Nothing in the session mutates any of
   them after `Session::start`.
3. **~95% of the per-call cost is `build_emitter_scan`'s pass 2**, which
   asks `threat_field` — the room and its neighbours, ~9.7 samples per
   room — for every room every roster member has ever stood in. The
   remaining ~5% is the per-creature read's own `threat_field` per visited
   room. Pass 3's trail copy is O(history of emitters) and there are no
   emitters on seed 42's derived roster; on seed 6 there is one.
4. **The whole tick re-samples ~45,000 rooms to commit 31 facts**, and the
   distinct rooms it asks about number ~11,000. The naïve fix — sample the
   union once — is a 4.1× and is subsumed by a memo that pays each distinct
   room once per *session*.
5. **The probe is the worst population and it is a wanderer.** Its distinct
   rooms equal its history at every band until 200 — it never revisits — so
   for it, "O(distinct rooms)" *is* O(history), which is why the fold's
   elasticity reads 0.92 and why a memo alone cannot move H4 (a): it turns
   samples into lookups without changing their count. The advancing form
   (§2.3) is what changes the count.

**What the count does not say.** It is one seed's derived roster and one
emitter seed's. The possession shape (a session's roster is the settlement's
residents since The Roll, 67–68 bodies) and the lab's `run_simulation` shape
reach different functions — the lab reads each creature at a *waking* instant
inside the tick, a past-instant read — and each is counted in Task 1 rather
than inferred here. The Pawl measured that the seed-6 possession shape
reaches the affect replay 9 times of 9 at past instants; its cost there is
not known and §3 rule 2 is where it gets known.

## 2. The architecture

### 2.1 The room memo — `GroundHazards`

A session-lived `Derived<Facet, Hazards>` (kernel `derived.rs`, The Forebay)
holding `terrain.hazards(room)` per room, `Validity::Pure`. Owned by
`Session` beside `mesh_memo`, `home_nav_cache` and the resident store, and
equally ownable by the two benches and the lab's `run_simulation`, which
build `LocaleTerrain` per tick and would find a terrain-scoped memo cold on
every tick they run (ledger #2).

**Read through interior mutability, filled on read.** `Terrain::hazards`
is `&self` and every caller reaches it through `&dyn Terrain`; the rooms a
tick will ask about are the union of every roster member's visited rooms and
their halos, which nothing knows before the reads run, so a prefill in the
mesh memo's style has nothing to prefill from. The memo is threaded into
`LocaleTerrain` as an `Option<&RefCell<…>>` — an additive builder method
beside `with_fields`, so no existing construction site changes signature —
and `LocaleTerrain::hazards` reads the memo, computing and inserting on a
miss. The `OwnedFolds` shape (The Pawl §2.2), with the same rule about
borrows: one guard per read, dropped before anything that could re-enter.

**Key-completeness by ownership (ledger #3).** The key is the `Facet`. The
derivation also reads the `LocaleContext` and the session's predator field;
both are owned by the session that owns the memo, and neither changes after
`Session::start`. The type doc states "one memo per `(LocaleContext,
predator field)`", the same sentence `SustenanceMemo` already carries for
the temperature field, and two tests hold it: chaos eviction (`evict_all`
after every read, output unchanged — the property the store was built to
pressure) and a two-terrain test that hands the memo to a second terrain
carrying a different predator field and asserts the answers differ, so a
shared memo would be caught rather than assumed impossible.

**The memo's counters are the campaign's counting instrument.** `Derived`
already keeps `hits()`/`misses()`; `misses` is exactly "terrain samples
taken", and a test-side counting `Terrain` wrapper (Task 1) counts the same
thing on the pre-fix tree, which is what lets the witness be red before it is
green.

### 2.2 The verdict index — `FrighteningGround`

The scan and the read both ask, per `(member, room)`, whether `threat_field
(room, member.threat_niche) × mettle_factor(member.boldness) ≥ DANGER_ACT`.
Over a fixed terrain that verdict is a constant. The resident store gains a
per-entity read-side index of it:

```
FrighteningGround, per entity:
  consumed:    usize                       trail entries already judged
  frightening: Vec<(WorldTime, Facet)>     rooms judged frightening, ascending
                                           by FIRST visit, one entry per room
  judged:      BTreeSet<Facet>             every room judged, either verdict
```

Advanced at read from the entity's `Trail` by the consumed-prefix cursor
(`MemoPartition`'s shape): the entries after `consumed` are the sightings
since the last read, each new room is judged once through the room memo,
and the cursor moves to the trail's end. **`absorb` still sees only facts**
(The Pawl's `Alarm`-is-not-a-tenant ruling, ledger #4): the index is a memo
of a pure predicate applied at read, which is what `KnownWater::water_at`
already is for `is_water`, with the answer kept instead of re-asked. Its
state is a pure function of `(ledger prefix, terrain)`, discardable at any
instant, and rebuilt on demand — pinned by the same discard-and-rebuild
schedule every Pawl tenant carries.

**A past `t` is served by the first-visit order.** The lab's shape reads at
a waking instant inside the tick, so `t` can precede the trail's end. A room
is in the scan's domain at `t` iff its first visit is `≤ t` (`LatestVisit::
rooms_at`'s own rule), so `frightening` sorted by first visit answers "the
frightening rooms at `t`" as a `partition_point` prefix — no rebuild, no
filter over the whole set.

### 2.3 The scan and the read, over the index

**`build_emitter_scan` becomes O(new sightings) per tick.** For each member,
`is_emitter(m, t)` is "the member's `frightening` prefix at `t` is non-empty,
or its home is frightening" (home judged once, through the memo); the halo
union is the union over members of the halos of their frightening prefix.
Both are monotone in `t` and in the ledger, so the per-tick work is judging
the rooms first visited since the last tick. Pass 3 — copying an emitter's
trail prefix to `t` into the `EmitterScan` — is kept as it is (emitters are
rare, and the copy is what lets the alarm loop run without a store guard);
§3 rule 4 measures it on the emitter seed rather than assuming it is free.

**The emitter-free read becomes a prefix read.** Today it is: for every room
in `latest_at(npc, t)`, `frightened_at(room, …, &[])`, which over an empty
roster is the terrain verdict alone. With terrain static, "any visit at or
before `t` was frightening" equals "first visit `≤ t` and the room is
frightening" — exactly the `frightening` prefix at `t`. So `shunned` is that
prefix's rooms, collected into the `BTreeSet` the type returns. Same set,
same order (a `BTreeSet` sorts on insert), same verdict per room.

**The emitter path keeps its shape and its arithmetic.** The terrain
shortcut, the halo pre-filter, the `position_at` binary search, the
`emitter_arousal` recursion, the clamp and `feels_frightening` are untouched;
what changes is that `terrain_threat` per room comes through the room memo
(one lookup per room and neighbour instead of a field blend) and the
per-room `latest` map is still `LatestVisit::latest_at`. The additions in
`alarm +=` run in the same order over the same emitters. This is the path
§5's identity witnesses are about.

### 2.4 The rides

Because the scan is rewritten: `EmitterScan` becomes `pub(crate)` and its
FOLD-equals-SCAN tests move out of `liveness.rs` into
`tests/suite/resident_folds.rs` beside every sibling
(`TOOL-emitter-scan-tests-out-of-liveness`); `believed_hazard_memo`, which
has no caller anywhere, is deleted (`TOOL-believed-hazard-memo-is-dead`) —
its one job was to be the planner-half accessor, and `hazard_memory_memo(…)
.shunned` is the same call for any caller that ever wants it.

### 2.5 What is measured, per shape

Three shapes reach these functions through different callers and each is
counted, never inferred from another (The Pawl's rule-6 lesson, where the
inference was wrong by a factor of 56):

| shape | entry | memo lifetime today | what Task 1 counts |
|---|---|---|---|
| seed-42 possession | `Session::wait` → `step_with_occupancy` | one `PrimaryAfraidMemo` per step; `snapshot` built its own until The Rack | hazards per tick, per creature, alarm replays |
| the lab, `run_simulation` | `health.rs` post-tick `affect_of_memo_occupied` at a waking instant | one memo per tick, a second per post-tick read loop | the same, plus reads with `t` before the ledger's end |
| the two benches | `step_with_occupancy` with caller-owned caches; the hazard probe with a FRESH memo per call | per call | the table in §1, re-taken as a committed witness |

## 3. Decision rules, not predictions

Each is a branch table the implementer executes, recording the branch in
the ledger.

1. **After the room memo alone (Stage 2), re-count before building Stage 3.**
   Run the counting witness on all three shapes. *Samples per repeated call
   fall to zero and samples per tick fall ≥ 10×* → the memo is doing its job;
   proceed to Stage 3 for the elasticity, which a memo cannot move (§1 item
   5). *Samples per tick fall < 10×* → something else samples hazards that
   the spike did not see (a caller building its own `LocaleTerrain` without
   the memo, most likely); find it with the wrapper's per-site count before
   Stage 3, and ledger it.
2. **The affect replay.** On the seed-6 possession shape, count
   `alarm_replays` per tick and time the replays' share of the hazard read
   with a warm room memo. *Share < 10% of the hazard read* → the
   reset-partition memo stays unbuilt and the registry row records the
   number. *Share ≥ 10%* → it is a real quarry on a real shape; it becomes a
   follow-on stage of THIS campaign only if it can be keyed by the emitter's
   reset partition without touching decision 0540's preserved semantics,
   else its own campaign. Either way the number goes in the row.
3. **Key aliasing.** The two-terrain test (§2.1) must FAIL against a memo
   shared across two predator fields before the ownership rule is written
   on the type. *It fails as expected* → ownership is the documented
   invariant. *It passes* → the predator axis does not reach `hazards()` on
   the test's rooms; pick rooms where it does (the predator field is
   non-zero on the flagship's territory since The Quarry) — a test that
   cannot fail is not a test.
4. **The emitter's trail copy.** On seed 6's 50-agent roster, count bytes
   copied per tick by `EmitterScan`'s pass 3. *Under 1% of the tick's
   allocation, or flat with history* → keep the copy. *Growing with history
   and material* → replace with a `(Body, prefix_len)` pair read through
   the store under the one-guard rule; the alarm loop's borrow shape is the
   cost and the ledger records it.
5. **Past-instant reads.** The lab's waking-instant reads must be served by
   the first-visit prefix (§2.2). The witness counts reads with `t` strictly
   before the read's own ledger end, with a floor (`> 0` on the lab shape,
   as The Pawl's rule-6 witness measured 9 of 9 on the replay shape). *Zero
   on every shape* → the prefix machinery has no production caller and the
   spec is wrong about the lab; STOP and re-read `health.rs` before
   building it.
6. **The Rack.** `campaign/the-rack` is at its stage gate and rewrites
   `Session` around a roster with a `felt` column, changes
   `step_with_occupancy`'s return type, and removes the snapshot's throwaway
   memo. *It lands before Stage 1 closes* → absorb at the boundary, re-record
   every identity witness MAIN-FIRST (decision 0541), and count the
   possession shape again. *It has not landed* → proceed; the campaign's
   edits to `step_with_occupancy` are inside the body (the memo threading)
   and the conflict, when it comes, is textual and small.
7. **`session_cost.rs` and `turn_budget.rs`.** Any ceiling those tests pin
   that this campaign moves DOWN is re-pinned from post-campaign readings on
   the canonical box, never loosened; a ceiling that moves UP is a finding
   and stops the task.

## 4. Preregistration

Inherited **verbatim** from The Tailrace §4 as frozen 2026-08-24 (decision
0016) and restated in The Pawl §4. Not re-frozen after seeing the substrate.

- **H4.** On `session_length_scaling.rs`, seed 42, 50 agents, 200 ticks,
  ≥ 3 runs, counting only runs with r² ≥ 0.5: `hazard_memory_memo`'s median
  elasticity falls below **0.20** (from 1.06–1.21; The Pawl's second readout
  measured 0.92 against a same-box control of 1.21), and its final-band cost
  falls by at least **10×** from **73–97 ms/call** (the frozen figure; The
  Pawl measured 93.15 ms/call against a same-box control of 146.55). The 10×
  is stated against the frozen figure AND against a same-box control taken
  in the same session, interleaved, at the campaign's merge base, and the
  verdict names which one it is against, both times.
- **H2 (c).** The whole-tick history share falls below **20%** (The Pawl:
  58.0% against a control of 74.4%). H2 (a) and (b) are already met and are
  re-reported as no-regression checks, not criteria.
- **H3.** No world-state artifact moves. Established by construction (§5).
- **The fix's own falsifier.** If `k` falls but `C` rises by more than the
  `k` saving at realistic session lengths, the memo is a pessimisation for
  short sessions. The Pawl's second readout put the crossover at h ≈ 26,
  below the sampled range; this campaign re-fits both lines and reports the
  crossover.

Added here, frozen before any code, as **counts** (deterministic, load-
independent, and gated in the commit tier):

- **H5.** On the 50-agent seed-42 shape at tick 60: a second
  `hazard_memory_memo` call on the same tick with a fresh `PrimaryAfraidMemo`
  makes **0** terrain hazard samples (pre-fix: 22,302); the whole tick makes
  **≤ 4,469** (a 10× fall from 44,694). Denominators: roster > 0, the probe's
  distinct rooms > 0, and the pre-fix count re-taken on the merge base and
  printed beside the post-fix one.
- **H6.** The scan's work per tick is O(new sightings): across ticks 15 → 60
  the samples the scan takes per tick grow strictly slower than the roster's
  distinct rooms (a ratio, stated as the guard's margin the way The Pawl's
  `history_growth`/`segment_growth` guard states it, with the margin
  printed).

- **M1, a measurement with no threshold (Nathan, 2026-09-02, at G3).** The
  bytes the two structures hold at the final band, on all three shapes:
  the room memo's entry count × entry size, and the per-creature index's
  total entries × entry size, reported beside the count of distinct rooms
  the roster has visited. Nothing evicts either structure in this campaign
  — Penstock stage 4 (the lifecycle) is gated on stage 3 producing exactly
  this number — so the readout carries it as the figure stage 4 enters on,
  not as a criterion.

Three instruments, all run: `session_length_scaling` (H4, H2 (c), the
falsifier), `agent_scaling` (the level, paired and interleaved against the
control, ms/tick at 200 agents over 20 ticks), and `fold_depth_sweep`
(which sweeps `drive_at` only and cannot see this fold — run as a
no-regression control on the Sustenance reads, and said to be that). All
three load averages recorded before and after every run; any run whose
1-minute average exceeds 10 at either end is set aside and listed. Every
criterion reported separately; a falsified prediction is a finding.

**What the spike's numbers are and are not.** The table in §1 was taken on
a box at load 3–77 and is a count, not a timing; its wall figures are quoted
only for the tick-200 cross-check against §12. No timing in §1 is a
baseline; the readout's control is measured fresh.

## 5. Determinism contracts (lead the G3 flagged section)

- **Byte-identical.** No new fact, predicate, stream label or epoch. The
  room memo returns the same `Hazards` value the field blend returns (it IS
  that value, computed once); the verdict index holds a `bool` per room the
  predicate already computed; the emitter path's additions run in the same
  order. No `f64` is combined differently.
- **The drift check cannot see this.** No committed artifact carries a
  ticked session ledger (The Pawl §5, verbatim), so the proof is the
  campaign-time hash witness (decision 0541): Task 1 mints constants for the
  seed-42 possession script and the seed-6 emitter script from the
  merge base, with a positive control (mutate the verdict predicate — e.g.
  `DANGER_ACT` at one site — and watch the hash move), re-records them
  MAIN-FIRST after every absorption, and retires them at close, leaving the
  constant-free two-fresh-runs-agree witnesses with their floors.
- **FOLD-equals-SCAN for the index.** The verbatim body of today's
  `build_emitter_scan` pass 2 and of the emitter-free read loop are kept in
  the test file as the oracle, and the index is compared against them at
  every position and every third position on the possession and lab shapes,
  with a floor on the number of rooms judged.
- **Chaos eviction on the memo.** `evict_all` after every read; output
  unchanged.
- **The one thing that would make the memo wrong**, stated so it cannot
  happen silently: a hazard that depends on the day. `Terrain::hazards`
  takes no `day`; a future seasonal hazard changes that signature, and the
  memo's key is the first thing the compiler will refuse. The predator field
  is computed once per session; a campaign that makes it move per tick must
  move the memo to `Validity::Ledger` or rebuild it per tick, and the type
  doc says so.
- **No `HashMap`, no wall clock.** `Derived` is a `BTreeMap`; the index is
  `Vec` and `BTreeSet`; the witnesses count, they do not time, except in the
  `--release` examples that already do.

## 6. The stage carve

| # | stage | delivers | gate |
|---|---|---|---|
| 1 | The instruments | the counting `Terrain` wrapper as a test utility; the counting witness on all three shapes with floors, RED on the merge base (H5's 22,302 and 44,694 re-taken and printed); the campaign-time hash constants with their positive control; `alarm_replays` and past-instant counts per shape (rules 2 and 5) | `make sluice-stage`; absorb main (rule 6) |
| 2 | The room memo | `GroundHazards` on the session, the benches and the lab; the two-terrain test (rule 3); chaos eviction; re-count (rule 1) | stage gate |
| 3 | The index and the scan | `FrighteningGround`; the scan and the emitter-free read over it; FOLD-equals-SCAN; the rides (§2.4); rule 4 | stage gate |
| 4 | The readout | §11: the three instruments, the same-box control, every criterion separately, M1's bytes on all three shapes; the registry row corrected with the number; retire the constants; chronicle, retro, decisions, Confidence Gradient re-score | `make sluice` |

## 7. In / out

**In.** The room memo; the verdict index; the scan and emitter-free read
over them; the counting instruments; `EmitterScan` visibility and its tests'
move; deleting `believed_hazard_memo`; correcting the registry row; the
readout.

**Out.** The reset-partition affect memo (rule 2 decides whether it is even
a quarry); 7b; hysteresis; `KnownWater` into `LatestVisit`; the
`resident`/`liveness` shared-vocabulary move; `is_fresh_water` and
`forage_value` memos (§9); `believed_water`'s A\*-per-water-room cost
(measured in passing at 4.4 ms/call over 121 samples — the samples are not
the cost; the plans are); Campaign 7.

## 8. Decisions this campaign will need (block 0626–0635)

- **0626 — A terrain verdict is held for the session, keyed by room, with
  its terrain's identity supplied by ownership.** The `Pure` class's second
  tenant in the tree and its first in `windows/vessel`; why the key is the
  room and not a fingerprint; the one-memo-per-`(LocaleContext, predator
  field)` rule.
- **0627 — The emitter scan advances over new sightings through a read-side
  verdict index, and is still not a tenant.** Reconciles with The Pawl's
  `Alarm` ruling: `absorb` sees facts; the read keeps the predicate's
  answer.
- **0628 — A registry row's mechanism is a count, not a reading.** Process:
  the row that named this campaign's target carried a mechanism read from
  code that a 60-second count refuted; the row is corrected rather than
  superseded, and the correction keeps the original claim as history.

Rule 2 may need a fourth; the block has room.

## 9. Frontier bookkeeping

- `TOOL-hazard-affect-cross-tick-memo` — corrected in place (the measured
  mechanism, the counts, and the decision rule that would build the affect
  memo), kept `raw`; closed or re-scoped at the readout.
- `TOOL-place-predicate-index` — NEW, `raw`: "a fold over visits × a static
  predicate over places"; the tenant holds the visits, the read applies the
  predicate, the predicate's answer per place is a `Pure` derived component.
  Instances: `is_water` (`KnownWater::water_at`, 121 samples per
  `believed_water` call), `forage_value`, the frightening verdict (this
  campaign). The abstraction ledger #1's ideonomy pass surfaced.
- `TOOL-emitter-scan-tests-out-of-liveness`, `TOOL-believed-hazard-memo-is-
  dead` — closed by §2.4, status flipped at close.
- `UNI-ecs-is-the-adaptive-cache` — the Where cell gains this campaign as
  the world-derived instance; status stays `raw` (Nathan's framing, his to
  promote).
- `book/src/open-questions.md`, the bet on the substrate being cheap to
  write and unpriced to read: re-scored at close with the fifth occasion.

## 10. Operational notes for the implementer

- **Absorb `campaign/the-rack` when it lands** (rule 6). It touches
  `liveness.rs` (206 lines changed), `session.rs` (933 lines changed) and adds `roster.rs`; this
  campaign's `session.rs` edits are one field and one builder call, and its
  `liveness.rs` edits are inside `build_emitter_scan`, `hazard_memory_memo`
  and the `Terrain` impl for `LocaleTerrain`. Regenerate aggregates after
  the absorption, never text-merge (`docs/audits/type-audit-report.md` is
  the one that conflicts).
- **Measure on a quiet box.** The Mac was at load 77 when this campaign
  began and 3 an hour later; the counting witnesses do not care, the
  readout does. Record all three averages before and after every run.
- **Every witness has a denominator.** A count of zero with an
  un-asserted floor is what put a wrong mechanism in the registry.
- **A cost witness targets the worst population** — the wanderer whose
  distinct rooms equal its history — and is red on the merge base before it
  is green here. Stage 1 prints the red.
- **`renaming a test is a commit-gate change`**: the subfloor roster selects
  by exact name; a renamed witness silently leaves the commit gate.
- **The spike is not the witness.** Its source is in the session
  scratchpad; the committed witness is written fresh under `tests/suite/`
  with floors, and the counting wrapper lives beside the sibling test
  utilities in `windows/vessel/tests/suite/` — read the suite's own helper
  layout before placing it (there is no `common/` directory there).
- **A null census is owed at close**, as every byte-identical campaign
  owes; `make sluice-census` is ordinary queued work (decision 0514).

## 11. What shipped, measured

Every number below is a fresh run on the development Mac, `--release`, taken
2026-09-02 between 19:26 and 20:52 local (23:26–00:52 UTC), paired and
interleaved against a merge-base control (§11.1). **§4 is not edited.** Every
number cites the file under `.superpowers/sdd/2026-09-02-the-detent/readout/`
it was read from. A criterion that fails is reported as NOT MET with its
margin, and nothing is averaged across a failure.

### 11.0 The quiet-box rule, and how it was applied

§4's rule, applied exactly: all three load averages recorded immediately
before and immediately after each run, and a run set aside if the **1-minute**
average is above 10 at either end. In addition — and going beyond what §4
asks — the 1-minute average was sampled **every 30 s during** each run and the
samples are appended to that run's own file, so a spike that landed and
decayed between the two endpoint readings cannot hide.

The box was not quiet on demand. Two other campaigns were resident on it for
the first hour: `campaign/the-rack` ran a debug test binary at 833% CPU
(measured 19:31, `ps aux`), and `campaign/the-plumb` ran ~8 parallel
`hornvale_lab` debug test processes at ~86% each (measured 19:58). The
15-minute average was above 30 when the session began. Runs were fired only
when a poller saw **no other worktree's `target/` process running, no `cargo`
process, and a 1-minute average below 6** — a stricter gate than §4's 10, so
that a spike arriving mid-run had headroom before it breached the rule.

**Two of twelve `session_length_scaling` runs were set aside**, both control
runs, both to another campaign's suite starting mid-run. **One `agent_scaling`
pair of four was set aside**, likewise a control side. Every run taken is
listed below with its loads, including the discarded ones.

| run | bench | tree | start (UTC) | load before (1/5/15) | load after | peak 1-min in run | verdict |
|---|---|---|---|---|---|---|---|
| B1 | `session_length_scaling` | control | 23:26:26 | 4.12 / 21.06 / 30.35 | **46.61** / 23.21 / 27.26 | — | **SET ASIDE** |
| B2 | `session_length_scaling` | control | 23:49:39 | 6.23 / 23.62 / 28.82 | **52.11** / 52.57 / 41.01 | — | **SET ASIDE** |
| B3 | `session_length_scaling` | control | 00:07:12 | 5.63 / 29.53 / 36.45 | 3.65 / 13.66 / 27.23 | 5.63 | valid |
| P1 | `session_length_scaling` | campaign | 00:12:20 | 3.68 / 13.49 / 27.09 | 2.82 / 9.78 / 23.74 | 3.68 | valid |
| B4 | `session_length_scaling` | control | 00:14:33 | 3.47 / 9.80 / 23.67 | 2.59 / 5.54 / 17.88 | 3.58 | valid |
| P2 | `session_length_scaling` | campaign | 00:19:34 | 2.59 / 5.54 / 17.88 | 2.54 / 4.52 / 15.78 | 2.80 | valid |
| B5 | `session_length_scaling` | control | 00:21:44 | 2.54 / 4.52 / 15.78 | 2.67 / 3.43 / 12.22 | 3.07 | valid |
| P3 | `session_length_scaling` | campaign | 00:25:41 | 2.67 / 3.43 / 12.22 | 2.40 / 3.14 / 10.90 | 3.61 | valid |
| B6 | `session_length_scaling` | control | 00:27:51 | 2.53 / 3.16 / 10.86 | 2.37 / 2.76 / 8.56 | 3.11 | valid |
| P4 | `session_length_scaling` | campaign | 00:32:00 | 2.37 / 2.76 / 8.56 | 2.21 / 2.59 / 7.73 | 2.49 | valid |
| BA1 | `agent_scaling` | control | 00:34:11 | 3.66 / 2.90 / 7.77 | 3.67 / 3.10 / 7.52 | 4.40 | valid |
| A1 | `agent_scaling` | campaign | 00:35:36 | 3.67 / 3.10 / 7.52 | 3.34 / 3.08 / 7.23 | 3.67 | valid |
| BA2 | `agent_scaling` | control | 00:36:43 | 3.34 / 3.08 / 7.23 | 2.86 / 2.96 / 6.90 | 3.34 | valid |
| A2 | `agent_scaling` | campaign | 00:37:47 | 2.86 / 2.96 / 6.90 | 3.29 / 3.10 / 6.71 | 3.75 | valid |
| BA3 | `agent_scaling` | control | 00:38:50 | 3.29 / 3.10 / 6.71 | **16.00** / 6.47 / 7.64 | 9.87 | **SET ASIDE** |
| A3 | `agent_scaling` | campaign | 00:43:16 | 5.13 / 6.19 / 7.29 | 3.53 / 5.57 / 6.99 | 5.13 | valid, **pair set aside** |
| BA4 | `agent_scaling` | control | 00:44:23 | 3.41 / 5.51 / 6.96 | 3.45 / 5.14 / 6.73 | 3.45 | valid |
| A4 | `agent_scaling` | campaign | 00:45:27 | 3.45 / 5.14 / 6.73 | 2.68 / 4.63 / 6.43 | 3.45 | valid |
| F-B | `fold_depth_sweep` | control | 00:46:06 | 2.68 / 4.63 / 6.43 | 2.68 / 4.63 / 6.43 | 2.68 | valid |
| F-P | `fold_depth_sweep` | campaign | 00:46:08 | 2.68 / 4.63 / 6.43 | 2.68 / 4.63 / 6.43 | 2.68 | valid |
| R1–R3 | `detent_rule2_probe` | campaign | 00:46:09–00:47 | 2.19–2.68 (1-min) | 2.31–2.65 | ≤ 2.68 | valid, **pre-M1 build** |
| R4–R6 | `detent_rule2_probe` | campaign | 00:50–00:52 | 2.19–3.00 (1-min) | 2.37–3.16 | ≤ 3.00 | valid |

A3 is a legitimate run by the rule and is reported, but its *pair* (BA3) is
not, so it contributes nothing to the paired figure in §11.5. R1–R3 ran the
probe before its M1 count line was added (§11.4); their timings are valid and
are reported beside R4–R6's, which are the ones quoted.

Files: `sls-{control,campaign}-N.txt`, `as-{control,campaign}-N.txt`,
`fds-{control,campaign}-1.txt`, `r2-campaign-N.txt`. The control's runs are
numbered 1–6 and the campaign's 1–4; B1/B2 are control runs 1 and 2, B3–B6 are
control runs 3–6.

### 11.1 The control, and the byte-identity witness

The control is a detached checkout of this campaign's merge base,
**`0dccce0292eb613c98aea6d9f731dcd2945e78a6`**, at
`…/scratchpad/detent-control`. It carries **no room memo** — `grep -c
with_ground windows/vessel/examples/session_length_scaling.rs` prints **0**
there — and its three benches carry the identical constants:

```
control  : 199:const AGENTS: usize = 50;  204:const TICKS: usize = 200;
           214:const FOLD_REPS: u32 = 200;  227:const BAND: usize = 20;
campaign : 199:const AGENTS: usize = 50;  204:const TICKS: usize = 200;
           214:const FOLD_REPS: u32 = 200;  227:const BAND: usize = 20;
```

Both trees built `--release`; the campaign tree at `5bf12d8ef` (its parent
`1ea0998d0` is the last commit touching code — `5bf12d8ef` is the ledger
document alone).

**The workload did not move between the trees.** The deterministic columns are
identical across all ten `session_length_scaling` runs and both trees —
checked by hashing the extracted columns, not by eye:

- `session_length_scaling`, all 10 runs (4 campaign, 6 control), the
  (`facts`, `searches`, `folded/a`, `drank/t`, `ledger_len`) tuple for every
  one of the 10 bands: **one md5, `3e583245b6264b33d9c71c2278f47839`, on every
  one of the ten files.** Band 1 reads `facts 2436 · searches 1658 · folded/a
  32.2 · ledger_len 24214`; band 10 reads `1320 · 523 · 124.4 · 35832`.
  `drank/t` is `0.0000` at every band on every run — the probe agent commits
  ZERO `drank` facts across 200 ticks, so the single-reset regime is its
  production regime, unchanged from The Pawl.
- `agent_scaling`, all 8 runs (4 campaign, 4 control), the (`facts/a/tick`,
  `search/a/tick`, `bytes/agent`, `total_bytes`, `facts`, `searches`) tuple for
  all four rungs: **one md5, `ea5f65bffc699eef572cab601dfeb8d1`, on every one
  of the eight files.** The largest rung reads `facts 9233 · searches 6543 ·
  total_bytes 4377119` on both trees.

That is what makes the pairing legitimate, and it is the readout's own small
witness for H3. It also discharges the specific worry the readout was warned
about: the control's merge base predates The Reservoir, which the campaign tree
has absorbed, and if The Reservoir had moved a walk the deterministic columns
would have disagreed across the trees. They do not, on any of the 18 runs.

*(These band figures differ from The Pawl §12.5's — `facts 2800 · searches 1825
· folded/a 35.5 · ledger_len 24485` — because the world moved between that
campaign's merge base and this one. The check is agreement across THIS
campaign's two trees, and that holds exactly.)*

### 11.2 `session_length_scaling` — the decisive H4 column (`hazard_memory_memo`)

50 agents, 200 ticks, bands of 20, seed 42, probe agent fixed at the
max-`agent-at` roster member, history 101 → 260 (2.57×) across the warm bands.
Read from `sls-{control,campaign}-N.txt`.

**Control at `0dccce029` (pre-campaign, same box, same session, interleaved).**

| run | k (µs/call/fact) | r² | elasticity | C (µs/call) | final-band µs/call |
|---|---|---|---|---|---|
| B1 — **SET ASIDE** | 444.45893 | 0.815 | 1.18 | −10 403.818 (negative) | 130 213.28 |
| B2 — **SET ASIDE** | 658.32771 | **0.491** | 1.19 | −16 485.373 (negative) | 107 964.21 |
| B3 | 338.47176 | 0.998 | **0.91** | **+5 244.125** | 91 546.59 |
| B4 | 344.82265 | 0.996 | **0.93** | **+4 215.829** | 91 866.62 |
| B5 | 345.11400 | 0.997 | **0.93** | **+3 954.747** | 92 157.46 |
| B6 | 346.51982 | 0.998 | **0.94** | **+3 725.546** | 92 945.29 |

**Campaign branch (`5bf12d8ef`).**

| run | k (µs/call/fact) | r² | elasticity | C (µs/call) | final-band µs/call |
|---|---|---|---|---|---|
| P1 | 0.17311 | 0.958 | **0.25** | **+87.027** | 132.09 |
| P2 | 0.16660 | 0.928 | **0.24** | **+89.509** | 132.60 |
| P3 | 0.16979 | 0.965 | **0.23** | **+93.573** | 139.06 |
| P4 | 0.19362 | 0.967 | **0.27** | **+85.609** | 137.54 |

**Medians over the valid runs: elasticity 0.93 before, 0.245 after. `k` 344.968
→ 0.17145, a factor of 2 012. `C` +4 085.288 → +88.268 µs/call, positive on both
sides. Final-band cost 92 012.04 → 135.07 µs/call — 92.012 ms/call before,
0.13507 ms/call after, a factor of 681.2.**

**The `r² ≥ 0.5` filter, stated rather than applied silently.** §4 counts only
runs whose fit clears r² 0.5. On this column the filter excludes **nothing among
the valid runs**: every campaign run sits at 0.928–0.967 and every valid control
run at 0.996–0.998. It does bite on one *set-aside* run — B2's 0.491, the only
reading below the floor anywhere in this table — and that run was already
excluded by the load rule, which is a small corroboration that the two filters
are pointing at the same noise. The verdict does not depend on how the boundary
is read:

| reading of the filter | qualifying campaign runs | median elasticity | median final-band µs/call |
|---|---|---|---|
| as printed, r² ≥ 0.5 | P1–P4 | **0.245** | **135.07** |
| strict, dropping P2 at the lowest r² (0.928) | P1, P3, P4 | **0.25** | **137.54** |
| ignore the filter, all four | P1–P4 | **0.245** | **135.07** |

The elasticity is above 0.20 under all three readings and the final-band cost is
two-and-a-half orders of magnitude down under all three. §11.4 states both
verdicts.

### 11.3 `session_length_scaling` — the whole tick, and attribution

| | k (ms/tick/fact) | r² | C (ms/tick) | history share at band 10 | band-2 ms/tick* | band-10 ms/tick* |
|---|---|---|---|---|---|---|
| control B1 — **SET ASIDE** | 4.32299 | 0.584 | +153.147 | 77.8% | 341.42 | 890.23 |
| control B2 — **SET ASIDE** | 14.06364 | 0.798 | −390.741 (negative) | — | 364.64 | 1 496.04 |
| control B3 | 3.53061 | 0.995 | +202.101 | 68.5% | 338.33 | 632.34 |
| control B4 | 3.51587 | 0.985 | +202.999 | 68.3% | 332.68 | 630.31 |
| control B5 | 3.55267 | 0.989 | +199.380 | 68.9% | 332.61 | 630.93 |
| control B6 | 3.58660 | 0.994 | +197.251 | 69.3% | 336.18 | 636.96 |
| campaign P1 | 2.30338 | 0.983 | +179.385 | 61.5% | 266.70 | 455.75 |
| campaign P2 | 2.32288 | 0.990 | +186.110 | 60.8% | 270.86 | 470.72 |
| campaign P3 | 2.31068 | 0.990 | +196.985 | 59.3% | 285.94 | 480.84 |
| campaign P4 | 2.28648 | 0.986 | +183.533 | 60.8% | 267.16 | 457.44 |

**Medians over valid runs: `k` 3.54164 → 2.30703 (−34.9%). `C` 200.7405 →
184.8215 (−15.919 ms/tick, i.e. it FELL — see §11.5). Share 68.70% → 60.80%
(−7.9 points). Band-10 ms/tick\* 631.63 → 464.08 (−26.5%). Band-2 ms/tick\*
334.43 → 269.01 (−19.6%).**

Final-band µs/call by fold, medians of the valid runs a side:

| fold | control (pre) | campaign (post) | ratio | pre elasticity | post elasticity |
|---|---|---|---|---|---|
| `drive_at` | 2.62 | 2.62 | 1.00× | 0.01 | 0.01 |
| `hunger_at` | 2.49 | 2.47 | 1.01× | −0.01 | 0.01 |
| `fatigue_at` | 60.38 | 62.44 | 0.97× | 0.29 | 0.30 |
| `believed_water` | 8 503.18 | 8 699.80 | 0.98× | 0.96 | 0.96 |
| `shared_believed_water` | 8 611.68 | 8 824.49 | 0.98× | 0.95 | 0.98 |
| **`hazard_memory_memo`** | **92 012.04** | **135.07** | **681.22×** | **0.93** | **0.24** |

One fold collapsed and nothing else did. That is the signature of the room memo
and the verdict index and of nothing else: the two Sustenance reads and the two
KnownWater tenants are within 3% of the control in both directions, which is
measurement noise on this bench, and their elasticities are unchanged to two
decimals.

**The attribution flipped.** The six folds' final-band costs sum to 109 192.40
µs/call before and 17 726.89 µs/call after. `hazard_memory_memo` was **84.27%**
of that sum and is now **0.762%**. The two KnownWater tenants
(`believed_water`, `shared_believed_water`) are now **98.86%** of it. The
campaign's own quarry is gone and the next one is named by the same table.

### 11.4 The verdicts, against §4

Every criterion separately. Where §4 names both a frozen figure and a same-box
control, both are given and the comparison §4 actually asked for is named.

| criterion | §4 threshold | measured | control (same box) | verdict |
|---|---|---|---|---|
| **H4 (a)** `hazard_memory_memo` median elasticity, runs with r² ≥ 0.5 | **< 0.20**, from 1.06–1.21 | **0.245** (0.245 / 0.25 / 0.245 under the three readings of the filter, §11.2) | 0.93 | **NOT MET** — over by **0.045**, under every reading |
| **H4 (b)** `hazard_memory_memo` final-band cost, ≥ 10× down | **≥ 10×**, from the **frozen 73–97 ms/call** | **0.13507 ms/call** → **540.5×** against the frozen 73 ms and **718.1×** against the frozen 97 ms | 92.012 ms/call → **681.2×** | **MET** — against the frozen figure (which is what §4's clause asks for) *and* against the same-box control, by 54–72× more than required either way |
| **H2 (c)** whole-tick history share at band 10 | **< 20%**, from 70–80% | **60.80%** | 68.70% | **NOT MET** — over by **40.8 points** |
| **H2 (a)** `drive_at` median elasticity (no-regression, not a criterion) | < 0.20 | **0.01** | 0.005 | no regression |
| **H2 (b)** `C` identifiable and positive on `drive_at` (no-regression) | positive | **+2.395 / +2.529 / +2.557 / +2.532**, positive on all four | positive on all four | no regression |
| **H3** no world-state artifact moves | — | held by construction (§5) + the campaign-time ledger-hash witnesses; the readout adds the byte-identity witness of §11.1, which holds across all 18 runs and both trees | — | **held, not re-measured** |
| **H5** repeat read takes 0 field samples; whole tick ≤ 4 469 | 0, and ≤ 4 469 from 44 694 | **0 warm `hazards()` calls, 0 warm field samples, 0 second-fresh samples**; whole tick 60 makes **3 168** `hazards()` calls and **0** field samples | pre-fix 22 302 / 44 694 (§4, taken on the merge base at Task 1) | **MET** — 3 168 is **14.1×** down, not 10× |
| **H6** scan work per tick is O(new sightings) | judged/tick grows strictly slower than distinct rooms | judged **141 → 94** (0.6667×) against distinct rooms **1 430 → 2 307** (1.6133×), **margin 2.4199×** | — | **MET** |
| **M1** bytes held, three shapes, no threshold | — | below | — | **recorded** |

Two criteria are met, two are not, and one no-threshold measurement is recorded.
Nothing is averaged across a failure.

**Why the two that failed still fail, as attribution rather than excuse.** Both
are *shares*, and both are now dominated by folds this campaign does not touch.
H4 (a) asks the memo's own cost to stop tracking history; it fell from
elasticity 0.93 to 0.24, but 0.24 is not 0.20, and the residual is the fear
path's remaining per-tick work — the walk's `Danger::urgency` sampling that
Task 6 measured at 2 268 of tick 60's 3 168 `hazards()` calls (71.6%), which the
memo answers cheaply but still answers once per candidate room per step.
H2 (c) asks the *whole tick's* history term to fall below a fifth; the tick's
remaining history term now belongs almost entirely to the two KnownWater
tenants, which are 98.86% of the six-fold sum (§11.3) and which this campaign
does not touch at all. A memo over the terrain cannot move a share the
KnownWater folds dominate. H4 (b) — the clause §4 itself called the weaker of
the two — is met by more than fifty times its own margin.

**H5's raw witness lines** (`h5-h6-witness.txt`, the `h5_witness` filter run
`--nocapture` against `hornvale-vessel`'s suite on the campaign tree):

```
--- H5 witness: seed 42, 50 agents, tick 60 ---
probe: FRESH memo 450 hazards() calls, WARM memo 0, scans +1 (with emitters +0),
       alarm replays +0, shunned 0, first-fresh samples 0, warm samples 0,
       second-fresh samples 0
whole tick 60: 3168 hazards() calls, 31 facts committed; roster distinct rooms 2307
ground memo: 11149 misses, 257546 hits, 11149 rooms held
whole tick 60: 0 field samples against 3168 hazards() calls
H5 attribution of tick 60's 3168 hazards() calls: (a) alarm_field_memo over the
  roster 450, (b) one fresh-memo hazard read 450, (a)+(b) = 900,
  walk remainder (c-a-b) = 2268
H6: judged/tick profile [0, 16, 0, 14, 0, 21, 73, 155, 146, 145, 139, 147, 141,
  149, 141, 143, 87, 2, 0, 2, 0, 95, 0, 0, 0, 2, 94, 2, 5, 2, 0, 94, 0, 2, 0, 2,
  96, 2, 0, 0, 0, 99, 0, 2, 0, 0, 98, 0, 0, 0, 0, 97, 0, 0, 0, 0, 94, 0, 0, 0]
H6: tick 60 judged 0 rooms; the last tick that judged anything is tick 57 (94 rooms).
  Comparison: tick 15 -> tick 57: judged 141 -> 94; distinct rooms 1430 -> 2307
H6: judged growth 0.6667x against distinct-room growth 1.6133x (margin 2.4199x)
test the_detent::h5_witness_the_hazard_reads_terrain_samples_on_the_bench_shape ... ok
```

The memo's own shape is printed beside the zero deltas deliberately (11 149
misses, 257 546 hits, 11 149 rooms held), because an *unthreaded* terrain would
produce the same zeroes vacuously. It is also a cross-check on the bench: 11 149
rooms held at 60 ticks is exactly `sls`'s band-3 `ground_len` (ticks 40–59), on
a different instrument.

**M1, the bytes and counts the two structures hold, on all three shapes.** Byte
figures are the structures' own `held_bytes` estimates, not an allocator
measurement, and are identical across all four campaign `sls` runs.

| shape | instrument | room memo entries | room memo bytes | index entries | index bytes |
|---|---|---|---|---|---|
| seed 42, 50 agents, 200 ticks (band 10) | `session_length_scaling` (`sls-campaign-*.txt`) | **18 902** | **1 455 454** (~1.455 MB) | **4 665** | **214 590** (~215 KB) |
| seed 42, 50 agents, 60 ticks | H5 witness (`h5-h6-witness.txt`) | **11 149** | not exposed | not printed | not exposed |
| seed 6, possession, 4 waits, 127 bodies | `detent_rule2_probe` (`r2-campaign-{4,5,6}.txt`) | **83** | not exposed | **355** | not exposed |

The bench's own per-entry ratios at band 10 are 77.0 bytes/room and 46.0
bytes/index-entry; applying them to the possession shape's 83 and 355 gives
~6.4 KB and ~16.3 KB, but the estimate is dominated by `room.path.len()`, which
differs per shape, so it is stated as an extrapolation and not as a measurement.
`Session` owns both structures privately and exposes counts but no byte
accessor; widening the production surface to give a readout a third byte figure
was declined.

**Band-by-band M1 growth** (`sls-campaign-1.txt`, identical on all four runs):

```
 band ground_len ground_bytes index_entries  index_bytes
    1       7748       596596          1521        69966
    2       9551       735427          1917        88182
    3      11149       858473          2307       106122
    4      12624       972048          2671       122866
    5      14051      1081927          3022       139012
    6      15424      1187648          3376       155296
    7      16474      1268498          3725       171350
    8      17390      1339030          4071       187266
    9      18244      1404788          4393       202078
   10      18902      1455454          4665       214590
```

Both structures grow monotonically and nothing in this campaign evicts either.
The band-over-band increments are falling (1 803 → 658 rooms per band), which is
the roster running out of new rooms rather than any bound in the mechanism.
**This is the figure Penstock stage 4 (the lifecycle) enters on**, and it is
stated as that, not as a criterion.

### 11.5 The falsifier, and the level

Both are stated before the campaign's favourable results elsewhere, because both
are the ones that could have gone against it.

**The falsifier does not fire, and this time it cannot.** §4's falsifier is "`k`
falls but `C` rises by more than the `k` saving at realistic session lengths".
From the whole-tick affine fits (§11.3 medians):

```
pre:  ms/tick = 200.7405 + 3.54164 h
post: ms/tick = 184.8215 + 2.30703 h
```

**`C` did not rise. It fell by 15.919 ms/tick, while `k` fell by 1.23461
ms/tick per fact.** The nominal crossover is `h = −15.919 / 1.23461 = −12.9` —
negative, meaning the two lines cross at a history no session can have, and the
post-campaign line is below the pre-campaign line at **every** `h ≥ 0`. The
Pawl's second readout put the crossover at `h ≈ 25.9`, just below the sampled
range; this campaign removes it from the positive axis entirely.

| session length | h | pre | post | verdict |
|---|---|---|---|---|
| 20 ticks | ≈ 32.2 | 314.78 ms/tick | 259.11 ms/tick | post **17.7%** faster |
| 50 ticks | ≈ 52.6 | 387.03 ms/tick | 306.17 ms/tick | post **20.9%** faster |
| 200 ticks | 124.4 | 641.32 ms/tick | 471.82 ms/tick | post **26.4%** faster |

The independent check is the shallowest band actually measured: band-2
`ms/tick*` is 334.43 before and 269.01 after — post **19.6%** faster, agreeing
with the fit's 20.9% at the comparable `h`.

**The level — reported, not predicted.** `agent_scaling`, ms/tick over 20 ticks,
paired and interleaved (`as-{control,campaign}-N.txt`).

| run | tree | start (UTC) | load before | load after | 10 | 50 | 100 | **200** |
|---|---|---|---|---|---|---|---|---|
| BA1 | control | 00:34:11 | 3.66 / 2.90 / 7.77 | 3.67 / 3.10 / 7.52 | 57.492 | 375.801 | 671.474 | **1 569.310** |
| A1 | campaign | 00:35:36 | 3.67 / 3.10 / 7.52 | 3.34 / 3.08 / 7.23 | 52.198 | 344.283 | 596.552 | **1 420.929** |
| BA2 | control | 00:36:43 | 3.34 / 3.08 / 7.23 | 2.86 / 2.96 / 6.90 | 57.997 | 377.139 | 649.747 | **1 566.497** |
| A2 | campaign | 00:37:47 | 2.86 / 2.96 / 6.90 | 3.29 / 3.10 / 6.71 | 51.691 | 342.166 | 593.317 | **1 492.925** |
| BA3 — **SET ASIDE** | control | 00:38:50 | 3.29 / 3.10 / 6.71 | **16.00** / 6.47 / 7.64 | 59.078 | 396.955 | 688.917 | 2 060.362 |
| A3 — pair set aside | campaign | 00:43:16 | 5.13 / 6.19 / 7.29 | 3.53 / 5.57 / 6.99 | 52.115 | 343.636 | 592.166 | 1 421.534 |
| BA4 | control | 00:44:23 | 3.41 / 5.51 / 6.96 | 3.45 / 5.14 / 6.73 | 58.196 | 376.631 | 650.083 | **1 559.718** |
| A4 | campaign | 00:45:27 | 3.45 / 5.14 / 6.73 | 2.68 / 4.63 / 6.43 | 52.234 | 344.014 | 594.854 | **1 426.377** |

**At 200 agents, the three valid pairs: 1 569.310 → 1 420.929 (−9.46%),
1 566.497 → 1 492.925 (−4.70%), 1 559.718 → 1 426.377 (−8.55%). Median
−8.55%.** By medians rather than pairs, 1 566.497 → 1 423.956 = −9.10%. The
level moved by roughly 9% in the campaign's favour, at every rung:

| rung | control median | campaign median | delta |
|---|---|---|---|
| 10 | 57.997 | 52.157 | **−10.07%** |
| 50 | 376.631 | 343.825 | **−8.71%** |
| 100 | 650.083 | 594.086 | **−8.61%** |
| 200 | 1 566.497 | 1 423.956 | **−9.10%** |

The fitted log-log slope is 1.08 on the control and 1.09 on the campaign branch
— unchanged. The campaign lowered the level without changing the shape of the
agent-count scaling, which is the expected signature of a per-agent read getting
cheaper.

**The one place two instruments disagree, stated plainly.** The affine fits
*predict* post 17.7% faster at `h ≈ 32.2`; `agent_scaling` *measures* 8.6–10.1%
across its rungs — a factor of about two, where The Pawl's equivalent
cross-check agreed to within half a point. The two benches are not measuring the
same object and the difference is structural, not noise: `agent_scaling` drives
`step_with_occupancy` directly with a caller-owned `HomeNavCache` and
`RoomMeshMemo` that persist across ticks, while `session_length_scaling`'s
whole-tick column is `Session::wait`'s full turn, which evaluates the walk a
second time through `hornvale_kernel::tick` on a throwaway cache. The fold the
campaign removed is paid twice in the second and once in the first, so a
prediction from the second overstates the first by about the ratio observed.
This is a caveat on the cross-check, not on either measurement: both instruments
move the same direction, on every rung and every band, on a quiet box, with
byte-identical workloads.

### 11.6 `fold_depth_sweep` — the no-regression control

**This bench sweeps `drive_at` only and cannot see this campaign's fold**, and
is run as the no-regression control on the Sustenance reads, which is what §4
says it is. One run each tree, back to back inside the same quiet window (both
at 2.68 / 4.63 / 6.43 before and after). µs/call, median of 6
alternating-direction passes (`fds-{control,campaign}-1.txt`).

| depth | PERIODIC control | PERIODIC campaign | SINGLE-RESET control | SINGLE-RESET campaign |
|---|---|---|---|---|
| 10 | 0.108 | 0.107 | 0.106 | 0.105 |
| 32 | 0.154 | 0.155 | 0.157 | 0.158 |
| 100 | 0.242 | 0.250 | 0.286 | 0.293 |
| 320 | 0.534 | 0.525 | 0.557 | 0.567 |
| 1 000 | 1.354 | 1.382 | 1.541 | 1.608 |
| 3 200 | 4.450 | 4.526 | 5.096 | 4.979 |
| 10 000 | 14.307 | 14.294 | 15.106 | 15.109 |

| | control | campaign |
|---|---|---|
| PERIODIC `k` (µs/call/fact) | 0.00142 (r² 1.000) | 0.00142 (r² 1.000) |
| PERIODIC raw elasticity, top third | 1.025 | 1.009 |
| SINGLE-RESET `k` | 0.00150 (r² 1.000) | 0.00150 (r² 1.000) |
| SINGLE-RESET raw elasticity, top third | 0.954 | 0.974 |

**No regression.** Every depth in both regimes agrees within 5%, the two `k`
values are identical to five decimals, and the single-reset column still sits on
top of the periodic one — the shape The Pawl's accumulator left behind,
untouched here.

### 11.7 Rule 2's number

Spec §3 rule 2 asks for the affect replay's *share of the hazard read*, which is
a time; the committed witness
(`rule_two_witness_the_affect_replay_share_of_the_hazard_read`) counts, because
`Instant` is banned in this project's tests. So the timing lives in a new
`--release` example, `windows/vessel/examples/detent_rule2_probe.rs`, over the
identical shape — seed 6, `PossessOpts::default()`, four `wait`s, then
whole-roster `Session::hazard_memories()` calls. It is a readout instrument and
never a gate, the standing of `turn_cost.rs` and `agent_scaling.rs`. It needs no
`world-build-sites.tsv` row: that roster scans `src/` and `tests/` only and
`examples/` is invisible to it by construction (`cli/tests/suite/
world_build_sites.rs`).

**The two instruments agree exactly on the counts** — the probe reprints
`2.6220` replays and `180.7953` warm memo lookups per body read, the same
figures Task 8's witness printed — which is what licenses reading the probe's
timings as belonging to the same measurement.

Medians over R4–R6 (`r2-campaign-{4,5,6}.txt`), 127 bodies:

| read | µs per body read | range over three runs | replays/read | memo lookups/read |
|---|---|---|---|---|
| **cold** (first whole-roster read) | **784.695** | 779.206 – 788.498 | 2.6220 | 186.6772 |
| **warm 1** | **784.941** | 783.548 – 786.745 | 2.6220 | 180.7953 |
| **warm 2** | **782.017** | 777.169 – 786.877 | 2.6220 | 180.7953 |

R1–R3, on the pre-M1 build, read cold 784.944 / 797.010 / 807.980 and warm
764.981–855.910 — the same figures within noise.

**"Cold" is not a cold session, and saying so is the point.** The four `wait`s
have already filled the room memo for every room the walk touched, so the first
whole-roster read differs from the second only by the 747 extra memo lookups it
takes (186.6772 − 180.7953 = 5.88 per body) filling in the remainder. The
measured difference between cold and warm is **0.25 µs per read on 785**, i.e.
nothing: **after four waits the memo is already warm, and a first whole-roster
read costs what a repeat read costs.** That is itself a result about the memo.

Totals over the run: `alarm replays 1620`, `ground hits 332745`, `ground misses
83` — 4 000 : 1 hits to misses, on 83 distinct rooms held. **The controller
decides rule 2's branch from these numbers; this task does not.**

### 11.8 What the readout hands forward

- **The largest remaining fold is `shared_believed_water`, then
  `believed_water`**, read straight off §11.3's table: 8 824.49 and 8 699.80
  µs/call at the final band against `hazard_memory_memo`'s 135.07. Together they
  are **98.86%** of the six folds' cost and their elasticities are 0.98 and 0.96
  — history-proportional, untouched by this campaign, and now the whole of the
  tick's remaining history term. They are why H2 (c) fails at 60.80% and they
  are the next quarry, exactly as `hazard_memory_memo` was this campaign's after
  The Pawl's readout named it.
- **The remaining terrain questions per tick are the walk's own, not the fear
  path's.** Tick 60 makes 3 168 `hazards()` calls, of which the alarm field
  takes 450 and one fresh-memo hazard read takes 450; the remaining **2 268
  (71.6%)** are `Danger::urgency`'s per-step sampling in `advance_one`'s decide
  loop — once per candidate room per step, which Task 6 measured and which this
  campaign deliberately does not touch. They now cost a memo lookup each rather
  than a field blend, which is why the tick's field samples are **0**, but they
  are still 2 268 questions asked.
- **Nothing evicts either structure**, and §11.4's table is the figure Penstock
  stage 4 enters on: 18 902 rooms / ~1.455 MB and 4 665 index entries / ~215 KB
  at 200 ticks on the 50-agent shape, growing monotonically with decelerating
  increments.
- **Two things the numbers say that the criteria did not ask.** First, the
  falsifier's intercept moved the *right* way for the first time in this program
  — The Pawl's `C` rose by 45.1 ms/tick and had to be defended against a
  crossover; this campaign's fell by 15.9, so there is no crossover to defend.
  Second, the two set-aside control runs are a small positive control on the
  quiet-box rule itself: B1 and B2 report elasticity 1.18 and 1.19 and r² 0.815
  and 0.491 against the four valid runs' 0.91–0.94 and 0.996–0.998, so the load
  rule and the r² floor flagged the same two runs independently. A rule that
  never excluded anything would have told us nothing about whether it was
  working.

## 12. The second readout, after the prefix read

Every number below is a fresh run on the development Mac, `--release`, taken
2026-09-02 between 21:20 and 22:17 local (2026-09-03 01:20–02:17 UTC), paired
and interleaved against the **same** merge-base control §11.1 introduced,
unchanged and un-rebuilt. **§4 is not edited and §11 is not rewritten**; both
readouts are reported, and §11 stands exactly as it was measured. Every number
cites the file under `.superpowers/sdd/2026-09-02-the-detent/readout2/` it was
read from. A criterion that fails is reported as NOT MET with its margin, and
nothing is averaged across a failure.

### 12.0 Exactly one post-unblinding change was made, and what kind of change it was

Between §11 and §12 the campaign made **one** change to production code:
**`932409875`**, "the emitter-free read no longer builds the latest-visit map".
It is 42 insertions and 23 deletions in one function, `hazard_memory_memo`
(`windows/vessel/src/liveness.rs`), and no other file.

**It is mechanism-completing, not constant-tuning, and the distinction is
checkable rather than rhetorical.** §2.3 specified the emitter-free read as a
PREFIX read over the verdict index. Task 6 shipped the prefix read but left
`visits.latest_at(npc.entity, t)` — an O(distinct rooms visited) `BTreeMap`
build — **above** the emitter-free early return, sharing a guard with the rule-6
witness, because the plan's own Task 6 text said to "leave the `latest` block
where it is". On the wandering probe this bench fixes, distinct rooms ARE
history (measured: the map holds exactly 121 entries at tick 60 and 260 at tick
200, equal to the probe's `agent-at` count at each depth), so the path the
design had made prefix-bounded still carried an O(history) term. The change
moves that map below the early return, into the emitter path, under its own
guard dropped before the emitter loop re-enters the store. The witness call
keeps its place and its ordering — it is taken FIRST, before any early return,
and needs only `trail.of(entity).last()`, which it already had.

**Ruling #7 (ledger, 2026-09-02) permitted it in advance, conditionally:**

> Exactly one post-unblinding change is permitted, under The Pawl's rule:
> verify the mechanism by MEASUREMENT before touching it; if confirmed,
> complete the design […]; prove byte-identity […]; then a §12 second readout,
> interleaved against the same control, with §11 left standing and the change
> disclosed as mechanism-completing — no threshold, constant or criterion
> moves. If the measurement does NOT confirm the mechanism, no change is made
> and H4 (a) is handed forward as a finding.

**The verification was run first, and its first attempt REFUSED the change.**
#7's verification clause required `latest_at` to be "≥ 50% of the fresh read's
cost at band 10"; the measurement (two quiet runs,
`step1-9c/step1-run{1,2}.txt`) put it at **24.00% and 24.46%**, and the task
stopped without touching the code. **Ruling #8 then corrected the clause, not
the number:**

> H4 (a) is a criterion about the SLOPE, and the clause tested the LEVEL […]
> Re-read #7's verification as share of the history term, which is the quantity
> the criterion is about; the hypothesis is confirmed at 72–76% […]. No
> threshold, constant or criterion moves; §11 stands.

The measured decomposition #8 acted on, from the same two runs: `latest_at`
costs 14.020/14.000 µs/call at tick 60 and 31.887/33.144 at tick 200, growing
**2.27×/2.37×** over a 2.149× history growth, which is **71.97%/75.80% of the
fresh read's history-driven growth** — of the fitted `k` that H4 (a) measures —
while being only 24% of a total that §11.2 had already fitted as
intercept-dominated (`C` = +88.268 µs/call). Three cross-checks against §11.2
were taken before the change: the implied `k` (0.1786/0.1817 µs/fact against
§11.2's fitted 0.17145), the remainder (~101 µs against `C` +88.268), and the
fresh read itself (132.878/135.487 against §11.2's median 135.07).

**No threshold, no constant and no criterion moved.** §4 is byte-for-byte what
it was when frozen on 2026-08-24; §11 is unedited; the campaign-time constants
(`ledger_hash_witness`'s two and `the_detent`'s two walk constants) hold
unchanged, which is what §12.5 re-witnesses. What #8 changed is **which
quantity a verification clause compares**, and it changed it in a ruling written
before the change was made and after the comparison under both readings was
recorded — which is the difference between correcting an instrument and tuning
to a result.

**The honest cost of this ordering: the second readout is not blind.** §11's
verdicts were taken without knowing what the fix would be; §12's were taken
knowing exactly what had been repaired and where to look. That is why §11 is
reported in full rather than superseded, and why the falsifier and the level —
the two results that could have gone against the campaign — are stated in §12.6
before §12.7 and §12.8.

### 12.1 The quiet-box rule, and how it was applied

§4's rule, applied exactly as §11.0 applied it: all three load averages recorded
immediately before and immediately after each run, and a run set aside if the
**1-minute** average is above 10 at either end. As in §11, the 1-minute average
was also sampled **every 30 s during** each run and appended to that run's own
file, so a spike that landed and decayed between the two endpoint readings
cannot hide; the peak-in-run column below is the maximum of those samples.

The box was contested again, by the same campaign. `campaign/the-rack` was
running a debug test binary at **886.8% CPU** (`ps aux`, 21:28 local) and drove
the 1-minute average to 90.5. The first `session_length_scaling` control run was
destroyed by it and is set aside; the session then polled — no worktree
`target/` process and a 1-minute average below 6 — until the window opened at
01:35:33Z, and a `claim` was posted to the board (`e43cd3e21094…`). Two idle
processes were resident throughout and are named rather than omitted: a
`target/release/hornvale-game --seed 42` at 0.0% CPU and a shell watcher at 0.0%
CPU, neither of which is a load.

| run | bench | tree | start (UTC) | load before (1/5/15) | load after | peak 1-min in run | verdict |
|---|---|---|---|---|---|---|---|
| B1 | `session_length_scaling` | control | 01:20:08 | 4.04 / 6.62 / 5.99 | **88.74** / 55.71 / 28.84 | 91.58 | **SET ASIDE** |
| B2 | `session_length_scaling` | control | 01:35:45 | 4.34 / 29.71 / 29.37 | 3.39 / 13.86 / 22.19 | 4.34 | valid |
| P1 | `session_length_scaling` | campaign | 01:40:21 | 3.39 / 13.86 / 22.19 | 4.58 / 10.56 / 19.67 | 4.86 | valid |
| B3 | `session_length_scaling` | control | 01:42:36 | 4.45 / 10.43 / 19.57 | 2.72 / 6.00 / 14.98 | 4.77 | valid |
| P2 | `session_length_scaling` | campaign | 01:47:14 | 2.72 / 6.00 / 14.98 | 2.69 / 4.94 / 13.37 | 3.02 | valid |
| B4 | `session_length_scaling` | control | 01:49:21 | 2.56 / 4.87 / 13.30 | 2.99 / 3.72 / 10.42 | 3.61 | valid |
| P3 | `session_length_scaling` | campaign | 01:53:55 | 2.99 / 3.72 / 10.42 | 2.30 / 3.16 / 9.31 | 2.99 | valid |
| B5 | `session_length_scaling` | control | 01:56:04 | 2.16 / 3.11 / 9.22 | **10.27** / 5.53 / 8.40 | 11.71 | **SET ASIDE** |
| P4 | `session_length_scaling` | campaign | 02:00:51 | **10.27** / 5.53 / 8.40 | 4.07 / 4.66 / 7.65 | 10.27 | **SET ASIDE** |
| B6 | `session_length_scaling` | control | 02:03:07 | 3.75 / 4.57 / 7.59 | 3.05 / 3.50 / 6.25 | 3.75 | valid |
| P5 | `session_length_scaling` | campaign | 02:07:40 | 3.05 / 3.50 / 6.25 | 3.37 / 3.44 / 5.85 | 3.40 | valid |
| BA1 | `agent_scaling` | control | 02:09:47 | 3.18 / 3.40 / 5.83 | 2.88 / 3.25 / 5.60 | 3.18 | valid |
| A1 | `agent_scaling` | campaign | 02:10:47 | 2.88 / 3.25 / 5.60 | 2.30 / 3.02 / 5.37 | 2.88 | valid |
| BA2 | `agent_scaling` | control | 02:11:42 | 2.30 / 3.02 / 5.37 | 3.59 / 3.33 / 5.33 | 3.59 | valid |
| A2 | `agent_scaling` | campaign | 02:12:43 | 3.59 / 3.33 / 5.33 | 3.20 / 3.26 / 5.17 | 3.59 | valid |
| BA3 | `agent_scaling` | control | 02:13:38 | 3.20 / 3.26 / 5.17 | 3.27 / 3.24 / 5.04 | 3.20 | valid |
| A3 | `agent_scaling` | campaign | 02:14:38 | 3.27 / 3.24 / 5.04 | 2.66 / 3.08 / 4.86 | 3.27 | valid |
| BA4 | `agent_scaling` | control | 02:15:39 | 2.60 / 3.06 / 4.84 | 2.88 / 3.05 / 4.72 | 2.88 | valid |
| A4 | `agent_scaling` | campaign | 02:16:39 | 2.88 / 3.05 / 4.72 | 3.10 / 3.09 / 4.63 | 3.07 | valid |
| F-B | `fold_depth_sweep` | control | 02:17:34 | 3.10 / 3.09 / 4.63 | 3.10 / 3.09 / 4.63 | 3.10 | valid |
| F-P | `fold_depth_sweep` | campaign | 02:17:35 | 3.10 / 3.09 / 4.63 | 2.93 / 3.06 / 4.61 | 3.10 | valid |

**Three of eleven `session_length_scaling` runs are set aside; all four
`agent_scaling` pairs are valid**, which is one more valid pair than §11 had.
B5's spike arrived in its last 60 s (samples 3.74 → 7.87 → 11.71 across
01:59:34–02:00:34) and P4 inherited it as its *before* reading; both are set
aside by the endpoint rule even though P4's own *after* reading was 4.07. P4 is
reported in §12.2 anyway, because its readings agree with the four valid
campaign runs and a discarded run that agrees is worth showing.

Files: `sls-{control,campaign}-N.txt`, `as-{control,campaign}-N.txt`,
`fds-{control,campaign}-1.txt`, `h5-h6-witness.txt`.

### 12.2 `session_length_scaling` — the decisive H4 column (`hazard_memory_memo`)

50 agents, 200 ticks, bands of 20, seed 42, probe agent fixed at the
max-`agent-at` roster member, history 101 → 260 (2.57×) across the warm bands —
the identical construction §11.2 read. Read from `sls-{control,campaign}-N.txt`.

**Control at `0dccce029` (pre-campaign, same box, same session, interleaved,
the same binaries §11 used).**

| run | k (µs/call/fact) | r² | elasticity | C (µs/call) | final-band µs/call |
|---|---|---|---|---|---|
| B1 — **SET ASIDE** | 1 231.33635 | 0.554 | 3.51 | −117 048.776 (negative) | 184 527.86 |
| B2 | 333.68503 | 0.995 | **0.90** | **+5 733.995** | 89 958.05 |
| B3 | 348.41856 | 0.999 | **0.91** | **+5 144.485** | 94 402.78 |
| B4 | 344.34523 | 0.999 | **0.91** | **+5 348.347** | 94 326.17 |
| B5 — **SET ASIDE** | 378.25561 | 0.972 | 0.98 | +938.685 | 94 400.97 |
| B6 | 346.19079 | 0.998 | **0.92** | **+4 739.927** | 93 355.71 |

**Campaign branch (`932409875`).**

| run | k (µs/call/fact) | r² | elasticity | C (µs/call) | final-band µs/call |
|---|---|---|---|---|---|
| P1 | 0.01442 | 0.122 | **0.03** | **+92.258** | 93.89 |
| P2 | 0.02068 | 0.295 | **0.04** | **+91.323** | 97.65 |
| P3 | 0.02501 | 0.467 | **0.04** | **+89.957** | 97.23 |
| P4 — **SET ASIDE** | −0.00151 | 0.003 | −0.00 | +95.282 | 94.62 |
| P5 | 0.02513 | 0.390 | **0.04** | **+89.561** | 95.32 |

**Medians over the valid runs: elasticity 0.91 before, 0.04 after. `k`
345.26801 → 0.02285, a factor of 15 113. `C` +5 246.416 → +90.640 µs/call,
positive on both sides. Final-band cost 93 840.94 → 96.275 µs/call — 93.841
ms/call before, 0.096275 ms/call after, a factor of 974.7.**

Against §11's own medians on the same instrument: elasticity 0.245 → **0.04**,
`k` 0.17145 → **0.02285** (7.5× further down), final-band 135.07 → **96.275
µs/call** (a further 1.40×). The 38.8 µs/call that left the final band is the
map: §12.0's isolated timing measured it at 31.9–33.1 µs/call at that depth, and
the balance is the map's allocation and iteration inside the larger function.

**The `r² ≥ 0.5` filter, stated rather than applied silently — and this time it
bites, in the direction nobody wrote it for.** §4 counts only runs whose fit
clears r² 0.5. On the control column it admits 4 of 4 valid runs at 0.995–0.999.
**On the campaign column it admits 0 of 4**, because the campaign runs read
0.122, 0.295, 0.467 and 0.390. That is not noise: `k` is now 0.023 µs/call/fact
against a `C` of 90.6 µs/call, so there is no slope left for a line to explain,
and a fit to a flat scatter has a low r² *by construction*. The filter was
frozen when this fold read 1.06–1.21 and its job was to exclude a contended run
whose fit had fallen apart — which is exactly what it did to B1 (0.554 with an
elasticity of 3.51, already excluded by the load rule) and to §11's B2 (0.491).
Applied to a criterion that has succeeded, it empties the sample.

| reading of the filter | qualifying campaign runs | median elasticity | median final-band µs/call |
|---|---|---|---|
| as printed, r² ≥ 0.5 | **none of the four** | **undefined** | **undefined** |
| the best-fitting run alone (P3, r² 0.467) | P3 | **0.04** | **97.23** |
| ignore the filter, all four valid | P1, P2, P3, P5 | **0.04** | **96.275** |
| ignore the filter, including set-aside P4 | P1–P5 | **0.04** | **95.32** |

**The verdict does not depend on where the boundary is drawn; it depends only on
whether the boundary admits anything at all.** Every threshold that admits at
least one campaign run yields a median of 0.03–0.04, far under §4's 0.20; the
printed threshold admits none and leaves §4's own statistic undefined. §12.4
states the verdict with that caveat attached rather than resolving it silently
in either direction.

**One reading independent of the fit**, for a reader who declines to trust an
r²-0.12 line at all: the final-band cost fell to **96.275 µs/call from a
same-box control of 93 840.94**, and the whole of the remaining figure is within
6 µs of the intercept `C` measured on the *same* runs (90.640). A cost that
equals its own floor has no history term left to have an elasticity about.

**A cross-check on §12.0's decomposition, which predicted this number before it
was measured.** Step 1's two-point estimate said the read with the map removed
would sit at elasticity **0.08–0.09**; §12 measures **0.04**. The prediction was
an over-estimate in the same direction and by about the same factor as its
estimator's known bias — the two-point form read §11's own column at 0.270 where
the ten-band fit read 0.245 — and both figures are under §4's 0.20. The
mechanism was predicted, the size was predicted to within a factor of two, and
the sign was exact.

### 12.3 `session_length_scaling` — the whole tick, and attribution

| | k (ms/tick/fact) | r² | C (ms/tick) | history share at band 10 | band-2 ms/tick* | band-10 ms/tick* |
|---|---|---|---|---|---|---|
| control B2 | 3.44200 | 0.993 | +211.989 | 66.9% | 341.31 | 633.25 |
| control B3 | 3.06545 | 0.986 | +260.884 | 59.4% | 412.34 | 643.51 |
| control B4 | 3.50813 | 0.994 | +210.109 | 67.5% | 344.61 | 640.39 |
| control B6 | 3.53977 | 0.993 | +204.629 | 68.3% | 338.05 | 635.94 |
| campaign P1 | 2.26297 | 0.974 | +190.527 | 59.6% | 265.82 | 463.60 |
| campaign P2 | 2.29188 | 0.990 | +182.504 | 61.0% | 267.93 | 466.36 |
| campaign P3 | 2.23360 | 0.987 | +188.401 | 59.6% | 270.75 | 460.54 |
| campaign P5 | 2.24324 | 0.987 | +183.405 | 60.3% | 267.34 | 457.32 |

**Medians over valid runs: `k` 3.47506 → 2.25311 (−35.2%). `C` 211.0490 →
185.9030 (−25.146 ms/tick, i.e. it FELL — see §12.6). Share 67.20% → 59.95%
(−7.25 points). Band-10 ms/tick\* 638.16 → 462.07 (−27.6%). Band-2 ms/tick\*
342.96 → 267.63 (−22.0%).** B3's band-2 reading of 412.34 is the one outlier in
this column (the others cluster at 338–345); it is a valid run by the rule and is
left in the median rather than trimmed, which is why the control's band-2 median
sits at 342.96 rather than at B3's value.

Final-band µs/call by fold, medians of the valid runs a side:

| fold | control (pre) | campaign (post) | ratio | pre elasticity | post elasticity |
|---|---|---|---|---|---|
| `drive_at` | 2.66 | 2.54 | 1.04× | 0.01 | 0.04 |
| `hunger_at` | 2.52 | 2.45 | 1.03× | 0.02 | 0.04 |
| `fatigue_at` | 61.67 | 61.44 | 1.00× | 0.27 | 0.30 |
| `believed_water` | 8 630.06 | 8 663.95 | 1.00× | 0.95 | 0.96 |
| `shared_believed_water` | 8 765.10 | 8 836.63 | 0.99× | 0.94 | 0.97 |
| **`hazard_memory_memo`** | **93 840.94** | **96.275** | **974.72×** | **0.91** | **0.04** |

One fold collapsed and nothing else did, exactly as in §11.3: the two Sustenance
reads and the two KnownWater tenants are within 4% of the control in both
directions, which is measurement noise on this bench, and their elasticities are
unchanged to two decimals.

**The attribution, one step further than §11.3 took it.** The six folds'
final-band costs sum to 111 302.96 µs/call before and 17 663.28 µs/call after.
`hazard_memory_memo` was **84.31%** of that sum and is now **0.545%** (§11:
84.27% → 0.762%). The two KnownWater tenants (`believed_water`,
`shared_believed_water`) are now **99.08%** of it (§11: 98.86%). The campaign's
quarry is not merely gone; it is now the fourth-cheapest of the six, behind
`fatigue_at`.

### 12.4 The verdicts, against §4 — side by side with §11

Every criterion separately, against the same frozen §4. Where §4 names both a
frozen figure and a same-box control, both are given and the comparison §4
actually asked for is named. **§11's column is reproduced from §11.4 unedited.**

| criterion | §4 threshold | §11 measured | **§12 measured** | §12 control (same box) | §12 verdict |
|---|---|---|---|---|---|
| **H4 (a)** `hazard_memory_memo` median elasticity, runs with r² ≥ 0.5 | **< 0.20**, from 1.06–1.21 | 0.245 — **NOT MET** by 0.045 | **0.04** (no single run above 0.04; 0.03/0.04/0.04/0.04) | 0.91 | **MET on the quantity, by 0.16** — with the filter reported inapplicable: it admits **0 of 4** campaign runs (r² 0.122–0.467) because there is no slope left to fit, and 4 of 4 control runs (0.995–0.999). Every threshold admitting ≥ 1 campaign run gives 0.03–0.04; the printed one gives an undefined median (§12.2) |
| **H4 (b)** `hazard_memory_memo` final-band cost, ≥ 10× down | **≥ 10×**, from the **frozen 73–97 ms/call** | 0.13507 ms/call → 540.5× / 718.1× frozen; 681.2× control — **MET** | **0.096275 ms/call** → **758.2×** against the frozen 73 ms and **1 007.5×** against the frozen 97 ms | 93.841 ms/call → **974.7×** | **MET** — against the frozen figure (which is what §4's clause asks for) *and* against the same-box control, by 75–100× more than required either way |
| **H2 (c)** whole-tick history share at band 10 | **< 20%**, from 70–80% | 60.80% — **NOT MET** by 40.8 pts | **59.95%** | 67.20% | **NOT MET** — over by **39.95 points** |
| **H2 (a)** `drive_at` median elasticity (no-regression, not a criterion) | < 0.20 | 0.01 | **0.04** | 0.01 | no regression |
| **H2 (b)** `C` identifiable and positive on `drive_at` (no-regression) | positive | positive ×4 | **+2.411 / +2.365 / +2.364 / +2.303**, positive on all four | positive on all four | no regression |
| **H3** no world-state artifact moves | — | held by construction + hash witnesses + §11.1's byte-identity | **held, and re-witnessed**: the deterministic-column md5 on both benches is **identical to §11's own md5**, across all 19 runs of this readout and both trees (§12.5) | — | **held** |
| **H5** repeat read takes 0 field samples; whole tick ≤ 4 469 | 0, and ≤ 4 469 from 44 694 | 0 warm samples; tick 60 = 3 168 calls, 0 field samples — **MET** | **byte-identical witness output to §11's**: 0 warm calls, 0 warm samples, 0 second-fresh samples; tick 60 = **3 168** calls, **0** field samples | pre-fix 22 302 / 44 694 | **MET** — 14.1× down |
| **H6** scan work per tick is O(new sightings) | judged/tick grows strictly slower than distinct rooms | judged 141 → 94 vs rooms 1 430 → 2 307, margin 2.4199× — **MET** | **byte-identical**: judged **141 → 94** (0.6667×) vs rooms **1 430 → 2 307** (1.6133×), **margin 2.4199×** | — | **MET** |
| **M1** bytes held, three shapes, no threshold | — | 18 902 / 1 455 454 and 4 665 / 214 590 at band 10 | **identical, byte for byte, on all four valid campaign runs**: 18 902 rooms / 1 455 454 B and 4 665 index entries / 214 590 B at band 10 | — | **recorded, unchanged** |

**Three criteria met and one not, where §11 had two and two.** The criterion that
moved is H4 (a), and it moved from 0.245 to 0.04 — through §4's threshold, not to
it. H4 (b), already met by fifty times its margin in §11, is now met by
seventy-five to a hundred times. H2 (c) is unchanged in substance: 60.80% →
59.95%, still three times its threshold.

**Why H2 (c) still fails, as attribution rather than excuse — and why it is now
provably not this fold's fault.** H2 (c) asks the *whole tick's* history term to
fall below a fifth. The tick's remaining history term belongs almost entirely to
the two KnownWater tenants, which are **99.08%** of the six-fold sum (§12.3) and
which this campaign does not touch at all. `hazard_memory_memo` is now 0.545% of
that sum: even removing it *entirely* — cost zero, not merely history-free —
would move the whole-tick share by a fraction of a point. §11.4 could argue
this; §12 can compute it, because the fold whose share was in question is now
smaller than the noise between the two Sustenance reads.

### 12.5 The byte-identity witness, re-taken

**The workload did not move — not between the trees, and not between the two
readouts.** The deterministic columns were hashed rather than eyeballed, by the
same extraction §11.1 used, and the extraction was validated as a positive
control by re-running it over §11's own 18 files and reproducing §11.1's two
published md5s exactly before it was pointed at this readout's files.

- `session_length_scaling`, **all 11 runs of this readout** (5 campaign, 6
  control, set-aside runs included), the (`facts`, `searches`, `folded/a`,
  `drank/t`, `ledger_len`) tuple for every one of the 10 bands: **one md5,
  `3e583245b6264b33d9c71c2278f47839`, on every one of the eleven files — and it
  is the same md5 §11.1 published.**
- `agent_scaling`, all 8 runs (4 campaign, 4 control), the (`facts/a/tick`,
  `search/a/tick`, `bytes/agent`, `total_bytes`, `facts`, `searches`) tuple for
  all four rungs: **one md5, `ea5f65bffc699eef572cab601dfeb8d1`, on every one of
  the eight files — again the same md5 §11.1 published.**

Because §11's md5s were taken on a tree WITHOUT the change and §12's on a tree
WITH it, and both equal the same value, the change is byte-identical on the
workload these benches drive. That is the readout's own witness for H3, and it is
stronger than §11.1's: §11.1 established agreement across two trees at one
moment, §12.5 establishes it across two trees and two code states.

Three more witnesses, taken before the readout and reported here because they are
what licence the pairing:

- **The campaign-time constants hold unchanged.** The `the_detent`,
  `resident_folds` and `ledger_hash_witness` filters over `hornvale-vessel`'s
  suite ran **53 tests, 53 passed, 0 failed**, including
  `ledger_hash_witness::the_seed_42_walk_commits_the_same_ledger_bytes`,
  `ledger_hash_witness::the_emitter_bearing_walk_commits_the_same_ledger_and_hazard_bytes`,
  `the_detent::the_detent_seed_42_walk_matches_the_campaign_time_constant` and
  `the_detent::the_detent_emitter_walk_matches_the_campaign_time_constants`. Not
  one constant was touched; the standing instruction was to stop and report if
  one moved.
- **The FOLD-equals-SCAN oracles hold**: `liveness::emitter_scan_tests`, 5
  passed, including `the_indexed_scan_and_read_equal_the_pre_index_oracles`. The
  wider `liveness::tests` (171) and `hornvale-lab` (126) are green.
- **H5 and H6's witness output is byte-identical to §11.4's**, line for line,
  including `ground memo: 11149 misses, 257546 hits, 11149 rooms held` and the
  60-entry `judged/tick` profile (`h5-h6-witness.txt`). A change that had altered
  what the fold reads would have moved one of those 60 integers.

### 12.6 The falsifier, and the level

Both are stated before §12.7 and §12.8, because both are the ones that could have
gone against the campaign.

**The falsifier does not fire, and its margin widened.** §4's falsifier is "`k`
falls but `C` rises by more than the `k` saving at realistic session lengths".
From the whole-tick affine fits (§12.3 medians):

```
pre:  ms/tick = 211.0490 + 3.47506 h
post: ms/tick = 185.9030 + 2.25311 h
```

**`C` did not rise. It fell by 25.146 ms/tick, while `k` fell by 1.22196 ms/tick
per fact.** The nominal crossover is `h = −25.146 / 1.22196 = −20.6` — negative,
so the two lines cross at a history no session can have and the post-campaign
line is below the pre-campaign line at **every** `h ≥ 0`. §11 put that crossover
at −12.9 and The Pawl's second readout put it at +25.9; the margin has moved
further from the positive axis, not toward it.

| session length | h | pre | post | verdict |
|---|---|---|---|---|
| 20 ticks | ≈ 32.2 | 322.95 ms/tick | 258.45 ms/tick | post **20.0%** faster |
| 50 ticks | ≈ 52.6 | 393.84 ms/tick | 304.42 ms/tick | post **22.7%** faster |
| 200 ticks | 124.4 | 643.35 ms/tick | 466.19 ms/tick | post **27.5%** faster |

The independent check is the shallowest band actually measured: band-2
`ms/tick*` is 342.96 before and 267.63 after — post **22.0%** faster, agreeing
with the fit's 22.7% at the comparable `h`.

**The level — reported, not predicted.** `agent_scaling`, ms/tick over 20 ticks,
paired and interleaved (`as-{control,campaign}-N.txt`). **All four pairs are
valid this time**, where §11 lost one to a spike.

| run | tree | start (UTC) | load before | load after | 10 | 50 | 100 | **200** |
|---|---|---|---|---|---|---|---|---|
| BA1 | control | 02:09:47 | 3.18 / 3.40 / 5.83 | 2.88 / 3.25 / 5.60 | 57.346 | 377.063 | 646.656 | **1 562.223** |
| A1 | campaign | 02:10:47 | 2.88 / 3.25 / 5.60 | 2.30 / 3.02 / 5.37 | 52.319 | 340.992 | 589.498 | **1 420.072** |
| BA2 | control | 02:11:42 | 2.30 / 3.02 / 5.37 | 3.59 / 3.33 / 5.33 | 64.600 | 377.375 | 650.306 | **1 565.142** |
| A2 | campaign | 02:12:43 | 3.59 / 3.33 / 5.33 | 3.20 / 3.26 / 5.17 | 51.094 | 340.013 | 587.369 | **1 417.535** |
| BA3 | control | 02:13:38 | 3.20 / 3.26 / 5.17 | 3.27 / 3.24 / 5.04 | 57.377 | 376.148 | 648.984 | **1 555.861** |
| A3 | campaign | 02:14:38 | 3.27 / 3.24 / 5.04 | 2.66 / 3.08 / 4.86 | 51.335 | 351.205 | 586.002 | **1 418.577** |
| BA4 | control | 02:15:39 | 2.60 / 3.06 / 4.84 | 2.88 / 3.05 / 4.72 | 57.486 | 373.906 | 649.546 | **1 573.806** |
| A4 | campaign | 02:16:39 | 2.88 / 3.05 / 4.72 | 3.10 / 3.09 / 4.63 | 52.027 | 339.304 | 587.004 | **1 413.170** |

**At 200 agents, the four valid pairs: −9.10%, −9.43%, −8.82%, −10.21%. Median
−9.27%** (§11: −8.55% over three pairs). By medians rather than pairs, 1 563.682
→ 1 418.056 = −9.31%. Every rung moved:

| rung | control median | campaign median | §12 delta | §11 delta |
|---|---|---|---|---|
| 10 | 57.431 | 51.681 | **−10.01%** | −10.07% |
| 50 | 376.606 | 340.502 | **−9.59%** | −8.71% |
| 100 | 649.265 | 587.187 | **−9.56%** | −8.61% |
| 200 | 1 563.682 | 1 418.056 | **−9.31%** | −9.10% |

The fitted log-log slope is 1.09 on both trees (BA2 alone reads 1.05, and its
rung-10 figure of 64.600 is the one outlier in this table against three control
readings of 57.35–57.49; both are left in). The campaign lowered the level
without changing the shape of the agent-count scaling — the same signature §11
reported, at the same size.

**The two instruments still disagree by about 2×, and the second readout does
not resolve it.** The affine fits predict post 20.0% faster at `h ≈ 32.2`;
`agent_scaling` measures 9.3–10.0% across its rungs. §11.5's explanation stands
unchanged and unverified: `agent_scaling` drives `step_with_occupancy` directly
with a persistent `HomeNavCache` and `RoomMeshMemo`, while
`session_length_scaling`'s whole-tick column is `Session::wait`'s full turn,
which evaluates the walk a second time through `hornvale_kernel::tick` on a
throwaway cache, so the removed fold is paid twice in one instrument and once in
the other. **That the ratio reproduced at the same size on an independent session
is new information and is worth recording**: it makes a structural explanation
more likely than a noise one, and it makes the cheap test §11.5 proposed (a
counter on `hazards()` calls per tick under each driver) worth running for
whoever picks up the KnownWater folds.

### 12.7 `fold_depth_sweep` — the no-regression control

**This bench sweeps `drive_at` only and cannot see this campaign's fold**, and is
run as the no-regression control on the Sustenance reads, which is what §4 says
it is. One run each tree, back to back inside the same quiet window (both at
3.10 / 3.09 / 4.63 before). µs/call, median of 6 alternating-direction passes
(`fds-{control,campaign}-1.txt`).

| depth | PERIODIC control | PERIODIC campaign | Δ | SINGLE-RESET control | SINGLE-RESET campaign | Δ |
|---|---|---|---|---|---|---|
| 10 | 0.107 | 0.107 | 0.0% | 0.103 | 0.105 | +1.9% |
| 32 | 0.157 | 0.158 | +0.6% | 0.153 | 0.157 | +2.6% |
| 100 | 0.254 | 0.249 | −2.0% | 0.280 | 0.285 | +1.8% |
| 320 | 0.525 | 0.520 | −1.0% | 0.557 | 0.572 | +2.7% |
| 1 000 | 1.404 | 1.373 | −2.2% | 1.528 | 1.463 | −4.3% |
| 3 200 | 4.504 | 4.455 | −1.1% | 4.562 | 4.801 | +5.2% |
| 10 000 | 15.024 | 15.035 | +0.1% | 14.637 | 14.981 | +2.4% |

| | control | campaign |
|---|---|---|
| PERIODIC `k` (µs/call/fact) | 0.00149 (r² 0.999) | 0.00149 (r² 0.999) |
| PERIODIC raw elasticity, top third | 1.057 | 1.068 |
| SINGLE-RESET `k` | 0.00145 (r² 1.000) | 0.00149 (r² 1.000) |
| SINGLE-RESET raw elasticity, top third | 1.023 | 0.999 |

**No regression.** Every depth in both regimes agrees within 5.2%, the PERIODIC
`k` values are identical to five decimals, and the two SINGLE-RESET `k` values
differ by 2.8% — inside this bench's own run-to-run spread, and in the direction
of the control reading low (§11 measured 0.00150 on both trees). The
single-reset column still sits on top of the periodic one, the shape The Pawl's
accumulator left behind, untouched here.

### 12.8 What the second readout hands forward

- **H4 (a) is met on the quantity and its instrument's filter is not.** The
  elasticity is 0.04 against a threshold of 0.20 and a control of 0.91, and no
  reading of the data puts a single run above 0.04 — but §4's `r² ≥ 0.5` clause
  admits none of those runs, because a fold with no slope cannot produce a
  well-fitting line. **A goodness-of-fit filter on a criterion whose success
  destroys the fit is a filter that fails at the finish line**, and this is the
  second clause in two days to name the wrong quantity for the same criterion
  (ledger #8 was the first). The campaign hands both forward: the verdict, and
  the observation that an H4-shaped criterion wants an effect-size floor rather
  than an r² floor.
- **The largest remaining fold is `shared_believed_water`, then
  `believed_water`**, read straight off §12.3: 8 836.63 and 8 663.95 µs/call at
  the final band against `hazard_memory_memo`'s 96.275. Together they are
  **99.08%** of the six folds' cost and their elasticities are 0.97 and 0.96 —
  history-proportional, untouched, and now essentially the *whole* of the tick's
  remaining history term. They are why H2 (c) reads 59.95% and they are the next
  quarry.
- **The remaining terrain questions per tick are still the walk's, unchanged.**
  The H5 witness is byte-identical to §11's: tick 60 makes 3 168 `hazards()`
  calls, of which 2 268 (71.6%) are `Danger::urgency`'s per-step sampling in
  `advance_one`'s decide loop. This change touched none of them, which is the
  point — it moved a map, not a question.
- **M1 is unchanged and is still the figure Penstock stage 4 enters on**: 18 902
  rooms / ~1.455 MB and 4 665 index entries / ~215 KB at 200 ticks on the
  50-agent shape. The change moved no allocation into or out of either
  structure, which the identical M1 table on all four runs demonstrates.
- **Two things the numbers say that the criteria did not ask.** First, the
  falsifier's intercept moved the right way *again* and further: `C` fell 25.1
  ms/tick here against 15.9 in §11 and a *rise* of 45.1 in The Pawl, so the
  crossover has now walked from +25.9 to −12.9 to −20.6 across three readouts.
  Second, a discarded run agreed with the kept ones: P4 was set aside for a load
  spike it inherited from B5's tail, and its readings (elasticity −0.00,
  final-band 94.62 µs/call) sit inside the four valid runs' spread — the load
  rule cost this readout a data point it did not need, which is the right
  direction for a rule to err.
