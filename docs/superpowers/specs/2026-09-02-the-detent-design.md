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
| 4 | The readout | §11: the three instruments, the same-box control, every criterion separately; the registry row corrected with the number; retire the constants; chronicle, retro, decisions, Confidence Gradient re-score | `make sluice` |

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
