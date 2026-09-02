# The Detent — decision ledger

Campaign: `campaign/the-detent` · Spec:
`docs/superpowers/specs/2026-09-02-the-detent-design.md` · Plan: (not yet
written) · Decision block: 0626–0635 (reserved on lefford 2026-09-02; main
ceiling was 0585 at reservation).

A detent is the catch that holds a mechanism in a position until something
deliberately releases it. The name is the mechanism: a verdict about the
ground — *is this room frightening to this creature* — is a pure function of
a terrain that never changes within a session, so once taken it is held, and
nothing about the passage of ticks releases it.

Committed per The Cartulary (decision 0486): every ruling is written here as
it happens, on this branch, in this worktree.

---

## #1 [G1] — which campaign follows The Pawl on the derived-working-set thread?

**Question.** Nathan's ranking to argue with: (a) the hazard fold's
cross-tick affect memo keyed by the emitter's reset partition; (b) 7b, the
typed intention; (c) the small consolidation (`KnownWater` into
`LatestVisit`, `EmitterScan` to `pub(crate)` with its tests out of
`liveness.rs`). Campaign 7 (spatial partition) is excluded by instruction.

**Decision.** (a)'s *target* — the hazard fold, the only thing that can move
H4 — with (a)'s *mechanism* replaced by the measured one, plus the half of
(c) that the rewrite touches anyway. The campaign builds a session-lived
memo of the fear path's terrain verdicts (a `Pure` derived component,
Penstock stage 3's territory, keyed by room) and makes the emitter scan and
the emitter-free hazard read advance over new sightings instead of
re-sampling every visited room of every roster member every tick. The
reset-partition affect memo is **not built**: no measured shape reaches the
path it would serve (below). 7b stays the recommended epoch after this.

**Why — the registry row's mechanism is reached zero times.**
`TOOL-hazard-affect-cross-tick-memo` says the remaining 93 ms/call is "per
tick, for every visited room × every emitter, the emitter's affect at that
room's latest-visit day is re-evaluated", and The Pawl's chronicle says that
mechanism is "legible from the code rather than merely suspected". It was
read, not measured, and the same campaign's own retrospective names that
defect shape twice. A throwaway counting probe on the H4 instrument's own
shape (`session_length_scaling`'s construction: seed 42, `derive_npcs(50)`,
`DriveMovements::step_with_occupancy`, one resident store; source parked at
the session scratchpad, to be rebuilt as Task 1's committed witness):

```
seed 42, 50 agents, one hazard_memory_memo call on the max-history probe
                       tick 15   tick 30   tick 60   tick 100   tick 200
FRESH memo (the bench's shape)
  terrain.hazards()    14,004    16,758    22,302    29,097     43,164
  alarm_replays             0         0         0         0          0
  scans with emitters       0         0         0         0          0
  wall (loaded box)     42 ms     35 ms     46 ms     59 ms      87 ms
WARM memo (production's per-creature read once the tick's scan exists)
  terrain.hazards()       684       819     1,089     1,449        729*
  wall                  1.4 ms    1.8 ms    2.3 ms    3.1 ms     1.5 ms
roster: distinct rooms   1,430     1,721     2,307     3,022      4,665
        rooms ∪ halo     7,006     8,357    10,832        —          —
whole tick, all 50 walks + reads
  terrain.hazards()    29,250    34,146    44,694        —          —
  facts committed         181        35        31        —          —
seed 6 (the emitter seed), 50 agents: 1 emitter in every scan, alarm_replays 0,
  hazards() 22,671 fresh / 1,152 warm at tick 60.
(* the tick-200 probe is a different body: the probe re-selects per checkpoint
   in the spike; the bench fixes it once. 87 ms against §12's 93 ms/call at
   the same band is the cross-check that this is the bench's cost.)
```

Every `hazards()` sample is a pure function of the room: `Terrain::hazards`
takes no `day` by contract ("a slow field"), `LocaleContext::hazards_at` reads
climate and regime, and the predator field is "computed once at" session
start. So the fold's cost is **static terrain re-sampled per tick**, ~95% of
it in `build_emitter_scan`'s pass 2 (roster × distinct rooms × the room and
its neighbours, ~9.7 samples per room), and the cross-tick affect memo would
move H4 by nothing on the instrument H4 is measured on. The emitter-free
production read is ~1,000 samples per creature per tick on top of the scan:
44,694 per tick on 50 agents while the walk commits 31 facts.

**Why not (b) first.** 7b is an epoch and a fidelity call (0238); H4 is a
measured, byte-identical quarry that the last campaign left carrying a
wrong mechanism in a committed registry row. Leaving that row as it is
would send the next reader to build the wrong thing.

**Why not (c) alone.** `KnownWater`→`LatestVisit` costs memory and advance
work, never a wrong answer, and moves no criterion. The `EmitterScan`
visibility and test move ride along here because the scan is rewritten.

**Alternatives discarded.**
- The reset-partition affect memo as the campaign (Nathan's (a) verbatim):
  reached 0 times on every measured shape; kept as a decision rule in the
  spec (build it only where a shape's `alarm_replays` is measured non-zero
  and its cost material — the seed-6 possession shape reaches it 9 of 9
  times at past instants per The Pawl, cost unmeasured).
- Putting the memo inside `LocaleTerrain`: the benches and the lab rebuild
  the terrain per tick, so a terrain-scoped memo is cold every tick on
  exactly the instruments the criteria run on. Overturned by the ideonomy
  pass (below); the memo is session-scoped, beside `mesh_memo` and the
  resident store.
- A whole-history spatial partition (Campaign 7): trigger unmet; excluded.
- Merging the three trail indexes now: deferred, row stands.

**Ideonomy passes / overturns: 2 / 1** (`ideonomy-plain`, tuple:
substitution + dimension-identification, organon cycle, prompts
distribution / size / connectivity). Pass 1 overturned the memo's scope
(terrain-object → session) and split the work into two levels; pass 2
(negation + abstraction lift, run by hand) added the neighbour-dedupe
observation and named the pattern; a third pass was judged to add nothing
material. The artifact:

```
The tick cycle, and where static terrain is re-sampled on it
(seed 42, 50 agents, tick 60; every phase's samples are of a field that
 does not change between phases or between ticks)

  freeze ledger
      |
      v
  [1] alarm field, present day        ~450 samples   (roster x 9 at position)
      |
      v
  [2] emitter scan, once per tick   22,302 samples   (roster x visited x 9.7)
      |
      v
  [3] per-creature hazard read      ~1,000 x 50      (visited x 9, each body)
      |
      v
  [4] the walk's own Danger drive     small           (31 facts committed)
      |
      v
  commit 31 facts ------> next tick: [1] again, on ground that did not move
                                      ^
  anti-phase: NOTHING samples hazards between ticks (the snapshot did, until
  The Rack); nothing samples them on the same room twice within [2] either,
  because [2] never asks the same (room) twice for the same member -- the
  repetition is ACROSS members (a shared room is sampled once per member
  that visited it) and ACROSS ticks (every room, every tick).

  Dimensions surfaced, and the value chosen:
    key granularity   room | (room, niche) | (entity, room) verdict | the scan
                      -> room for the field (shared by every reader), and a
                         per-entity verdict INDEX for the scan's monotone half
    lifetime          call | tick | SESSION | world   -> session (one terrain)
    ownership         terrain object | session | resident store
                      -> session-owned, threaded like mesh_memo; the verdict
                         index lives with the entity in the resident store
    invalidation      never (Pure) | dependency (Ledger)  -> Pure; the one
                      thing that could release it is a different terrain, and
                      a store already assumes one LocaleContext (SustenanceMemo)
    scan shape        rebuild per tick | advancing over new sightings
                      -> advancing: is_emitter and alarm halos are MONOTONE
                         because the verdict per (entity, room) never changes
    size x1000        the scan is O(roster x rooms) per tick; at 1000x agents
                      the memo alone leaves it O(roster x rooms) LOOKUPS, so
                      the advancing form is what scales, not the memo
    connectivity      who else reads hazards(): the Danger drive per step,
                      alarm_field, serviceability -- all served by the room
                      memo; is_fresh_water has the same shape in believed_water
                      (121 samples/call, 4.4 ms) but its cost is the A* per
                      water room, not the sampling -- out of scope, recorded
  Negation (pass 2): no memo at all -- sample the union of rooms and halo once
    (10,832 distinct against 44,694 samples, a 4.1x) is a pure algorithmic
    win; it is subsumed by the room memo, which pays each distinct room once
    per SESSION rather than once per tick.
  Abstraction lift: "a fold over visits x a static predicate over places" --
    KnownWater/is_water, LatestVisit/frightening, forage. The tenant holds the
    visits, the read applies the predicate, and the predicate's answer per
    place is a Pure derived component. That sentence is the campaign.
```

**Capture actions.** Registry: `TOOL-hazard-affect-cross-tick-memo` is
corrected in place at spec time (the measured mechanism, the number, and the
row's original claim kept as history); a new row for the place-predicate
index pattern (the abstraction above) as `raw`; the `believed_water` A*-per-
water-room cost is added to the followup register. The neighbour-dedupe
observation is recorded in the spec's §1 as subsumed. Nothing else raised.

---

## #2 [Q] — where does the room memo live, and how does a `&self` reader reach it?

**Question.** `Terrain::hazards(&self)` is read through `&dyn Terrain` by
every caller, so a memo behind it needs either interior mutability or a
prefill step; and the benches, the lab and `Session::wait` all rebuild
`LocaleTerrain` per tick, so the memo cannot live in the terrain value.

**Decision.** Session-scoped, beside `mesh_memo` and the resident store, and
threaded into `LocaleTerrain::with_fields` the way `Some(&self.mesh_memo)`
already is. Interior mutability (`RefCell`, the `OwnedFolds` shape) rather
than prefill, because the set of rooms a tick will ask about is the union of
every roster member's visited rooms and their halos, which is not known
before the reads run — the mesh memo's prefill works because "each NPC's
current position and neighbours" is known in advance; this one is not.

**Why (precedent).** The Pawl §2.2 put the seam at read and refused the
throwaway rebuild for exactly the `&self` reader case (`Session::snapshot`);
decision 0537 (a reader never observes a fold behind its ledger) is about
ledger folds, but its shape — advance on read, idempotent — is the one a
fill-on-read memo has. The Forebay's `RoomMeshMemo` is the precedent for a
session-owned `Pure` derived store read through `Option<&…>` by `&self`
terrain methods.

**Alternatives discarded.** Prefill per tick from the resident store's
visited sets (known at advance time): correct, but O(roster × rooms) map
work per tick on the prefill path and a second place that has to know which
rooms a hazard read will touch. A memo inside `LocaleContext` (locale layer):
the predator field is a vessel-side blend, so half the value would be
computed outside the memo anyway; and locale is a window other windows read
without a session.

**Ideonomy passes / overturns: 1 / 0** — the G1 pass's ownership dimension
is this question; its row stands.

**Capture.** None beyond the spec.

---

## #3 [Q] — what is the memo's key, and what makes it complete?

**Question.** Decision 0206 makes key-completeness the typed obligation:
the key must carry every parameter the derivation reads. `hazards(room)`
reads the room, the `LocaleContext` (climate, regime budget, geosphere
level, nearest-vertex index) and the session's predator field.

**Decision.** Key by `Facet` alone, with the terrain's identity supplied by
OWNERSHIP rather than by the key: one memo per `(LocaleContext, predator
field)`, owned by the session that owns both, documented on the type the way
`SustenanceMemo` documents "one `LocaleContext` per store", and pressured by
the chaos-eviction property (`Derived::evict_all` at every step, output
unchanged) plus a two-terrain test that asserts a memo handed a second
terrain with a different predator field is a DIFFERENT memo, not a shared
one.

**Why (precedent).** `RoomMeshMemo` folded `Geosphere::level()` into the
key because that is one `u32`; a `LocaleContext` is not a key-sized value.
`SustenanceMemo` already takes the second road — its integrals depend on
the temperature field and the memo's doc says "one `LocaleContext` per
store; a caller that must change the field builds a new store" — so the
resident layer already carries this assumption and the room memo adds no
new one. Decision 0206's own text allows it: "world-derived" is `Pure`
with the world's identity folded in, and here the identity is the owner.

**Alternatives discarded.** A `u64` fingerprint of the context in the key:
would need a stable hash of a large struct for a property ownership already
guarantees. `Validity::Ledger`: the field is not a fold over the ledger.

**Ideonomy passes / overturns: 1 / 0** — the G1 pass's invalidation and
size dimensions; the "what could release it" question came back with only
"a different terrain", which is what this entry answers.

**Capture.** A future day-dependent hazard (seasonal heat/cold) would make
`Terrain::hazards` take a `day`, which is a signature change the memo's key
would have to absorb; noted in the spec's determinism section as the one
thing that would make this memo wrong, and it cannot happen silently
because the trait method's signature is the contract.

---

## #4 [Q] — is the advancing scan a tenant, a read, or a memo?

**Question.** The Pawl ruled `Alarm` is not a tenant: everything the scan
accumulates from the ledger is already held by `Trail` and `LatestVisit`,
and the predicate needs terrain, which no `LedgerFold::absorb` may read.
Making the scan advance over new sightings seems to reopen that.

**Decision.** It does not. The scan stays a READ over the two tenants plus
the predicate; what changes is that the predicate's answer per `(entity,
room)` is remembered at the read side — a per-entity **verdict index**
(frightening rooms, ascending by first visit; `ever` as its non-emptiness;
the halo union as a derived set) advanced from the trail by a consumed-prefix
cursor, the `MemoPartition` shape. `absorb` still sees only facts; the
verdict index is a read-side memo of a pure predicate, exactly what The
Pawl's ruling said KnownWater's `is_water` is, with the one difference that
the answer is kept rather than re-asked.

**Why (precedent).** The Pawl ledger #4 and the `Alarm`-is-not-a-tenant
ruling (Task 5); `SustenanceMemo` as the precedent for a read-side memo that
lives in the store, keyed by entity, advanced by a consumed index against
the trail.

**Alternatives discarded.** A tenant whose `absorb` takes terrain: refused
by the purity rule the FOLD-equals-SCAN battery depends on. Recomputing the
scan per tick over the room memo alone: O(roster × rooms) lookups per tick;
the counting witness would still read ~22,000 per call and the elasticity
would not move.

**Ideonomy passes / overturns: 1 / 0** — the G1 pass's scan-shape
dimension.

**Capture.** None beyond the spec.

---

## #5 [G2] — spec self-review

**Checked.** Placeholders: none. Internal consistency: §2.1 owns the memo on
the session and §2.2 puts the index in the resident store — both behind
interior mutability, both discardable; §6's stage table carries every §3
rule to a stage. Scope: one plan. Ambiguity: "10×" in H4 is stated against
BOTH the frozen figure and the same-box control, with the readout required
to name which.

**Claims verified by command rather than reasoning.** (i) `Terrain::hazards`
takes no `day` — the trait signature at `liveness.rs`. (ii) The predator
field is computed once — `Session.predator`'s own doc ("computed once at").
(iii) The bench builds a FRESH `PrimaryAfraidMemo` per probe call —
`probe_hazard_memory_memo_us`, read. (iv) The counts in §1 — the probe's
output, verbatim. (v) `docs_consistency` green after the registry edits
(28 passed).

**Fixed inline.** Three sentences: the-rack's diffstat misread as `+933/−252`
(it is 933 lines changed); a `tests/suite/common` directory that does not
exist in `windows/vessel`; a "two sites that would have wanted"
`believed_hazard_memo` when it has no caller at all.

**One process slip, recorded.** The registry edit script asserted a 600-
character cap and aborted before writing, and the spec commit went in with a
message claiming registry edits that had not happened. Caught by `grep -c`
on the next command (0 matches), fixed, and the unpushed commit amended so
the message is true. The lesson is the standing one: verify the artifact,
not the script's exit path — a heredoc that fails after the commit command
was already queued in the same shell line reads as success.

**Ideonomy passes / overturns:** none run for this entry; it is a review of
the artifact the G1 pass produced, not a new decision.

---

## G3 — approved (Nathan, 2026-09-02)

Spec approved as presented, with one addition Nathan asked for after the
lifetime question: **M1**, the bytes the room memo and the per-creature
index hold at the final band on all three shapes, reported without a
threshold as the number Penstock stage 4 (the lifecycle) is gated on. Nothing
in this campaign evicts either structure; that is stage 4's job and it now
has a measurement to enter on. Added to spec §4 and the stage-4 row.

---

## #6 [G4] — plan self-review against the approved spec

**Plan:** `docs/superpowers/plans/2026-09-02-the-detent.md`, nine tasks,
stage gates after Tasks 2, 4 and 8. Coverage walked section by section in
the plan's own self-review; one gap named (rule 7's `turn_budget.rs` is
The Rack's and unmerged — checked at absorption, not by a task).

**Two rulings the plan makes that the spec did not, both recorded here so
they are decisions and not drift:**

1. **The registry row's remedy for `EmitterScan` cannot be done as
   written.** `TOOL-emitter-scan-tests-out-of-liveness` says "make it
   `pub(crate)` and move the tests to `tests/suite/resident_folds.rs`";
   an integration-test binary is a separate crate and cannot see
   `pub(crate)`. The tests leave `liveness.rs` through a `#[cfg(test)]
   #[path = "liveness_tests/emitter_scan.rs"] mod` instead — out of the
   19,675-line file, still in-crate, `EmitterScan` stays private and no
   `pub` widens. The row is closed at Task 9 with this correction in its
   Where cell. *Why:* the row's intent (the tests beside their siblings,
   the file shorter) is met; its mechanism was wrong. *Alternatives:*
   `pub` + `#[doc(hidden)]` on `EmitterScan` — widens the crate's surface
   for a test's convenience, and the type-audit would then tag its fields.
   No ideonomy pass: a visibility fact, not a design choice.
2. **H5 has two readings and the plan asserts both, in order.** Stage 2
   asserts on FIELD SAMPLES (the memo's misses) — 0 on a repeated read,
   ≤ 4,469 per tick; Stage 3 asserts the same thresholds on `hazards()`
   CALLS, which is stricter (the index answers without asking the terrain
   at all). The spec's wording, "terrain hazard samples", is the Stage 2
   reading; nothing is loosened. *Why:* a memo alone leaves the call count
   at O(roster × rooms) and would pass H5 while H4 (a) stays at 0.92 —
   spec §1 item 5 says so; asserting calls at Stage 3 is what pins the
   advancing form. *Alternative discarded:* one reading (samples only),
   which the memo satisfies and the index does not need to.

**One methodological concession, stated.** Task 4's first H5 clause cannot
take its red from a failing assertion, because the assertion's subject (memo
misses) does not exist before the memo does; the plan takes Task 1's printed
count (22,302 fresh / 1,089 warm hazards() calls, equal to field samples on
a memo-less tree) as that clause's red. Task 6's red is a real failing
assertion on calls, taken by swapping in the pre-Task-6 `liveness.rs` by
copy.

**Ideonomy passes / overturns:** none for this entry; the G1 pass's cycle
artifact is what the plan's stage carve follows.

---

## Task 1 — complete (`c99d128e5`, review clean, 2 minors deferred)

The counting terrain and the H5 witness, floors only. The witness
reproduces spec §1's tick-60 column EXACTLY — FRESH 22,302 / WARM 1,089 /
whole tick 44,694 hazards() calls over 31 committed facts — which is the
construction-equals-the-bench check the task existed to establish. Two
minors deferred to the final review: no per-field docs on the test-crate
structs; `#[allow(dead_code)]` scoped to the whole `BenchShape` struct
rather than its two not-yet-read fields. No ideonomy pass: a transcription
task with no ruling in it.

## Task 2 — in review (`a23deff1c`)

Constants minted from two agreeing runs: seed-42 ledger
`0xabc4731e5cf1ab21` (the same value main printed at The Pawl's close, so
the walk has not moved since), emitter (seed 6) ledger
`0xc851e64b010538b2`, emitter hazard `0xa9f17d82c1832854`. Positive control
`DANGER_ACT` 0.3 → 0.05 via `scripts/mutate.py` moved BOTH emitter hashes
(shunned 186 → 428, dread 6 → 177, replays 342 → 12,044) and did NOT move
the seed-42 hash: the flagship's derived residents carry no fear verdict
that reaches a route, exactly as The Pawl found — so the seed-42 constant
witnesses the walk's byte-identity and is blind to this campaign's path;
the emitter constants are the load-bearing pair. Recorded here so no later
reader takes a green seed-42 witness as evidence about the fear fold.

## Task 2 — complete (`a23deff1c`, fix `78fe6da08`; one fix round)

One Important at review: the constants' doc carried no caveat that the
seed-42 constant is blind to the fear path (the fact was only in the task
report, which is scratch). Fixed in the committed doc and the assert
message, pointing at `ledger_hash_witness.rs`'s existing seed-42 note.
Re-review: addressed, no new breakage. **Stage 1 closes here.** `origin/main`
has not moved since the branch was cut (`0dccce029`); `campaign/the-rack` is
still at its stage gate, so rule 6's absorption is not yet due.

## Task 3 — complete (`d2af9a6e9`, fix `3e8b9029d`; one fix round)

`GroundHazards` (`windows/vessel/src/ground.rs`), `OwnedGround`, and
`LocaleTerrain::with_ground`; `hazards()` computes through one closure and
the memo owns the call when present. The implementer found a THIRD
`LocaleTerrain` constructor (`with_calendar`) the brief's count missed —
caught by the compiler, which is the enumeration a struct literal gets for
free. The three hash constants held (byte-identity). One Important at
review: the chaos-eviction test printed the memo's `len()` without asserting
it; fixed with an independently computed comparator (the union over every
roster member of visited rooms, their neighbours, home and its neighbours —
what `build_emitter_scan` samples), which matched exactly (1,339 = 1,339).
No ideonomy pass: the task's one ruling (which set the comparator is) was
settled by reading the sampler, not by design.

## Stage 1 gate — green (`req-289cd34771d3`, all stage phases rc=0 in 964 s, main unchanged at `0dccce029`)

## Task 4 — complete (`49187febf`, fix `3517089a5`; one fix round)

One memo each for the session (five `with_fields` sites, not the plan's
six — the grep is the count), the lab's `run_simulation_with_locale` (the
function that actually rebuilds terrain per tick; `run_simulation` takes a
caller's terrain and builds none — a brief defect the implementer corrected
by reading), and both benches. The hazard probe keeps its fresh memo per
call. H5's first clause, spec §3 rule 1's re-count: on the H5 shape the
repeated read and the second fresh read take **0 field samples**; tick 60
takes **0** against 44,694 `hazards()` calls; across the whole 60-tick run
the memo recorded **11,149 misses against 1,934,552 hits** and holds 11,149
rooms — so the memo answers 99.4% of the fear path's terrain questions from
one session-lifetime fill, and the CALL count is untouched, exactly as spec
§1 item 5 predicted. **Rule 1's branch: samples fell ≥ 10× (to zero on the
measured tick); proceed to Stage 3 for the elasticity.**

**One Important at review, and it is this campaign's own lesson landing on
its own witness:** every H5 assertion (`== 0`, `≤ 4,469`) was satisfiable by
a memo nobody had wired in — drop `.with_ground` and the deltas read zero and
the test stays green. Fixed with absolute floors (`misses > 0`, `hits > 0`,
`misses == len`) and a printed shape line. Recorded because a witness
written by the campaign that named the defect still shipped the defect on
its first draft; only the reviewer's "what if the plumbing were absent"
question found it. **Stage 2 closes here.**
