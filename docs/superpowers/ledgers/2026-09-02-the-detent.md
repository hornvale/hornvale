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

## Task 5 — complete (`12de5c027`, fix `4e7c0723c`; one fix round)

`FrighteningGround` in `resident.rs`: a read-side verdict index per entity,
advanced from the `Trail` by a consumed-prefix cursor, NOT a `LedgerFold`
(ledger #4), outside `advance()` and `position()`. Four unit tests, including
the interleaved-trail one that pins "first occurrence in trail order is the
earliest day". One Important at review: a `.min(trail.len())` clamp on the
cursor slice that no invariant could ever exercise, undocumented — replaced
with a `debug_assert!` naming the invariant (append-only trail; cursor set
only to its length; the index is discarded with the store it lives in) so a
violation panics on the slice instead of being clamped silently. Nothing
calls the index yet; the hash constants held.

## Stage 2 gate — green (`req-062811a20e79`, all stage phases rc=0 in 952 s, main unchanged at `0dccce029`)

## Task 6 — complete (`1292b3748`, fix `9b5a54203`; one fix round)

The scan and the emitter-free read advance over the index. Byte-identity
was proven site by site at review (opus): the emitter-free domain ("some
visit ≤ t") equals the index's ("first visit ≤ t") because a room's visit
days are ascending and its first is the minimum; the scan's `ever` and halo
are the same set in a different insertion order into a `BTreeSet`; the one
shared predicate `feels_frightening(threat, 0.0, boldness)` agrees with the
scan's old `threat × mettle ≥ DANGER_ACT` on EVERY `f64`, not only the
30,401-point sweep, because the clamp can only move a value across 1.0 or
0.0 and the threshold is 0.3; the witness-first ordering and the emitter
path are untouched. Oracles are verbatim pre-rewrite bodies (diffed against
`fb9e6d94d`): 12 probes × 2 seeds, 69 of 120 emitter slots, 497 frightening
pairs, identical. `frightened_at`/`alarm_at` are `#[cfg(test)]` now — their
only production caller was the replaced loop.

**H5, second reading (calls):** RED on the pre-Task-6 tree — warm read
1,089, tick 60 44,694; GREEN — warm read **0**, tick 60 **3,168** (≤ 4,469).
**H6:** judged rooms per tick grow 2.42× slower than the roster's distinct
rooms, taken at tick 57 because tick 60 judges zero rooms (disclosed, not
picked). **The 3,168 remaining calls, measured, not reasoned** (fix round
1): `alarm_field_memo`'s per-member gate 450 + the scan's per-member home
judgement 450 + the walk's own `Danger::urgency` per-step sampling 2,268.
The fear fold is now 28% of the tick's terrain questions; the live drive's
per-step sampling is the other 72% — carried to the followup register as
the next quarry on this axis.

Two Importants at review, both fixed: the index had no stated
terrain-ownership rule (now the same `(LocaleContext, predator field)` rule
`GroundHazards` and `SustenanceMemo` carry, with a test that OBSERVES the
aliasing it forbids); the remainder above was unattributed. Four minors
deferred to the final review: hardcoded tick indices; the growth assertion's
message not naming its late tick, and `late` being data-selected; the chaos
comparator's reach shrinking to 90 rooms once the index is warm; the
oracle test comparing the full-roster emitter-free branch zero times.

## Task 7 — complete (`e289368d7`, review clean)

The Pawl's `EmitterScan` equivalence tests moved out of `liveness.rs` into
the sibling `liveness_tests/emitter_scan.rs` (bodies identical, names
unchanged; `PlantedTerrain` and seven helpers made `pub(super)` rather than
duplicated); `believed_hazard_memo` deleted and its three doc mentions
rewritten with their caller counts corrected. `liveness.rs` 19,809 → 19,577
lines. **A consequence recorded rather than discovered later:** the two
moved tests were listed in `docs/timings/subfloor-roster.tsv` by their old
path (`liveness::tests::…`), so the commit gate now runs 1,096 sub-floor
tests instead of 1,098 until the Stage 3 gate's chamber run rewrites the
roster with the new paths — the "renaming a test is a commit-gate change"
rule, in its path form. The stage gate runs the full suite either way.

## Stage 3 boundary — main moved, and the constants were re-measured MAIN-FIRST (decision 0541)

`origin/main` advanced `0dccce029` → `4b82e544d` (The Reservoir: the suite
reads the committed seed-42 world instead of rebuilding it, plus a
build-site ratchet, `cli/tests/suite/world_build_sites.rs` / decision
0606; it touches `windows/vessel/src/session.rs` by 13 lines). Before
absorbing, a detached checkout of `4b82e544d` carrying none of this
campaign's code was given `ledger_hash_witness.rs`'s two scripts and asked
for its hashes:

```
witness              campaign constant      main @4b82e544d
seed-42 ledger       0xabc4731e5cf1ab21     0xabc4731e5cf1ab21
emitter ledger       0xc851e64b010538b2     0xc851e64b010538b2
emitter hazard       0xa9f17d82c1832854     0xa9f17d82c1832854
                     (127 bodies, 186 shunned, 6 dread — agreeing)
```

Main's walk did not move, so the constants stand and the merged tree is
required to reproduce them unchanged. The absorption also owes the ratchet
a row: `windows/vessel/src/liveness_tests/emitter_scan.rs` carries one
`build_world(` site (the in-crate oracle's bench helper, seeds 42 and 6 —
seed 6 has no fixture, so the reason is `identity`), and `liveness.rs`'s
own count stays at 6 because the tests Task 7 moved build no world.

## Task 8 — complete (`82f2fb21c`, review clean)

The three rule witnesses and M1, decided nowhere, measured everywhere:

- **Rule 2, seed-6 possession, four waits:** 2.62 past-day affect replays
  per hazard read and 180.8 room-memo lookups per read (654 replays, 341
  shunned rooms — both floors real). A count share, not a time share; the
  time share is the readout's (Task 9) because it needs a quiet box.
- **Rule 4, seed 6 × 50 agents × 60 ticks:** the emitter timeline copy is
  **zero entries at every tick** although all 60 scans found an emitter —
  and the mechanism is stated, not guessed: a member is an emitter when its
  HOME is frightening, before it has committed a single dated `agent-at`,
  so `prefix_len` is 0 and there is nothing to copy. A real measurement of
  the cheapest case; a second shape would be needed to stress it. **Branch:
  keep the copy.**
- **Rule 5, the lab's waking-instant shape:** 8 past-instant hazard reads
  over 10 ticks of 10 agents (the prefix machinery has a production caller),
  and 10 of 10 warm-store reads at past instants equal a fresh store's.
- **M1, `session_length_scaling` at band 10 (200 ticks, 50 agents):** the
  room memo holds 18,902 rooms, ~1.46 MB by the stated estimate; the index
  holds 4,665 entries, ~215 KB. This is the figure Penstock stage 4 enters
  on.

Two info notes: two new `Session` accessors have no caller until the
readout; rule 4's zero carries its mechanism. No ideonomy pass: the task
produced numbers and no ruling.

## Stage 3 boundary — main absorbed (`99d0ce2d2`), the ratchet row added (`1ea0998d0`)

A clean auto-merge of `4b82e544d` (The Reservoir): no conflict anywhere,
including `docs/audits/type-audit-report.md`, which the implementer
regenerated regardless (a clean merge of an aggregate is not evidence it
merged right). The Reservoir's build-site ratchet gained one row for this
campaign's in-crate oracle helper (`windows/vessel/src/liveness_tests/
emitter_scan.rs`, `identity:1` — it builds seed 6, which has no fixture);
`liveness.rs`'s count stayed 6. The three constants reproduced on the
merged tree without edit, as the main-first measurement above predicted.
`make gate-commit` ran 1,115 sub-floor tests (main's roster additions
merged in). **A controller slip, recorded for the retrospective:** the
Task 8 ledger entry was committed into this worktree while the absorption
implementer was mid-task; the merge had already auto-committed, so nothing
was clobbered, but the rule — no controller commits while a subagent works
in the same tree — was broken. **Stage 3 closes here**; the stage gate is
`req-1ea0998d039c`.

## Stage 3 gate — green (`req-1ea0998d039c`, all stage phases rc=0 in 973 s)

## Task 9a — the first readout (`736eaf1e3`, spec §11)

Measured on a quiet box against a merge-base control (`0dccce029`, no
memo), interleaved, three control runs set aside for other campaigns'
suites starting mid-run. **H4 (b) MET by a wide margin:** the hazard fold's
final-band cost 92.012 → 0.13507 ms/call, **681× against the same-box
control and 540–718× against the frozen 73–97 ms**, where The Pawl had
1.57× and, against the frozen figure, nothing. **H4 (a) NOT MET by 0.045:**
elasticity 0.245 against < 0.20 (0.245/0.25/0.245 under the three readings
of the r² filter; control 0.93). **H2 (c) NOT MET:** 60.8% against 20%
(control 68.7%). H2 (a)/(b) hold; H3 held and re-witnessed (one md5 per
bench across all 18 runs of both trees); H5 and H6 met by the committed
witnesses. The falsifier cannot fire: `C` FELL by 15.9 ms/tick alongside
`k`, crossover at h = −12.9. The level: **−8.55%** median paired at 200
agents, −8.6 to −10.1% at every rung, slope unchanged. M1: 18,902 rooms /
1.46 MB and 4,665 index entries / 215 KB at band 10. Rule 2's instrument
(`examples/detent_rule2_probe.rs`): 785 µs per hazard read on the seed-6
possession shape, cold ≈ warm, 2.62 replays per read.

## #7 [G5] — one post-unblinding change, and the protocol for it

**Question.** H4 (a) fails by 0.045. Is the residual history term a property
of the design, or of the implementation?

**What the code says, before any measurement.** `hazard_memory_memo`
builds the per-room `latest` map (`LatestVisit::latest_at`, O(distinct
rooms visited)) for EVERY call, under the witness-first guard, and only then
takes the emitter-free early return over the index prefix. Spec §2.3 said
the emitter-free read "becomes a prefix read" and that `latest` is "still
needed for the EMITTER path". The plan's Task 6 text (my own) told the
implementer to "leave the `latest` block where it is and only replace the
loop inside the early return", to preserve the witness-first ordering — so
the O(rooms) map survives on the path the design specified as O(prefix),
and for the wanderer the probe follows, rooms ∝ history. That is the same
shape The Pawl's §12.0 named: the implementation falsified against its own
design, in plan text I wrote.

**Decision.** Exactly one post-unblinding change is permitted, under The
Pawl's rule: verify the mechanism by MEASUREMENT before touching it; if
confirmed, complete the design (the witness-first call needs only the
entity's last trail day, which `trail.of(entity).last()` already supplies;
`latest_at` moves below the emitter-free return so only the emitter path
pays for it); prove byte-identity (FOLD-equals-SCAN, the hash constants,
H5); then a §12 second readout, interleaved against the same control, with
§11 left standing and the change disclosed as mechanism-completing — no
threshold, constant or criterion moves. If the measurement does NOT confirm
the mechanism, no change is made and H4 (a) is handed forward as a finding.

**Why (precedent).** The Pawl §12.0 and decision 0016: a mechanism the
design specified and the implementation omitted may be completed after
unblinding provided the first readout stands unedited and the second is
declared not blind. Cost if wrong: an hour of quiet box and a §12 that
reads 0.245 again — which would itself be the finding that the residual is
elsewhere.

**Rule 2's branch, ruled now:** the share is NOT resolved by the count
instrument (785 µs per read with 2.62 replays per read is a whole-read cost,
not a replay share), so the reset-partition affect memo stays UNBUILT and
the registry row carries the number and the open question. **Rule 4's
branch:** keep the copy (zero entries at every measured tick, mechanism
stated).

**Ideonomy passes / overturns: 1 / 0** — a substitution pass over "where
the O(rooms) work could hide" (the map, the witness, the scan, the set
clone) named the map first and the measurement is what decides.

## #8 [G5] — #7's verification clause named the wrong denominator

**What was measured (Task 9c, Step 1, two quiet runs).** On the bench's
shape, a fresh-memo hazard read costs 108/110 µs at tick 60 and 133/135 µs
at tick 200; `latest_at` alone costs 14.0 µs and 31.9/33.1 µs at the same
depths (map size 121 and 260 — exactly the probe's history, the wanderer
premise confirmed). Share of the TOTAL: 13% and 24%. Share of the
history-driven GROWTH — the fitted `k` that H4 (a) measures — **72–76%**:
`latest_at` grows 2.3× over a 2.15× history growth while the rest of the
read is the ~88 µs intercept §11.2 already fitted. Two-point elasticity of
the read 0.270; of the read with the map removed **0.08–0.09**, against
§4's 0.20. Cross-checks agree with §11.2 to within a few percent (implied
`k` 0.18 vs fitted 0.171; remainder ~101 µs vs `C` +88).

**The implementer stopped, correctly, because #7's first clause said "≥ 50%
of the fresh read's cost" and 24% is not 50%.** The clause was wrong, not
the stop: H4 (a) is a criterion about the SLOPE, and the clause tested the
LEVEL — the exact "name the denominator" failure this project's memory
already records. #7's second clause ("grows with depth") holds emphatically.

**Decision.** Re-read #7's verification as share of the history term, which
is the quantity the criterion is about; the hypothesis is confirmed at
72–76%, and Steps 2–4 proceed under #7's other terms unchanged: complete
§2.3's prefix read, prove byte-identity, take §12 interleaved against the
same control, disclose. No threshold, constant or criterion moves; §11
stands. **Alternatives discarded:** accepting the 50% clause as written
(it measured a level to decide a slope question); lowering the threshold
to fit the number (that is the metric-chasing the freeze forbids — the
change here is to WHICH quantity is compared, and the comparison is
recorded before the change is made). **Cost if wrong:** a §12 that reads
0.245 again, which is itself the finding.

**Ideonomy passes / overturns: 1 / 1** — the pass was the implementer's
own tension report, which overturned the clause; recorded as such rather
than dressed up as mine.

## Task 9c — the second readout (`932409875`, `6aeb01e52`, spec §12) — in review

Step 1 confirmed the mechanism on the right denominator (#8); the change
moved `latest_at` off the emitter-free path (spec §2.3 completed);
byte-identity held (oracles, the three constants unchanged, H5). §12,
interleaved against the same control on a quiet box: **H4 (a) 0.04 (from
0.245) — MET by 0.16**; H4 (b) 0.096 ms/call — 758–1,008× against the
frozen figure, 975× against the control — MET; **H2 (c) 59.95% — NOT MET**
by 40 points, and the reason is now legible: with the hazard fold at 0.5%
of the six folds, `believed_water` (KnownWater's A* per water room) is 99%
of them; H5/H6/M1 byte-identical to §11; the falsifier's crossover moved
to h = −20.6; the level −9.27% at 200 agents on four valid pairs. One
finding no criterion asked for: §4's r² ≥ 0.5 filter admits 0 of 4
campaign runs BECAUSE the criterion succeeded (`k` 0.023 against `C` 90.6
leaves no slope to fit) — reported under four readings, all 0.03–0.04, and
handed forward: an H4-shaped criterion wants an effect-size floor, not an
r² floor.

## Close boundary — main moved again, re-measured MAIN-FIRST (decision 0541)

`origin/main` is at `a712371dc`: The Rack (the tick writes the roster and
the turn reads it), The Plumb, The Reservoir's close-out, and one
regenerated-conflict fix landed. A detached checkout of `a712371dc` with
none of this campaign's code, given `ledger_hash_witness.rs`'s scripts:

```
witness              campaign constant      main @a712371dc
seed-42 ledger       0xabc4731e5cf1ab21     0x36eb5f3117e82539   MOVED (The Rack)
emitter ledger       0xc851e64b010538b2     0xc851e64b010538b2   unchanged
emitter hazard       0xa9f17d82c1832854     0xa9f17d82c1832854   unchanged
```

So at absorption the seed-42 constant is re-recorded to MAIN's value —
taken before the merge, so the merged tree is required to reproduce a
number this campaign did not produce — and the emitter pair stands. The
merge itself conflicts in `session.rs` (The Rack rewrote it: seven
`with_fields` sites now, a triple return from `step_with_occupancy`) and in
the type-audit aggregate; both are resolved by hand and regeneration
respectively, in a task of their own, after 9c's review.

## Task 9c — complete (review clean; three doc minors deferred, two fixed in the absorption)

Byte-identity of the one post-unblinding change held at every point the
reviewer checked (witness-first argument unchanged; the emitter-free arm
never read the map; the map's guard drops before the emitter loop; the
scan-entry ordering unchanged; no arithmetic moved). §12 is a pure append
(485/0); §4 and §11 byte-untouched; the deterministic-column md5s were
reproduced independently from the raw files and equal §11's.

## Close boundary — main `a712371dc` absorbed (`bd4d58c7b`) — in review

`session.rs` hand-resolved: The Rack's file verbatim, this campaign's 63
lines re-applied (seven `with_fields` sites, seven `with_ground`; the
stateless seed's terrain takes a local memo moved into the struct literal,
as The Rack does with `folds`). The seed-42 constant re-recorded to MAIN's
`0x36eb5f3117e82539` and reproduced on the merged tree; the emitter pair
unchanged. **Two things the merge did that nobody predicted:** a CLEAN
auto-merge duplicated the `UNI-ecs-is-the-adaptive-cache` registry row
(both campaigns edited its Where cell; `docs_consistency` caught it — the
"a clean merge can duplicate a registry row" lesson, again), and The
Plumb's default-deny constant lint refused the oracle test's two haunted-
overlay constants, now tagged (one-line tags only; a wrapped tag parses as
malformed). `latest_visit_and_witness` deleted for `trail_and_witness`;
`latest_visit`'s doc names its one caller. Gate 1,123 sub-floor tests.
