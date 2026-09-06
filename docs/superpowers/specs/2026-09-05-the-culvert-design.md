# The Culvert — design

**Status (2026-09-06):** COMPLETE. Tasks 1-7 and 9 shipped; Task 8 excluded by
Task 5's measurement under Rule 3's conservative-default branch (ledger #14).
Readout in §11: **C1a PASS** (the criterion of record), **C1b PASS**, **C2
FAILED** at 1.13x against a 10x floor with the cause measured rather than
argued, **C3 PASS** as a no-regression control. Byte-identity held through two
absorptions and was re-established at the close by a same-tree control
(ledger #16). Chronicle, retrospective, registry rows and the reconciliation
rows landed with it.

**Campaign** `campaign/the-culvert` · branch cut from `main` at `a8bde6769`
**Decision block** 0806–0815 (main ceiling 0756 at reservation)
**Ledger** [`2026-09-05-the-culvert.md`](../ledgers/2026-09-05-the-culvert.md)
**Predecessors** [The Detent](2026-09-02-the-detent-design.md) (§12.3, §12.8),
[The Kerf](2026-09-04-the-kerf-design.md) (§11),
[The Waymark](2026-07-31-the-waymark-design.md) (Task 4, Task 5)

---

## 0. What this campaign is, and the thing it is not

It is the last axis of `TOOL-known-water-plan-per-water-room`, which The Kerf
left as that row's sole survivor: `believed_water` re-runs a budgeted
graph search **once per known water room, once per read**, and neither the
origin nor the water set changes between reads.

It is **not** a change to what any creature believes, drinks, or does. Every
committed byte is identical before and after. It is not a change to
`water_at`, which The Kerf just rewrote. It is not the affect-replay memo, not
7b, and not a merge of the trail indexes — all three are out by the brief.

**And it is not, in the end, the mechanism the brief proposed.** The brief
specified "a per-entity memo of `(home, water room, hazard set) → plan length`"
and warned that "the hazard set is an input, so the key must carry it or the
memo is wrong." Two thirds of that sentence is false against the code, and
the counting probe falsified the remaining third. The corrected mechanism is
smaller and stronger; §1 and §2 say why, and ledger entries #2, #3, #5 and #7
record how the correction was arrived at rather than presenting it as though
it had been obvious.

---

## 1. The finding, restated

### 1.1 What the code does

`believed_water` (`windows/vessel/src/liveness.rs:1199`) reads the entity's
distinct visited water rooms off `LatestVisit`, then ranks them:

```rust
seen.into_iter()
    .filter_map(|r| {
        plan_to_room(&npc.home, &r, budget, &std::collections::BTreeSet::new())
            .map(|p| (p.len(), r))
    })
    .min_by(|(la, ra), (lb, rb)| la.cmp(lb).then_with(|| ra.cmp(rb)))
    .map(|(_, r)| r)
```

Three facts about that loop decide this whole campaign, and each is a reading
of the code rather than an inference from the prose around it:

1. **The avoid set is a freshly-allocated EMPTY `BTreeSet`.** So are the ones at
   `shared_believed_water` (`:1880`) and `nearer_to_home` (`:8509`). The only
   production call that passes a real hazard set is `HomeNavCache::home_nav`
   (`:5554`). **The belief ranking is hazard-blind.**
2. **`NavSpace` never consults `Terrain`.** It holds exactly `dest` and
   `avoid`; `edges_from` computes `move_cost(i, &n, self.avoid)` over
   `Facet::neighbors`. So `plan_to_room(from, dest, budget, ∅)` is a pure
   function of **mesh geometry alone** — not of the ledger, not of the world,
   not of the tick.
3. **`NavSpace::heuristic` returns `0`.** This is Dijkstra, not A\*. The
   registry row says "an A\* with a 1,000-node budget"; that half of the row is
   wrong and is corrected at close.

Only `p.len()` is consumed. The plan itself is dropped.

### 1.2 What the counts say

A throwaway probe counted `plan_to_room` calls, distinct `(home, dest)` pairs
and node expansions per roster-wide `believed_water` sweep, on both shapes.
Counts are deterministic, so they were taken on a box at load average 88 with
no loss of validity. Full tables are in ledger #5.

**Shape A — `session_length_scaling`: seed 42, 50 agents, 200 ticks, 10 bands.**

| band | non-empty | max n_i | median n_i | calls | distinct pairs | expansions | budget-exhausted |
|---|---|---|---|---|---|---|---|
| 1 | 10/50 | 12 | 0 | 37 | 37 | 15,437 | 14 |
| 5 | 11/50 | 36 | 0 | 67 | 67 | 41,174 | 39 |
| 10 | 11/50 | 46 | **0** | 83 | 83 | 57,190 | 55 |

**Shape B — the possession shape: seed 17, `Session::start` + 12 waits, roster 67.**

| wait | non-empty | max n_i | median n_i | calls | distinct pairs | expansions | budget-exhausted |
|---|---|---|---|---|---|---|---|
| 1 | 18/67 | 3 | 0 | 31 | 8 | 479 | 0 |
| 6 | 52/67 | 16 | 7 | 363 | 48 | 30,743 | 0 |
| 12 | 52/67 | 23 | **9** | 529 | 83 | 66,002 | **0** |

Totals: Shape A **679 calls / 425,042 expansions / 83 distinct pairs**; Shape B
**4,060 calls / 392,391 expansions / 83 distinct pairs**.

### 1.3 The three things that are not in the brief

**(a) The dominant cost is FAILURE, not distance.** In Shape A, 404 of 679
calls (59.5%) exhaust the 1,000-node budget and return `None` —
`none_returned == budget_exhausted` at every band, so every `None` is
exhaustion. Those 404 calls are `404 × 1001 = 404,404` of 425,042 expansions:
**95.1% of all the work**. The 275 calls that succeed average **75.0**
expansions and return a median **3-hop** plan. The creature walked to those
rooms, so they are reachable; they simply sit outside the ball a
zero-heuristic Dijkstra covers in 1,000 expansions.

The consequence is a design constraint, not a curiosity: **the memo must store
the negative result.** A memo that caches only successes re-pays 95.1% of the
cost forever while its hit rate reads 87.8% — a number that would look like a
win and be one only for the calls that were already cheap.

**(b) The two shapes need the memo for opposite reasons.** Shape A's
within-sweep duplicate rate is **exactly 1.00× at all ten bands**: no two
agents ever share a `(home, water_room)` pair, so a per-tick memo buys
literally nothing and only a session-lived one helps (87.8% hit, 100% by band
10). Shape B's residents are co-located: the same pair is re-planned
**6.4×–9.1× inside a single sweep**, so within-sweep dedup alone removes 86.2%
of calls, and the session-lived memo reaches 98.0%. A design fitted to either
shape alone misjudges the other; a session-lived memo collects both.

**(c) Therefore the memo is SHARED, not per-entity — which the brief got
backwards.** An entity never duplicates its own pair within a sweep, because
`water_at` returns distinct rooms. So **every** one of Shape B's 6.4×–9.1×
duplicates is a duplicate ACROSS entities: two residents sharing a home, or
sharing a water room reached from the same home. A per-entity memo cannot see
them and would forfeit the whole 86.2%. `HomeNavCache` is per-entity for a
reason that does not transfer — it keys on `pos` and an avoid-epoch, both of
which are per-entity — and copying its shape here would be copying the wrong
half of the precedent.

**(d) The distinct-pair population is tiny and it saturates.** 83 pairs in each
shape, against 679 and 4,060 calls; Shape A adds **zero** new pairs at band 10.
The memo is bounded by about a hundred entries, not by history length. That
bound is what makes holding one for a whole session safe, and §2.3 states the
one case where it would not hold.

---

## 2. The architecture

### 2.1 The mechanism

A caller-owned, session-lived memo of the pure function the belief folds
already call:

```
(from, dest, budget) → Option<hop_count>
```

- **Shared across entities**, for §1.3(c).
- **Stores `None`**, for §1.3(a).
- **Stores the hop count, not the plan** — only `p.len()` is ever consumed.
  This is the refinement The Waymark's own ideonomy pass reached for
  `HomeNavFeature` ("cache the consumed feature, not the full plan"), and it
  is the reason the memo's entries are 16 bytes rather than a `Vec<Action>`.
- **Takes no `avoid` parameter at all.** This is the key-hardening rider from
  `HomeNavState`, applied structurally rather than by comment: a future caller
  holding a real hazard set cannot reach the memo, because there is nowhere to
  pass one. That is a compile error instead of a silently wrong answer.
- `budget` is in the key for the same reason `HomeNavState` puts `home` and
  `budget` in its own: today every belief site passes `PLAN_BUDGET`, and
  nothing structurally enforces that.

### 2.2 Which sites it serves

| site | anchor | in scope | why |
|---|---|---|---|
| `believed_water` (`:1224`) | `npc.home` — fixed for the session | **yes** | the subject |
| `nearer_to_home` (`:8509`) | `home` — same anchor, same key space | **yes** | it must agree with `believed_water`'s distances or the tie-break it was written to match breaks; sharing one memo makes that agreement structural |
| `shared_believed_water` (`:1880`) | `here` — the CURRENT position, which moves | **no, pending measurement** | see below |

**Why the third site is carved out rather than included.** Its anchor moves
every step, so its key space is `positions × water rooms` rather than
`homes × water rooms`. That is the one way the §1.3(d) bound fails: an
unbounded memo in a long session. The probe measured the home-anchored fold and
**did not measure this one**, and this spec will not assume the result
transfers. Shape A has zero co-located peers (The Kerf), so `shared` collapses
to its alone-path `return own` there and the question does not even arise;
Shape B is where it would.

**Task 2 measures it and §3 rule 3 decides it.** The measurement is the same
probe with a second key space; the decision is a rule, not a prediction.

### 2.3 What invalidates the memo

**Nothing, within a process.** By §1.1(2) the memoized function is pure over
mesh geometry, so no tick, commit, terrain rebuild, belief change or hazard
change can move an answer. The memo is nevertheless scoped to a session, for
two reasons that are about hygiene rather than correctness: a memo must not
outlive the world whose mesh it describes, and a lifetime bounded by the thing
that owns the mesh is one a reader can check.

**Three things WOULD invalidate it, and each is excluded by construction:**

| would invalidate | excluded by |
|---|---|
| a non-empty avoid set | the memo takes no `avoid` parameter (§2.1) |
| a different `budget` | `budget` is in the key |
| a different mesh | the memo dies with its session |

**Decision 0756 does not bind this.** That rule — "a resident index earns its
state only when a read is asymptotically cheaper than on its parent" — governs
**ledger folds**, and this is not one: it is derived from no fact, absorbs no
fact, and has no parent to be cheaper than. It is the same category as
`RoomMeshMemo` and `PrimaryAfraidMemo`, both of which are memos of pure
functions held beside the sim rather than tenants of the store. The spec says
so explicitly because a reviewer reaching for 0756 here would be reaching for
the nearest rule rather than the right one.

### 2.4 The mechanisms that were rejected, and on what

| mechanism | rejected on |
|---|---|
| **one-to-many distance field** (one Dijkstra from `home` settles every candidate) | Three properties must be established first and none is today (§9). Its cost is also **shape-dependent in a way the memo's is not**: a field pays a full budget per distinct home *whatever is asked of it*, so it wins where the memo's misses are expensive (Shape A: at most 11 distinct homes → **≤ 11,011** expansions against the memo's ~57,155) and loses where they are cheap (Shape B: **zero** budget exhaustions and a **maximum of 548** expansions on any call, so the memo's 83 misses cost ~8,300, which a field beats only if Shape B's residents share fewer than about eight homes between them). **The distinct-home counts are NOT measured — the probe counted `(home, dest)` pairs, not homes** — so this row rejects the field on its proof burden and on the fact that its advantage is shape-dependent, not on a cost comparison it does not have. The home count is the number that would settle it, and §9's row asks for it. |
| **incremental argmin** (maintain a running best per entity; O(1) per read) | **Unsound**, on a fact the repository already holds. Belief reads run at PAST instants — that is what `ReadWitness::note_belief` counts, `beliefs_in_the_past` reports, and `rule_six_witness_belief_reads_run_at_past_instants` pins. A running argmin keyed on entity alone would answer a past-instant read from a larger set than that instant admits. The pair memo is sound precisely because its key carries no time. This is also why `nearer_to_home` — which *is* the incremental fold — is a separate mid-walk path rather than `believed_water`'s implementation. |
| **an admissible octile heuristic on `NavSpace`** | It changes which least-cost path is returned, therefore can change `p.len()`, therefore can change the chosen water room: a behaviour change and an epoch. Out under this campaign's byte-identity discipline; recorded as a registry row. |
| **threading `RoomMeshMemo` into the three belief calls** | Byte-identical and cheap, but it accelerates searches this campaign removes. Kept as a registry row; §7 says why it is not free to bundle. |
| **memoising inside `plan_to_room` for all callers** | `home_nav`'s avoid set genuinely varies, so a callee-side memo needs the very key §2.1 deletes. |
| **per-entity memo** | §1.3(c): forfeits Shape B's entire 86.2% within-sweep win. |
| **per-tick memo** | §1.3(b): Shape A's within-sweep duplicate rate is exactly 1.00×, so it buys nothing there. |

---

## 3. Decision rules, not predictions

Each rule names the branches and the response, so that no step of the plan
smuggles an outcome inside an imperative.

**Rule 1 — the artifact diff after `make rebaseline`.**
`book/src/gallery/`, `cli/tests/fixtures/`, `clients/game/core/tests/fixtures/`
or any world/almanac/scene output moved → **STOP**, this is an epoch event and
the campaign's premise has failed; do not commit, report. Only
`docs/audits/type-audit-report.md`, `docs/audits/placement-audit-report.md`,
`docs/audits/plumb-report.md`, `docs/digest/` or a `docs/timings.md` row moved
→ regenerate and commit in the same commit. Nothing moved → commit as is.

**Rule 2 — the memo's answer versus a fresh search.** The instrument is a
direct comparison, not a hash. For every `(from, dest, budget)` the memo is
asked during a real run, `memo.get(...)` equals a fresh
`plan_to_room(from, dest, budget, ∅)`, including when both are `None`. Any
disagreement → **STOP**; the purity argument of §2.3 is false and the campaign
has no mechanism. **This is deliberately stronger than a ledger hash**, on The
Kerf's own finding: its control B moved none of four script hashes yet reddened
all four real-shape FOLD-equals-SCAN sweeps, so *a hash is a weaker instrument
than the direct set comparison*.

**Rule 3 — the moving-anchor site (`shared_believed_water`).** Task 5 measures
its would-be key population and the criterion is a **shape, not a magnitude**:

- Distinct `(here, dest)` pairs **saturate** — the count stops rising as ticks
  are added, the way the home-anchored population does at 83 → **include** the
  site, and name what bounds it.
- Distinct pairs **keep rising** with tick count without saturating →
  **exclude** the site, say so in a comment at the call, and record the curve.
- **The curve cannot decide** — the run is too short, or the two populations do
  not separate → **exclude**, and say plainly that the verdict rests on the
  conservative default rather than on a measured separation. This third branch
  is not a formality: a plateau in this system has been observed to be
  TEMPORARY (Task 5 found a five-wait flat stretch that later resumed
  climbing), so "it stopped rising" is weaker evidence than it looks, in both
  directions. A verdict that overstates its evidence is a defect even when it
  points the right way. (Controller ruling R12 — the plan's Task 5 brief
  offered this branch and this section did not; the brief was the intent.)

**The criterion is deliberately not "comparable to 83".** An earlier draft said
that, and it is an observed value standing in for a predicate — the same defect
R9 corrected one task earlier. A magnitude cannot decide this question: a
population of 200 that has stopped growing is safe to memoize and a population
of 40 that is still climbing is not. Saturation is the property; the number is
only how you see it.

**The measurement is a one-off diagnostic and is `#[ignore]`d by
construction.** Seeing saturation needs enough ticks to distinguish "stopped"
from "rising slowly", which means the 200-tick shape, which costs ~129 s. That
is paid ONCE, by hand, and its result is recorded — never on every gate. What
lands in the gate is whatever assertion the verdict licenses, on a cheap shape.
(Controller ruling R10.)

**Rule 4 — the belief probe's vacuity.** Task 1 turns the two `println!`
vacuity notes into failures. If the repaired probe still reports a vacuous
belief set for its chosen agent, that is not a licence to proceed: the agent
selection is wrong and Task 1 is not done. The selection rule is "the roster
member with the most known water rooms at this band", and §1.2 says what that
member looks like (46 rooms at Shape A band 10; 23 at Shape B wait 12), so a
zero is a defect and not a possibility.

**Rule 5 — the predicate registration.** Task 1 repairs
`session_length_scaling` and `agent_scaling`. If the repaired examples run to
completion, the campaign has its instruments. If either still panics on a
different missing predicate, the roster of hand-registered predicates is
enumerated and the divergence guard of §6 Task 1b becomes mandatory rather than
recommended.

**Rule 6 — the campaign-time hash constants.** They are minted at Task 1 from
two agreeing runs, given a positive control that MUST move them before they are
believed, re-recorded main-first after every absorption, and retired at close
with the dated record kept in the test module's own doc — the discipline The
Detent used and The Kerf sharpened. A control that moves no constant means the
constants do not reach this campaign's path, and the response is to find a
control that does, not to keep the constants and note the gap.

---

## 4. Preregistration

Frozen before any implementation. Three instruments, all run. A falsified
prediction is a finding.

### 4.1 The lead criterion is a COUNT, not a clock

**C1 is deterministic, reproducible on any box at any load, and it is the
quantity the mechanism actually moves.** It is preferred over a timing
criterion for exactly the reason `HomeNavCache::searches` exists: *a
deterministic search count is the witness the work asks for, in preference to a
wall-clock proxy.* **It has two tiers, because the two halves are instrumented
at different costs and only one of them can be committed.**

**C1a — SEARCHES per roster-wide `believed_water` sweep. Permanent, committed,
and the criterion of record.** A search is a memo miss. Counting them needs
nothing but a counter on the memo itself — the exact shape
`HomeNavCache::searches` already has, written by the memo and read by the
instrument.

**The count is taken THROUGH `RouteMemo::searches()`, over a real
`believed_water` sweep — never re-derived analytically beside it.** This is not
a stylistic preference. The witness as first specified summed
`LatestVisit::water_at` set sizes and never called `believed_water` at all;
since the memo does not touch `water_at`, that witness would have reported its
pre-fix number forever and could never have gone green (ledger #15). The
quantity that moves must be read from the code that moves it. A non-vacuity
guard (`occurrences > distinct_pairs`) runs first, so a shape with nothing to
collapse reddens rather than passing for free.

| | before (measured) | criterion |
|---|---|---|
| Shape A, whole 200-tick run | 679 searches | **≤ 100** |
| Shape B, whole 12-wait run | 4,060 searches | **≤ 100** |
| either shape | — | memo entries **≤ 200** at the end of the run, and `entries == searches` |

Both denominators are the call counts (679 and 4,060), non-zero and reported.
`entries == searches` is the strongest single check available: a miss inserts
exactly one entry, so any divergence means the memo is either re-searching a
key it holds or holding a key it never searched.

**C1b — node EXPANSIONS per sweep. Campaign-time, retired at close.** Expansions
are the honest cost quantity — a budget-exhausted miss costs 1,001 where a
successful one costs 75, and C1a counts those the same. But counting them needs
a counter inside `AStarSolver::solve`, and **the kernel is the determinism
substrate; it does not acquire a permanent instrumentation surface so that one
campaign can measure itself.**

So C1b is taken the way this repository already takes campaign-time hash
constants: a temporary counter applied for the measurement runs, the numbers
recorded with their date and the SHA they were taken at, and the counter
reverted before merge with the dated record kept in the instrument's own module
doc. The before figures — Shape A **425,042** expansions over 679 calls, Shape B
**392,391** over 4,060 — were taken this way at `a8bde6769`; the after figures
are taken the same way at the campaign's own head.

| | before (measured) | criterion |
|---|---|---|
| Shape A, whole 200-tick run | 425,042 expansions | **≤ 60,000** |
| Shape B, whole 12-wait run | 392,391 expansions | **≤ 15,000** |

**What retiring C1b costs, said plainly.** After close, nothing in the
repository can reproduce the expansion figures without re-applying the patch —
the same limitation The Detent accepted for its hash constants, and named:
a constant-free witness guarantees determinism but *cannot detect a behaviour
change at all*. C1a is what survives, and C1a cannot tell an expensive miss
from a cheap one. That is why the criterion of record is `≤ 100` searches
rather than a ratio: at a hundred searches against 679 and 4,060 calls, the
distinction between miss kinds stops being able to matter.

### 4.2 The timing criterion is an EFFECT-SIZE floor, never an r²

**C2 — `k`, the slope of `believed_water`'s µs/call against the probe agent's
history in facts, measured on the REPAIRED instrument against the roster
member with the most known water rooms.**

The criterion is **`k` falls by at least 10× against the same-box control**,
stated in µs/call/fact with both values reported. It is **not** gated on r².

This follows The Detent §12.8 verbatim: its H4 was met on the quantity (0.04
against a threshold of 0.20, control 0.91) and failed its own instrument's
`r² ≥ 0.5` filter, which admitted 0 of 4 campaign runs — because a fold with no
slope left cannot produce a well-fitting line. *A goodness-of-fit filter on a
criterion whose success destroys the fit is a filter that fails at the finish
line.* The r² is still **printed**, as a diagnostic; it gates nothing.

**C2 is expected to be the weaker instrument here and is preregistered as
such.** The memo leaves an O(|set|) map lookup per read, and |set| grows with
history, so `k` stays strictly positive and small. That is why C1 leads.

### 4.3 The control

**C3 — `fold_depth_sweep`**, which sweeps `drive_at` only and structurally
cannot see this fold. Run as a no-regression control on the Sustenance reads
and reported as being that. It is the one instrument of the three that runs on
`main` today (§6 Task 1).

### 4.4 Conditions on every timed run

All three load averages recorded before and after. **Any run whose 1-minute
average exceeds 10 at either end is set aside and listed.** This is not
boilerplate for this campaign: the counts in §1.2 were taken at load average
88, which is exactly why they are counts. Every criterion is reported
separately.

---

## 5. Determinism contracts (leads the G3 flagged section)

**No epoch. No save-format change. No seed-label change. No stream-consumption
change. No new predicate.** The campaign commits byte-identical worlds,
almanacs, scenes and fixtures, and Rule 1 makes any movement a stop rather than
a rebaseline.

The byte-identity argument is **by construction, not by testing**:
`plan_to_room(from, dest, budget, ∅)` is pure over mesh geometry (§1.1(2)), so
a memo of it returns what the call would have returned. Testing confirms the
construction; it is not the reason to believe it. The three ways the
construction could be wrong are enumerated in §2.3 and each is excluded
structurally.

**Two witnesses, and the weaker one is named as weaker.** Rule 2's direct
memo-versus-fresh-search comparison is the primary instrument. Campaign-time
ledger hashes (Rule 6) are the secondary one, and The Kerf's control-B result
is the reason the ordering is that way round and not the reverse.

---

## 6. The stage carve

Four stages. `make sluice-stage BRANCH=campaign/the-culvert REF=<full-sha>` at
each boundary; absorb main and regenerate aggregates first.

**Stage 1 — the instruments (no production code moves).**
- *Task 1a.* Repair the `slept-on` panic in `session_length_scaling` and
  `agent_scaling`. Rule 5 governs the branch.
- *Task 1b.* A guard against the next such divergence. The two examples
  hand-maintain a copy of the drive stack's predicate list with nothing
  checking it; a drive that commits a new predicate breaks them again, silently,
  because no gate runs an example. The cheapest honest fix is one shared list
  the drive stack publishes; the cheapest guard is a test that reddens when the
  examples and the stack diverge. **The implementer chooses which after reading
  the emission sites — this spec does not prescribe one from outside the code.**
- *Task 1c.* The belief columns: vacuity becomes a failure (Rule 4), the probed
  agent becomes the roster member with the most known water rooms, and the
  roster-wide reading is added beside it — the treatment the bench already gave
  `DRANK`. The pre-fix column is reported beside the fixed one so the change of
  instrument is visible rather than silent.
- *Task 1d.* Mint the campaign-time hash constants with their positive control
  (Rule 6).

**Stage 2 — the counting witness and the moving-anchor measurement.**
- *Task 2a.* Promote the throwaway probe's counting into a committed instrument
  with a denominator on every ratio, red on the pre-fix tree.
- *Task 2b.* Measure the `shared_believed_water` key population; Rule 3 decides
  the site's inclusion.

**Stage 3 — the memo.** Built test-first against Rule 2's comparison. The
home-anchored sites first; the moving-anchor site only if Rule 3 admitted it.

**Stage 4 — readout and close.** The three instruments, C1/C2/C3 reported
separately, chronicle, retrospective, registry, decisions, `make rebaseline`.

---

## 7. In / out

**In.** The memo and its two (possibly three) call sites; the instrument
repairs of Stage 1; the counting witness; the registry and decision-record
corrections §9 lists.

**Out, and why each is out rather than merely unmentioned.**

- **7b, the affect-replay memo, merging the trail indexes** — out by the brief.
- **The one-to-many field** — §2.4. It is the better *idea* and the worse
  *mechanism here*; it is a campaign of its own with three properties to
  establish.
- **The octile heuristic** — an epoch (§2.4).
- **Threading `RoomMeshMemo` into the belief calls** — byte-identical and
  cheap, and deliberately not bundled: it would accelerate the very searches
  the memo removes, so shipping both in one campaign would make each one's
  contribution unattributable in the readout. A campaign that cannot attribute
  its own effect has measured nothing.
- **`plan_to_water`** (`:2268`) — same one-to-many shape, but it passes a real
  hazard set, so §2.1's key does not serve it.
- **Changing `water_at`** — The Kerf just rewrote it; The Kerf's own ruling 6c
  leaves a note for whoever touches it next, and this campaign is not that.

---

## 8. Decisions this campaign may need (block 0806–0815)

Candidates, not commitments; a decision is minted only if a second site would
otherwise re-derive the boundary — the restraint The Kerf exercised in leaving
0757 unminted.

- **0806 (likely).** A memo of a pure geometric function is not a resident
  index, and decision 0756 does not govern it. Worth minting because the two
  are now adjacent in the same file and a reviewer will reach for 0756.
- **0807 (conditional on Rule 3).** A memo whose key space is unbounded in
  session length is excluded rather than bounded.
- **0808 (conditional on Task 1b).** An instrument no gate runs owes a
  divergence guard where it duplicates a list the code owns.

---

## 9. Frontier bookkeeping

Rows to correct or add at close:

- **`TOOL-known-water-plan-per-water-room`** — shipped or refuted by this
  campaign's readout. Its present text says "an A\* with a 1,000-node budget";
  `NavSpace::heuristic` returns `0`, so it is Dijkstra. Correct that regardless
  of outcome.
- **`TOOL-belief-probe-vacuity-is-a-println`** — adopted as Stage 1 work rather
  than left `raw`.
- **`TOOL-place-predicate-index`** — its sentence "its A\* per admitted water
  room is [the cost]" survives; the finer reading is §1.3(a): the cost is the
  *budget-exhausted* searches, 95.1% of expansions.
- **New — the one-to-many field**, carrying the unmeasured quantity that
  would decide it — the number of DISTINCT HOMES per shape, which the probe did
  not count (it counted `(home, dest)` pairs) — and citing The Waymark's `#[ignore]`d
  `reverse_field_matches_forward_search_for_every_empty_avoid_room` and the
  three properties it must establish: that the distance half (asserted in a doc
  comment with no live assertion, over 346 rooms at **budget 300**) still holds
  at **budget 1,000**; that equal octile cost implies equal hop count on this
  mesh, given that `12 × 17 = 17 × 12` is an equal-cost pair with hop counts 17
  and 12; and that the budget cutoff is reproducible from a single run.
- **New — instruments no gate runs accrue invisible breakage.** Two of three
  vessel benches panicked on `main` for two days across two merged campaigns.
- **New — the octile heuristic as an epoch candidate.**
- **New — 59.5% of belief searches exhaust `PLAN_BUDGET`.** A fact about the
  mesh and the budget, not about this campaign.

---

## 10. Operational notes for the implementer

- The lexicon guard counts the token `cell` per file against a committed
  inventory, and `std::cell::Cell` trips it. A counter wants `AtomicU64`, or a
  `// lexicon: <reason>` waiver — reasonless is a failure, not a pass. This
  cost the counting probe a red gate.
- `plan_to_room` is `hornvale_vessel::action::plan_to_room`; it is **not**
  re-exported from `liveness`.
- `PLAN_BUDGET` is a private const mirrored by hand in two examples
  (`PROBE_BUDGET`, `BUDGET`) with nothing enforcing agreement. If Task 1b's
  guard generalises to cover that, say so; if not, leave it.
- The throwaway probe is **not in the worktree**; it is parked outside it,
  because it does not compile without the campaign-time kernel counter of §4.1
  (C1b) and an uncompilable example under `examples/` breaks every
  `--all-targets` build in the tree. Task 2a decides what of it survives.
- **The Zenith (`52c53d78f`, absorbed at `2538f0be5`) retired `SkyChoice` and
  changed `sky_of(..).calendar()` from an `Option` to a value.** It edited both
  broken examples to keep them COMPILING and did not notice that neither RUNS —
  a second witness for §6 Task 1b's premise, landing during this campaign's own
  spec review.

---

## 11. What shipped, measured

Taken at Task 9 against the criteria §4 froze, each reported separately.
**C1a PASS. C1b PASS. C2 FAILED. C3 PASS.** Head `8acd377c5`; the before
column re-taken on the Stage 2 boundary `d36a23bd7` with the same
instrument. Full working, every load average and every set-aside run:
`.superpowers/sdd/2026-09-05-the-culvert/task-9-report.md`. **Read the
standing constraint immediately below before quoting any µs/call figure from
this section.**

### A standing constraint on this section, set during execution

**No `believed_water` µs/call figure may appear here without naming the
creature it was measured on.** Three different subjects have now produced three
figures on this fold, and any two of them placed side by side read as a
regression that did not happen:

| figure | subject | what it timed |
|---|---|---|
| 8,663.95 µs/call | The Detent's probe: the roster's max-`agent-at` member, on The Detent's tree | an EMPTY belief set — `water_at` and nothing else |
| 79,128.70 µs/call | the same selection, on this campaign's tree before Task 2 | the same empty set, different tree, contended box |
| 109,510.31 µs/call | member 40, the max-known-water member, after Task 2 | 46 known rooms, **none reachable** — 46 budget-exhausting searches per call |

The third is not a regression against the second. It is the instrument being
pointed at the subject the campaign is about, which is what §6 Task 1c
required, and it is the column the memo is expected to collapse. Reporting the
pair unlabelled would assert a 38% slowdown this campaign did not cause —
which is exactly the misreading the campaign-record discipline exists to
prevent, and it would be a plausible number, in the right units, next to a real
measurement.

(Controller ruling R8, taken at Task 2's review.)

### 11.1 C1a — searches per sweep. The criterion of record. **PASS.**

| | before | after | criterion | verdict |
|---|---|---|---|---|
| Shape A — whole 200-tick run | 679 | **83** | ≤ 100 | **PASS** (8.18×) |
| Shape B — whole 12-wait run | 4,060 | **83** | ≤ 100 | **PASS** (48.92×) |
| Shape A — memo at end | — | 83 entries | ≤ 200, `entries == searches` | **PASS** (83 == 83) |
| Shape B — memo at end | — | 83 entries | ≤ 200, `entries == searches` | **PASS** (83 == 83) |

Denominators: Shape A asked 679 route questions over 83 distinct
`(home, dest)` pairs, Shape B 4,060 over 83. `occurrences > distinct_pairs`
holds on both, so neither passes vacuously. Both after-columns were read
through `RouteMemo::searches()` over a real roster-wide `believed_water`
sweep, never re-derived analytically, and a campaign-time kernel counter on
`AStarSolver::solve` agreed with each.

### 11.2 C1b — node expansions per sweep. Campaign-time, retired here. **PASS.**

| | before | after | criterion | verdict |
|---|---|---|---|---|
| Shape A — whole 200-tick run | 425,042 | **57,190** | ≤ 60,000 | **PASS** (7.43×) |
| Shape B — whole 12-wait run | 392,391 | **14,474** | ≤ 15,000 | **PASS** (27.11×) |

Shape A clears by 4.7%, Shape B by 3.5%. Both are real passes and neither has
much room; a shape whose distinct-pair population grew would cross them.

**Shape A's after figure is the memo's STRUCTURAL FLOOR, and that is checkable
in one line.** 57,190 is exactly the control arm's band-10 cost — and band 10's
sweep asks 83 occurrences over 83 distinct pairs, a within-sweep duplicate rate
of exactly 1.00× (§1.3(b)), so the control there *is* one search per distinct
pair. The memo's whole-run total therefore equals one uncached pass over the
full pair population: it searched each of the 83 pairs **exactly once, and never
twice, across all ten bands**. Nothing cheaper exists without changing what is
searched. So the thin 4.7% margin is a fact about **this shape's pair population
against a threshold set close to the mechanism's floor**, not run-to-run
fragility — and it is also the tidiest single check that both instruments are
correct, since a memo that re-searched anything, or a counter that
double-counted, would break the equality.

**THE DATED RECORD (§4.1's own discipline).** Recorded 2026-09-06. Before
column at `d36a23bd7612a26e39bf6419213f6cd7c180c803`; after column at
`8acd377c5`. Instrument: two `AtomicU64` statics beside `AStarSolver::solve`'s
local `expansions` in `kernel/src/astar.rs`, with a read/reset pair —
`AtomicU64` and never `std::cell::Cell`, because the lexicon guard counts the
token `cell` per file and a `Cell` there reddens
`lexicon_guard::no_vertex_sense_cell_comes_back`. Reverted before merge;
`git diff --stat -- kernel/` is empty at the head above. After close nothing in
the repository can reproduce these two rows without re-applying that patch,
which is the cost §4.1 named and accepted.

**The before column agrees with §4.1's `a8bde6769` figures digit for digit** —
not only at the totals but at every band and every wait: Shape A 15,437 /
41,174 / 57,190 expansions and 37 / 67 / 83 calls at bands 1 / 5 / 10; Shape B
479 / 30,743 / 66,002 and 31 / 363 / 529 at waits 1 / 6 / 12. Twelve numbers,
two trees, two independently written instruments. The 99 commits of `main`
absorbed at the Stage 3 boundary did not move this quantity.

### 11.3 C2 — the effect-size floor on `k`. **FAILED, and the reason is the instrument.**

**Subject: roster member 40 of the seed-42 / 50-agent / 200-tick shape — the
max-known-water member, 46 known rooms at band 10, none reachable within the
1,000-node budget.** Every figure in this subsection is that creature.

| run | tree | `k` (µs/call/fact) | r² (diagnostic only) | final-band µs/call |
|---|---|---|---|---|
| before, run 2 | `d36a23bd7` | 317.61954 | 0.975 | 112,996.01 |
| before, run 4 | `d36a23bd7` | 313.53877 | 0.917 | 110,337.93 |
| after, run 1 | `8acd377c5` | 244.70012 | 0.754 | 90,274.63 |
| after, run 3 | `8acd377c5` | 312.98384 | 0.970 | 110,538.97 |

The two before-tree final-band figures (112,996.01 and 110,337.93 µs/call,
member 40) reproduce the 109,510.31 µs/call the constraint table above records
for the same creature at Task 2, to within 3.2% — so the pre-memo column is
stable across the Stage 3 absorption as well as across instruments.

**before mean 315.58, after mean 278.84, ratio 1.13× against a criterion of
10×. FAILED by a factor of 8.8.** The r² gates nothing, per §4.2, and would
not have changed the verdict either way. The two after runs disagree with each
other by 1.28×, more than the before/after difference, and the two before runs
agree to 1.3% — so the honest statement is **`k` did not move**, not "`k` fell
1.13×".

**§4.2's preregistered reason was right in direction and wrong in mechanism,
and the difference matters.** It predicted `k` would stay "strictly positive
and small" because a residual O(|set|) map lookup remains. That assumed a WARM
memo in the probe. `probe_believed_water_us` constructs a **fresh `RouteMemo`
inside its own repetition loop**, deliberately — Task 7 landed it that way so
the column would stay comparable to pre-campaign runs, and said so at the site.
`LatestVisit::water_at` returns distinct rooms, so a fresh memo never hits
within one call and every room runs a real search.

**That is measured, not inferred.** A same-tree control arm on the campaign
head — the identical sweep with a fresh memo per call — ran **679 searches /
425,042 expansions (Shape A) and 4,060 / 392,391 (Shape B), digit-identical to
the pre-memo tree at every band and every wait.** The C2 instrument's call path
performs byte-identical search work before and after the campaign. A column
whose work is unchanged cannot fall 10×; the residual 1.13× is noise. **C2 was
unable to detect this mechanism, and a count established that where no clock
could.** C1 leads, exactly as §4.1 said it would.

**Context, not a criterion: the whole-tick column DID move.** Whole-tick `k`
2.19468 / 2.14866 → 0.95896 / 1.31034 ms/tick per fact (1.91×); band-10
normalised 465.92 / 477.46 → 230.88 / 280.80 ms/tick (1.84×). That column is
the tick walk, which `session_length_scaling::run` threads the **run-lived**
memo into. The separation between the two columns is the finding: the fold
probe holds the memo cold on purpose and sees nothing; the tick that owns a
warm one nearly halves. It is reported here so the next reader does not take it
off the same output as a C2 result, because it is not one.

**A determinism cross-check that came free.** Every deterministic column of
`session_length_scaling` — facts per band, `HomeNavCache` searches per band,
folded/agent, drank/tick, ledger length — is identical across all four runs and
both trees, band for band.

### 11.4 C3 — the control. **PASS: no regression, and it is a control.**

`fold_depth_sweep` sweeps `drive_at` only, constructs no `RouteMemo`, and never
calls `believed_water` or `nearer_to_home` — it **structurally cannot see this
fold**. It is evidence the campaign broke nothing on the Sustenance reads, and
it is not evidence the campaign worked.

| regime | before `k` | after `k` |
|---|---|---|
| periodic resets (S bounded) | 0.00131 µs/call/fact (r² 0.999) | 0.00125 (r² 1.000) |
| single early reset (S == H) | 0.00127 µs/call/fact (r² 1.000) | 0.00135 (r² 1.000) |

The two moves are in opposite directions, 4.6% and 6.3% — run-to-run noise.
Monotonicity 6/6 rises in both regimes on both trees.

### 11.5 Conditions, and the runs set aside

**Three of six timed runs were set aside** under §4.4 for a 1-minute load
average above 10 at one end: 50.74, 64.46 and 46.05, against valid runs taken
between 1.42 and 6.58. Their `k` readings scatter from **31.838 to 469.840** —
an order of magnitude, in both directions around the valid runs' 313–318 — and
the lowest of them, taken on a *before* tree, would have read as a 9.8× **rise**
in `k` (312.98384 / 31.838, against valid after-run 3): an apparent catastrophic
regression this campaign did not cause. **Contention could not have manufactured
a pass, only alarm.** The criterion ratio is before/after, so the largest
spurious *fall* available anywhere in this set — max before over min after,
469.840 / 244.700 — is **1.92×**, nowhere near the 10× floor.

*(This sentence said "9.8× fall" as first written, at fix round 1. The
magnitude and the tree attribution were right and the direction was inverted,
which turned "noise nearly manufactured alarm" into "noise nearly manufactured
a pass" — the stronger claim, and the false one. It is the third time this
campaign produced a real number, in the right units, beside a real measurement,
attributed to the wrong thing; the arithmetic flags none of them, which is why
§11's standing constraint exists and why this correction is left visible rather
than quietly applied.)*

§4.4 is not boilerplate on this box: on the night
these were taken the 1-minute average moved between 1.4 and 64.5 under other
campaigns' gates, and no run of this length could be scheduled around it.

**All three load averages, before and after, for every timed run.** Recorded
here rather than only in the task report, because that report lives in
git-ignored campaign scratch and dies with the worktree, and §4.4 is a
condition on the numbers above.

| run | window (UTC, 2026-09-06) | load before | load after | verdict |
|---|---|---|---|---|
| C3 before (`fold_depth_sweep`, `d36a23bd7`) | 05:07:15–05:07:17 | 5.39 / 23.98 / 36.80 | 5.39 / 23.98 / 36.80 | valid |
| C3 after (`fold_depth_sweep`, `8acd377c5`) | 05:07:20–05:07:22 | 5.36 / 23.66 / 36.61 | 5.36 / 23.66 / 36.61 | valid |
| C2 after, run 1 | 06:25:32–06:32:25 | 4.20 / 24.25 / 39.25 | 4.72 / 8.60 / 25.44 | valid |
| C2 before, run 2 | 06:32:33–06:39:51 | 5.25 / 8.60 / 25.24 | 1.92 / 3.71 / 16.05 | valid |
| C2 before, run 4 | 07:07:58–07:15:34 | 5.27 / 19.40 / 22.19 | 2.69 / 7.85 / 15.32 | valid |
| C2 after, run 3 | 07:52:51–07:59:43 | 4.71 / 12.02 / 16.94 | 6.58 / 6.64 / 12.19 | valid |
| C2 before, run 1 | 04:44:53–04:56:00 | 6.30 / 27.36 / 36.58 | **50.74** / 52.08 / 46.88 | **SET ASIDE** — read k = 31.838, r² = 0.001, final band 57,069.96 |
| C2 before, run 3 | 06:40:26–06:48:38 | 1.42 / 3.40 / 15.44 | **64.46** / 28.56 / 20.55 | **SET ASIDE** — read k = 469.840, r² = 0.853, final band 140,695.95 |
| C2 after, run 2 | 07:15:39–07:23:26 | 2.47 / 7.72 / 15.23 | **46.05** / 22.89 / 17.82 | **SET ASIDE** — read k = 327.447, r² = 0.565, final band 143,742.68 |

Count runs, listed for completeness and exempt under §4.1: Shape A after
8.09/11.02/27.00 → 46.08/57.20/46.76; Shape B after 10.13/11.62/27.68 →
8.85/11.26/27.27; Shape A before 60.85/51.71/46.04 → 52.37/45.55/43.89;
Shape B before 38.76/46.67/43.92 → 49.42/48.51/44.66.

**The counts were taken first and are exempt**, as §4.1 says: they were read at
loads between 8.09 and 60.85 and every one of the twelve cross-checks in §11.2
came out exact, which is the evidence the load did not reach them.

### 11.6 The two collapse numbers, kept apart

- **529 occurrences → 83 searches (6.37×)** is **within one sweep**, at wait 12
  of the possession shape — cross-entity duplicate collapse, what a single
  roster-wide read can see. Pinned by
  `culvert_sweep_collapses_calls_onto_distinct_pairs`.
- **4,060 → 83 across twelve waits (48.92×)**, and **2,692 asks → 1 search
  across eight waits** on the seed-42 session, are **across reads** — the
  session-lived property, which is the campaign's real lever. Pinned by
  `turn_budget::the_route_memo_survives_between_waits`.

Different claims about different things. They are never averaged, compared, or
presented as one headline.
