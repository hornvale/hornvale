# The Pawl: The Resident Fold Store — A Campaign Design

> **STATUS: SHIPPED (2026-09-02), merge pending.** Branch `campaign/the-pawl`;
> decision block 0536–0545, of which 0536–0541 were ratified and 0542–0545 stay
> unused; ledger `docs/superpowers/ledgers/2026-09-01-the-pawl.md`; chronicle
> `book/src/chronicle/the-pawl.md`; retrospective
> `docs/retrospectives/the-pawl.md`. **The measured result is §11 (the first
> readout) and §12 (the second, after the accumulator) — read both; §11 is not
> superseded and §4 was never edited.** §12.4 carries the verdict table: H2 (a)
> and (b) met, H2 (c) and both H4 clauses not met, H3 held by construction.
> §12.8 names the remaining quarry. Awaiting the merge queue at the time this
> line was written.

Program: **The Penstock** (`docs/superpowers/specs/2026-08-22-the-penstock-metaplan.md`),
stage 7a's tenants — the stages The Tailrace
(`docs/superpowers/specs/2026-08-24-the-tailrace-design.md`) carved as 3–5 and
said "need their own plan". Lineage: Penstock → Tailrace (7a, the primitive) →
**The Pawl** (7a's tenants and the store they live in) → 7b (the typed
intention, `UNI-intention-is-structured`) → 7c (fact lifetime,
`TOOL-log-bounding-epoch-fact-lifetime`), order forced by decision 0238.

A pawl is the catch that lets a ratchet keep what it has gained and only ever
advance. A fold advances and is never invalidated (decision 0236). This
campaign makes the liveness stack stand on folds.

## 0. What this campaign is, and the thing it is not

**Byte-identical.** No fact is added, removed or reworded. The commit site —
the `MoveTo` arm of the walk that pushes `agent_at_fact(npc.entity, &n,
st.day, provenance)` — is not touched. `Ledger` is not touched. Nothing new
enters the save. Every committed artifact stays as it is, and §5 says why the
drift check is not the instrument that proves it.

**It is one store, not six caches.** The Tailrace shipped `kernel/src/fold.rs`
with no tenant; today `grep -c 'Folded<\|LedgerFold' windows/vessel/src/liveness.rs`
prints `0`. The obvious carve — migrate each of the folds it named onto
`Folded` where it stands — would produce six private places to keep a fold
and six half-answers to "when does it advance". This campaign instead builds
**the session-owned, per-entity resident store** those folds are tenants of,
and that store is the first concrete instance of the layer Nathan named on
2026-08-31 (`UNI-ecs-is-the-adaptive-cache`): a data-oriented layer over
DERIVED data, holding tick-by-tick state the ledger already determines, so
that persisting it buys time and never truth. The ledger stays the only
stored truth; the store is discardable at any instant and provably so (§5).

**7b is out.** The typed intention changes what is committed and what
historiography renders — the per-step `provenance` is authored prose and it
is content (0238's own argument). That is an epoch, and bundling an
"artifacts moved" campaign with a "nothing moved" one destroys both
campaigns' legibility (The Tackle / The Deed). 7b is the recommended next
campaign, and this one leaves it a store to anchor an intention in.

**Hysteresis is out.** `PSY-drive-arbitration-limit-cycle` (a seed-42
rust-monster alternating fear and belonging forever, measured by The
Pavement) is a behaviour change. The store is where its state will live,
and §9 says so; the fix is not this campaign's.

## 1. The finding, restated with what has been learned since

The Tailrace established, with direct timing rather than a profile (its §4):

- `drive_at` is history-proportional: `ms/tick = C + k·h` with `k > 0` on
  four runs, elasticity 0.86–1.24 against a 2.48× growth in history. Session
  cost is therefore **quadratic in session length**, and an agent enters
  that regime early in an ordinary session.
- Four of the five siblings are the same shape: `hunger_at`, `believed_water`,
  `shared_believed_water`, `hazard_memory_memo` at elasticity 0.8–1.4 on the
  three quieter runs. `fatigue_at` is not (no stable sign, three orders of
  magnitude cheaper) and is **left alone**.
- The absolute costs at the final band, µs per call, runs 2–4:
  `drive_at` 765–1011 · `hunger_at` 770–1018 · `believed_water` 4,344–6,325 ·
  `shared_believed_water` 4,983–6,804 · **`hazard_memory_memo` 73,460–96,630**.

Reading the code today adds the mechanism behind that last number, which
the Tailrace timed but did not decompose, and it changes what the fix must
be:

1. `hazard_memory_memo` walks the agent's trail into a latest-visit map,
   then for **every visited room** whose halo an emitter could reach, asks
   each emitter's **affect at that room's latest-visit day** via
   `emitter_arousal` → `affect_of(frozen, m, &[], day, …)` → `drive_at(m,
   day)` / `hunger_at(m, day)`. Each of those is a **past-instant read of the
   thirst fold**, O(history) apiece, memoised per `(entity, day)` in a
   `PrimaryAfraidMemo` that is constructed fresh **every tick**
   (`step_with_occupancy`, two sites) and every snapshot (`Session::snapshot`).
   So the hazard fold is O(rooms visited × emitters × history), rebuilt per
   tick. That is why it is two orders of magnitude above `drive_at`.
2. `build_emitter_scan` rebuilds every roster member's sorted trail from
   scratch, per tick, inside a per-agent call — O(agents × history), the
   O(agents²)-shaped term `agent_scaling.rs` suspected.
3. `Session::wait` evaluates the walk **twice** — once through
   `step_with_occupancy` for the occupancy read, once through
   `hornvale_kernel::tick` for the facts it commits ("the same walk, read
   twice", `session.rs`). Any state advanced per *evaluation* would
   double-absorb. This is a constraint on the store's design, not a defect
   to fix here.
4. `decide_step` already reads `frozen` PLUS this tick's own emitted `out`
   facts — the in-tick overlay. The store must reproduce that exactly.

So the past-instant read is not a corner case served by `rebuild_upto`. It is
**on the hot path, inside the most expensive fold**, and decision 0237's
checkpoint design (a past-position read is served from the last reset at or
before it) is load-bearing rather than decorative.

## 2. The architecture

### 2.1 The store

A `ResidentFolds` value (name is the plan's) owned by `Session` beside
`mesh_memo` and `home_nav_cache`, and equally ownable by the two benches that
drive `step_with_occupancy` directly with caller-owned caches
(`agent_scaling.rs`, `session_length_scaling.rs`). Keyed by `EntityId`
(`BTreeMap`; `Ord` derives). Never serialized: no `Serialize`, not reachable
from `World`, by construction as the Tailrace did for `Folded` (its §11).

A **tenant** is a `LedgerFold` impl plus the key it lives under. The store
holds one `Folded<S>` per `(entity, tenant)` and knows nothing about what
`S` means. Nathan's standing direction applies (Penstock §6.6, 2026-08-23):
keep it general. A future tenant — hysteresis state, 7b's intention anchor,
a §5.7 threshold crossing — is a new `S`, not a new store.

### 2.2 The currency invariant, and where the seam is

**A reader never observes a fold behind the ledger it is given.**

The seam is at READ, not at COMMIT. `Ledger::commit` gained no hook in the
Tailrace, on purpose, and gains none here. Every read of a tenant first calls
`Folded::advance_to(ledger)`, which is idempotent in position: a second
evaluation of the same walk, a re-read within a turn, a snapshot after a
`wait` — each costs O(new facts since last read) and absorbs nothing twice.
That is what makes finding 1.3 above a non-issue instead of a hook design.

Two reader shapes exist and both must satisfy the invariant:

- `&mut` readers (the tick, `wait`): advance in place.
- `&self` readers (`Session::snapshot`, which today constructs *throwaway*
  `PrimaryAfraidMemo`, `RoomMeshMemo` and `HomeNavCache` because it cannot
  reach the session-lived ones): **a throwaway rebuild is not acceptable
  here** — it would keep an O(history) term on the per-turn path and the
  snapshot runs once per committed turn for every creature. The sanctioned
  mechanism is interior mutability (`RefCell`) on the store, advanced on
  read. `clippy.toml` bans `HashMap`/`HashSet` and wall-clock time; it does
  not ban `RefCell`, and a `RefCell` holds nothing the ledger does not
  already determine.

### 2.3 The in-tick overlay

`decide_step` folds `frozen` + `out`. With the store, the tick reads a
tenant's state at `frozen`'s end (advanced there — nothing commits during a
bulk-synchronous tick, so `frozen` *is* the ledger) and passes this tick's
`out` facts for the agent to the read as an **overlay slice**, exactly the
`(day, room)` list `decide_step` builds from `out` today. The overlay is a
read-side argument, never absorbed: a tenant's state is keyed by entity
inside one fold, so cloning it per creature per tick would be O(agents) per
creature — the O(agents²) shape this campaign is removing. **No kernel edit.**
(An earlier draft of this section said `Folded<S>` must become `Clone`; the
plan's G4 self-review found the overlay-as-argument shape needs no clone and
the ledger records the change, #5.)

### 2.4 The tenants

| tenant | replaces | state | bounded by | past-instant read |
|---|---|---|---|---|
| **Trail** | `agent_sightings`'s per-call rebuild; `build_emitter_scan`'s per-member timeline | append-only `Vec<(WorldTime, Facet)>` of the entity's `agent-at`, in the order `agent_sightings` sorts (§3 rule 4) | O(trail) in size, **O(1) absorb, never rebuilt** | binary search by day |
| **Sustenance** (×2: thirst, hunger) | `drive_at` / `hunger_at` / `decide_step`'s integral; `last_fact_day_at_or_before` for `DRANK`/`EATEN` | accumulated integral `A`, last sighting `(d_n, p_n)`, the segment-start position, last reset; plus the **checkpoint list** — the state at each reset | the interval since the last reset; checkpoints O(resets) | resume from the last checkpoint ≤ t, advance over the Trail range `(reset, t]` (decision 0237, made concrete) |
| **KnownWater** | `believed_water`'s set | `BTreeSet<Facet>` of water rooms visited | water rooms visited | **none on a hot path** (§3 rule 6); if one is found, the set becomes a first-visit-day map and filters by ≤ t |
| **LatestVisit** | `hazard_memory_memo`'s `latest` | `BTreeMap<Facet, WorldTime>` — latest visit day per room | rooms visited | **none on a hot path** (§3 rule 6); if one is found, a per-room visit list |
| **Alarm** | `build_emitter_scan`'s `alarm_source_rooms` and `ever` | `BTreeSet<Facet>` of halo rooms; `bool` per member | frightening rooms visited × halo | none |

Unchanged: `fatigue_at` (not history-proportional), `RESTED` lookups, the
`afraid` memo's per-tick lifetime (it memoises within a tick; the past-day
reads *inside* it become bounded through Sustenance's checkpoints, which is
where the two orders of magnitude go).

**Trail is a resident permutation index, not the cached hub.** The Tailrace
said the hub `agent_sightings` must be deleted, not cached, because its
output is O(history) *however cheaply kept*. That stands for the hub as an
object rebuilt per call. What Trail is instead is the `UNI-28` shape — a
sorted view of one entity's `agent-at` facts, absorbed once each, searched by
day — and it is needed by three tenants at once (Sustenance's range reads,
Alarm's emitter positions, catch-up's replay). Its memory is a strict subset
of the ledger's own. The hub is still deleted: nothing rebuilds a timeline
per call any more.

## 3. Decision rules, not predictions

Each of these is a branch table the implementer executes, with the branch
recorded in the ledger. None is a claim about what will be found.

1. **Trap 5 (Tailrace §2): today's reset lookup is unfiltered.** `drive_at`
   and `hunger_at` take `last_drank` as the max over **every** reset in the
   ledger, with no `≤ t`; `decide_step` and `catch_up` use
   `last_fact_day_at_or_before`, which *is* filtered. A fold at position
   `p(t)` sees only resets ≤ t. Rule: write a witness that asks, on the real
   seed-42 session and the two benches, whether any `drive_at`/`hunger_at`
   call ever runs with `t` earlier than a reset of the same entity. *No such
   call* → the fold's filtered semantics are byte-equivalent on every reached
   path; document it on the tenant and move on. *Such a call exists* → STOP;
   it is a live semantic choice (0237's rule vs today's code) and goes in
   the ledger as a `Q` before any tenant lands.
2. **Trail order.** `agent_sightings` sorts by `(day, Facet)`; `build_emitter_scan`
   sorts by day, stable. Rule: a witness asks whether an entity's `agent-at`
   facts ever commit out of day order. *Never* → Trail appends and both sorts
   are the identity; the FOLD ≡ SCAN test still pins the `(day, Facet)`
   tie-break against `agent_sightings`'s own output. *Sometimes* → Trail
   inserts at the sorted position (O(log h) search plus a memmove — still
   never a rebuild) and the divergence between the two existing sorts
   becomes a `Q`.
3. **The `&self` reader.** Rule: if the plan can thread `&mut` to
   `Session::snapshot`'s per-creature `affect_of_memo_occupied` call, do that;
   otherwise `RefCell` per §2.2. A throwaway rebuild is refused.
4. **Sustenance ≡ `integrate_thirst`.** The accumulator must reproduce the
   integral's four traps (Tailrace §2, rules 1–3 and 5): clamp at read only;
   temperature sampled at the segment's START; the reset's governing
   position is the latest sighting ≤ the reset, which is *earlier* than the
   reset; and `bounds.dedup()`'s effect on coincident instants. The oracle is
   `integrate_thirst` itself over `agent_sightings` — the scan half — and the
   fold half reaches its state through `absorb_at`, never through
   `advance_to` (the Tailrace's vacuous-oracle finding, §11 item 1).
5. **Past-day affect.** The `afraid` path's `affect_of(m, day)` at a past
   `day` must read Sustenance by resume-and-advance. Rule: mutate the resume
   to start from the *wrong* checkpoint and confirm a hazard test reddens;
   a green under that mutation means the test does not reach the path, and
   the campaign has not proven what it claims.
6. **Set-shaped past reads.** KnownWater and LatestVisit can answer "at a
   past t" only by filtering or by `rebuild_upto`. Rule: a witness lists
   every call of `believed_water`, `shared_believed_water` and
   `hazard_memory_memo` with `t` strictly before the ledger's last day, on
   the real session and both benches. *Only test-suite calls* → the tenants
   are plain folds and `rebuild_upto` serves tests. *A production call* →
   that call is the finding; it goes to the ledger and the tenant grows a
   first-visit day (KnownWater) or a per-room visit list (LatestVisit)
   before it lands — never a silent rebuild.

## 4. Preregistration

Inherited **verbatim** from the Tailrace §5 (frozen 2026-08-24, decision
0016) — re-freezing a criterion after seeing the substrate is what
metric-chasing looks like:

- **H2.** On `session_length_scaling.rs`, same seed, 50 agents, 200 ticks,
  ≥3 runs, counting only runs with r² ≥ 0.5: `drive_at`'s median elasticity
  falls below **0.20** (from 0.86–1.24); `C` becomes identifiable and
  positive; the whole-tick history share falls below **20%** (from 70–80%).
  Any one failing while the others pass is a finding, not an average.
- **H3.** No world-state artifact moves. Established by construction (§5),
  not by the drift check.
- **The fix's own falsifier.** If `k` falls but `C` rises by more than the
  `k` saving at realistic session lengths, the store is a pessimisation for
  short sessions. Stated, not buried.

Added here, frozen before any tenant exists:

- **H4.** `hazard_memory_memo`'s median elasticity on the same runs falls
  below **0.20** (from 1.06–1.21 on runs 2–4), and its final-band cost falls
  by at least **10×** (from 73–97 ms/call). The 10× is the weaker of the two
  claims and is stated because elasticity alone cannot see a fold that
  became flat at the same enormous level.
- **Measured on a quiet box**, with all three load averages recorded per run
  and any run above load 10 set aside, not averaged in — the Tailrace's own
  run-1 lesson (elasticity 0.18 at load 14.8–16.3 was noise that already
  "met" H2 before the fix existed).

Not predicted: any share of the Penstock §6.4 level (5.75–8.76 ms per
agent-tick) that the history term explains; that is a level claim on a
different bench.

## 5. Determinism contracts (lead the G3 flagged section)

- **Byte-identical.** No new fact, predicate, stream label or epoch. The
  commit site and `Ledger` are untouched. Every committed artifact is
  byte-identical, and every session's ledger at the end of any script is
  byte-identical to today's.
- **The drift check cannot see this**, and is not cited as proof: no
  committed artifact carries a ticked ledger (the session fixtures are
  turn-0 and chamber-band views). Proof is (a) the construction — the diff
  touches no producer of a committed fact — and (b) a **ledger-hash witness**:
  the seed-42 `possess` script's final ledger, hashed before and after, in
  a test that is *known to redden* when a fold's value is perturbed (positive
  control first, per `an-empty-diff-needs-a-positive-control`).
- **Discarding the store is unobservable.** The chaos schedules from
  `kernel/tests/suite/fold.rs` (every position; every third position — both,
  since the first gives no signal on `absorb`'s purity) apply to every tenant,
  and to the store as a whole: drop it between any two turns and the session
  continues identically.
- **FOLD ≡ SCAN per tenant**, oracle reached through `absorb_at`, compared
  against the *existing* scan function (`agent_sightings` + `integrate_thirst`,
  `believed_water`, the `latest` map), not against `Folded::rebuild`.
- **No serialized surface.** The store has no `Serialize`; nothing in `World`
  reaches it; a save written mid-session contains nothing from it.
- **The overlay equivalence.** For the live tick, `state(frozen) ⊕ out` equals
  today's `frozen + out` scan, pinned by a test that mutates the overlay
  order.

## 6. The stage carve

Strangler-fig, each stage byte-identical on its own and gated on the last.

| # | stage | delivers | gate |
|---|---|---|---|
| 1 | **The store and the keystone** | `Folded: Clone`; the store type; **Trail** and **Sustenance** (thirst + hunger) as its first tenants; `agent_sightings` deleted; `drive_at`/`hunger_at`/`decide_step`/`last_fact_day_at_or_before` read the store; rules 1, 2, 4 executed and ledgered; the ledger-hash witness with its positive control | — |
| 2 | **Belief and hazard** | **KnownWater**, **LatestVisit**, **Alarm**; `build_emitter_scan`'s per-call timelines replaced by Trail; the `afraid` past-day path reads Sustenance by resume-and-advance; rules 3, 5, 6 executed and ledgered | stage 1 green |
| 3 | **The readout** | `session_length_scaling.rs` re-run per §4 on a quiet box; `agent_scaling.rs` re-run for the level, reported not predicted; H2/H3/H4 stated met or not | stages 1–2 |
| 4 | **Close** | decisions 0536–0539 written; registry rows moved (§9); chronicle; retrospective; the Penstock §6 table and the Tailrace §6 table annotated | stage 3 |

Stage 2 may not be skipped on the strength of stage 1's number: H4 lives in
stage 2 and it is the larger quarry.

## 7. In / out

**In:** the store; five tenants; the read sites named in §2.4; the
instruments re-run; the witnesses in §3. No kernel edit.
**Out:** 7b and 7c (0238); hysteresis (`PSY-drive-arbitration-limit-cycle`);
`fatigue_at`; the commit site; `Ledger`; the `afraid` memo's lifetime; the
O(agents²) roster term as such — Trail removes one of its two factors and the
readout says how much, but no separate roster-scaling claim is made.

## 8. Decisions this campaign will need (block 0536–0545)

1. **0536 — The resident fold store is session-owned and never serialized.**
   The first instance of the layer `UNI-ecs-is-the-adaptive-cache` names:
   holds only what the ledger determines, is discardable at any instant,
   lives beside the session's other memos. Refines the reading of 0069
   (snapshot-for-speed is separate from commit-for-truth), amends nothing.
2. **0537 — A reader never observes a fold behind its ledger; the seam is
   at read.** Advance-on-read, idempotent in position; `Ledger::commit`
   keeps its no-hook contract; `&self` readers use interior mutability and
   never a throwaway rebuild.
3. **0538 — The trail is a resident index, not a cached hub.** Distinguishes
   the append-only, absorbed-once, binary-searchable Trail from the
   per-call timeline the Tailrace ruled must be deleted; the hub is still
   deleted.
4. **0539 — A past-instant read resumes from the reset checkpoint and
   advances over the trail range.** 0237 made concrete for the tenant that
   needs it on a hot path (`emitter_arousal` → `affect_of` at a past day).
5. **0540, only if rule 1 or 2 finds a live divergence** — the semantics of an
   unfiltered reset lookup, or of two sorts that disagree. Otherwise unused;
   gaps in a block cost nothing.

## 9. Frontier bookkeeping

- `UNI-ecs-is-the-adaptive-cache`: **Where** gains this spec. Status stays
  `raw` — moving a status is Nathan's call (G3 flag).
- `PSY-drive-arbitration-limit-cycle`: **Where** gains "hysteresis state is a
  tenant of The Pawl's resident store" so the next campaign knows where it
  goes.
- `TOOL-penstock-7a-tenant-migration`: → `shipped` at close, Where → this
  spec and chronicle.
- `UNI-intention-is-structured` (7b): unchanged; named as next.
- Followups register: a §5.7 threshold-crossing tenant; the Penstock stage 6
  row-width question, now that the store makes fact size the remaining
  memory term; `agent_scaling.rs`'s whole-memo clone per tick (Penstock §6.7),
  still a harness property and still unmeasured.

## 10. Operational notes for the implementer

- `windows/vessel/src/liveness.rs` is ~16,200 lines and `session.rs` ~16,800.
  **Line numbers rot within a day; function names are the handle** — every
  citation in this spec is by name.
- The one live branch touching vessel at drafting time is
  `campaign/the-wicket` (per-species sleep-debt rate; touches
  `windows/vessel/tests/suite/thing.rs`). Absorb main at every stage boundary
  through `make sluice-stage`.
- New `pub` items need `type-audit:` tags and regenerate
  `docs/audits/type-audit-report.md` in the same commit. Every edit is in
  `windows/vessel`, so `gate-commit` stays a windows-layer cost.
- Both benches run `--release` only, and their module docs explain why the
  two instruments are complements (internal vs ecological validity). Neither
  may be deleted.
- `PrimaryAfraidMemo`'s "one per tick" doc is load-bearing for the
  attribution probe: constructing it once across `FOLD_REPS` would measure a
  hit rate, not a fold.
- `make prewarm` was started in this worktree at G1; the worktree was
  created cold (no recyclable pool member was clean).

## 11. What shipped, measured

Stage 3, the readout. Every number below is a run on the development Mac,
`--release`, taken 2026-09-02 between 02:17 and 05:01 local. **§4 is not
edited**; this section reports against it.

### 11.0 The quiet-box rule, and how it was applied

§4 requires all three load averages recorded per run and any run above load
10 set aside. The three averages are recorded immediately BEFORE and
immediately AFTER each run. A run is set aside if the 1-minute average is
above 10 at either end — the 1-minute figure is the one that describes the
box *during* a six-minute run; the 15-minute figure was still carrying an
unrelated neighbour's build for most of the session and would have excluded
everything.

The box was not quiet on demand. A sibling campaign worktree
(`campaign/the-roll`) ran release builds intermittently throughout, and
**five of the eight `session_length_scaling` runs taken were set aside for
load** — one on the campaign branch (load 23.83 after) and four on the
control. Every run taken is listed below, including the discarded ones, with
its loads.

### 11.1 A same-box control was added, and it is not in §4

§4 compares against the Tailrace's own §4 tables, taken 2026-08-24 on a box
at load average 7.0–16.3. Those tables report `k`, `r²`, elasticity and the
share, but **no per-run `C`** — §4 of the Tailrace says so explicitly — and
the falsifier is a claim about `C`. Reconstructing `C` from the printed
share is arithmetically possible and was tried; it is not trustworthy,
because the `ms/tick*` column is normalised to *each run's own* band 1, so
two runs taken a week apart on differently-loaded boxes are not calibrated
to one another.

So the pre-campaign side was **re-measured on the same box, in the same
session, interleaved with the post-campaign runs**: a detached worktree at
the campaign's merge base `18f63ebfa`, whose
`windows/vessel/examples/session_length_scaling.rs` carries the identical
`AGENTS`/`TICKS`/`FOLD_REPS`/`BAND` constants and no resident store
(`grep -n 'resident::\|OwnedFolds'` returns nothing there). This is the
paired control §4 could not have specified, because the code it controls
against did not exist when §4 was frozen.

**It is a control, not a re-freezing of a criterion.** The thresholds below
are §4's, unchanged, and every verdict is stated against §4's own
pre-campaign numbers as well as against the control.

**The deterministic columns are byte-identical across the control and the
campaign branch**, on both benches, which is what makes the pairing legitimate:
`session_length_scaling` band 1 reads `facts 2800 · searches 1825 · folded/a
35.5 · ledger_len 24485` and band 10 reads `1587 · 650 · 151.9 · 38711` on
every run of both trees; `agent_scaling`'s largest rung reads `facts 9769 ·
searches 6825` on both. The workload did not move. That is also the readout's
own small witness for H3.

### 11.2 `session_length_scaling` — the decisive column (`drive_at`)

50 agents, 200 ticks, bands of 20, seed 42, probe agent fixed at the
max-`agent-at` roster member, history 101 → 260 (2.57×) across the warm
bands.

**Campaign branch (`6538428a3`).**

| run | load before (1/5/15) | load after | k (µs/call/fact) | r² | elasticity | C (µs/call) | final-band µs/call | monotone |
|---|---|---|---|---|---|---|---|---|
| 1 | 3.78 / 6.92 / 16.08 | 2.24 / 4.33 / 12.30 | 2.27532 | 0.999 | **1.00** | −0.074 | 597.42 | 8/8 |
| 2 — **SET ASIDE** | 2.09 / 3.99 / 11.76 | **23.83** / 20.56 / 16.90 | 3.88036 | 0.829 | 1.09 | −47.792 | 946.05 | 6/8 |
| 3 | 3.41 / 9.15 / 13.35 | 5.41 / 8.21 / 11.72 | 2.30474 | 0.997 | **1.03** | −10.259 | 596.53 | 8/8 |
| 4 | 5.05 / 8.09 / 11.65 | 5.68 / 6.43 / 9.75 | 2.34758 | 0.991 | **1.01** | −3.050 | 594.27 | 8/8 |

**Control at `18f63ebfa` (pre-campaign, same box, same session).**

| run | load before | load after | k | r² | elasticity | C | final-band µs/call | monotone |
|---|---|---|---|---|---|---|---|---|
| B1 — **SET ASIDE** | 3.67 / 5.32 / 8.87 | **11.19** / 7.90 / 8.53 | 4.17622 | 0.690 | 1.75 | −265.520 | 1159.12 | 8/8 |
| B2 — **SET ASIDE** | **14.40** / 8.95 / 8.90 | **29.50** / 23.64 / 15.32 | 1.77930 | 0.354 | 0.68 | 133.430 | 625.61 | 5/8 |
| B3 — **SET ASIDE** | **29.50** / 23.64 / 15.32 | 1.88 / 8.89 / 11.00 | 1.90456 | 0.760 | 0.74 | 105.176 | 613.27 | 7/8 |
| B4 — **SET ASIDE** | 1.95 / 4.82 / 9.05 | **28.64** / 11.48 / 9.90 | 3.43276 | 0.671 | 1.42 | −154.550 | 614.71 | 7/8 |
| B5 — **SET ASIDE** | **28.64** / 11.48 / 9.90 | 2.63 / 6.19 / 7.94 | 2.58837 | 0.847 | 1.14 | −47.631 | 685.99 | 7/8 |
| B6 | 2.63 / 6.19 / 7.94 | 3.03 / 4.04 / 6.32 | 2.32428 | 0.998 | **1.01** | −4.204 | 607.27 | 8/8 |
| B7 | 2.74 / 3.89 / 6.20 | 6.93 / 5.76 / 6.15 | 2.33624 | 0.999 | **1.01** | −2.255 | 611.54 | 8/8 |
| B8 | 6.93 / 5.76 / 6.15 | 4.58 / 5.74 / 5.97 | 2.30746 | 0.995 | **0.99** | +1.898 | 593.30 | 8/8 |

Three valid runs a side, every one clearing the `r² ≥ 0.5` floor.

**Medians: elasticity 1.01 before, 1.01 after. `k` 2.324 before, 2.305
after. Final-band cost 607.27 µs/call before, 596.53 µs/call after (1.8%).**
`drive_at`'s cost on this bench did not move.

Band by band, the two are the same curve (`fold us*`, campaign run 1 against
control B6): 233.61/233.26 · 269.98/276.08 · 327.56/326.56 · 366.29/368.27 ·
407.20/419.62 · 452.68/449.55 · 499.23/509.87 · 545.03/550.97 ·
597.42/607.27.

### 11.3 `session_length_scaling` — the whole tick

| | k (ms/tick/fact) | r² | C (ms/tick) | history share at band 10 | band-2 ms/tick* | band-10 ms/tick* |
|---|---|---|---|---|---|---|
| campaign run 1 | 2.86447 | 0.994 | 264.299 | 62.2% | 396.05 | 686.08 |
| campaign run 3 | 2.92731 | 0.909 | 267.301 | 62.4% | 386.17 | 699.01 |
| campaign run 4 | 2.85724 | 0.915 | 285.308 | 60.3% | 403.96 | 685.57 |
| control B6 | 3.92091 | 0.995 | 219.016 | 73.1% | 405.24 | 817.04 |
| control B7 | 3.97251 | 0.999 | 212.560 | 73.9% | 400.12 | 813.16 |
| control B8 | 4.44790 | 0.891 | 204.854 | 76.7% | 411.17 | 874.02 |

**Medians: `k` 3.973 → 2.864 (−28%). `C` 212.560 → 267.301 (+54.7 ms/tick).
Share 73.9% → 62.4% (−11.5 points). Band-10 ms/tick 817.04 → 686.08
(−16.0%). Band-2 ms/tick 405.24 → 396.05 (−2.3%).**

### 11.4 `session_length_scaling` — attribution, final-band µs/call

Medians of the three valid runs a side.

| fold | control (pre) | campaign (post) | ratio | post elasticity (median) | post r² |
|---|---|---|---|---|---|
| `drive_at` | 607.27 | 596.53 | 1.02× | 1.01 | 0.991–0.999 |
| `hunger_at` | 611.42 | 596.03 | 1.03× | 1.04 | 0.541–0.917 |
| `fatigue_at` | 0.13 | 0.15 | 0.87× | 0.00–0.06 | 0.002–0.244 |
| `believed_water` | 8579.24 | 8752.98 | 0.98× | 1.04 | 0.403–0.852 |
| `shared_believed_water` | 8700.51 | 8926.99 | 0.97× | 1.02 | 0.254–0.921 |
| **`hazard_memory_memo`** | **143668.28** | **94362.17** | **1.52×** | **0.92** | 0.677–0.985 |

`hazard_memory_memo`'s own fits: elasticity 1.20/1.28/1.19 (r² 0.997/0.981/
0.986) before, 0.92/0.79/1.03 (r² 0.985/0.677/0.931) after.

### 11.5 The verdicts against §4

| criterion | threshold | measured | verdict |
|---|---|---|---|
| **H2 (a)** `drive_at` median elasticity, runs with r² ≥ 0.5 | < 0.20, from 0.86–1.24 | **1.01** (1.00 / 1.03 / 1.01; control 1.01) | **NOT MET** |
| **H2 (b)** `C` identifiable and positive on the decisive column | positive | **−0.074 / −10.259 / −3.050** — negative on all three | **NOT MET** |
| **H2 (c)** whole-tick history share | < 20%, from 70–80% | **62.4%** median (73.9% on the same-box control) | **NOT MET** |
| **H3** no world-state artifact moves | — | not this task's instrument; established by construction (§5) and by the ledger-hash witnesses of Tasks 1–5. The readout adds one small witness: both benches' deterministic columns are byte-identical across the control and the campaign branch (§11.1) | **held, not re-measured here** |
| **H4 (a)** `hazard_memory_memo` median elasticity | < 0.20, from 1.06–1.21 | **0.92** (0.92 / 0.79 / 1.03; control 1.20) | **NOT MET** |
| **H4 (b)** `hazard_memory_memo` final-band cost | ≥ 10× down, from 73–97 ms/call | **1.52×** down (143.7 → 94.4 ms/call, same box); against §4's own 73–97 ms/call figure, **no reduction at all** | **NOT MET** |

Five of six preregistered criteria NOT MET, and none of them is averaged
away or rescued. §11.7 is why, and it is a finding rather than a failure.

### 11.6 The level, and the falsifier

**The level — reported, not predicted.** `agent_scaling`, ms/tick at 200
agents over 20 ticks, against Penstock §6.3's pre-campaign 1712–1752.

| run | tree | load before | load after | 10 | 50 | 100 | **200** |
|---|---|---|---|---|---|---|---|
| A1 — **SET ASIDE** | campaign | 3.15 / 5.12 / 5.72 | **10.72** / 7.93 / 6.80 | 80.853 | 625.514 | 817.741 | 1680.564 |
| A2 | campaign | 9.94 / 7.82 / 6.76 | 4.02 / 6.48 / 6.33 | 60.482 | 428.131 | 712.752 | 1682.660 |
| A3 — **SET ASIDE** | campaign | 4.02 / 6.48 / 6.33 | **54.42** / 19.81 / 11.30 | 60.776 | 464.723 | 1011.751 | 2461.183 |
| BA1 | control | 1.95 / 5.72 / 11.38 | 2.00 / 5.04 / 10.74 | 59.336 | 426.823 | 709.138 | **1664.025** |
| BA2 | control | 2.00 / 5.04 / 10.74 | 1.77 / 4.35 / 10.06 | 59.187 | 425.021 | 703.091 | **1663.496** |
| A4 | campaign | 1.81 / 4.28 / 9.97 | 2.22 / 3.88 / 9.40 | 58.739 | 421.072 | 698.870 | **1667.657** |
| A5 | campaign | 2.22 / 3.88 / 9.40 | 3.17 / 4.02 / 9.05 | 60.712 | 449.341 | 709.662 | **1664.894** |

BA1/BA2/A4/A5 are four runs inside one tight quiet window (04:56–05:00),
alternating tree, and they are the comparison to read. **At 200 agents:
1663.5–1664.0 ms/tick before, 1664.9–1667.7 ms/tick after — a difference of
+0.1% to +0.25%, which is noise.** The level did not move. A2, valid but
taken 45 minutes earlier at a busier moment, reads 1682.660 and is 1.1%
above the window; the spread between windows is larger than the spread
between trees, which is the usual reason this bench is read paired.
Both sit ~3–5% below Penstock §6.3's 1712–1752, a box-and-date difference,
not a campaign effect. The fitted log-log slope is 1.10 on both trees.

**The falsifier — `C` DID rise, and it does not falsify the fix at either
named session length.** From the whole-tick affine fits (§11.3 medians):

```
pre:  ms/tick = 212.560 + 3.973 h
post: ms/tick = 267.301 + 2.864 h
```

`C` rose by **54.741 ms/tick**; `k` fell by **1.109 ms/tick per fact**. The
two lines cross at `h = 54.741 / 1.109 = 49.4` facts of mean per-agent
history — between band 2 (h = 47.9, tick 39) and band 3 (h = 61.1, tick 59),
so **around tick 45**. Evaluating each run's own fit and taking medians:

| session length | h | pre | post | verdict |
|---|---|---|---|---|
| 50 ticks | ≈ 55 | 434.67 ms/tick | 428.30 ms/tick | post 1.5% faster — a wash |
| 200 ticks | 151.9 | 815.98 ms/tick | 711.96 ms/tick | post **12.7% faster** |

**So: NOT a pessimisation at 50 ticks, and a clear gain at 200.** Three things
must be said with it rather than after it.

1. **The fitted `C` is an extrapolation to `h = 0`, and `h = 0` is outside
   the data** (the sampled range is 47.9–151.9). §4's own text and the
   instrument's `report_affine` both say the intercept is not trustworthy at
   this depth, which is why H2 (b) is written as "becomes identifiable" — it
   did not.
2. **At the shallowest band actually measured the two are equal**: band-2
   `ms/tick*` is 405.24 before and 396.05 after (post 2.3% faster). The
   crossover the fit predicts sits just below the data and is not observed
   inside it.
3. **`agent_scaling` measures the short-session regime directly** — 20 ticks,
   h ≈ 35 — and finds the two indistinguishable (+0.1% to +0.25%). That is a
   measurement, not an extrapolation, and it is the strongest evidence
   against the pessimisation reading.

**Verdict on the falsifier: the store is not a pessimisation at any session
length this campaign can measure.** The intercept rise the fit reports is
real as a fit parameter and unobserved as a cost.

### 11.7 Why five criteria failed, and what the campaign actually bought

The ecological bench says `drive_at` did not move. The synthetic bench says
its **order changed**. Both are right, and reconciling them is this readout's
real result.

`windows/vessel/examples/fold_depth_sweep.rs`, one run each tree, same quiet
window (campaign: loads 3.17/3.98/8.95 → 3.08/3.95/8.91; control:
2.72/3.80/8.75 → 2.46/3.71/8.65). µs/call, median of 6 passes:

| depth | PERIODIC pre | PERIODIC post | ratio | SINGLE-RESET pre | SINGLE-RESET post | ratio |
|---|---|---|---|---|---|---|
| 10 | 0.747 | 0.131 | 5.7× | 0.749 | 0.125 | 6.0× |
| 32 | 1.249 | 0.200 | 6.2× | 1.481 | 0.342 | 4.3× |
| 100 | 2.492 | 0.220 | 11.3× | 5.141 | 1.059 | 4.9× |
| 320 | 6.473 | 0.503 | 12.9× | 27.593 | 3.832 | 7.2× |
| 1 000 | 19.867 | 1.405 | 14.1× | 194.648 | 13.331 | 14.6× |
| 3 200 | 68.380 | 4.662 | 14.7× | 1 734.082 | 51.738 | 33.5× |
| 10 000 | 210.609 | 15.729 | 13.4× | **19 215.004** | **194.490** | **98.8×** |

| | pre | post |
|---|---|---|
| PERIODIC `k` (µs/call/fact) | 0.02106 (r² 1.000) | 0.00156 (r² 0.999) |
| PERIODIC raw elasticity, top third | 0.987 | 1.067 |
| SINGLE-RESET `k` | 1.90398 (r² 0.949) | 0.01947 (r² 0.997) |
| **SINGLE-RESET raw elasticity, top third** | **2.111** | **1.162** |
| SINGLE-RESET raw elasticity, top half | 1.994 | 1.164 |

**In the single-reset regime the campaign changed the order: elasticity
1.99–2.11 (quadratic, `O(S·H)`) became 1.16 (linear), and cost at depth
10 000 fell 98.8×.** That is precisely what §0 set out to do, and it is not
visible on `session_length_scaling` for a reason that is arithmetic, not
interpretive.

**The reconciliation.** At depth 260 — the probe agent's history at band 10 —
the synthetic single-reset sweep interpolates (log-log, off the 100 and 320
readings) to **20.44 µs/call before and 3.05 µs/call after: a predicted
saving of 17.4 µs**. The ecological bench at that same history measures
607.27 → 596.53 µs/call, a saving of **10.7 µs**. **The two instruments do
not disagree: the predicted saving is smaller than this bench's own
run-to-run spread on that very column** (final-band `drive_at` reads
593.30–611.54 µs/call across the three control runs, a spread of 18.2 µs).
An effect the size of the noise is not resolvable, and §4's criterion asked
this bench to resolve it. It is ~2–3% of a ~600 µs/call figure — and the
*other* ~99%, whatever it is, is itself history-proportional, so the
elasticity stays at 1.0.

**What that ~99% is, is not established here, and the leading candidate is
named rather than asserted.** The ecological `drive_at` costs ~597 µs at
h = 260 where the synthetic one costs ~3 µs — a ~200× gap between the same
function on the same history depth. The difference between the two call sites
is the terrain: the sweep passes a trivial `Terrain`, the session passes a
real `LocaleTerrain` backed by `RoomMeshMemo`, and `sustenance_at` samples
temperature once per segment boundary. That per-segment sample is `O(segments
since the reset)` — the *same* order as the trail scan the store removed, on
the same segments, which is exactly the signature the elasticity shows. **No
probe isolates it and this readout does not claim it; it is the followup.**

**Three further facts that make the H2 result a property of the instrument
rather than a surprise.**

1. **The probe agent never drinks.** `session_length_scaling` chooses the
   roster member with the MOST `agent-at` postings, and the bench reports of
   it: "the probe agent committed ZERO `drank` facts across 200 ticks",
   with 21 of 50 agents (42.0%) the same. Its `S` equals its `H` for the
   whole run. So H2's decisive column measures the single-reset regime — the
   worst case for a fix whose saving is `O(S)` in place of `O(H)` — by
   construction of the probe's selection, not by chance. The instrument was
   frozen before the fix existed and this is not a criticism of it; it is a
   fact about what its number can mean.
2. **`drive_at` is `O(S)`, and the campaign's own deterministic witnesses
   said so before this stage.** The ledger records segments-integrated ratios
   of **0.889** (Task 3, busiest drinker, 1391 → 1238 segments per turn) and
   **0.683** (Task 5, the past-day shape, 120 → 82). Those are constant-factor
   savings of 11% and 32% on the *count of segments*, never a change of order
   on a single-reset agent — and the readout is consistent with them to
   within noise. Nothing about §11.2 contradicts what stages 1–2 measured.
3. **The 70–80% share in §4 was never `drive_at` alone.** It fell 73.9% →
   62.4% on the same-box control, and the fold that moved is
   `hazard_memory_memo` (1.52×), not `drive_at` (1.02×). The whole tick is
   16.0% faster at band 10. That is the campaign's ecological gain, and it is
   real, modest, and far from H2 (c)'s 20%.

### 11.8 What the readout hands forward

- **The instrument needed no change.** `probe_fold_us` and its five siblings
  thread the run's own `OwnedFolds` — the store production owns, at the scope
  production owns it — and their doc comments say why a fresh store per
  reading would time an `O(ledger)` advance instead. Verified by reading, not
  assumed. `probe_hazard_memory_memo_us` still builds a fresh
  `PrimaryAfraidMemo` per repetition, which is correct and matches §4's
  baseline construction. No commit touched an example.
- **The open question is the ~98%.** A seventh probe that times
  `sustenance_at` against a trivial terrain on the real session's trail would
  settle in one run whether the residual is the per-segment terrain sample.
  Until then, `drive_at`'s ecological cost has an unattributed dominant term
  and any future H2-shaped criterion written against this bench will fail the
  same way.
- **A criterion written against an ecological bench cannot see a change of
  order that the ecological range does not reach.** §4 froze H2 on the
  instrument that produces the share, because the share is what makes the
  campaign worth doing — and `session_length_scaling.rs`'s own module doc
  already says the sweep is the instrument for a shape finding and this one is
  the instrument for a share finding. The two were both run and they disagree
  only in appearance. A future preregistration for a change-of-order fix
  should state its shape criterion on `fold_depth_sweep.rs` and its share
  criterion here, rather than asking one bench for both.

## 12. The second readout, after the accumulator

Every number below is a fresh run on the development Mac, `--release`, taken
2026-09-02 between 07:13 and 08:20 local (11:13–12:20 UTC), paired and
interleaved against the same merge-base control §11.1 introduced. **§4 is not
edited and §11 is not rewritten**; both readouts are reported, and §11 stands
exactly as it was measured.

### 12.0 Exactly one post-unblinding change was made, and what kind of change it was

Between §11 and §12 the campaign made **one** change to production code:
Task 5b (`5124b9bba`, with the doc correction `f29768d56`), the `Sustenance`
prefix accumulator.

**It is mechanism-completing, not constant-tuning, and the distinction is
checkable rather than rhetorical.** §2.4 specified `Sustenance`'s state as
"accumulated integral `A`, last sighting, segment-start position, last reset,
plus the checkpoint list". Stage 1 shipped the checkpoint list alone and
**re-integrated from the last reset on every read**. For the 21 of 50 agents
on this bench that never drink — H2's probe agent among them — "the last
reset" is genesis, so that read was `O(history)` with a terrain sample per
segment, and it was the unattributed ~99% of `drive_at`'s ecological cost that
§11.7 named as its leading candidate and explicitly declined to claim. Task 5b
built the accumulator §2.4 described. **No threshold moved, no constant was
retuned, and no criterion was rewritten**: §4 is byte-for-byte what it was
when frozen on 2026-08-24, and every verdict below is stated against it and
against the same-box control.

The change was gated on determinism the way stages 1–2 were — bit-identity
preserved by keeping today's summation order — and the readout's own
byte-identity witness (§12.5) confirms the workload did not move.

**The honest cost of this ordering:** the second readout is not blind. §11's
verdicts were taken without knowing what the fix would be; §12's were taken
knowing exactly what had been repaired and where to look. That is why §11 is
reported in full rather than superseded, and why the falsifier and the level —
the two results that could have gone against the campaign — are stated below
before the ones that went for it.

### 12.1 The quiet-box rule, and how it was applied

§4's rule, applied exactly as §11.0 applied it: all three load averages
recorded immediately before and immediately after each run, and a run set
aside if the **1-minute** average is above 10 at either end. The box was
quieter than §11's session but not quiet on demand — it began the morning at
load 41 and was polled down in bounded stretches rather than measured through.
**One of eight `session_length_scaling` runs was set aside** — a control run
(load 17.81 after); none on the campaign branch — against §11's five of
eight. **One `agent_scaling` pair of three was set aside.** Every run taken is
listed below with its loads, including the discarded ones.

### 12.2 `session_length_scaling` — the decisive column (`drive_at`)

50 agents, 200 ticks, bands of 20, seed 42, probe agent fixed at the
max-`agent-at` roster member, history 101 → 260 (2.57×) across the warm bands.
The probe agent still commits **ZERO `drank` facts** across 200 ticks, and 21
of 50 agents (42.0%) are the same — the single-reset regime is still what this
column measures, unchanged from §11.

**Campaign branch (`57e30acf9`).**

| run | start (UTC) | load before (1/5/15) | load after | k (µs/call/fact) | r² | elasticity | C (µs/call) | final-band µs/call |
|---|---|---|---|---|---|---|---|---|
| P1 | 11:13:03 | 6.40 / 16.60 / 29.78 | 4.25 / 9.06 / 22.66 | 0.00081 | 0.500 | **0.05** | **+2.439** | 2.73 |
| P2 | 11:39:52 | 6.25 / 6.26 / 10.67 | 5.73 / 5.84 / 9.23 | 0.00066 | 0.337 | **0.04** | **+2.505** | 2.66 |
| P3 | 11:50:30 | 3.95 / 4.42 / 7.34 | 5.00 / 5.04 / 6.79 | 0.00114 | 0.846 | **0.07** | **+2.384** | 2.72 |
| P4 | 12:01:03 | 5.87 / 4.95 / 6.02 | 3.30 / 4.19 / 5.40 | 0.00132 | 0.790 | **0.09** | **+2.353** | 2.68 |

**Control at `18f63ebfa` (pre-campaign, same box, same session, interleaved).**

| run | start (UTC) | load before | load after | k | r² | elasticity | C | final-band µs/call |
|---|---|---|---|---|---|---|---|---|
| B1 — **SET ASIDE** | 11:17:43 | 4.25 / 9.06 / 22.66 | **17.81** / 12.72 / 18.93 | 2.86783 | 0.928 | 1.14 | −52.944 | 663.27 |
| B2 | 11:33:46 | 4.36 / 8.04 / 13.57 | 6.25 / 6.26 / 10.67 | 2.39830 | 0.997 | **1.00** | −0.293 | 628.92 |
| B3 | 11:44:38 | 5.43 / 5.78 / 9.19 | 3.95 / 4.42 / 7.34 | 2.37151 | 0.999 | **1.01** | −4.626 | 619.37 |
| B4 | 11:55:11 | 4.84 / 5.01 / 6.76 | 5.87 / 4.95 / 6.02 | 2.38529 | 0.999 | **1.02** | −5.669 | 620.34 |

**Medians: elasticity 1.01 before, 0.07 after. `C` −4.626 before (negative,
not identifiable), +2.411 after. Final-band cost 620.34 → 2.70 µs/call, a
factor of 229.8.** The column that did not move in §11 moved by more than two
orders of magnitude.

**The `r² ≥ 0.5` filter needs stating rather than applying silently.** §4
counts only runs whose `drive_at` fit clears r² 0.5, and post-fix two of the
four runs sit at or below it (0.500 and 0.337). **That is the criterion's own
success condition showing through its filter, not a noisy run**: the fit's r²
is low because the cost is now flat in history, so a line through history
explains almost none of the residual variance — the exact opposite of §11's
0.991–0.999, where history explained nearly all of it. The verdict does not
depend on how the boundary is read:

| reading of the filter | qualifying runs | median elasticity |
|---|---|---|
| as printed, r² ≥ 0.5 | P1, P3, P4 | **0.07** |
| strict, dropping P1 at the floor | P3, P4 | **0.08** |
| ignore the filter, all four | P1–P4 | **0.06** |

All three are below 0.20 by more than a factor of two.

### 12.3 `session_length_scaling` — the whole tick, and attribution

| | k (ms/tick/fact) | r² | C (ms/tick) | history share at band 10 | band-2 ms/tick* | band-10 ms/tick* |
|---|---|---|---|---|---|---|
| campaign P1 | 2.51717 | 0.998 | 253.147 | 60.2% | 368.85 | 631.80 |
| campaign P2 | 1.94967 | 0.665 | 336.030 | 46.8% | 387.47 | 622.92 |
| campaign P3 | 2.35998 | 0.998 | 264.281 | 57.6% | 371.77 | 617.54 |
| campaign P4 | 2.39394 | 0.995 | 258.443 | 58.4% | 363.95 | 612.48 |
| control B2 | 4.26061 | 0.993 | 222.286 | 74.4% | 429.98 | 876.59 |
| control B3 | 4.03390 | 0.999 | 216.239 | 73.9% | 403.66 | 823.35 |
| control B4 | 4.11788 | 1.000 | 212.370 | 74.6% | 409.86 | 839.21 |

**Medians: `k` 4.118 → 2.377 (−42%). `C` 216.239 → 261.362 (+45.1 ms/tick).
Share 74.4% → 58.0% (−16.4 points). Band-10 ms/tick 839.21 → 620.23 (−26.1%).
Band-2 ms/tick 409.86 → 370.31 (−9.6%).** For §11's comparable figures: `k`
−28%, share −11.5 points, band-10 −16.0%, band-2 −2.3%.

Final-band µs/call by fold, medians of the valid runs a side:

| fold | control (pre) | campaign (post) | ratio | pre elasticity | post elasticity |
|---|---|---|---|---|---|
| **`drive_at`** | **620.34** | **2.70** | **229.8×** | 1.01 | **0.06** |
| **`hunger_at`** | **618.58** | **2.51** | **246.5×** | 1.01 | **0.04** |
| `fatigue_at` | 0.14 | 0.14 | 1.00× | 0.00 | −0.04 |
| `believed_water` | 8 686.28 | 8 702.08 | 1.00× | 0.96 | 0.96 |
| `shared_believed_water` | 8 805.54 | 8 836.45 | 1.00× | 0.95 | 0.96 |
| `hazard_memory_memo` | 146 550.13 | 93 153.03 | 1.57× | 1.21 | 0.92 |

The two Sustenance reads collapsed together and nothing else did. That is the
signature of the accumulator and of nothing else: `believed_water` and
`shared_believed_water` are KnownWater tenants and are untouched at
1.00×, exactly as in §11.4.

### 12.4 The verdicts, against §4, side by side with §11

| criterion | §4 threshold | §11 measured | §11 verdict | §12 measured | §12 verdict |
|---|---|---|---|---|---|
| **H2 (a)** `drive_at` median elasticity, runs with r² ≥ 0.5 | < 0.20, from 0.86–1.24 | 1.01 (control 1.01) | **NOT MET** | **0.07** (0.06–0.08 under every reading of the filter; control 1.01) | **MET** |
| **H2 (b)** `C` identifiable and positive on the decisive column | positive | −0.074 / −10.259 / −3.050 | **NOT MET** | **+2.439 / +2.505 / +2.384 / +2.353**, positive on all four (control negative on all four) | **MET** |
| **H2 (c)** whole-tick history share | < 20%, from 70–80% | 62.4% (control 73.9%) | **NOT MET** | **58.0%** (control 74.4%) | **NOT MET** |
| **H3** no world-state artifact moves | — | held by construction (§5) + Tasks 1–5 ledger-hash witnesses; readout adds a byte-identity witness | **held, not re-measured** | unchanged; the byte-identity witness re-taken and still holding (§12.5) | **held, not re-measured** |
| **H4 (a)** `hazard_memory_memo` median elasticity | < 0.20, from 1.06–1.21 | 0.92 (control 1.20) | **NOT MET** | **0.92** (0.92 / 0.90 / 0.93 / 0.92; control 1.21) | **NOT MET** |
| **H4 (b)** `hazard_memory_memo` final-band cost | ≥ 10× down, from 73–97 ms/call | 1.52× (143.7 → 94.4 ms/call); vs §4's own 73–97, no reduction | **NOT MET** | **1.57×** (146.55 → 93.15 ms/call, same box); against §4's own 73–97 ms/call, **still no reduction at all** | **NOT MET** |

**Two of the five failed criteria are now met, three are not, and nothing was
averaged across a failure.** H2's two decisive clauses — the elasticity and
the identifiable positive floor — pass on all four runs. H2 (c), H4 (a) and
H4 (b) fail by margins that did not meaningfully move.

**Why the three that failed still fail, stated as attribution rather than
excuse.** The whole tick's remaining history term and the whole of H4 belong
to folds the accumulator does not touch. At the final band the ecological
per-call costs are `drive_at` 2.70 µs, `hunger_at` 2.51 µs,
`believed_water` 8 702 µs, `shared_believed_water` 8 836 µs and
`hazard_memory_memo` 93 153 µs. The Sustenance pair is now **0.005%** of that
sum; the hazard memo alone is **84.2%** of it. A fix to Sustenance cannot move
a share the hazard memo dominates, and H4 (b)'s 10× was always, in §4's own
words, "the weaker of the two claims". The hazard memo remains
history-proportional at elasticity 0.92 and is the campaign's clearest
handoff.

### 12.5 The byte-identity witness, re-taken

The deterministic columns are **identical across all fourteen runs and both
trees** — checked by hashing the extracted columns rather than by eye:

- `session_length_scaling`, all 8 runs (4 campaign, 4 control), the
  (facts, searches, folded/a, ledger_len) tuple for every one of the 10 bands:
  one md5 (`ee88c747…`) across all eight files. Band 1 reads
  `facts 2800 · searches 1825 · folded/a 35.5 · ledger_len 24485` and band 10
  reads `1587 · 650 · 151.9 · 38711`, matching §11.1 exactly. `drank/t` is
  `0.0000` at every band on every run.
- `agent_scaling`, all 6 runs (3 campaign, 3 control), the
  (facts/a/tick, search/a/tick, bytes/agent, total_bytes, facts, searches)
  tuple for all four rungs: one md5 (`a0126aeb…`). The largest rung reads
  `facts 9769 · searches 6825 · total_bytes 4413684` on both trees, matching
  §11.1.

The workload did not move across the accumulator, and it did not move across
the merge base either. That is what makes the pairing legitimate, and it is
the readout's own small witness for H3.

### 12.6 The falsifier, and the level

Both are stated before the campaign's favourable synthetic result, because
both are the results that could have gone against it.

**The falsifier — `C` still rises, and it still does not fire.** From the
whole-tick affine fits (§12.3 medians):

```
pre:  ms/tick = 216.239 + 4.118 h
post: ms/tick = 261.362 + 2.377 h
```

`C` rose by **45.123 ms/tick**; `k` fell by **1.741 ms/tick per fact**. The
lines cross at `h = 45.123 / 1.741 = 25.9` facts of mean per-agent history.
**That crossover is now BELOW the sampled range entirely** (the bands span
h = 47.9 to 151.9), where in §11 it sat at h ≈ 49.4, just inside the shallow
end. So there is no measured band in which the pre-campaign tree is faster.

| session length | h | pre | post | verdict |
|---|---|---|---|---|
| 50 ticks | ≈ 55 | 442.72 ms/tick | 392.09 ms/tick | post **11.4% faster** (§11: 1.5%) |
| 200 ticks | 151.9 | 841.74 ms/tick | 622.42 ms/tick | post **26.1% faster** (§11: 12.7%) |

The two independent checks §11.6 used both strengthen:

1. **At the shallowest band actually measured the post tree is now ahead, not
   level.** Band-2 `ms/tick*` is 409.86 before and 370.31 after — post 9.6%
   faster, where §11 measured a 2.3% wash.
2. **`agent_scaling` measures the short-session regime directly** — 20 ticks,
   h ≈ 35 — and no longer finds the two indistinguishable. The affine fits
   *predict* post 4.4% faster at h = 35; the bench *measures* 3.8–4.1% (below).
   A prediction from one instrument landing inside half a point of another
   instrument's measurement is the strongest single piece of evidence in this
   readout that the two lines describe the same thing.

**Verdict on the falsifier: it does not fire, and it fires less than it did
in §11.** The fitted intercept rise is real as a fit parameter and remains
unobserved as a cost at any session length either bench can reach.

**The level — reported, not predicted.** `agent_scaling`, ms/tick at 200
agents over 20 ticks, paired and interleaved, against Penstock §6.3's
pre-campaign 1712–1752.

| run | tree | start (UTC) | load before | load after | 10 | 50 | 100 | **200** |
|---|---|---|---|---|---|---|---|---|
| BA1 | control | 12:06:35 | 2.78 / 3.93 / 5.22 | 2.73 / 3.67 / 5.03 | 59.621 | 427.031 | 713.502 | **1678.101** |
| A1 | campaign | 12:07:40 | 2.73 / 3.67 / 5.03 | 3.36 / 3.71 / 4.95 | 56.637 | 414.841 | 687.975 | **1609.841** |
| BA2 — **SET ASIDE** | control | 12:08:43 | 3.36 / 3.71 / 4.95 | **12.12** / 7.08 / 6.16 | 102.902 | 639.998 | 964.546 | 1771.331 |
| A2 — **SET ASIDE** | campaign | 12:10:01 | **12.12** / 7.08 / 6.16 | 7.53 / 6.69 / 6.08 | 59.204 | 430.337 | 712.992 | 1656.294 |
| BA3 | control | 12:17:53 | 3.35 / 5.63 / 5.92 | 3.04 / 5.06 / 5.68 | 58.755 | 445.894 | 724.296 | **1731.837** |
| A3 | campaign | 12:18:59 | 3.04 / 5.06 / 5.68 | 3.68 / 4.89 / 5.57 | 59.386 | 429.742 | 715.531 | **1666.461** |

**At 200 agents, paired: 1678.101 → 1609.841 (−4.07%) and 1731.837 →
1666.461 (−3.77%).** Two independent pairs agreeing to within 0.3 points.
**The level moved, by about 4%, in the campaign's favour** — where §11
measured +0.1% to +0.25%, i.e. nothing. Both trees still sit below Penstock
§6.3's 1712–1752, which is a box-and-date difference, not a campaign effect.
The fitted log-log slope is 1.10 on both trees, unchanged; the campaign
lowered the level without changing the shape of the agent-count scaling,
which is the expected signature of a per-agent read getting cheaper.

### 12.7 `fold_depth_sweep` — the shape, re-run on a quiet box

One run each tree, back to back inside the same quiet window (control
3.44/4.80/5.53 → 3.32/4.75/5.51; campaign 3.32/4.75/5.51 unchanged).
µs/call, median of 6 alternating-direction passes.

| depth | PERIODIC pre | PERIODIC post | ratio | SINGLE-RESET pre | SINGLE-RESET post | ratio |
|---|---|---|---|---|---|---|
| 10 | 0.786 | 0.108 | 7.3× | 0.754 | 0.106 | 7.1× |
| 32 | 1.277 | 0.160 | 8.0× | 1.529 | 0.158 | 9.7× |
| 100 | 2.553 | 0.266 | 9.6× | 5.237 | 0.294 | 17.8× |
| 320 | 6.539 | 0.535 | 12.2× | 28.005 | 0.578 | 48.5× |
| 1 000 | 20.047 | 1.440 | 13.9× | 205.249 | 1.546 | 132.8× |
| 3 200 | 68.039 | 4.777 | 14.2× | 1 777.183 | 5.093 | 349.0× |
| 10 000 | 219.398 | 14.917 | 14.7× | **19 631.876** | **15.460** | **1 269.8×** |

| | pre | post |
|---|---|---|
| PERIODIC `k` (µs/call/fact) | 0.02191 (r² 1.000) | 0.00148 (r² 1.000) |
| PERIODIC raw elasticity, top third | 1.028 | 0.999 |
| SINGLE-RESET `k` | 1.94528 (r² 0.949) | 0.00154 (r² 1.000) |
| **SINGLE-RESET raw elasticity, top third** | **2.108** | **0.975** |
| SINGLE-RESET raw elasticity, top half | 1.981 | 1.000 |

**The single-reset column now sits ON TOP of the periodic one** — 15.460 vs
14.917 µs/call at depth 10 000, a 3.6% gap where the pre-campaign trees
differ by 89× at the same depth. `S` no longer costs anything the periodic
regime does not also pay, which is the accumulator's whole claim expressed as
a measurement. The residual linearity in both columns is the bench's own
shape, not the read's: `fold_depth_sweep` rebuilds the ledger per depth and
amortises one cold advance over 50 reps, so each reading is
`(O(depth) first touch + 50 × O(1)) / 50`.

Against §11.7's numbers, taken on the same instrument at the same merge base:
the control agrees to within 2% (19 631.876 here vs 19 215.004 there at depth
10 000; 219.398 vs 210.609 periodic), and the campaign side improved a further
**12.6×** on top of the 98.8× §11.7 measured after stage 2.

### 12.8 What the second readout hands forward

- **The instrument still needed no change**, and none was made. No example
  file differs from the one §11 ran on either tree; the control worktree is a
  detached checkout of `18f63ebfa` whose three benches carry the identical
  `AGENTS = 50` / `TICKS = 200` / `FOLD_REPS = 200` / `BAND = 20` constants
  and no resident store (`grep -n 'resident::\|OwnedFolds'` returns nothing
  there).
- **The ~200× gap §11.7 named is closed, and it was what §11.7 guessed.**
  That section wrote that the ecological `drive_at` cost ~597 µs at h = 260
  where the synthetic one cost ~3 µs, named `sustenance_at`'s per-segment
  terrain sample as the leading candidate, and said "no probe isolates it and
  this readout does not claim it; it is the followup." The accumulator removes
  that per-segment sample and the ecological figure lands at **2.70 µs/call**,
  on top of the ~3.05 µs/call the synthetic sweep interpolated for depth 260.
  A named-but-unclaimed hypothesis was tested and held.
- **The remaining quarry is the hazard memo, and it is bigger than what was
  fixed.** `hazard_memory_memo` is 93 153 µs/call at the final band — 84% of
  the six probes' total, elasticity 0.92, and only 1.57× down from the
  merge base. H4 was written against it and H4 is the criterion that did not
  move. Anything that wants H2 (c)'s 20% share must go there next.
- **§11's methodological finding survives unchanged**, and this readout is
  its confirmation rather than its refutation: a criterion written against an
  ecological bench could not see the change of order the synthetic bench
  measured at stage 2, and it took a *further* fix — one that made the
  ecological cost fall 230× — before the same criterion could. A future
  preregistration for a change-of-order fix should still state its shape
  criterion on `fold_depth_sweep.rs` and its share criterion here.
