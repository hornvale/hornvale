# The Pawl: The Resident Fold Store — A Campaign Design

> **STATUS: DRAFT, awaiting G3.** Branch `campaign/the-pawl`; decision block
> 0536–0545; ledger `docs/superpowers/ledgers/2026-09-01-the-pawl.md`.

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
bulk-synchronous tick, so `frozen` *is* the ledger), clones it, and absorbs
this tick's `out` facts for the agent into the clone. `Folded<S>` must
therefore be `Clone` where `S: Clone` — a one-line additive kernel change,
and the only kernel edit this campaign makes. (A kernel-layer edit rebuilds
more units at `gate-commit`; the plan pays that once, early.)

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

**In:** the store; five tenants; the six read sites; the `Folded: Clone`
kernel line; the instruments re-run; the witnesses in §3.
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
  `docs/audits/type-audit-report.md` in the same commit; the stage-1 kernel
  line makes that commit's `gate-commit` a kernel-layer one.
- Both benches run `--release` only, and their module docs explain why the
  two instruments are complements (internal vs ecological validity). Neither
  may be deleted.
- `PrimaryAfraidMemo`'s "one per tick" doc is load-bearing for the
  attribution probe: constructing it once across `FOLD_REPS` would measure a
  hit rate, not a fold.
- `make prewarm` was started in this worktree at G1; the worktree was
  created cold (no recyclable pool member was clean).
