# The Temperament — Implementation Plan

**Spec:** `docs/superpowers/specs/2026-07-19-the-temperament-design.md`
**Branch/worktree:** `the-temperament`

**Grounding (from the drive/psychology map):** all runtime work is in
`windows/vessel/src/liveness.rs` unless noted. Key sites: `DriveParams`
(`~:100`, `SUSTENANCE` `~:115`), `drive_at` (fold over `DRANK`, `~:249`),
`decide` (`~:327`), `DriveMovements` tick (`~:402`), `Perceived` (`~:295`),
`Terrain` trait (`~:125`, has `elevation`/`is_fresh_water`), `downhill_step`
(`~:159`), `GoapSpace`/`PlanState`/`Action` (`~:701`), `plan_to_water`/`NavSpace`
(`~:771`/`~:785`). Temperature: `LocaleContext::describe(room, t)
.fields.temperature_c` (`windows/locale/src/lib.rs`), per-day
`GeneratedClimate::temperature_at(cell, day)` (`domains/climate/src/provider.rs`).
Niche: `ConditionNiche.temperature` (`domains/species/src/lib.rs`),
`ConditionResponse::eval` (`kernel/src/ecology.rs`). Cognition edge:
`kernel/src/manifest.rs`; `cold`/`heat` manifests in `domains/climate/src/lib.rs`.
All new state is session-sandboxed (session ledger clone, never genesis).

---

## Stage 0: Thirst → the `Drive` trait (BYTE-IDENTICAL)
**Goal:** extract the single existing need into a `Drive` abstraction with the
thirst walk provably unchanged — de-risks the abstraction before anything new.
**Deliverable:**
- A `Drive` trait: `fn urgency(&self, …) -> f64` + `fn affordance(&self, pos,
  terrain) -> Plan` (the "how to reduce it"). Thirst becomes `Drive #1` (a stock
  drive): `urgency` = the current `drive_at` fold; `affordance` = the existing
  `believed_water`→`plan_to_water` / `explore_step` chain wrapped behind the trait.
- `decide` rewritten to consult the (single-element) drive set through the trait,
  producing the SAME `Intent` sequence as today.
**Success criteria:** the thirst walk is **byte-identical** — the vessel walker
tests and the drive determinism tests (`drive_at_*`, `believed_water_*`,
`downhill_step_*`, the pinned reachability finding, `build_world_is_deterministic`)
pass unchanged; a possession-walk fixture (if any) regenerates identical.
**Tests:** `cargo test -p hornvale-vessel`; the determinism suite named above;
`make gate` scoped.
**Status:** Complete — `Drive` trait (`urgency`/`act_threshold`/`affordance`) +
`Thirst` stock-drive; `decide` consults it, same `Intent` for every state. Walk
BYTE-IDENTICAL (vessel suite green incl. the determinism tests; possession-walk
fixtures diff = 0), independently verified. type-audit/clippy/fmt clean.

## Stage 1: The thermal drive (a flow-drive)
**Goal:** a creature reads its own cell temperature against its niche and seeks
comfort.
**Deliverable:**
- `Terrain::temperature(&RoomAddr, day) -> f64` beside `elevation`, backed by
  `describe(room, day).fields.temperature_c` — using the **per-day** field
  (`temperature_at(cell, day)`) so the swing is diurnal/seasonal (a small thread
  of `day` through `describe`/`Terrain`).
- A thermal `Drive` (flow): `urgency` = deviation past `ConditionNiche
  .temperature`'s tolerance width from `optimum` (first runtime read of the
  niche); `affordance` = gradient-follow toward the comfortable neighbour (a
  near-copy of `downhill_step`, direction set by too-cold/too-hot toward the
  optimum, same `total_cmp` + ascending-`RoomAddr` tie-break). No belief cache.
- A comfort goal for the planner: a nav-like `WarmthSpace` keyed on a
  `comfortable(position)` predicate (mirroring `NavSpace`), or the fuller
  `PlanState.warm` — pick the smaller (a comfort nav-space is preferred).
**Success criteria:** a single-drive creature in a cell outside its niche walks
toward a comfortable one and stops; reload-stable (recompute twice → identical);
a cold-niche species tolerates cold a warm-niche species flees.
**Tests:** new thermal-drive unit tests (firing, gradient direction, niche
respect, determinism); `cargo test -p hornvale-vessel`.
**Status:** Complete — `Terrain::temperature` (per-day via a new
`LocaleContext::temperature_at`, `describe` untouched → render byte-identical);
`Thermal` flow-drive (niche-relative urgency, comfort gradient-step, same
tie-break); 5 unit tests incl. niche-respect (cold tolerates what warm flees).
NOT wired to the live decide (Stage 2). Walk BYTE-IDENTICAL, independently
verified.

## Stage 2: Arbitration + psychology (action-centric)
**Goal:** two competing drives resolve; psychology visibly changes the resolution.
**Deliverable:**
- `Perceived.drive` (single scalar) → a set of `(DriveKind, urgency)` (or the
  drive set evaluated in `decide`).
- **Action-centric `decide`:** enumerate candidate actions (≤3 neighbour
  `MoveTo`s + consume/bask), score each `Σ_d urgency_d × reduction_d(action)`,
  pick max-utility. `deliberation_latency` slides *loudest-drive-only* (grab) ↔
  *weighted-sum* (weigh).
- **Soft Maslow:** urgency ceilings per drive (survival→1.0, comfort caps lower)
  so severe cold beats mild thirst but nothing beats dying of thirst.
- **Commitment as an explicit mode:** a session-sandboxed `Pursuing(drive)` /
  `Homing` / `Idle` behavioural state with hysteretic transitions (engage at
  `act`, release below `act − h`, switch only when a challenger's utility exceeds
  the incumbent's by δ). Deterministic tie-breaks throughout.
**Success criteria:** a creature that is both thirsty and cold picks the move
best relieving total discomfort; changing `deliberation_latency` changes the
resolution (grab vs weigh); no dithering/flip-flop; reload-stable.
**Tests:** arbitration unit tests (max-utility pick, grab-vs-weigh divergence,
hysteresis no-flip-flop, urgency-ceiling ordering, determinism).
**Status:** Complete — action-centric `arbitrate` (utility = Σ capped-urgency ×
serviceability; grab↔weigh via `deliberation_latency`; soft-Maslow ceilings
thirst 1.0/thermal 0.6; commitment `Mode{Idle,Homing,Pursuing}` + hysteresis δ);
niche + latency threaded onto the NPC. Thirst-only PRESERVED (`decide` delegates
to `arbitrate({Thirst})`; regression test byte-identical); walk delta ZERO
(seed-42 hobgoblins temperate → thermal never fires), genesis byte-identical,
independently verified; `make gate` green.

## Stage 3: Affect + the health metric
**Goal:** the decision's character is legible, and the population self-scores.
**Deliverable:**
- A derived **affect** read: `arousal = max urgency`, `valence = progress −
  blocked`, labelled by region (content / eager / searching / frustrated /
  panicked / lost / helpless) + the intentional object + reason; learned
  helplessness as a persistence-gated sticky state. Surfaced in narration
  (`Session::needs`/`narrate_motion`).
- A **Lab health metric** (`windows/lab`): the family — prevalence, **chronicity**,
  **recovery-rate** (spike half-life), by-cause, by-species — over a seed sweep,
  measured as **deviation from a null control** (a resource-abundant,
  niche-matched world → ≈0 persistent distress). Preregistered + drift-checked.
**Success criteria:** affect renders legibly; searching is excluded from the
metric; the null control reads ≈0; an injected novel event spikes-then-recovers;
an injected unsatisfiable need spikes-and-persists (the bug signal).
**Tests:** affect-read unit tests; the Lab metric study + its null-control and
injected-fault probes.
**Status:** Complete. **3A (affect):** `arbitrate` returns
`Resolution{intent,mode,affect}` (decide byte-identical, takes `.intent`); the
6-region derivation (Content/Eager/Searching/Frustrated/Lost/Helpless) is
STRUCTURAL — from met/known/unknown/blocked, not an arousal threshold (which
broke on the real `act=0.85` + binary serviceability); `affect_of` snapshots a
co-located NPC's felt state and `Session::needs` renders it as object-coloured
prose. The dead `DriveParams.sated` (only reader was the old `needs`) removed.
**3B (health metric):** `windows/lab/health` — a headless drive-sim harness
(reuses `derive_npcs`/`DriveMovements`/`affect_of`; lab now depends on
vessel+locale, window→window) + the distress family (prevalence/chronicity/
recovery/by-cause/by-species; Searching excluded). Preregistered
(`health_calibration.rs`, 6 tests): null control reads 0 (seed 42 + a seed
sweep), the reduction FIRES on injected spike-recover vs spike-persist, a pure
Searching run reads 0, by-species separates. FINDING: genuine distress is rare
(a creature searches, not despairs), so the metric is a regression alarm armed
by staying quiet. `Npc` gained a `species` field.

## Stage 4: The correspondence payoff
**Goal:** `cold`/`heat` earn their cognitive handle.
**Deliverable:** flip `cold`/`heat`'s manifest cognition edge from
`Absent(Void::Uncognized)` to `Present(CognitiveHandle)` in
`domains/climate/src/lib.rs`; regenerate `concept-manifest-generated.md`.
**Success criteria:** the audit moves 0/76 → 2/76; the trial balance still foots;
byte-free to worlds (the edge is `#[serde(skip)]`); `cli/tests/correspondence.rs`
green.
**Tests:** `cargo test -p hornvale --test correspondence`; the manifest drift-check.
**Status:** Complete — cold/heat cognition edge flipped to Present(CognitiveHandle);
audit 0/76 → 2/76; trial balance foots; manifest regenerated.

---

## Definition of Done
- Five stages; `make gate` green; Stage 0 byte-identical; determinism suite green.
- Chronicle (`book/src/chronicle/the-temperament.md`) + freshness sweep; retro;
  Confidence-Gradient re-score (the new self-scoring health metric is a bet);
  idea-registry: this campaign shipped (a cognition row), the cognition column
  moved 0/76 → 2/76.
- G6 digest to Nathan: leads with the byte-identical thirst refactor + the
  session-sandbox determinism, and the health-metric null-control anchoring.

## Deferred (spec §14)
- Drive coupling (heat → faster thirst); `time_horizon` anticipation; more drives
  (hunger, danger/terror, social); the social layers (`SOC-belief-sharing`,
  `SOC-information-economy`); a real `CognitiveHandle` payload.
