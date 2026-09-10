# The Vent: Succession Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend the merged static Waterworld overlay with a deterministic,
bounded, query-pure vent succession snapshot over exact world time.

**Architecture:** Keep the composition-root implementation in
`windows/worldgen`. `WaterWorld` owns stable substrate and seeded vent source
parameters; `WaterWorldSnapshot` is derived by a pure time query and owns
present fields, stocks, transport, and observations. Current transport and
candidate-ring migration remain separate bounded channels.

**Tech Stack:** Rust workspace, `hornvale-worldgen`, `WorldTime` tick lattice,
existing terrain/climate providers, `Seed`/`Stream`/`StreamLabel`, ordered
`Vec` storage, consolidated worldgen integration suite, nextest.

**Spec:** `docs/superpowers/specs/2026-09-10-the-vent-succession-design.md`

## Global Constraints

- Keep the Waterworld model in `windows/worldgen`; domains depend only on the kernel.
- Preserve explicit marine nouns: substrate, fields, stocks, currents, vents, and observation.
- Use exact `WorldTime` ticks; no wall-clock time, dense planet × time cache, or unbounded event queue.
- Add only isolated versioned streams, keyed by stable source vertex and consumed in stable order.
- Snapshot and render queries do not draw, mutate, rebuild, or cache.
- Use ordered vectors and explicit tie-breaking; do not introduce `HashMap`/`HashSet`.
- Every perturbation asserts source inequality before downstream inequality.
- Keep all stocks aggregate and bounded; do not add species, metabolism, reproduction, or organisms.
- Verify generated-artifact, tool, timing, and cost claims with commands and record output in the ledger.
- Do not merge or push by hand; use local `make gate-commit` and the Sluice.

## File map

- Modify `windows/worldgen/src/waterworld.rs`: stable vent parameters, phase
  state, pure snapshot derivation, independent temporal fields and stocks.
- Modify `windows/worldgen/src/waterworld_propagation.rs`: candidate-ring
  migration, bounded current transport, refresh samples, actual-loop counters.
- Modify `windows/worldgen/src/waterworld_render.rs`: ordinary and diagnostic
  snapshot observations, uncertainty/provenance language, pure rendering input.
- Modify `windows/worldgen/src/lib.rs`: public exports and composition-root
  wiring without changing unrelated worldgen paths.
- Modify `windows/worldgen/src/streams.rs` only if a new seeded parameter is
  earned; preserve the existing `waterworld/vent/v1` draw order otherwise.
- Modify `windows/worldgen/tests/suite/waterworld.rs`: all red/green behavioral,
  determinism, purity, bounds, and counter evidence.
- Modify `docs/superpowers/ledgers/2026-09-10-the-vent-succession.md` at each
  ruling/task boundary; update reconciliation only when record paths change.
- At close, create the chronicle/retrospective and update frontier registry
  follow-ups through the closing skill.

### Task 1: Stage 1 — Temporal seam inventory and non-vacuous red probes

**Goal:** Prove the current merged Waterworld seam and expose a behavioral red
for time-sensitive reads before implementing succession.

**Success Criteria:** Existing substrate, marine column, temperature,
insolation, current, boundary, edifice, and feature witnesses are nonempty;
future climate time actually changes at least one consumed source; disabled
Waterworld remains empty and stable; the new temporal test fails because the
static overlay does not yet change with time, not because a symbol is missing.

**Tests:** `waterworld::seams`, `waterworld::temporal_red`, and
`waterworld::absent_overlay` in the consolidated suite.

- [ ] Write source inventory tests using the existing `fixture` and
  `source_at` helpers. Assert witness-source inequality before any derived
  comparison: marine vs non-marine, seabed vs open-column, distinct depth
  bands, nonzero current, and terrain source context.
- [ ] Add a climate-time witness test using
  `WorldTime::from_ticks(WorldTime::TICKS_PER_STD_DAY * 10)` and the exact
  `GeneratedClimate::temperature_at` consumer. Assert the source temperature
  differs before using it in an overlay assertion.
- [ ] Add the temporal red against the current static path: construct the
  same enabled Waterworld at genesis and the future time through the planned
  test seam, then assert the present field/vent readout differs. Record the
  behavioral red; a compile-only failure does not count.
- [ ] Re-run only the focused test and inspect the failure output once. If the
  source witness is unchanged, stop and find a different real witness rather
  than weakening the assertion.
- [ ] Define the smallest stable/dynamic boundary needed to compile the red:
  `VentState`, stable source parameters on `WaterVent`, and
  `WaterWorldSnapshot`. Keep the static `waterworld_from` substrate/vent
  admission behavior byte-compatible.
- [ ] Implement `WaterWorld::at(&self, climate: &GeneratedClimate,
  time: WorldTime) -> WaterWorldSnapshot` as a pure call that initially
  delegates to existing genesis derivations where no temporal term is yet
  connected. Do not add a cache or new stream draw in this step.
- [ ] Turn the red into a green seam test only after the actual temporal input
  is connected in Stage 2; retain the source-first assertions.
- [ ] Record witness counts and the red/green commands in the ledger.
- [ ] Commit: `test(the-vent): establish temporal Waterworld seam`.

### Task 2: Stage 2 — Deterministic succession and independent field propagation

**Goal:** Derive five vent states, bounded migration, and independent local
chemistry/temperature consequences from stable seeded source parameters.

**Success Criteria:** Exact tick boundaries produce all five states across
real witnesses; repeated same-input snapshots are byte-identical; state
changes alter local chemistry/temperature as claimed; failed sources preserve
identity and seabed; candidate-ring migration changes position without
rewriting substrate; stream order is stable and audited.

**Tests:** `waterworld::succession`, `waterworld::fields`,
`waterworld::migration`, `waterworld::determinism`.

- [ ] Write red tests for `VentState::{Absent,Nascent,Active,Weakening,Failed}`
  at exact tick boundaries, source-state distinction, local chemistry and
  temperature independence, and migration/substrate preservation.
- [ ] Select a single new versioned stream label only if phase offsets,
  duration weights, or candidate-ring tie-break parameters cannot be pure
  functions of existing stable source inputs. Document its source key and
  draw order before adding it; run the stream manifest/audit command and
  record the exact output.
- [ ] Add stable vent parameters in vertex order. Use integer tick arithmetic
  for cycle selection; keep the categorical state and continuous phase
  position separate. Make state boundaries finite and named in code comments
  tied to the spec.
- [ ] Implement local source contribution as separate chemistry and thermal
  terms. A state perturbation must not silently change light, pressure,
  salinity, or current.
- [ ] Implement the ordered candidate ring in
  `waterworld_propagation.rs`. Cap ring size, sort by stable vertex key, and
  select at most one active influence position. Preserve the substrate vector.
- [ ] Run focused succession/field/migration tests. If a test remains green
  after changing its supposed source, assert that the source mutation was
  real and replace the witness or seam; do not accept a vacuous test.
- [ ] Run the existing Waterworld suite once with `--no-fail-fast` after the
  focused pass; inspect all failures from that one run.
- [ ] Record stream compatibility and state witness counts in the ledger.
- [ ] Commit: `feat(the-vent): add deterministic vent succession`.

### Task 3: Stage 3 — Bounded stocks and current-mediated redistribution

**Goal:** Make local and transported aggregate consequences measurable while
keeping temporal work and memory bounded.

**Success Criteria:** Plankton, chemosynthetic bloom, nutrients, and reef/kelp
suitability have separate source tests; residue is added only if a probe shows
instantaneous stocks cannot distinguish active/weakening/failed consequences;
current perturbations change transport but not substrate/source identity;
counters measure actual loops and scale with configured bounds.

**Tests:** `waterworld::stocks`, `waterworld::transport`,
`waterworld::memory`, `waterworld::cost`, `waterworld::purity`.

- [ ] Write stock red tests for light→plankton, chemistry→chemosynthetic
  bloom, nutrient input→nutrient reserve, and substrate/temperature/chemistry
  →reef/kelp suitability. Assert source inequality first and `[0, 1]`/finite
  bounds after each comparison.
- [ ] Write a memory decision test comparing active→weakening→failed reads. If
  present fields already distinguish the needed stock consequence, record
  “no residue” and keep stocks instantaneous. Otherwise add one analytical,
  bounded residue term based on exact ticks since the last active interval;
  never store per-cell history.
- [ ] Implement stock derivation over snapshot fields with named contributors;
  keep local source influence and transported influence in separate fields.
- [ ] Write current perturbation tests that assert the changed current vector,
  unchanged substrate and vent identity, and changed downstream transport.
- [ ] Implement finite current-following propagation using existing ordered
  marine vertices and explicit hop/attenuation limits. Do not add a fluid
  solver or a new propagation stream unless a measured seam requires one.
- [ ] Add counters at the candidate-ring, stock, propagation, refresh, and
  observation loops. Assert counters are nonzero for active work and zero for
  disabled overlay paths where the contract says no work occurs.
- [ ] Compare one/multiple sources and low/high configured limits. Record
  actual counter rows and wall-time commands; do not state asymptotics beyond
  the measured fixtures.
- [ ] Snapshot stable inputs, call snapshots and propagation in varied order,
  and assert stable inputs and returned values are unchanged/equal.
- [ ] Commit: `feat(the-vent): add bounded temporal stocks and transport`.

### Task 4: Stage 4 — Observation, performance evidence, documentation, and close

**Goal:** Expose present consequences and inferred causes honestly, verify the
campaign gates, and prepare Sluice submissions.

**Success Criteria:** Ordinary and diagnostic observations differ in the
specified direction; both are pure and bounded; generated/tool/timing claims
have command evidence; `make gate-commit` passes; stage and merge requests
use the full tested SHA and the Sluice.

**Tests:** `waterworld::observation`, focused Waterworld suite, `make
docs-tests`, `make gate-commit`, and the repository’s required Sluice phases.

- [ ] Write observation red tests: ordinary output reports present substrate,
  stocks, and transport consequences; diagnostic output reports phase,
  source provenance, local/transported split, and uncertainty without calling
  inferred causes direct observations.
- [ ] Implement pure snapshot observation and rendering preparation. Keep
  absent, failed, and zero-baseline language distinct.
- [ ] Run repeated/reordered ordinary and diagnostic calls, compare exact
  strings/structures, and assert no source or snapshot mutation.
- [ ] Run focused tests, then `make docs-tests`; fix only the specific
  reconciliation/knowledge drift it reports. Do not add close artifacts early
  merely to satisfy a record check.
- [ ] Add measured performance evidence to the ledger and create the chronicle
  and retrospective at close. Add or update registry follow-ups for deferred
  species, reef fragmentation, current-network dynamics, signal distortion,
  and full vent histories.
- [ ] Run `make gate-commit` locally and record its exit code and relevant
  output. Do not use `--no-verify`.
- [ ] Run the applicable review checkpoint and request the stage gate with
  `make sluice-stage BRANCH=campaign/the-vent-succession REF=<full-sha>`.
- [ ] At G6, present the post-G3 ledger digest and wait for Nathan. After
  approval, use `closing-a-campaign`, then submit the merge with
  `make sluice BRANCH=campaign/the-vent-succession REF=<full-sha>`.

## Plan self-review

- Spec sections 1–10 map to the question/scope, stable-dynamic architecture,
  field/stock model, ownership, determinism, measurement, bounds, four
  stages, follow-ups, and G3 package above.
- Placeholder scan: no placeholder marker, “implement later” instruction, or
  unspecified error-handling step appears in the plan.
- Type consistency: `WaterWorld::at` returns `WaterWorldSnapshot`; Stage 2
  creates the fields Stage 3 consumes; Stage 4 observes the same snapshot.
- The existing root `IMPLEMENTATION_PLAN.md` is unrelated active work and is
  intentionally preserved; this campaign plan is the canonical plan file.
