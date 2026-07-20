# The Confluence — settlements condense near fresh water

**Campaign:** The Confluence (settlement placement, realizing SEQ-2)
**Registry:** SEQ-2 (rivers as the skeleton of settlement) · MAP-7 (the
carrying-capacity field — this re-points its freshwater term) · consumes
DOM-5/The Freshet's `WaterKind`
**Status:** G3 draft (awaiting Nathan)

---

## 1. Summary

Towns should sit on rivers, and ours don't. The Gathering built settlements as
**conserved attractors of an up-gradient population flow over the carrying-
capacity field K**, and `K` already has a freshwater term — but that term is a
smooth proxy (`drainage_norm.max(moisture)`), so settlements condense in *watered
regions*, not *on the water*. The Confluence re-points the freshwater term at the
**real river network** — The Freshet's `WaterKind::River` cells — so `K` spikes
near rivers and condensation pulls towns onto or adjacent to them. It realizes
SEQ-2 (rivers as the skeleton of settlement) and completes The Surmise's payoff:
an agent can now actually walk to drinkable water.

Settlement placement is **emergent**, not authored — we make water valuable and
let the flow find the peaks (the anti-lookup-table discipline, decision 0021).
Because settlements move, this is a **genesis change: an epoch + a census regen**
(Nathan authorized in principle; the AWS regen is his explicit call at execution).

## 2. Background: the seam

`domains/demography/src/carrying_capacity.rs`:
`K = BASE·npp + FRESHWATER_BONUS·freshwater + (coastal ? COAST_BONUS : 0)`, where
`npp` is a Miami/Lieth NPP proxy and `freshwater ∈ [0,1]` is a per-cell input.
`FRESHWATER_BONUS = 0.5` is a frozen calibration constant (The Gathering, Task 8,
fit against the biomass-by-latitude gradient).

The freshwater input is built in the composition root
(`windows/worldgen/src/lib.rs:425`): `let freshwater = drainage_norm.max(moisture)
.clamp(0.0, 1.0);` — a smooth blend of normalized drainage and the moisture field.
`drainage_norm` is continuous, so it never sharply marks the actual river cells;
a settlement condenses toward high-K *regions*, which need not contain a river
cell. (Each species then scales this base by its psychology at
`lib.rs:484-491` — unchanged here.)

The Freshet exposes the sharp truth: `TectonicGlobe.water_kind: CellMap<WaterKind>`
marks each cell `Ocean/SaltBasin/River/DryLand`; `River` is fresh flowing water.

## 3. The design

### 3.1 The river-proximity freshwater term

Replace the smooth proxy with a **river-proximity bonus** over a moisture floor:

```
freshwater(cell) = max( moisture(cell) · MOISTURE_FLOOR_WEIGHT,
                        river_proximity(cell) )        // both in [0,1]
```

`river_proximity(cell)` is a sharp, decaying function of hop-distance to the
nearest `WaterKind::River` cell: `1.0` on a River cell, high when adjacent, decaying
to `0` over a small radius `RIVER_REACH` (a few cells). Computed once at genesis by
a bounded BFS over the geosphere from the River cells outward (`CellMap<f64>`,
deterministic, `total_cmp`), exactly the shape of The Freshet's `nearest_water`.
The moisture floor keeps riverless-but-wet regions habitable (K stays grounded
where rivers are absent); the `max` means a river dominates when present.

### 3.2 The scale tension resolves

The Surmise reads `LocaleFields.water` at a room's **coarse max-weight cell**. So a
settlement whose cell is a River cell — or one hop from one — has fresh water within
a *short walk*. The tuning dial is **sharpness / `RIVER_REACH`**: tune so settlements
condense **adjacent** to rivers (the ignorant NPC explores a few rooms and
*discovers* water — The Surmise's discovery demo fires) rather than exactly *on*
them (the agent's own room is already fresh → it drinks immediately, no journey).
Both are honest; adjacency is the better demonstration.

### 3.3 Re-calibration

Sharpening and strengthening the freshwater term changes K's magnitude, so its
grounding against the biomass-by-latitude gradient must be re-checked and, if
needed, `FRESHWATER_BONUS` (and/or the term's normalization) re-fit — the same
discipline The Gathering used (`windows/worldgen/tests/beta_calibration_sweep.rs`,
the calibration study). The calibration target is unchanged (the real
biomass gradient, ~27× tropical-vs-polar); only the freshwater *input* sharpens.
Constants land as frozen save-format values with a provenance comment.

## 4. Architecture & files

- **`domains/terrain/`** (or `demography/`) — a `river_proximity` field builder
  (BFS from `WaterKind::River` cells; pure over the globe + geosphere). Its natural
  home is beside `water.rs`/`drainage.rs` in terrain (it reads `water_kind`), exposed
  for worldgen to consume.
- **`windows/worldgen/src/lib.rs:425`** — replace the freshwater proxy with the
  river-proximity term over the moisture floor.
- **`domains/demography/src/carrying_capacity.rs`** — `FRESHWATER_BONUS` re-fit if
  calibration requires; the K formula shape is unchanged.
- **Calibration** — `windows/worldgen/tests/beta_calibration_sweep.rs` (or a sibling)
  re-run to confirm K stays grounded; the constant re-pinned with provenance.
- **Genesis artifacts** — the seed-42 settlement gallery, census settlement metrics,
  and downstream (religion/language keyed to settlement identity) re-baseline under
  the epoch.

## 5. Testing & acceptance

The deliverable is **emergent settlement-near-water**, measured, not authored:

1. **`river_proximity` is correct & deterministic** — on a synthetic globe, a River
   cell → 1.0, adjacent → high, far → floor; pure, `total_cmp`, no HashMap, reload-
   stable. (Mutation: a constant field fails the decay assertion.)
2. **K spikes near rivers** — on seed-42, mean K in cells within `RIVER_REACH` of a
   River cell is materially higher than the riverless-land mean. (The mechanism the
   condensation rides.)
3. **Settlements condense near rivers (the keystone, emergent).** On seed-42 (and a
   small seed sweep), the fraction of settlements within a short walk of fresh water
   rises sharply vs the pre-Confluence baseline — measured over the generated
   settlement set, never authored per-town. A preregistered threshold (e.g. "≥ X% of
   settlements within N of a River cell", set from the measured distribution, not
   invented) is the acceptance gate. (Mutation: reverting the term drops it back to
   the baseline.)
4. **The Surmise discovery demo fires** — the possessed agent's home settlement can
   now reach fresh water within `PLAN_BUDGET`; the parked
   `seed_42_...reachability_is_a_measured_t5_finding` flips from "0 drinks" to a
   working discovery journey. (This test lives in vessel; The Confluence updates it
   to assert success — the campaign's visible payoff.)
5. **K stays calibrated** — the biomass-by-latitude grounding holds within tolerance
   after re-calibration (the-gathering calibration test green).
6. **Determinism / same-seed identity** — same seed + pins → byte-identical world
   under the new epoch; the epoch is applied uniformly (no partial-epoch — decision
   lesson: a cheap blast radius can be a symptom).

## 6. Determinism & save-format — the carve-out (leads G3)

**This is a genesis change: an EPOCH + a CENSUS REGEN.** Re-pointing the freshwater
term changes K → changes the condensation → **settlements move**. That alters
committed genesis output (settlement positions/identities) and everything keyed to
them, so:

- **Epoch:** the change is versioned as a deliberate regeneration (a stream-label
  epoch suffix where a seed-derivation is affected, per the save-format contract —
  the plan pins the exact epoch surface). Applied **uniformly** (Lorenz-safe: reload
  re-derives from the seed).
- **Census regen (AWS — Nathan's explicit authorization at execution).** The 1000-
  seed census settlement metrics move; the census fixtures refresh once, pre-merge,
  on the AWS box (`scripts/aws-gate/regen-git.sh`) — never locally, never without
  Nathan's OK. This is the campaign's gating dependency.
- **Artifact re-baselines** (in-commit): the seed-42 settlement gallery, any
  settlement-derived book pages, and the Surmise possession transcript (the demo now
  fires).

Determinism rules hold throughout (no HashMap, `total_cmp`, no RNG in the field
builder; no wall-clock). The calibration constants are frozen save-format values.

## 7. Non-goals (reserved)

- **Named river/lake entities + trade/borders/toponymy** (The Freshet's B; the full
  SEQ-2 people-infrastructure) — this campaign moves *where* towns are, not the river
  as a named actor.
- **The coarse→fine river-resolution bridge** — if adjacency-at-coarse-cell still
  leaves the fine river channel a long walk within the cell, resolving the channel at
  walk-depth (MAP-30 palimpsest territory) is a follow-on. The coarse-cell sampling
  (§3.2) is expected to suffice; measured in acceptance #4.
- **Smarter exploration for ignorant agents** — a Surmise followup; not needed if
  settlements are near water.
- **Closed-basin lake physics / lakes as settlement magnets** (The Freshet's C /
  CLIM-endorheic).

## 8. Judgment calls

- **`RIVER_REACH` / sharpness** — the proximity decay radius. Tuned so settlements
  land *adjacent* to rivers (short discovery walk), verified by acceptance #3/#4.
- **`MOISTURE_FLOOR_WEIGHT`** — how much the old moisture proxy survives as a floor
  where rivers are absent (keeps riverless regions habitable, K grounded).
- **`FRESHWATER_BONUS`** — re-fit only if calibration (#5) requires; otherwise the
  frozen 0.5 stands with an updated provenance note.

## 9. Task shape (for the plan)

Roughly four tasks: (T1) `river_proximity` field builder + tests; (T2) re-point the
worldgen freshwater term over the moisture floor + the "K spikes near rivers" +
"settlements condense near rivers" measurements; (T3) re-calibration (K-grounding
check, re-fit `FRESHWATER_BONUS` if needed) + the epoch surface + genesis
byte-identity-under-epoch; (T4) flip the Surmise reachability test to success +
artifact re-baselines + chronicle/retro/registry (flip SEQ-2, advance MAP-7) +
close. **The census regen is a separate, Nathan-authorized step at merge (AWS).**
