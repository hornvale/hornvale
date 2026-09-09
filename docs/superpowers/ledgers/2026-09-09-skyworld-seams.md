# Skyworld Seams campaign ledger

**Campaign:** Skyworld Seams · **Branch:** `campaign/skyworld`
**Parent campaign:** `docs/superpowers/ledgers/2026-09-09-the-skyworld.md`
**Status:** Task 4 contract probes and seam-audit capture complete; awaiting
the campaign's normal review and close process.

This ledger records the follow-up seam-audit campaign separately from the
parent Skyworld implementation ledger. The parent ledger retains the broader
Skyworld design, species, lifecycle, tethering, Waterworld, and cultural
follow-ups; this file records only the boundary and cost experiment.

## #1 [G4] — How should the Skyworld seam audit be executed?

**Recommendation:** use four bounded tasks: map the existing seam with
non-vacuous environment probes; verify independent environmental propagation;
measure substrate reuse and requested-work scaling; then record the reusable
cross-realm contract and close the slice.

**Why:** the approved seam design identifies uncertainty about ownership and
cost, not a missing realm feature. The plan keeps the existing
`skyworld_from` boundary and only permits a production wrapper or measurement
hook when a failing probe demonstrates that it earns its complexity. It also
puts the expensive or potentially misleading questions behind explicit tests:
source perturbations must change, reconstruction counters must stay at zero,
and detail/query paths must remain pure.

**Alternatives discarded:** adding a `BuildDepth` rung or save fact would make
the overlay durable before its cost is known; a universal realm or Sugar
abstraction would erase world-specific axes; adding organisms, species, or
lifecycle would multiply consumers before the boundary is measured; a dense
planet-by-time atmospheric simulation would violate the bounded-work goal.

**Plan:** `docs/superpowers/plans/2026-09-09-skyworld-seams.md`.

## #2 [G5] — Does the environmental propagation matrix prove its dependencies?

**Decision:** accept the propagation matrix after two review-driven fix
rounds. Surface/climate perturbations now prove source inequality before
checking dependent overlay outputs; the preservation probe regenerates both
fixtures and verifies terrain, climate, and `BiomeExpr` remain intact.

The tectonic probe initially remained correlational because plate-count
fixtures changed elevation, coast, climate, and edifices together. The final
fix extracts the pure score contribution, passes the real per-vertex
`has_edifice` flag from production, and unit-tests the documented `0.25`
increase directly. This makes the test fail if the edifice bias is removed.

**Evidence:** all 34 Skyworld integration tests and the edifice unit test pass
after The Staple D4 was incorporated from `origin/main`; the merged-tree
commit gate also passed. No Skyworld production API, BuildDepth rung, save
fact, stream roster, or cross-realm implementation was added.

**Capture:** Task 2 commits `b5c9b39d3`, `a410a2bfb`, and `4d9f3005b`.

## #3 [G5] — Does Skyworld work scale with requested materialization?

**Decision:** accept the cost evidence after one review-driven fix round.
Test-only loop counters now measure actual generation, trajectory, raster, and
pixel-loop work; the probes compare inactive/active territory counts, low/high
trajectory samples, and all three render details. The previous returned-state
proxy and unsupported print-only measurement were removed.

**Evidence:** the focused cost probe, 34 Skyworld integration tests, and
`make gate-commit` passed. Counters remain `cfg(test)` and do not affect
runtime output, stream consumption, save facts, or BuildDepth. The merged
branch still carries only the known unstaged gate timing drift.

**Capture:** Task 3 commits `f2fa42771` and `2a398be5e`.

## #4 [G5] — What crosses a realm seam without making a universal realm?

**Decision:** retain direct typed `&GeneratedTerrain` and
`&GeneratedClimate` inputs at `skyworld_from`, and record the reusable seam
as a four-layer contract rather than adding a wrapper, cache, `BuildDepth`
rung, artifact field, or save fact.

| Layer | Owns | Reusable minimum | Deliberately realm-specific |
|---|---|---|---|
| Surface substrate | terrain classification, elevation, ocean mask, `BiomeExpr`, and features | fixed surface inputs and projection target | waterbed/vent geometry and cave topology |
| Ambient fields | temperature, moisture, wind, radiation, aether, and astronomical forcing | prerequisite fields sampled by a habitat | currents, cave energy, and aether-band profile |
| Habitat derivation | phenotype, bounded aggregate stocks, trajectory, adjacency, and footprints | stocks plus movement/adjacency and bounded propagation channels | each realm's distribution and viability rules |
| Observation | ordinary PNG/readout and diagnostic readout | ordinary/diagnostic lenses over derived state | projection and diagnostic vocabulary |

**Evidence:** `render_and_query_paths_leave_inputs_unchanged` samples every
surface vertex (including terrain/climate values and `BiomeExpr`), snapshots
the generated `SkyWorld` and all three ordinary/diagnostic detail outputs,
then exercises altitude, trajectory, propagation, PNG, ordinary-readout, and
diagnostic-readout paths. The before/after comparisons remain equal. Its
companion source-contract probe pins `BuildDepth` to `Astronomy`, `Terrain`,
`Settlements`, and `Full`; pins `BuildArtifacts` to `world`, `terrain`, and
`climate`; and rejects Skyworld serialization/save vocabulary. The Task 3
counter probe's measured fixture counts remain: 81,924 surface visits with no
territories, 2 active territories, 4 then 16 trajectory samples at 2 then 8
requested samples, 122,886 surface visits for the 8-sample run, and 3 rasters
/ 98,304 pixels for the three detail renders. `cargo test -p
hornvale-worldgen --test suite -- skyworld` reports 35 passed, 0 failed;
`cargo test -p hornvale --test suite -- docs_consistency` reports 41 passed,
0 failed.

**Rejected:** a wrapper would only rename the existing direct typed inputs;
a cache would obscure the proved cache-independent query path and create new
invalidation ownership; a universal realm abstraction would erase the
realm-specific dimensions above. No production abstraction is justified by
the measured seam.

**Explicit non-results:** this is not a Waterworld or Underworld
implementation, does not add vents, currents, cave energy, aether bands, or
new projection rules, and does not add organisms, species, lifecycle mutation,
tethering, mutable atmosphere, a census, a build rung, or a save-format fact.

## Follow-ups

- Model Skyworld lifecycle and co-evolution only after a mutable ownership
  boundary and its compatibility cost are designed explicitly.
- Evaluate tethering and species as separate habitat consumers; neither is
  implied by aggregate stocks or read-only movement.
- Transfer the four-layer contract to a Waterworld experiment only after it
  names its own current, vent, and projection rules rather than inheriting
  Skyworld's aether assumptions.

## Deferred

- No organisms, species, lifecycle mutation, tethering, mutable atmosphere,
  Waterworld implementation, or census refresh belongs to this slice.
- The parent Skyworld ledger remains the source of truth for broader design
  follow-ups and previously recorded ideonomy results.
