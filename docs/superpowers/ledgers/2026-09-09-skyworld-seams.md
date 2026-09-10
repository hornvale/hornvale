# Skyworld Seams campaign ledger

**Campaign:** Skyworld Seams · **Branch:** `campaign/skyworld`
**Parent campaign:** `docs/superpowers/ledgers/2026-09-09-the-skyworld.md`
**Status:** Tasks 1–4 complete through `c1a20ce71`; broad review approved
in the closing handoff. Local close artifacts prepared; canonical submission
and merge remain pending.

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

**Historical decision:** the scoped review accepted the propagation matrix
after two fix rounds. Broad review found that the climate fixture still
coupled several axes; #5 supplies the independent evidence. Surface/climate perturbations now prove source inequality before
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

**Historical decision:** the scoped review accepted the cost evidence after
one fix round. Broad review found omitted work; #5 extends the counters and
limits the claims to what they measure.
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


## #5 [G5] — What did the four-finding broad review correct?

**Inputs:** test-only overrides change copied values immediately after the
real provider/function read. The helper asserts that the requested axis
actually changed and all other hooked axes did not, before the caller may
assert downstream differences. Every probe snapshots the surface projection,
including terrain elevation/ocean/unrest and climate temperature, moisture,
storm propensity, cached current, and `BiomeExpr`.

| Isolated input | Perturbation | Observed dependency and preserved quantities |
|---|---|---|
| Mean temperature | +10 °C at each consumed vertex | Ambient temperature rises by 10 °C; all other fields and all territories remain equal |
| Moisture | Halve each consumed value | Ambient moisture and altitude-derived cloud water fall; distribution scores change; unrelated ambient fields and drawn altitudes remain equal |
| Terrain elevation | +500 m at the score input | Distribution scores change; independently drawn altitude and all ambient fields remain equal |
| Storm propensity | Halve the score input | Distribution scores change; independently drawn altitude and all ambient fields remain equal |
| Prevailing wind | Reverse the vector | Ambient wind reverses; shear magnitude and unrelated fields remain equal; a fixed drifting territory changes its next surface |
| Derived ocean current | Reverse the vector | A fixed current-following territory changes its next surface; origin, phenotype, stocks, genesis sample, and sampled altitude remain equal |
| Habitat altitude | 4,000 → 8,000 m | Temperature, pressure, density and moisture fall; radiation and aether rise; wind changes; lapse rate and astronomical forcing remain equal |

The movement tests hold mobility, identity, starting footprint, origin and
stocks fixed in both arms. They consume the real circulation functions over
the same generated substrate. Radiation/aether isolation and the pure edifice
score test remain in the existing suites.

**Input non-results:** `GeneratedClimate::current_at` is not read by
`next_surface`; it derives `ocean_current` from the mesh, ocean mask and
band count instead. No cached-current independence is claimed. Temperature is
currently a profile input, not a stock/distribution input. Terrain elevation
does not set the independently drawn habitat altitude. These experiments
isolate the Skyworld consumption seam, not the upstream physical consistency
of a hypothetical regenerated climate. The renamed
`ocean_fraction_changes_climate_and_overlay_together` remains a coupled
integration probe.

**Work:** `bounded_work_counts_generation_and_rendering_at_their_loops`
asserts these values on the 40,962-vertex seed-42 unit fixture. The counters
increment at actual loop bodies; index counts increment at the actual
construction sites. Index-internal visits are not counted.

| Configuration | Territories | Total samples | Surface-scan visits | Propagation indexes | Movement candidates | Physical neighbor visits | Expansion neighbor visits | Adjacency-pair checks |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| Inactive, fraction 0 | 0 | 0 | 81,924 | 1 | 0 | 0 | 0 | 0 |
| One vertex ceiling, 2 samples | 1 | 2 | 122,886 | 1 | 6 | 0 | 240 | 2 |
| Fraction 0.10, 2 samples | 2 | 4 | 122,886 | 1 | 12 | 7,514 | 19,824 | 8 |
| Fraction 0.10, 8 samples | 2 | 16 | 122,886 | 1 | 84 | 30,063 | 79,296 | 32 |

The one-territory case sets fraction to `1.5 / vertex_count`, producing a
one-vertex ceiling. All cases use propagation radius 2. Changing the fraction
also changes footprint size, so the one-versus-two-territory comparison is
not an isolated measurement of per-territory cost. The two-versus-eight-sample
comparison holds territory count and coverage configuration fixed.

| PNG detail, two territories/eight samples | Surface-map visits | Render indexes | Raster pixels | Overlay marks | Occupied-set input visits | Stamp candidates |
|---|---:|---:|---:|---:|---:|---:|
| Planet | 81,924 | 1 | 32,768 | 736 | 2,598 | 724 |
| Regional | 81,924 | 1 | 32,768 | 2,616 | 2,598 | 1,060 |
| Habitat | 81,924 | 1 | 32,768 | 2,616 | 2,598 | 1,060 |

Each row is one raster and zero generation samples or propagation indexes.
The original zero-terrain/zero-climate-reconstruction probes remain green.
Regional and Habitat PNGs change palette, not traversal counts.

**Cost scope:** generation retains fixed substrate work, including score
sorting; each PNG builds two full surface maps plus an index. That is not a
terrain/climate reconstruction, but it is real planet-size work per render.
Footprint work depends on retained body size and expansion radius as well as
samples; a sufficiently large radius can cover the globe. Adjacency compares
territory pairs per sample (including the self check), and corridor membership
can scan a sample-derived route. The counts do not establish a universal
linear bound or exclude every possible future uninstrumented allocation.
Index internals, sorting, set/map comparisons and copying, corridor membership,
ordinary/diagnostic string formatting, and PNG encoding are outside these
totals. No dense runtime planet-by-time structure or cache was introduced.

**BuildDepth:** the readable source contract now keeps every nonempty,
non-documentation line and compares it with the four approved unit variants.
It rejects unknown syntax rather than filtering it away. Regression cases
cover a new unit variant, tuple and struct payloads, an assignment, a missing
trailing comma, and an attributed payload variant.

**Documentation:** every completed Task 1–3 step is checked in the plan and
local task briefs. Parent ledger #10 now acknowledges the production
`score_distribution_environment` extraction at `4d9f3005b`. Plan, spec,
briefs, this ledger and reconciliation point to the same evidence and limits.
The local briefs remain untracked execution material; this ledger is durable.

**Red evidence:** the new BuildDepth case failed on `Sky(u8),`; the identity
perturbation helper failed with `VACUOUS: Temperature source never changed`;
the uninstrumented propagation-index counter failed with actual 0, expected
1. Hooks/counters were then implemented and the unit suite passed. The
counter tables above are asserted values, not print-only measurements.

**Verification:** final correction checks passed:

- `cargo test -p hornvale-worldgen --lib --test suite -- skyworld`: 10 unit
  tests and 35 integration tests passed, zero failures.
- `cargo test -p hornvale --test suite -- docs_consistency`: 41 passed,
  zero failures.
- `HV_TIMINGS_LEDGER=/tmp/skyworld-review-fix.p5jXVV/timings.md timeout --foreground 3600s make gate-commit`:
  exit 0, wall 119.546 s; formatting, workspace Clippy, audits and report
  freshness passed; all four subfloor chunks passed (1,425 / 1,394 / 1,305 /
  362 test executions). The timeout is 3,600,000 ms. Gate output is retained
  in `/tmp/skyworld-review-fix.p5jXVV/gate-commit.log`.

The existing timing override records this run separately so the pre-existing
`docs/timings.md` drift stays byte-identical and unstaged.

**Explicit non-results:** no census, new realm, lifecycle mutation, organisms,
species, tethering, save fact, build rung, public runtime diagnostic API, or
new seeded draw. Broad review is approved; #6 records local close preparation.

## #6 [G6 preparation] — What survives the close?

The closing handoff approves the broad review of `c1a20ce71`. The dedicated
ledger and parent ledger were read end to end, including all post-G3 entries.
The retained direct-input boundary and rejected wrapper/cache/universal realm
are captured in spec §§3, 6 and 10 and the chronicle; the full input matrix
and measured limits remain in #5. Approval does not imply a canonical gate.

**Backfilled deferred minor:** Task 3's scratch review recorded that exact
printed counts were neither independently asserted nor durable evidence.
`2a398be5e` fixed that, and `c1a20ce71` extended the counters. The explicit
minor disposition was missing from the contemporaneous ledger; it is now
**fixed**, with the process miss recorded in the retrospective's “Scratch
routing and deferred minors” section. The root-export review chain likewise
survives there rather than only in scratch diffs.

**Durable routing:** `docs/retrospectives/skyworld-seams.md` names every
scratch review outcome and routes broader follow-ups to permanent registry
IDs. `book/src/chronicle/skyworld-seams.md` carries the product account and
measurement limits. No scratch-only ruling is a prerequisite to reading the
result. Local task briefs now point to the completed plan and this record.

The routing audit's concrete locations are in
`docs/retrospectives/skyworld-seams.md`: Task 1 source/selection failures at
line 12, Task 2 isolation and production-extraction corrections at line 18,
Task 3 work evidence at line 28, Task 4 source-contract fixes at line 39,
D4 reintegration at line 48, and scratch/minor/follow-up dispositions from
line 64. These locations carry the outcomes, not just links to scratch.

Close-walk disposition:

1. Local `git merge-tree --write-tree origin/main HEAD` succeeds; D4's
   reintegration at `b8c8d7393` and its chronicle were reviewed. Canonical
   compatibility must be checked again at submission.
2. Scratch and both durable ledgers reviewed; minor and follow-up routing is
   recorded above and in the retrospective.
3. Chronicle, SUMMARY, retrospective, architecture freshness and registry
   updates prepared. Confidence Gradient re-score is N/A: its consumer-cost
   and ecosystem bets are unchanged. The unrelated population/Lot
   `IMPLEMENTATION_PLAN.md` remains untouched. The seam plan is complete;
   the parent canonical stage remains explicitly pending.
4. No golden re-pin is needed for these documentation changes. Implementation
   report freshness and golden checks remain part of the local gate.
5. Seed-42 world fixture has no campaign delta. Refreeze is deferred to the
   actual merge boundary; no fixture is silently re-authored here.
6. Gate and headline preparation only. Push, enqueue, nudge and merge are
   excluded by the closing-preparation assignment.
7. Process lessons are durable in the retrospective; no separate private
   memory is needed to recover them.
8. Worktree removal and release are excluded by the assignment; the worktree
   remains allocated to `campaign/skyworld`.

**Fresh close verification:** `cargo test -p hornvale-worldgen --lib --test
suite -- skyworld` passed 10 unit and 35 integration tests; `cargo test -p
hornvale --test suite -- docs_consistency` passed 41 tests after fixing
duplicate close-artifact ownership in reconciliation. The local
`HV_TIMINGS_LEDGER=/tmp/skyworld-close.0rqp7j/timings.md timeout --foreground
3600s make gate-commit` exited 0 in 39.554 s; formatting, workspace Clippy,
audits/report freshness and all four subfloor chunks passed (1,425 / 1,394 /
1,305 / 362 executions). Log: `/tmp/skyworld-close.0rqp7j/gate-commit.log`.
`docs/timings.md` retained SHA-1 `000bb3a06f45d5b41b9b345127ffdcaeca7bb398`
and remains unstaged. Reconciliation's trailing empty TSV columns are schema
delimiters, not whitespace to trim. No canonical job was run by this closer.

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
