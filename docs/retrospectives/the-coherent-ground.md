# The Coherent Ground — retrospective

Process, not product. The product is in
[the chronicle](../../book/src/chronicle/the-coherent-ground.md).

## The estimate changed shape

The campaign began as a facet-realization idea and had to be reopened after a
green contract slice failed to prove that normal Planetarium rendering used
the new geometry. The revised campaign shipped four implementation stages
through a sequence of small commits. The largest surprise was not terrain
math but the proof boundary: a readiness boolean, an entity count, or a PNG
digest is not evidence that the intended pixels were rendered.

The final focused proof measured 3,363 ms source generation and 1,576,468,480
bytes peak RSS on the local Metal run. The feature catalog contained 44,831
entities and the causal PNG comparison found 1,136 changed feature pixels.
These are observations, not budgets. The substantial mesh/application cost
is why broader coverage and Level 8 remain deferred until a larger measured
region demonstrates a visual need.

## Review failures were useful only after being made falsifiable

The first Task 4 proof passed while falsely reporting that fallback geometry was
absent, because it inferred visibility from readiness. It also counted feature
entities instead of feature pixels, sampled RSS once, and copied camera and
revision metadata into both records. Independent review found all four.

Repair rounds then exposed a second fallback mistake: hiding the whole globe
made uncovered regions disappear. The final handoff reads ECS visibility,
keeps the fallback for uncovered regions, suppresses only covered terrain with
positive depth bias, measures causal PNG changes with features toggled, reads
serialized frame metadata back independently, and uses `getrusage` for
process peak RSS. This is the durable lesson: visual claims need an observable
that can fail for the exact visual defect, not a nearby lifecycle proxy.

Task 3 supplied the same lesson from performance. Adaptive strips initially
reconstructed all rills for every sample and made a valid source request hang.
A deterministic spatial candidate index and cached expanded segments reduced
the final bounded segment visits to 38 and brought the valid request to about
1.93 seconds. The optimization preserved a bit-exact oracle, but it arrived
only after treating the hang as a correctness problem and profiling the real
lookup path.

## Close routing and deferred minors

The parent ledger's opening status lagged the implementation and was corrected
with the rendered proof evidence. The durable amendment ledger was read in full;
its follow-ups now have explicit homes:

- Dynamic clouds, precipitation advection, currents, snow evolution, foam, and
  weather-qualified roughness remain deferred to a Living Surface campaign;
  see the registry row `RENDER-living-surface`.
- Broader regional coverage and any Level-8 macro experiment remain deferred
  pending a larger artifact-backed measurement; see
  `RENDER-broader-ground-refinement`.
- The exact patch transport remains derived and outside the save format; this
  is now the shipped protocol boundary, not an unresolved save-format change.
- The scratch progress record contains no additional deferred/minor/blocked or
  follow-up finding. All review findings were fixed in later commits and are
  represented above rather than left in ephemeral SDD material.

No Confidence Gradient score was changed: this campaign strengthened the
bounded Planetarium rendering witness, but did not claim a new general
simulation or gameplay capability. No new terrain epoch, pin, save-format
field, or census golden was introduced. The branch remains active until the
Sluice validates and lands its merge product; this close package does not
submit the branch.

## Explicit close decisions

- `IMPLEMENTATION_PLAN.md`: N/A as a tracked artifact; it is intentionally
  ignored and absent. The durable plan under `docs/superpowers/plans/` records
  completion.
- Keystone refreeze: N/A on this pre-Sluice branch; it belongs to the merge
  product at the queue boundary.
- Census regeneration: N/A; no census golden moved, and local census execution
  is prohibited by repository policy.
- Open-questions re-score: N/A; no existing confidence-gradient bet changed
  status, though the bounded witness is documented in the Planetarium and
  refinement chapters.
