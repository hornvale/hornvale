# The Skyworld — decision ledger

Campaign: **The Skyworld** — a generated and rendered mobile habitat overlay.
Branch: `campaign/skyworld`. Autopilot is engaged; G3 spec review and G6
close remain hard stops.

## #1 [G1] — What is the Skyworld's foundational architecture?

**Decision:** use a sparse, seeded Skyworld habitat overlay over the existing
land and ocean surface, with an augmented temporal adjacency graph. Keep local
biome description separate from mobile habitat identity, movement, lineage,
and influence.

**Why:** the existing faceted biome model (`realm : formation : stratum`) is a
good local description but cannot alone represent moving territories,
historical scars, or changing adjacency. The overlay preserves the existing
world model while giving Skyworld and later Waterworld a common habitat
grammar.

**Alternatives discarded:** adding a large family of sky-specific `Biome`
variants would encode combinations as taxonomy; replacing the biome system
would be premature before Skyworld and Waterworld pressure-test the seams; a
planet-wide atmospheric simulation would exceed the campaign's scope and
performance budget.

**Ideonomy passes / overturns:** five successful passes across synchronization,
mutable identity, phenotype, footprint, and propagation. No recommendation
overturned. The passes added physical/exchange/influence/rendered footprints,
lineage memory, corridor propagation, and the distinction between stable
fields and sparse mobile habitats.

**Capture:** architecture is in §§3–5 of the design spec. Reusable
cross-realm habitat ideas are captured in the registry rows added with this
campaign.

## #2 [G2] — What is the first Skyworld specimen and scope?

**Decision:** generate and render one mature, free-drifting orchard descended
from a biological sky reef. Stop before individual organisms, populations,
culture, GOAP, tether engineering, mutable atmosphere, or co-evolutionary
simulation.

**Why:** the orchard exercises world-specific altitude ecology, aether and
radiation fields, plankton productivity, resource stocks, movement,
projection, influence, and rendering without requiring a full society or
agent simulation. Free drift makes tethering a later intervention rather than
a hidden prerequisite.

**Alternatives discarded:** beginning with a tethered orchard would hide the
natural movement and itinerary problem; beginning with an archipelago would
add split/merge/recombination before the single-habitat overlay is understood;
implementing organisms or cultures now would turn a generation/rendering
campaign into a general simulation campaign.

**Ideonomy passes / overturns:** five design passes; no overturn. The passes
added the sky-reef lifecycle, upper-atmospheric dual limitation, independent
stocks, and the stable-baseline/mutable-future seam.

**Capture:** scope and specimen are in §§2, 3.4, 4.4–4.5, and §10 of the
design spec.

## #3 [G2] — How should performance and determinism constrain propagation?

**Decision:** use coarse deterministic fields for continuous forcing, sparse
corridors for structured transport, and discrete events for rare changes.
Keep rendering and caching derived from the same deterministic inputs.

**Why:** this preserves meaningful wind, current, tidal, and migration
patterns without requiring all-cells/all-ticks fluid or particle simulation.
It also keeps stream consumption and output independent of render order or
cache state.

**Alternatives discarded:** full atmospheric integration is out of scope;
pure distance falloff cannot express wind/current corridors; a global dense
influence repaint would make the overlay's cost proportional to the whole
planet at every sample.

**Ideonomy passes / overturns:** one dedicated propagation pass plus the five
earlier passes; no overturn. The pass added the distinction between local
kernels, corridors, itineraries, events, and broadcasts.

**Capture:** performance, determinism, and testing requirements are in §§5,
7, and 8 of the design spec.

## #4 [Q] — What does Stage 1 store for trajectory and propagation?

**Ruling:** keep the brief's `trajectory`, `exchange`, and `influence` fields
in the public overlay model, but generate no trajectory samples, expanded
exchange envelope, corridors, or events in Stage 1. `trajectory` and
`SkyPropagation` therefore begin empty, while `exchange` begins as the
physical footprint; the later movement/propagation stage owns every derived
addition.

**Why:** the task brief makes those fields part of the intended interface,
while the implementer dispatch explicitly forbids implementing orchard
trajectories or propagation in this stage. Storage without derived behavior
preserves the interface and the stage boundary. If this ruling is wrong, the
cost is a small additive initialization change in the later stage, not a
changed draw or surface-biome epoch.

**Alternatives discarded:** omitting the fields would contradict the brief's
public type shape and force a later structural API change; deriving even a
one-sample route or influence kernel now would cross the explicit Stage 1
scope; optional renderer-owned state would violate the overlay's ownership
rule.

**Ideonomy passes / overturns:** one abstraction-lift matrix pass over
informational versus active state and externally driven versus autonomous
behavior; no overturn. The pass exposed the useful boundary as
"storage present / autonomous derivation absent," rather than treating type
existence and simulation behavior as one decision.

**Capture:** this ruling is local to Stage 1 and recorded here; no speculative
follow-up was created because the approved plan already assigns the absent
behavior to later tasks.

## #5 [G5] — What is materialized for a Task 2 trajectory query?

**Ruling:** materialize exactly `SkyWorldConfig::trajectory_samples` coarse
samples per active territory at generation, beginning at genesis and advancing
by one exact `WorldTime` standard day. `trajectory_at` is an exact keyed lookup
over those ordered samples and consumes no randomness. Each sample owns its
physical, exchange, influence, and adjacency readouts; the territory-level
physical, exchange, and local channels describe the genesis sample, while its
corridor and sparse event channels summarize the sampled route. Keep
`SkyPropagation` as the existing struct, enriching the `corridors` and `events`
entries with typed records rather than replacing the three-channel shape.

**Why:** the approved design requires cache-independent query order, bounded
work, explicit temporal adjacency, and no all-cells/all-ticks integration. The
configuration provides a sample count but no cadence or retained derivation
context, so an exact finite vector is the smallest honest contract. Per-slice
movement variation uses labels derived beneath the existing movement stream;
it cannot perturb Stage 1's sequential mobility draw. Bloom, storm, and
collapse entries are readouts only and never mutate lifecycle state. If this
ruling is wrong, the cost is an additive cadence/configuration change and
trajectory readout epoch, not a surface-biome or lifecycle mutation.

**Alternatives discarded:** retaining terrain, climate, and draw context inside
`SkyWorld` for arbitrary lazy queries would enlarge the compact overlay and
make cache behavior load-bearing; integrating every tick would violate bounded
work; storing only positions would force later renderers to recompute footprint
and adjacency semantics; replacing propagation with an enum would contradict
the Stage 1 public shape and make simultaneous channels impossible.

**Ideonomy passes / overturns:** one dimension-identification pass rendered as
a cardinality/longevity tree; no overturn. It separated finite configured
samples lasting with the generated overlay from unbounded lazy samples and a
dense permanent time field, and confirmed the finite branch as the only one
that preserves both compactness and cache independence.

**Capture:** the ruling is implemented in `skyworld_propagation.rs`; a future
need for non-daily samples is captured below as an explicit-cadence extension,
not as permission for opportunistic caching.

## #6 [G6] — How does the first Skyworld renderer expose the overlay?

**Ruling:** keep the ordinary renderer as a surface-plus-overlay lens with
three explicit detail levels. Planet detail shows coverage, centroids, broad
corridors, and sparse events; regional detail adds physical, exchange, local
influence, and routes; habitat detail adds phenotype, lifecycle, and stocks.
Atmospheric causes remain in a separate diagnostic readout. Rasterization is
fixed, equirectangular, nearest-vertex, and byte-stable; sparse geographic
stamps ensure a moving route sample changes only a bounded local pixel set.

**Why:** the renderer should make the Skyworld legible without repainting or
re-deriving the whole planet per moving territory, and ordinary observers see
consequences rather than hidden aether/radiation causes. This is additive to
the fixed land/sea surface and leaves future client-specific presentation open.

**Capture:** implemented in `skyworld_render.rs`; future clients may consume
the same detail contracts without changing generation or lifecycle semantics.

## #7 [G6] — What did the first implementation prove?

**Result:** Tasks 1–3 now generate and render a deterministic Skyworld
overlay. The focused suite exercises 27 Skyworld probes, including both land
and ocean coverage, minimal ceilings, environmental variation, bounded
trajectories, three footprint classes, explicit propagation channels, detail
separation, surface preservation, sparse render locality, and read-only
rendering. The local commit gate passes its 1,425-test subfloor across four
chunks, with clippy, type-audit, placement-audit, and plumb checks green.

**Non-results:** no lifecycle transitions, individual organisms, species
expansion, tethering, mutable atmosphere, archipelago recombination, magic,
technology, or census refresh were added. The reconciliation row remains
active until the queued stage gate and final campaign review.

## Follow-ups

- Run the first Skyworld implementation as a pressure test of the existing
  `BiomeExpr`/environment-axis seams before considering a sweeping biome
  rewrite.
- Decide at G3 whether sky is represented only by the overlay or also by a
  formal realm/formation projection.
- Coordinate the astronomy-to-climate forcing seam with The Wanderers after
  its merge; consume compact time-slice forcing rather than raw ephemeris
  internals.
- Preserve mutable atmospheric profiles as a later modifier layer over the
  stable generated baseline.
- Add an explicit trajectory-sample cadence to `SkyWorldConfig` if a later
  consumer needs non-daily slices; keep queries finite and generation-owned.
- Add a follow-up campaign for the multi-fragment sky archipelago: splitting,
  merging, inherited traits, recombination, and hybrid habitats.
- Add a follow-up campaign for tethering: natural anchors first, then
  permanent, semi-permanent, seasonal, engineered, and magical tethering.
- Add a co-evolution campaign for free-floating sky plankton, flora,
  pollinators, fauna, disease, and habitat structure.
- Use Waterworld as the second pressure test: turtle islands, submerging
  islands, reef lineages, currents, and living habitats.
- Generalize the overlay for Overworld and Underworld mobile/eventful
  habitats only after a second realm demonstrates the shared shape.
- Keep the ability-reservoir, actor/environment exchange, affordance
  discovery, GOAP experimentation, deception, and channel-specific detection
  ideas in their own future work; existing affordance precedents 0347–0349
  remain authoritative.
- Explore local calendars, spirits, animism, religion, and cultural memory
  only after the world can generate persistent, consequential phenomena.

## #8 [G3] — What should the next Skyworld slice pressure-test?

**Recommendation:** run a diagnostic seam-and-cost campaign before adding
organisms, species, lifecycle mutation, or tethering. Keep `BuildDepth` and
the save format unchanged while making the terrain/climate-to-overlay boundary
explicit, measuring artifact reuse, and testing which environment axes actually
propagate into Skyworld outputs.

**Why:** the first implementation already demonstrates the overlay shape. The
next uncertainty is whether the existing biome and environment system earns
its abstractions when the same surface feeds a mobile habitat. A measurement
slice is reversible and can expose an impedance mismatch before it becomes a
cross-realm API or epoch commitment.

**Alternatives discarded:** adding a new build-depth rung now would entangle a
derived overlay with the fact-committing ladder before its cost is understood;
adding species or lifecycle now would multiply consumers of an untested seam;
replacing `BiomeExpr` would be a big-bang rewrite without evidence; a universal
Sugar model would violate the approved worldly vocabulary.

**Ideonomy pass / overturns:** one organon-construction map pass using the
side-effect and modularity dimensions. It surfaced four dense regions — fixed
surface, atmospheric fields, habitat derivation, and observation — and the
most important border between them: an overlay that is technically modular
but accidentally rebuilds its substrate per observer. No overturn; the pass
added the explicit cost/ownership experiment and the cross-realm transfer
test.

**Capture:** design package at `docs/superpowers/specs/2026-09-09-skyworld-seams-design.md`.

## #9 [G5] — Does the existing composition boundary need a production wrapper?

**Decision:** retain the direct `skyworld_from(world, terrain, climate,
config)` typed inputs. Task 1 adds a test-only surface sampler and no
production `SkySurfaceSample` wrapper.

**Measured ownership:** `GeneratedTerrain` owns land/ocean, elevation, unrest,
boundary presence, and tectonic feature extents. `GeneratedClimate` owns mean
temperature, moisture, storm propensity, current, and `BiomeExpr`; the latter
remains the climate-owned taxonomy, not a Skyworld classification.
`skyworld_from` reads mean temperature and moisture into global `SkyFields`,
uses prevailing wind for wind and shear, and reads terrain elevation,
land/ocean/coastal state, moisture, storm propensity, and edifice state for
coverage and territory footprints. Propagation reads prevailing wind and ocean
current. Skyworld then derives territory phenotype, stocks, trajectories, and
footprints from those inputs; it does not write the surface providers.

**Evidence:** `cargo test -p hornvale-worldgen --test suite -- skyworld::seams`
passed 3 probes after the test-only sampler was implemented. The deterministic
`plates=2` → `plates=64` perturbation changed a sampled source tuple and the
resulting Skyworld output, while world-seeded pressure, aether, and lunar
forcing remained equal. `cargo test -p hornvale-worldgen --test suite --
skyworld` passed all 30 Skyworld probes.

**Rejected alternative:** a public read-only wrapper would only rename the
already explicit terrain and climate references. The sampler proves consumers
can identify source ownership without duplicating production derivation, so a
wrapper would add API surface without clarifying cost or ownership.

**Ideonomy pass / overturns:** one implication pass over the direct-input and
wrapper alternatives. It added the distinction between source ownership and
Skyworld-derived outputs; no overturn.

**Capture:** Task 1 seam inventory is committed with its focused probes. No
`BuildDepth`, artifact, save, stream, terrain-domain, or climate-domain change
was made.

## #10 [G5] — Do the existing environmental derivations propagate independently?

**Decision:** retain the existing pure Skyworld derivations unchanged. Task 2
adds a one-axis-at-a-time test matrix over radiation, aether, surface climate,
tectonic features, and altitude while preserving the read-only surface
projection.

**Why:** the focused seam filter passed all seven probes on the first run.
Radiation and aether independently change only their respective altitude
fields; terrain-derived climate changes reach temperature and moisture while
world-seeded aether remains stable; and the `plates=2` versus `plates=64`
fixtures change tectonic-feature count and derived coverage without adding a
fixed placement rule. An implementation change would therefore widen the
composition root without correcting an observed missing dependency.

**Alternatives discarded:** a Skyworld-local climate pin or replacement
`BiomeExpr` would duplicate climate ownership; direct tectonic placement would
turn environmental scoring into authored geography; a production seam wrapper
would repeat Task 1's rejected abstraction.

**Ideonomy passes / overturns:** one dependency-separation pass over surface,
high-sky, and tectonic axes; no overturn. It confirmed that altitude fields
are the independent high-sky input surface, while terrain pins remain the
appropriate source of climate and tectonic perturbations.

**Fix-round evidence:** the tectonic probe now finds a shared vertex whose
exact `hazard::has_edifice` value changes under the plate perturbation, then
requires its low-coverage Skyworld selection to change. Replacing the
production volcanic bias with zero makes that probe fail. The preservation
probe now regenerates both terrain/climate/Skyworld fixtures, snapshots each
terrain/climate/`BiomeExpr` projection around generation, and observes only
the dependent overlay climate fields change.

**Capture:** no `skyworld.rs`, `BuildDepth`, save fact, stream, domain, biome,
organism, lifecycle, or census change was required. The matrix remains in the
test-only seam sampler.

## Rejected for this campaign

## #11 [G5] — Does Skyworld reconstruct its already-built substrate?

**Decision:** retain the direct `skyworld_from(world, terrain, climate,
config)` construction and the current finite trajectory/rendering paths. Task
3 adds counter-backed unit probes only; no production cache, saved fact,
stream, or build-depth change is required.

**Evidence:** the initial integration counter probe red-compiled with
`E0425: cannot find value TERRAIN_OF_CALLS in crate hornvale_worldgen`, which
confirms that the reconstruction counters remain crate-private test
diagnostics. The probes therefore live in `skyworld.rs`'s unit-test module,
where they reset and read the existing counters without exposing runtime API.
Generation, all exact trajectory/propagation queries, and every PNG/ordinary
readout/diagnostic-readout detail path each recorded zero additional terrain
or climate constructions; repeated queries and renders remained identical and
left the generated `SkyWorld` equal to its snapshot. The focused cost command
printed `territories=2 short_samples=4 long_samples=16 sparse_records=1094
render_pixels=[32768, 32768, 32768]`: samples follow the requested two/eight
per-territory count, sparse state remains below the 5,136 vertex-by-time
comparison, and each detail materializes the fixed 256×128 requested raster
rather than a planet-by-time field.

**Alternatives discarded:** publishing counter reset/read functions would
make a test diagnostic part of the runtime API; retaining terrain/climate or a
random stream inside `SkyWorld` would make query order and cache state
load-bearing; adding a dense planet×time field would defeat the bounded-work
contract.

**Ideonomy passes / overturns:** one boundary-and-cardinality pass over
counter visibility, materialized samples, and fixed raster output; no
overturn. It confirmed that crate-local probes are the narrowest observable
and that the existing direct references already establish ownership.

**Capture:** `skyworld_generation_reuses_passed_substrate`,
`rendering_does_not_reconstruct_substrate`,
`queries_do_not_consume_randomness`, and
`detail_changes_materialization_not_generation` are unit probes; the focused
integration cost probe records the active territory/sample/pixel counts.

## #12 [G5] — Is bounded Skyworld work measured at actual loops?

**Decision:** replace Task 3's returned-state/`println!` cost claim with a
crate-test-only `SkyWorldWork` counter. It increments at each surface scan,
territory construction, trajectory-sample construction, raster invocation,
and raster pixel loop body. The counter is compiled only under `cfg(test)`,
has crate visibility solely for the propagation and renderer modules, and
neither changes runtime output nor exposes an API.

**Evidence:** the new unit probe first red-compiled with missing
`reset_skyworld_work`/`skyworld_work` helpers. Its green run independently
asserts the seed-42 fixture's zero-territory configuration performs zero
territory and trajectory work with 81,924 surface visits; its active
two-territory configurations construct exactly four (two-sample) and sixteen
(eight-sample) trajectory samples, each with 122,886 surface visits; and
three detail renders execute exactly three rasters and 98,304 pixel loop
bodies. These are counter values from generation/render loops, not sums of
the returned sparse state. The existing terrain/climate reconstruction
counters and their zero-call probes remain unchanged.

**Alternatives discarded:** summing `SkyWorld` records cannot observe
discarded intermediates; a public runtime diagnostic or cache API would widen
the production surface; a general allocation counter would measure unrelated
work and make the deterministic test host-sensitive.

**Ideonomy passes / overturns:** one combination pass crossing the work
observable's materiality (returned data, loop execution, runtime API) with
its reversibility (test-only, runtime, saved). It selected loop execution ×
test-only as the only narrow, direct, reversible seam; no overturn.

**Capture:** `skyworld::tests::bounded_work_counts_generation_and_rendering_at_their_loops`
is the deterministic report: it asserts every ledger count directly, with no
`println!` measurement claim remaining.

- A universal `sugar` renaming of world concepts.
- Individual plankton or full atmospheric particles.
- A single scalar habitat health value.
- A universal truth-revealing perception or `Detect Magic` mechanism.
- Authored mythology or authored superstition.
- Magic or technology as a current requirement for sky-island formation.
