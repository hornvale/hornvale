# The Coherent Ground — decision ledger

Campaign: **The Coherent Ground** — deterministic facet-scale realization for
the Planetarium's terrain and surface detail.
Branch: `campaign/the-coherence`. Autopilot is engaged; G3 design review and
G6 close remain hard stops.

**Current status:** G2 design direction approved in conversation; Astra review
incorporated; revised design package awaits G3 approval. No implementation has
started.

## #1 [G1] — What problem is this campaign solving?

**Decision:** make the Planetarium's local surface a coherent realization of
the existing Level-6 macro world, rather than a visual interpolation of its
~41K authoritative vertices. The realization may invent plausible facet-scale
detail, but it must respect macro elevation, climate, drainage, catchments,
coasts, and mountain structure.

**Why:** the audience needs a convincing world, not geological correctness.
The perceptual failures that matter are visible discontinuities: rivers that
vanish or appear without a source, confusing flow direction, overly straight
coasts, abrupt biome boundaries, and mountain forms that do not read as ranges.

**Alternatives discarded:** a global Level-8 macro rewrite is too disruptive
for the first proof and does not by itself guarantee semantic continuity;
pure interpolation preserves seams but cannot add meaningful local structure;
GPU-only noise can improve appearance but cannot safely invent rivers, coasts,
or biome semantics.

**Ideonomy passes / overturns:** three successful passes using negation,
cross-domain reinstantiation, substitution, lattice, notation, timeline,
cycle, and dimension analysis. No recommendation overturned. The latest pass
confirmed two separate concerns: autonomous/lazy realization and cyclic visual
review/regeneration. It did not change the source-owned semantic realization
versus renderer-owned microappearance boundary.

**Capture:** the approved architecture is in §§2–7 of the design spec.

## #2 [G2] — What is the implementation boundary?

**Decision:** Campaign A builds a deterministic, lazily addressed facet patch
system for coherent ground: terrain relief, channel and tributary geometry,
banks and floodplains, shelves and coastlines, ridge structure, and blended
surface materials. It also adds Planetarium patch scheduling, caching, and LOD
consumption. Dynamic clouds, precipitation, currents, snow evolution, foam,
and weather are deferred to a later Living Surface campaign.

**Why:** ground coherence establishes the spatial contract that all later
weather fields must consume. Keeping living weather out prevents a second
independent system from inventing incompatible surface boundaries.

**Alternatives discarded:** implementing every cloud and water phenomenon in
the first slice would obscure whether facet addressing and seams work;
changing the canonical Level-6 world or save format would expand the blast
radius without improving the first visual proof; making renderer code the
owner of semantic rivers or biomes would prevent non-rendering observers from
using the same realization.

**Capture:** scope, non-goals, and the proof slice are in §§3, 8, and 10 of
the design spec.

## #3 [G2] — How are facets realized without creating seams?

**Decision:** address each local patch by
`{world_revision, macro_face, refinement_level, time_bucket}`. The patch is a
deterministic function of Level-6 macro authority, neighboring macro context,
the existing hydrology/channel skeleton, and stable labeled noise. Refinement
levels subdivide a macro face locally; they do not promote the whole planet to
a new canonical macro level.

Every patch emits shared-boundary-compatible geometry and continuous fields.
Channels must connect to a parent, outlet, lake, or ocean; branch discharge
cannot exceed its parent; a channel may not cross a macro basin divide merely
because local noise asks it to. Categorical biome labels are rendered from
continuous material weights.

**Why:** stable patch keys make lazy generation and caching safe, while
boundary conditions make adjacent independently generated patches agree. The
hydrology skeleton supplies semantic continuity; noise supplies believable
local variation without becoming a second drainage authority.

**Alternatives discarded:** independently seeded patches create visible seams;
subdividing only scalar height leaves rivers and materials disconnected;
allowing local noise to reroute water produces plausible-looking fragments
that fail at patch boundaries; storing all refined facets globally defeats
lazy regional detail.

**Capture:** the patch contract and continuity invariants are in §§4–7 of the
design spec.

## #4 [G3] — Astra review and revision resolution

**Finding:** the architecture met the perceptual goal, but five areas were too
aspirational for implementation: hydrology topology was conflated with bed
geometry; same-resolution seams did not cover mixed LOD; narrow features could
fall between patch vertices; composition ownership and revision behavior were
underspecified; and the proof/scope boundary left cloud hooks and performance
acceptance vague.

**Resolution:** retain the Level-6 authority plus lazy facet realization, and
make the missing contracts explicit. The revised spec separates inherited
macro routing from realized bed geometry; defines headwaters, confluences,
terminal basins, outlets, mouths, continuation tokens, and bed-profile tests;
separates feature identity from sampling resolution and adds canonical
edge/corner evaluation, mixed-LOD stitching, parent-child preservation, and
refine/coarsen tests; requires source-owned curves, fields, or adaptive
geometry for narrow features; names `windows/worldgen` as composition owner and
`clients/visual/source` as protocol owner; defines revision/configuration
identity, stale-reply rejection, stream versus hash-label behavior, and byte
versus numerical comparison rules; removes cloud/precipitation hooks; and
expands the fixture and before/after acceptance set.

**Why:** these changes directly target the audience's visible failure modes and
prevent the earlier facet mistake of passing local one-step checks while
creating globally incoherent flows. They preserve the approved campaign scope
and defer Living Surface weather.

**Alternatives discarded:** rejecting the campaign in favor of a global Level-8
rewrite would increase cost without solving semantic continuity; accepting the
original wording would leave the implementation to rediscover the same seam
and routing ambiguities; moving semantic curves into Bevy would violate the
simulation ownership boundary.

**Ideonomy passes / overturns:** three prior G1/G2 passes; Astra's independent
review added no new campaign branch and overturned no approved direction. Its
findings were incorporated as explicit contracts and acceptance tests.

**Capture:** revised §§3–10 of the design spec and this ledger entry. The
reconciliation row records the Astra revision review.

## Follow-ups

- Decide the exact patch serialization/transport shape at implementation
  planning; keep it derived and out of the save format unless a later decision
  explicitly changes the epoch boundary.
- Define the minimum macro-neighbor context needed to condition a patch and
  test it against face corners, coast crossings, and basin divides.
- Pressure-test the existing channel/rill machinery as the facet realization's
  skeleton before adding a new hydrology authority.
- Add a later Living Surface campaign for dynamic clouds, precipitation,
  currents, snow, water motion, and weather-qualified roughness.
- Measure patch-generation cost on the first proof slice before considering
  broader refinement levels or any Level-8 macro experiment.

## Task 7–8 proof evidence

- Proof seed/region: seed 42, with five required live cases: confluence,
  terminal basin, coast crossing, cube-sphere face corner, and unequal-LOD
  adjacency. The proof exercised 13 live operations, including both fine
  addresses, both coarse transitions, all three corner neighbors, and Bevy
  mesh/material application.
- Acceptance checks: source-owned coherent ground, continuous seams and
  coasts, declared river terminals, directional ranges, blended material
  transitions, and dynamic weather explicitly deferred. No cloud,
  precipitation, save-format, epoch, or new-stream interface entered this
  campaign.
- Measurements: the proof records per-operation source generation latency,
  Bevy mesh/material application time, and maximum before/after process RSS.
  The focused proof suite took 110.036 s wall time (5 tests, 2 slow live
  tests); no unsupported performance target is claimed. The evidence supports
  retaining lazy refinement and deferring any Level-8 macro expansion until a
  separate campaign supplies a broader measured region and an explicit visual
  need.
