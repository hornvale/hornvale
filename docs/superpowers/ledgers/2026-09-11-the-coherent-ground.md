# The Coherent Ground — decision ledger

Campaign: **The Coherent Ground** — deterministic facet-scale realization for
the Planetarium's terrain and surface detail.
Branch: `campaign/the-coherence`. Autopilot is engaged; G3 design review and
G6 close remain hard stops.

**Current status:** G2 design direction approved in conversation; design
package is prepared for G3 review. No implementation has started.

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

