# The Coherent Ground

**September 2026 · implementation complete; Sluice pending**

The Coherent Ground gives the Planetarium a local surface that is more than an
interpolation of Level 6's roughly 41,000 authoritative vertices. A bounded,
camera-driven patch catalog carries source-owned terrain realization into the
ordinary Bevy scene. Conditioned relief, inherited channel beds and ridges add
facet-scale structure without becoming a second drainage authority; adaptive
feature strips keep narrow rivers and ridges visible even when they miss the
terrain vertices.

The integration remains lazy and regional. Ready patches replace only the
covered part of the fallback globe, with depth bias and lifecycle identity
preventing gaps and z-fighting during mixed-LOD transitions. Revision,
macro-face, child path and render generation remain part of the stale-reply
contract, and the source boundary remains independent of the renderer.

The proof is rendered rather than inferred. A fixed seed and camera produced
distinct before/after PNGs through the production Bevy renderer; the after
capture contained one patch entity and 44,831 narrow-feature entities, with
causal feature pixels and the expected regional fallback behavior. The proof
records source generation, mesh/material application, first-visible and
steady-state capture costs, plus OS peak RSS. It establishes one convincing
camera-visible region, not global Level-8 macro authority or dynamic weather.

Clouds, precipitation advection, currents, snow evolution, foam, and
weather-qualified roughness remain a later Living Surface campaign. Broader
coverage or another macro refinement level waits for a measured visual need.
