# The Coherent Ground — visible integration amendment ledger

Campaign: **The Coherent Ground**, amendment dated 2026-09-12.
Parent ledger: `docs/superpowers/ledgers/2026-09-11-the-coherent-ground.md`.
Status: G3 design approved; implementation plan written; implementation not
yet started.

## #1 [G5] — What did the first stage-gated implementation actually prove?

**Decision:** Treat the first implementation as a valid contract slice, not
as a completed visual campaign. Its green stage gate proves deterministic
facet data, source transport, Bevy conversion tests, and proof-fixture
construction. It does not prove that normal Planetarium rendering activates
patch entities, that facet geometry departs from interpolation, that
between-vertex features remain visible, or that a rendered frame changed.

**Why:** Astra's independent G6 review traced the ordinary lifecycle and
found the legacy globe path still owns normal rendering; it also found that
the proof supplied review booleans and measured conversion rather than a
rendered frame. The review's file/line anchors are retained in the campaign
conversation and the amendment spec.

**Alternatives discarded:** Closing on the green contract gate would make
construction evidence stand in for visible behavior. Requiring a global
Level-8 rewrite would solve neither the ownership nor proof gap and would
expand scope unnecessarily.

**Ideonomy passes / overturns:** One pass using negation, dimension
identification, and cross-domain re-instantiation; no overturn, but it
surfaced the distinction between patch existence and patch visibility as a
load-bearing invariant.

**Capture actions:** Reopen the campaign under the 2026-09-12 amendment spec
and plan; retain the earlier stage result as contract evidence only.

## #2 [G3] — How should the visible integration be repaired?

**Decision:** Add a camera-driven bounded patch catalog, source-owned
conditioned facet geometry, adaptive feature strips, and artifact-backed
before/after rendering proof. Keep Level 6 as macro authority, keep the
legacy globe as a loading fallback, and replace fallback coverage atomically
only after ready patches are visible.

**Why:** This directly addresses the four P1 findings while preserving the
existing layer ownership and deterministic revision boundary.

**Alternatives discarded:** Vertex-only semantic masks cannot guarantee
narrow-feature visibility. Renderer-invented rivers or biomes violate the
source ownership rule. A fragment-only shader solution is deferred as a
fallback because explicit strips are easier to inspect and test.

**Ideonomy passes / overturns:** One pass using the same tuple; no overturn.
The pass added explicit visibility, fallback overlap, and render-generation
dimensions to the design.

**Capture actions:** Spec `2026-09-12-the-coherent-ground-design.md`, plan
`2026-09-12-the-coherent-ground.md`, and this ledger record the decision.

## Follow-ups

- Record actual per-operation source, application, first-visible,
  steady-state frame, and RSS measurements during the rendered proof.
- Correct the parent ledger's stale opening/status when the amendment's
  implementation evidence is available; do not claim completion from the
  prior contract gate alone.
- Keep Living Surface work deferred: clouds, precipitation advection,
  currents, snow evolution, foam animation, and weather-qualified roughness.
- Reassess broader regions or additional refinement levels only after the
  artifact-backed proof supplies measured cost and a visual need.
