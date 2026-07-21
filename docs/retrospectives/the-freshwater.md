# Retrospective — The Freshwater

Process lessons from a two-repo campaign (sim producer + wasm release + client),
whose data already existed and only needed carrying out.

## What worked

- **A pure read is the cheapest safe save-format change.** Adding water to two
  save-format-class schemas sounds risky, but because it is a *read* of the
  already-built terrain provider — no new seed draw, no re-sculpt — the
  determinism contract held for free: the stream manifest never changed, and the
  whole-branch review confirmed byte-identity trivially. When enriching the scene,
  emit from the built provider, never re-derive; the risk collapses.

- **Additive-append + quantize-at-emit made the schema change a non-event.** The
  scene's existing stability contract (append, never reorder) + the existing
  `quantize_serde` boundary meant the new fields dropped in with a verified
  purely-additive golden diff and no epoch. Reuse the contract the schema already
  carries rather than inventing versioning.

- **Determinism instinct in the containment helper.** The waterfall-in-region
  test was written transcendental-free (the algebraic inverse of the cube-sphere
  projection — dot products + a division) rather than via lat/lon bounds, so it is
  cross-platform byte-identical. The implementer reached for the exact,
  platform-stable form unprompted; that instinct is what keeps the substrate
  deterministic.

- **Data-verification substituted for a flaky visual.** The seed is ocean-heavy,
  so zooming the map reliably lands on water and a clean river screenshot is luck.
  Rather than re-roll indefinitely, a direct count of the scene (624 river tiles,
  44 lakes; 96/24 above the globe thresholds) *proved* the water is present and
  the thresholds sane. When the visual is luck-dependent, measure the datum.

## What to do differently / carry forward

- **Registry ids at CLOSE, again.** MAP-61, -62, -63 were all claimed by parallel
  campaigns during this one; the id was computed only at close (MAP-64). This is
  the second campaign in a row to pay the collision tax — it is now simply the
  rule: never write a registry number before the branch meets current main at
  close.

- **The wasm release is atomic with the client.** The client reads fields the
  deployed wasm must emit, so cutting the `world-wasm-vN` release, re-pinning the
  Orrery, and deploying the client are one indivisible step — deploy the client
  against the old pinned wasm and the live site breaks. Sequence the producer
  first (unblocks local dev via a locally-built wasm), the release last (with the
  client). This campaign built the wasm mid-flight to unblock development but held
  the *versioned release* to the close.

- **The map's land-luck is a real UX gap.** The sub-camera lands on ocean often;
  a "snap to nearest landmass" on handoff would make both zoom-in and every future
  demo reliable. Promoted from The Cartographer's followups — it bit again here.

## Follow-ups
- Waterfall rendering (icons on the map / markers on the globe) — sparse; seed 42
  has none at globe resolution; needs grid-index emission or a globe marker tier.
- Land-snap on map handoff.
- Vector rivers from an emitted downstream flow-graph (smooth polylines vs cells).
- River/lake naming (a language-domain tier).
