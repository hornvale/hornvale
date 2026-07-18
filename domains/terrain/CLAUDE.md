# CLAUDE.md — working in `domains/terrain/`

Terrain is the most byte-identity-sensitive domain: it draws plates, cratons,
and rifts, then sculpts elevation through many stages of full-precision
floating-point. A one-ULP change anywhere ripples into the elevation map, the
biome classification, the coastlines, and every downstream census. Read
`domains/CLAUDE.md` first; this is the terrain-specific map.

## The pipeline (who feeds whom)

- `plates.rs` — plate skeleton + `assign_plates` (per-cell, edge-noised).
- `crust.rs` — drawn cratons, the stateless crust fields, `CrustField` +
  `strongest` (the per-cell winning-craton query). `sphere_fbm01` /
  `SphereFbm` live here.
- `rift.rs` — rift-and-fit: seams, clipping (`clip_over_seams`,
  `seam_side`). Gated to run only near seams.
- `elevation.rs` — `assemble_elevation`/`generate_elevation`: isostasy,
  boundary profiles, seamounts, fBm relief.
- `carve.rs` — sculpting epoch v3 (decision 0056): incision, sediment
  routing, deltas (`deposit_wedge`), barriers (`raise_barriers`), the marine
  `trim_to_sea`. `carve` is the orchestrator; `CarveParams` are the tuning
  constants.
- `globe.rs` — assembles the whole thing into a `Globe`.

## Byte-identity discipline (this is where it lives or dies)

- **`sphere_fbm01` compresses variance toward 0.5** — the `crust.rs` comment
  block explains why the raw fBm is rebalanced. Don't "simplify" the tanh
  rebalance or the three-slice mean; they are calibrated.
- **Two branches, not one with a no-op multiply.** `CrustField::strongest`'s
  `None` (no-rift) branch is kept *instruction-for-instruction* identical to
  the pre-rift code, because even `x * 1.0` is not guaranteed a float no-op in
  every case. When you refactor here, preserve the exact arithmetic and
  accumulation order — verify with the artifact drift check, not just tests.
- **`SphereFbm`/`Fbm` are byte-identical to the free functions by
  construction** (same derived seeds, same accumulation order). If you add a
  per-cell noise consumer, build the sampler once above the loop.
- **Calibration constants** (`REBALANCE_GAIN`, `wave_cut_m`, `LOBE_*`,
  `CarveParams` defaults) were chosen from data sweeps against worst-case
  seeds (decision 0057). Their values are load-bearing; changing one is a
  retune, not a cleanup — and often an epoch.

## Verifying a change here

1. `cargo test -p hornvale-terrain` — includes `tectonic_properties.rs` and
   `carve_properties.rs` (pin-isolation, determinism, invariants).
2. **Artifact drift check** — regenerate and diff, because a byte change may
   pass every test yet move the committed elevation map / census:
   `SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh` then
   `git diff book/src/gallery book/src/reference book/src/laboratory docs/audits`.
3. If you changed a `pub` signature, `type-audit check` too (moving a tagged
   primitive staled its tag once this session — see the memory note).

## Streams

`streams.rs` lists the permanent seed labels. Retired labels (e.g.
`plate-kind`, superseded by `cratons` in epoch v2) are documented and never
reused. New draw → new label; changed behaviour on an existing label → `/v2`
epoch.
