# 0512. The cube-sphere projection is tangent-warped, and there is one of it

**Status:** Accepted (2026-08-31) · **Decider:** Nathan (autopilot, spec §2.0,
G3 flagged and resolved to one projection) · **Relates:**
[0506](0506-the-occupancy-lattice-is-a-cube-sphere.md),
[0033](0033-serialized-floats-are-quantized-for-cross-platform-determinism.md),
[0041](0041-libm-for-portable-transcendentals.md)

In the context of adopting a cube-sphere for the occupancy lattice
([0506](0506-the-occupancy-lattice-is-a-cube-sphere.md)), facing an obvious
projection that `windows/scene/src/region.rs` already used and that measures
**5.2x** area distortion — worse than the icosphere it replaces — we decided
that **the projection is tangent-warped, and there is exactly one of it in this
repository**, accepting the loss of the transcendental-free property in
exchange for a 3.7x reduction in distortion.

## Context

The naive map — `normalize(n + a·û + b·v̂)` — bunches cells at a face's corners
and stretches them at its centre. Measured by sweeping one face and converging
in `N`:

```
    N      naive    tan-warped        max/min CELL AREA across one face
    8     3.740x       1.276x
   32     4.856x       1.379x
  128     5.114x       1.406x
  256     5.155x       1.410x         naive -> 3*sqrt(3) = 5.196 (analytic)
```

The icosphere's own room-mesh spec (§13.4) records "centre children ~1.5-2x the
corner children". Adopting the naive cube map verbatim would have made the very
property this campaign exists to improve **strictly worse**, while the spec
claimed the opposite.

The warp is standard: `a' = tan(a·π/4)`, `b' = tan(b·π/4)`, then
`normalize(n + a'·û + b'·v̂)`. It measures **1.41x** — better than the
icosphere.

**The acceptance criterion is LOCAL and is met by three orders of magnitude.**
The 1.41x is a whole-FACE spread, between opposite corners of a cube face,
thousands of kilometres apart and never both on one screen. The local gradient
falls as `1/N`:

```
 depth  cells/edge   adjacent cells   21x21 viewport   whole face
     6          64        1.0236x          1.342x        1.397x
     8         256        1.0061x          1.115x        1.410x
     9         512        1.0031x          1.061x        1.412x
```

## What was decided

- **Tangent-warped, at the one place the parameters enter the projection.**
- **ONE projection, not two that agree today.** `region.rs` carried its own
  copy of the cube-face basis, `param`, `locate_on_cube` and `face_unit`.
  `Facet::containing`/`Facet::centroid` and `region.rs`'s tile sampler now both
  call straight into `hornvale_kernel::cube`; there is nothing left to drift,
  and `windows/scene/tests/suite/one_projection.rs` is what would have caught
  the day the copy diverged.
- **The transcendental-free property is knowingly traded.** `locate_on_cube`
  was dot products and one division, so it was byte-identical across platforms
  *by construction*. With the warp it calls `tan` and `atan`, which under
  [0041](0041-libm-for-portable-transcendentals.md) route through
  `kernel/src/math.rs`'s pure-Rust `libm` and are bit-identical across
  platforms by the same guarantee every other transcendental in this project
  already relies on. It is a weaker guarantee — a library contract rather than
  an arithmetic one — bought for a 3.7x reduction in distortion.

## Consequences

- **`scene/tiles-region/v1`'s tiles move.** The warp changes which ground a
  tile covers, and `clients/atlas` consumes that wire. Decision 0356 retired the
  external clients, so the cross-repo additive-or-versioned constraint that
  would once have forced a version bump has lapsed; what remains non-negotiable
  is that the two projections must not silently disagree INSIDE this repo,
  which is what the one-projection clause settles.
- **A future consumer may read a warped `(a, b)` back.** The inverse uses
  `atan` and is exactly as reversible; round-tripping is asserted across every
  face.
- **What we give up:** an arithmetic byte-identity argument, replaced by
  0041's library one. Also the ability to reason about a face's parameter grid
  as evenly spaced on the sphere — it is evenly spaced in the TANGENT plane,
  which is the whole point, and any code that assumed otherwise is wrong now
  rather than merely imprecise.

## See also

`docs/superpowers/specs/2026-08-30-the-pavement-design.md` §2.0;
`kernel/src/cube.rs` (`CUBE_FACES`, `face_unit`, `locate`);
`windows/scene/src/region.rs`; `windows/scene/tests/suite/one_projection.rs`.
