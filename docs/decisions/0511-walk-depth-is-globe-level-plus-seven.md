# 0511. Walk depth is `globe_level + 7`, chosen to preserve step length

**Status:** Accepted (2026-08-31) · **Decider:** Nathan (autopilot, spec §2.3) ·
**Relates:** [0506](0506-the-occupancy-lattice-is-a-cube-sphere.md),
[0287](0287-a-zoom-rung-is-a-mesh-depth.md) (whose "band B is depth 12" is
restated by this record, not contradicted in principle),
[0508](0508-a-diagonal-costs-root-two.md)

In the context of the walk band moving from icosphere triangles to cube-sphere
quads ([0506](0506-the-occupancy-lattice-is-a-cube-sphere.md)), facing a base
mesh whose faces are a different size, we decided that **walk depth is
`globe_level + 7` — depth 13 at the canonical globe level 6 — chosen because it
preserves the length of one step**, accepting that a number stated in sixteen
places across the tree had to move at once.

## Context

Measured, not assumed:

```
ICOSPHERE (before)                   CUBE-SPHERE (after)
 depth 12: 1.874 km side              depth 12: 2.251 km side
                                      depth 13: 1.126 km side
 effective step before = 1.08 km centre-to-centre
 (measured 1.083 / 1.107 km alternating, seed 42)
```

Keeping the offset at `+ 6` would have put the walk band at depth 12 on the new
base and **silently doubled the ground covered per step**. Nothing would have
gone red: `windows/vessel/src/clock.rs`'s authored 0.1-day `MoveTo` and every
duration calibrated against it are stated in TIME, so a step that covers twice
the ground still costs the same tick count and simply makes a body twice as
fast. That is the same class of error 0508 exists to close, arriving through a
different door.

## What was decided

- **`walk_depth(ctx) = ctx.globe_level() + 7`**, and the criterion is step
  length rather than a round number.
- **It is stated ONCE, in `windows/locale/src/lib.rs`.** `hornvale_vessel::
  walk_depth` is a `pub use` of it, not a second definition. This crate is the
  bottom of the chain and the owner of both halves, so it is the only place the
  function can be called from by everything that needs it.
- **A restatement is a defect, and is scanned for.** The campaign found
  **sixteen** sites restating `globe_level() + 6` rather than calling the
  function — including two production `--depth` defaults that had silently
  fallen a whole band behind. `cli/tests/suite/walk_depth_agreement.rs` is a
  whole-tree completeness scan that fails when an offset appears anywhere its
  roster does not account for.

## Consequences

- **0287's "band B is depth 12" is now depth 13**, and 0287 is append-only, so
  this record is where a reader who finds that sentence should land. Its
  principle — a zoom rung IS a mesh depth, and the ladder is
  `depth - globe_level` rungs — is unaffected; only the arithmetic moved, and
  the ladder is seven steps rather than six.
- **An absolute depth literal is invisible to the offset scan**, and that
  blindness arrived in practice at a count nobody guessed: eleven `const WALK:
  u32 = 12;` fixtures across `windows/vessel` went stale at once, carrying no
  `globe_level()` for the offset arm to see and absent from the opt-in roster
  the absolute arm reads. A second arm now covers every constant named exactly
  `WALK`; a fresh inline literal and a differently-named constant are still not
  covered, and `walk_depth_agreement.rs` discloses that rather than implying
  otherwise.
- **What we give up:** one more rung on every ladder expressed as
  `depth - globe_level`, and 20% more rooms at the walk band — `6·4^13` =
  402,653,184 quads against `20·4^12` = 335,544,320 triangles, a factor of
  exactly **1.2**. (Staying at `+ 6` would have gone the other way and lost
  70% of them: `6·4^12` is 100,663,296, which is the doubled step length
  stated as a count.)

## See also

`docs/superpowers/specs/2026-08-30-the-pavement-design.md` §2.3;
`windows/locale/src/lib.rs` (`walk_depth`);
`cli/tests/suite/walk_depth_agreement.rs`.
