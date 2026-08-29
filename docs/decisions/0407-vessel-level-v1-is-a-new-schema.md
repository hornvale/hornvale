# 0407. `vessel/level/v1` is a new schema, not a reuse or a generalization

**Status:** Accepted (2026-08-28) · **Decider:** Nathan (autopilot) ·
**Relates:** [0406](0406-the-underworld-is-a-band-not-a-fold.md) (the band
this schema carries) · [The Gallery](../../book/src/chronicle/the-gallery.md)

In the context of the pane needing a wire document for the underground band,
we decided that **`vessel/level/v1` ships as its own schema**, accepting a
third spatial document (alongside `vessel/chart/v1` and `vessel/plan/v1`)
rather than one generalized shape all three bands squeeze into.

## Context

A cave level and a room plan share a coordinate grid, but a cave has no
chamber index, no colour channel (§4.1's visibility states are glyph-only by
rule, never a shade), and a palette keyed on `(kind, state)` rather than
`(kind, colour)`. Forcing one document to serve all three would either widen
`chart`/`plan` with fields only the third band uses, or make the underground
band lie about having chambers it does not have.

## The rule

`vessel/level/v1` carries `rung`, `depth_m`, `extent`, a `palette` keyed on
`(kind, state)`, a sparse `cells` list (only cells ever seen — a never-seen
cell is omitted, not flagged, §4.1.1), `you`, and `marks` (empty until §3.6
lands). `depth_m` is the document's only float and goes through the
established `quantize_serde` emit-boundary pattern.

## Consequences

- **A never-seen cell costs nothing to represent.** The sparse list, not an
  extent sweep, is what keeps a 2,040-cell rung's wire document small; the
  full accounting is decision 0409's bitset, which this document's `cells`
  is read from at emit time.
- **Client-side enforcement is a workspace-side test, not a client-side
  compile error.** `clients/game/core`'s own `Spatial` mirror does not depend
  on the sim (decision 0055), so nothing there forces a match arm when
  `SpatialChannel` grows a variant; `cli/tests/suite/client_band_coverage.rs`
  exhaustively matches the real `SpatialChannel` for exactly this reason —
  see the chronicle for why the spec's original claim that the compiler
  already enforced this was false.
