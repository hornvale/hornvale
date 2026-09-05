# 0727. Relation rank is set by whether a reversal is wanted

**Status:** Accepted (2026-09-04) · **Decider:** Nathan (autopilot) ·
**Campaign:** The Tenon · **Relates:**
[0726](0726-a-kind-to-kind-edge-is-derived-from-traits-both-kinds-carry.md)
(the derived edge),
[0697](0697-what-an-afforded-site-is-worth-is-a-property-of-the-sleeper.md)
(the inherited grade ceiling)

## Context

A species scalar multiplied by a surface scalar can make one sleeper care more
than another, but it cannot reverse two surfaces' order: if ledge beats rushes
for one species, it beats rushes for all. Such a rank-1 form is separable and
contains no information about the pair.

The named demand was an actual preference: one people should prefer the hard
surface while another prefers the soft one. An additive reconstruction of the
shipped ladder also needed an unexplained interaction constant to reproduce the
1.50 ceiling.

## Decision

**Choose relation rank by the strongest ordering the consumer must express. A
reversal requires rank 2.** Rest therefore carries one common magnitude and one
contrast on each side: surface hardness against a species substrate-response
curve.

Rank 1 was rejected because it cannot invert an ordering. Rank 3 was rejected
because no third contrast has a live consumer. The multiplicative fold was
chosen because it reproduces the inherited bed column without an authored
interaction constant.

## Consequences and costs

- The lower bound is behavioural, not aesthetic: P2 pins a live reversal in
  actual composed rooms.
- The upper bound is equally deliberate: a latent dimension with no consumer
  would be speculative vocabulary.
- Rank 2 cannot express an independent third preference axis. If a future
  consumer needs one, the relation must grow and its old rows be reviewed.
- Today's dwarves carry no subterranean distinction. The live reversal uses
  drow, whose authored `HabitatRealm::Subterranean` row supplies the contrast,
  rather than inventing a dwarf trait merely to preserve an example sentence.

## See also

`domains/species/src/lib.rs` (`HabitatRealm`, `SubstrateResponse`);
`windows/vessel/src/liveness.rs` (the reversal witness);
[The Tenon chronicle](../../book/src/chronicle/the-tenon.md).
