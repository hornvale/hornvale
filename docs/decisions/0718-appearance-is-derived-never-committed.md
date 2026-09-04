# 0718. Appearance is derived, never committed, and never read back

**Status:** Accepted (2026-09-04) · **Decider:** Nathan (autopilot, spec §8.3) ·
**Relates:**
[0346](0346-an-affordance-is-derived-never-committed.md) (the same rule for a
different derived quantity — this record extends its shape rather than
restating its subject),
[0716](0716-a-view-consumes-spectra-and-owns-its-observer.md) (what a view
consumes), [The Wash](../../book/src/chronicle/the-wash.md)

In the context of *The Wash* letting a view read spectra out of the sim to
compose colour, we decided that **appearance is derived, never committed, and
never read back by sim logic** — accepting that a rendering may never become
a world-fact even when doing so would be convenient.

## The decision

No appearance quantity — reflectance, illuminant, signal, or any colour
derived from them — is written to the ledger, and nothing in `domains/` or
`windows/` branches on one. Appearance is computed from committed state on
demand and discarded.

## Why it needs a record rather than a convention

The licence for this work was that rendering does not affect the simulation.
A discipline that relies on nobody wiring it backwards will not survive
several campaigns: the backward wire is always locally convenient, and each
instance looks harmless. Making the rule structural means the licence stays
true without anyone remembering it.

The precedent is exact.
[0346](0346-an-affordance-is-derived-never-committed.md) made this ruling for
affordances, for the same reason: a derived convenience that becomes
committed state stops being derivable and starts being a thing to keep in
sync.

## Consequences

- **The acceptance test for any appearance work is byte-identity**: a world
  generated with the rendering path present is unchanged from one without.
- **That test is easy to write badly**, and *The Wash* wrote it badly first.
  Its original form asserted the world is byte-identical with and without the
  renderer — which cannot fail, because every method the view calls takes
  `&self` and the locale context never retains the world at all. A guard
  guaranteed by the type system is not a guard. **The test that means
  something asserts a committed artifact is unmoved**, and must be seen to go
  red under a mutation of the code it covers.
- Appearance may read anything committed. The prohibition is one-directional.
