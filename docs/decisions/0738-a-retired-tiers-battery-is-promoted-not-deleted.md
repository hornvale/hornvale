# 0738. A retired tier's battery is promoted, not deleted

**Status:** Accepted (2026-09-04) · **Decider:** Nathan (autopilot, spec §7) ·
**Relates:** [0736](0736-the-provider-tier-doctrine-is-retired.md)

In the context of *The Zenith* retiring astronomy's coarse provider, facing a
cross-tier battery whose original topology no longer existed, we decided that
**when a coarse tier retires, its cross-tier battery is promoted to a
conformance battery over the surviving provider** — accepting that the test
loses its provider-to-provider comparison while preserving the claims that
comparison existed to enforce.

## The general rule

A retired tier does not make its constraints disposable. Identify the
behavioural promises the coarse tier supplied, freeze those expectations as
test-owned literals, and assert them directly over every relevant regime of
the surviving provider. Delete the battery only when its claims, rather than
merely its old fixture, are obsolete.

The literals must not be sourced from production code inside the crate under
test. Otherwise implementation and expectation can drift together and the
battery will certify the change it was meant to detect.

## The astronomy instance

The former refinement battery asserted that each sky has exactly one day-sky
sun, nothing outranks it, the sun remains in the visible bodies, and any
period it carries is the calendar's day. Those are now direct sky-conformance
claims across seeds, rotation regimes, and hours.

The old battery read the sun's kind, salience, and rendered name from
`ConstantSun` itself — production code inside `hornvale-astronomy`. The
promoted battery freezes those expectations as literals (`celestial-body`,
`1.0`, and `"the sun"`) so a production change cannot rewrite its own oracle.

