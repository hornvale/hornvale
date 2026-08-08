# 0112. The synthetic route requires that the behaviour read committed facts

**Status:** Accepted (2026-08-07, G3) · **Decider:** Nathan · **Relates:**
[0093](0093-seed-hunting-is-not-a-test-mechanism.md),
[0108](0108-a-reachability-claim-is-a-census-coverage-table.md)

In the context of this campaign's own spec routing
`diachronic::a_crisis_fires_on_a_real_generated_sky` to a hand-built
synthetic world, on the model of decision 0093's own Stage 3
(`windows/worldgen/tests/doctrine.rs`'s `synthetic_flagship`) — and finding,
only when an implementer wrote code against the target function's actual
signature, that this was infeasible — we decided that **the synthetic route
is available only when the behaviour under test reads committed facts; a
behaviour that re-derives from a generated sky, a sculpted globe, or any
other live computation cannot be synthesized**, and recorded the fact that
this was found *after* the spec had passed G2 self-review, the G3 package,
and Nathan's approval.

## Why the crisis route fails the test

`observations_from` (`windows/worldgen/src/chorus.rs:1671`) opens with
`crate::sky_of(world)?` and refuses anything but a `crate::Sky::Generated`,
then derives its event list from real orbital mechanics via
`hornvale_astronomy::eclipse_events(sky.system(), sky.calendar(), from, at)`.
`crisis_from` (`chorus.rs:1891`) calls `observations_from` and additionally
requires ≥ `K_PREDICT` (8) witnessed events of one recurrence class and a
miss-run in the tail (`crisis_live`). None of that is a fact a test can hand-
commit: there is no `Fact` in the ledger that says "this culture witnessed
these eight eclipses" the way `doctrine.rs`'s synthetic world commits a
cult-form fact directly. The behaviour's precondition is a live derivation
from orbital state, not a lookup against the ledger — so a hand-built world
would have to *fake* a `Sky::Generated` carrying real ephemeris data
consistent enough to produce a genuine recurrence-class run, which is not
building a synthetic fixture, it is building a second generator.

`doctrine.rs`'s `synthetic_flagship` works precisely because `doctrine_from`
reads a **committed** cult-form fact — the exact shape 0093's decision
record describes: *"a hand-built world carrying the committed fact."* The
distinction this record makes explicit is that "carrying the committed
fact" is the load-bearing clause, not "hand-built world" on its own.

## The decision

Before routing a behaviour to the synthetic instrument, read the target
function's own preconditions. If the function's result is a pure read of
facts already in the ledger (or can be made one by committing the
right fact), the synthetic route applies — build a minimal world, commit
the fact directly, test the behaviour against it, at zero build cost. If the
function's result depends on re-deriving state that is not, and structurally
cannot be, expressed as a hand-committed fact (a generated sky's ephemeris,
a sculpted globe's terrain), the synthetic route is unavailable regardless
of how badly a campaign's plan wants it, and the claim routes elsewhere —
here, to `claim: rate(census: crisis-fires)` plus one live structural arm at
a seed the census itself identified (`CRISIS_SEED = 0`, read out of the
regenerated fixture, not re-hunted).

## Consequences

- **This is, in the end, the better answer for this claim** — not merely the
  fallback. The census searches once, in release, on the canonical host, and
  the seed it finds is recorded rather than re-hunted on every commit; the
  retired hunt could report only that its search terminated somewhere in
  1..=200, never how common the mechanism actually is. The regenerated
  census answers that too: `crisis-fires` fires on 659 of 1,000 worlds
  (65.9%), with zero `Absent` — two worlds in three hold a live prediction
  crisis, a number the hunt could never have produced.
- **The retrospective lesson is broader than this one claim.** The
  autopilot's verify-claims discipline already covered generated artifacts
  and tool behaviour; this was a third kind of claim to verify — what an
  *existing function can be driven with* — found only by an implementer
  reading the signature, after every review gate before code had passed.
  The generalizable rule: before a plan routes a behaviour to a synthetic
  fixture, read the target function's preconditions, the same way a caller
  would have to.
- **This does not reopen 0093's Stage 3 precedent.** `synthetic_flagship`
  remains the correct shape for a fact-reading behaviour; this record only
  narrows which behaviours qualify.

## See also

`docs/superpowers/specs/2026-08-07-the-assay-design.md` §3.4's "Correction,
found while planning" and §5's `behavior(synthetic)` routing rule;
`docs/retrospectives/the-reassay.md` (the process lesson this finding
belongs to — filed under that slug because `docs/retrospectives/the-assay.md`
already belongs to the unrelated 2026-07-21 potency campaign).
