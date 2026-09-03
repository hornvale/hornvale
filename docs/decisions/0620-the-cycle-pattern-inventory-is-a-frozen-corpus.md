# 0620. The cycle-pattern inventory is a frozen corpus of nine rows, and solvability for a body holding nothing is the invariant

**Status:** Accepted (2026-09-03) · **Decider:** Nathan (G3) / autopilot ·
**Relates:** [0011](0011-studies-are-data-metrics-are-code.md) (the corpus is
data, the resolver is code),
[0016](0016-studies-preregister-hypotheses.md) (frozen before measurement),
[0516](0516-a-reachable-lock-implies-a-reachable-key-is-left-empirical.md) (the
strongbox's clause, deliberately untouched),
[0616](0616-a-gate-is-a-requirement-on-a-way.md),
[0617](0617-a-locks-substance-is-derived-from-rock-and-work.md) ·
[The Brattice](../../book/src/chronicle/the-brattice.md)

In the context of Dormans' fourteen cycle patterns being a published figure the
repository can cite but not execute, facing the question of how a placement
grammar stays a corpus rather than becoming a pile of tuning knobs, we decided
that **the cycle-pattern inventory is a FROZEN CORPUS of exactly NINE rows,
each citing a source the repository can attest, its count asserted by a test,
applied one row per realm — and that the invariant every placement must
preserve is SOLVABILITY FOR A BODY HOLDING NOTHING: the default body, able only
to walk and wade, can reach the terminus and every key from the entrance AND
return, on every descent, by construction** — accepting that a placement which
would break that is unstamped and recorded as a refusal rather than shipped,
and accepting that adding, renaming or removing a row is a deliberate act with
a test to change.

## Context

The corpus/resolver split is decision 0011's, applied the way the trope,
system and sentence corpora apply it: a constant table of rows, each citing its
source, and code that resolves it — the table read by nothing but the resolver,
and the resolver reading no file. The freeze before measurement is decision
0016's. The count is asserted so that changing the inventory cannot happen by
accident, and after this record it is an epoch of `underworld/gate/v1` under
decision 0618.

**Ten rows were frozen at the design review and nine shipped**, and the missing
one is the reason the count is stated here rather than in prose alone.
`the-landing-hall` wanted a realm whose two paths are both short and which
crosses a floor, and it drew zero times over 4,412 realms. The first reason
recorded for that was false, and the task review said so: "a cross-floor lower
path is laid with at least three edges, so both paths cannot be short" does not
follow, because a two-edge path against a three-edge one classifies as
short-short under the frozen rule.

**The true argument is a parity argument, and it is load-bearing because it
survives growth where a case analysis would not.** A cross-floor realm's lower
path lands on the same grid squares as the realm's own endpoints — its two
stairs move no square — so both of a realm's paths are unit-step walks between
one pair of squares. A grid is bipartite, so the two lengths are congruent
modulo two. A spliced detour is itself a walk between the two squares it
replaces, so it moves a length by an even amount and the congruence survives
every growth move. Short-short requires the two lengths within one of each
other and not both at least three; equal parity turns "within one" into
"equal", forcing both to be at most two — while the lower path holds two stairs
and at least one hop, so it is at least three. Contradiction; the cell is
empty.

The section's own rule then removes the row: a pattern nothing selects is dead
data, not inventory.

## Decision

Nine rows, asserted. One draw per realm from the pattern leg
`underworld/gate/v1/pattern`, uniform over the rows admissible for that realm's
class, span and derivable substance (decision 0617), in table order. The draw is
made even when one row or none is admissible, so the draw count depends on the
number of realms and never on the data. Nothing after the draw is random: the
resolution of a row's placements to edges and nodes, and the refusals, are
deterministic reads of the graph — which is what makes a skip a fact about the
world's geometry rather than about luck.

Every tentative stamp is checked against the default body — `{Walk, Wade}`,
holding nothing — and unstamped if it breaks either half of solvability.

## Consequence

- **Solvability is the ROUND TRIP, not a reach.** The design as approved asked
  only that the terminus and every key be reachable *from* the entrance. A chute
  taken down into a realm whose upper path a nested sump blocks would then leave
  the default body with no way back — a trap, and Dormans' "unknown return path"
  never means an absent one. So the pass also requires a gated round trip to
  exist. Measured: on the panel's first seed the change converts exactly one
  chute from placed to refused.
- **Dead rows red a test now, rather than sitting in a healthy-looking table.**
  A sweep asserts every remaining row is applied somewhere over 1,800 plans, and
  the parity lemma above is *witnessed* directly by a second sweep rather than
  only argued. This is the mechanical form of a failure the campaign could only
  find by hand.
- **The two rollback paths are still exercised only by construction, mostly.**
  Across a 4,412-realm sample no placement was refused for want of room or for
  unsolvability; the full first-seed panel exercises the unsolvable path twice.
  A hand-built plan that forces each is a captured minor.
- **Decision 0516 is not reopened.** That record leaves "a reachable lock
  implies a reachable key" *empirical* for the strongbox, whose key placement is
  a prop-management knob awaiting residents. A descent gate is a different
  object — the plan places both its ends — so its rule is structural, and 0516's
  own text scopes itself to the strongbox clause.
- **The resident is asserted separately**: for a body holding every key with
  every capability, every standable cell of the descent is reachable, which is
  decision 0566's whole-descent connectivity restated for a world that now has
  gates. Gates cost the resident nothing, which is the campaign's keystone
  stated as a test.
- The whole-descent invariant runs at merge cadence, not commit cadence: the
  sanctioned sweep is 14,400 plans and 65 s, which the sub-floor roster would
  never admit, so it carries the heavy tier's tag (decision 0426's cadence) and
  the heavy roster reads 65.

## See also

- [The Brattice design](../superpowers/specs/2026-09-02-the-brattice-design.md)
  §3.2, §3.4, §3.8.
- [The Brattice ledger](../superpowers/ledgers/2026-09-02-the-brattice.md) #10
  (rulings D, E), #14 (ruling K); Task 2's fix round (ruling F).
- [The Brattice chronicle](../../book/src/chronicle/the-brattice.md).
