# 0907. A preregistered check must name the axis it could fail on

**Status:** Accepted (2026-09-07) · **Decider:** Nathan ·
**Relates:** [0016](0016-studies-preregister-hypotheses.md) (preregistration,
which this sharpens rather than amends),
[0117](0117-the-client-re-derives-nothing-the-sim-emits.md) (cited by the
campaign this record comes from); `docs/superpowers/ledgers/2026-09-06-the-sett.md`
(S4, plan defects 7 and 9)

In the context of four separate preregistered checks — across three
campaigns — having been written on an axis along which the mechanism under
test **could not express itself**, and each having been read as evidence, we
decided that **a preregistered hypothesis names the axis on which it could
come back false, and a readout that reports a different quantity than the
one frozen is a null rather than a confirmation**, accepting that this adds
a sentence to every preregistration and cannot be checked mechanically.

## The four instances

They are not variations on a theme. They are the same defect, and three of
the four were written by someone who could already state the mechanism
correctly.

**1. The Pavement's H1 — the direction that cannot fail.** *"From 200
distinct seed-42 start cells, walking `n` for 500 steps leaves the walker
within 0.5 cell of the starting meridian at every step."* On a cube-sphere
face the constant-`a` lines lie in planes containing the polar axis, so they
cut the sphere in meridians: for `n`, the meridian and the great circle are
the same line and the lattice column is exactly it. Re-run over 216 start
cells: **north and south are exact — 0.00 cross-track, 0 of 144 failing.
East and west fail 144 of 144, at 0.1445 step-lengths per step**, which is
1.68x the drift the campaign quotes as the defect it repaired. Written on
`e` it would have failed on its first start cell. It also appears never to
have been run: the probe file its own plan names does not exist, and neither
the chronicle nor the retrospective reports a result, though both report H2
and H3.

**2. The Newel's P1 — settled at readout on a weaker property.** It froze
*"agreement(R) = fraction of `(facet, word)` pairs with a neighbour where
`box_R(neighbour) == ideal(word)`"* and predicted 1.000 by construction. It
was reported held on the evidence that both candidates *"build a
duplicate-free, hole-free window on a face interior"* — a statement about
the raster, where the frozen measure is a statement about the raster **and
the word**. Run as written, the chosen option scores 100% on the four
equatorial faces and **43.1% on the polar caps**. The prediction that would
have separated the two candidates was replaced, at readout, by one that
could not.

**3 and 4. The Sett's own, in the campaign that was writing this record.**
Task 3's headline test — the assertion that the reported bug is fixed —
passed on the unmodified raster it was written to reject: **all four
cardinals already agreed** at the chosen start, which sits at latitude
-4 degrees where a compass step and a Mercator column coincide locally.
The recommended stronger form measured **778 of 780 boxes agreeing** before
the change. Task 4's test was then sited on the **west** neighbour, where
the two rasters agree; only **south** discriminates there.

## Why this is not covered by 0016 already

0016 requires the hypothesis and its success criteria to be frozen before
the code that would move them, and all four of these were. Freezing is a
claim about *when*; this is a claim about *what*. A hypothesis can be frozen
early, stated precisely, and still be evaluated at a point where the
mechanism it names has no expression — and the resulting green is
indistinguishable, in the record, from a green that means something.

## Why naming the pattern is not enough, which is the finding

**Instance 4 was written after instances 1 and 2 had been diagnosed, in the
same document that diagnosed them, by the session that diagnosed them.** The
diagnosis was four sections above the defect. The task was even rewritten
afterwards, with the diagnosis in hand, and the siting survived the rewrite —
because the rewrite was correcting *scope*, and the siting was not what was
being looked at.

So the rule cannot be "remember this". What kills it is mechanical, and this
repository already asks for it everywhere: **run the assertion against the
unfixed code and watch it fail.** Every red-then-green step in every plan is
this check. All four instances would have been caught by it, and instance 4
was — by an implementer who measured the old behaviour before touching
anything, because the dispatch told it the plan had been wrong before.

## What this requires, concretely

- A preregistered hypothesis states the axis along which it could come back
  false, and — where the mechanism is directional, positional or
  populational — says why the chosen axis is one where it *can*.
- A readout reports the quantity that was frozen. Reporting a different one,
  however reasonable, is a **null**: the frozen check did not run.
- A check that passes on arrival is not a pass. It is an unanswered question
  about whether it can fail at all, and it is answered by a positive control
  — running it against the behaviour it describes and watching it redden.

## What it costs, and what it does not buy

It costs a sentence per preregistration, and it is unenforceable: nothing
mechanical can tell an axis that cannot fail from one that happens not to.
`cli/tests/suite/preregistration_guard.rs` is narrower than its name and
does not do this — it is a default-deny scan requiring every `#[ignore]` in
a lab calibration test to carry a reason.

It also does not protect a check whose axis is right and whose *fixture* is
absent: the same campaign specified a search for a seed-42 start satisfying
three conditions, and **no start satisfies them** — one roster entry and one
drawable facet reach the plate at either available start, both the
observer's own. Declining to name a fixture is not the same as establishing
one exists. That is a neighbouring failure and this record does not cover
it.
