# The Sources — retrospective

Rung 2 of the Underworld Larder. Process lessons, not product; the product is
in [the chronicle](../../book/src/chronicle/the-sources.md).

**Merged:** 2026-08-27

## The headline: the controller was wrong about the tree four times, the same way each time

Every implementer-facing brief this campaign shipped was pre-verified against
the tree immediately before dispatch, and that habit caught real defects
(nine `.expect()` calls on an infallible constructor, an invented
`fixture_world()` helper, a `BuildDepth` mis-price that would have buried the
campaign's positive control in the heavy tier, an `Ord`-less enum keyed into
a `BTreeMap`). But the controller was **also** wrong about the tree four
times, in a single shape every time: **trace one hop, stop, report the
answer as though it covered the file.**

1. Told Task 1's implementer "do not expect renames," grepped five specific
   symbols, all zero — and had the counter-evidence in hand already
   (`use hornvale_kernel::{CellId, ...}` in the probes' own imports) without
   asking whether `CellId` still existed. It did not; `Vertex` had replaced
   it 300+ tokens deep in the same file.
2. Assumed `rungs()` excludes `Surface`. It does not, and a midpoint test
   built on that assumption would have panicked on the very first rung it
   touched.
3. Told a reviewer "ridge crust is youngest, so the gradient is maximal
   there" as a verified fact. True, but not discriminating: the gradient is
   uniformly maximal across *all* non-continental crust, so the claim proved
   nothing about vents specifically. The implementer's own report surfaced
   this; a reviewer's independent probe then measured just how total it was
   (medians 0.19% apart).
4. Told Nathan and an implementer that a census refresh "costs hours" —
   lifted from a five-campaign-old figure in the project's own standing
   guidance, which **explicitly warns against reading a number off that
   block and uses this exact error as its worked example.** The real cost,
   one `grep` away, was fifteen minutes.

None of the four was found by re-reading the claim. Each was found by
running one more command than the claim rested on — the compiler's own
error surface, the accessor's own return type, an independent probe with its
own comparison group, `grep '| census |' docs/timings.md | tail`. A
controller who traces one hop and stops produces a confident, wrong answer
that reads exactly like a confident, right one; nothing in the prose
distinguishes them.

## Two vacuous checks, both found by an implementer refusing a null

`make lab-diff STUDY=the-census` compares a committed file against its own
working-tree copy — which nothing regenerates without a census run on the
canonical box — so it reported "no metric moved" whatever had actually
changed, on both occasions it was specified as the check. Both times, the
implementer said so plainly instead of banking the convenient null, and went
and found the instrument that actually re-executes the changed code (a live
re-probe against committed fixtures, which did move). A null that arrives
without having to fight for it is the one most worth being suspicious of.

## What actually caught things, ranked by yield

**1. Verifying each task's brief against the tree immediately before
dispatch**, not at plan-authoring time. Caught the structural defects above
plus several more (a report figure — "251 vertex-rung entries compared" —
whose reproducible count was actually 4,370; a missing crate-root
re-export). Every line number in the plan was correct on the day it was
written and every one of them rotted by the time its task ran.

**2. A mutation that does *not* redden is the most valuable line in a
report, and the easiest one to drop.** One implementer disclosed exactly
that — a field function's threading of a depth argument was unpinned, and
zeroing it left the campaign's own monotonicity test green. That disclosure,
not the passing tests around it, is the only reason the seam got closed.

**3. Reporting findings against one's own interest.** Three separate agents
did this in one campaign: the uncaught mutation above, the vacuous census
check (twice), and a review that weakened its own task's headline framing
(the vent/ridge finding) rather than let a flattering but wrong claim stand.

## Absorption arrived at close, not at any stage boundary, and both predicted traps fired

The branch met `main` for the first time 55 commits behind, at merge —
the stage-boundary cadence this project's standing guidance calls for was
never invoked. Both of the traps that guidance predicts for exactly this
situation fired: `docs/audits/type-audit-report.md` conflicted (resolved by
regenerating it, never text-merged) and `book/src/frontier/idea-registry.md`
auto-merged **clean**, which is the same shape that has silently duplicated
a registry row twice in recent weeks. It did not duplicate anything this
time — checked by hand, then confirmed by the registry's own uniqueness
test — but the absence of damage is not evidence the cadence was safe to
skip; it is evidence this particular merge happened not to collide.

## Do differently next time

Run `make sluice-stage` at every plan-stage boundary, not only at close, on
any campaign carrying more than a handful of tasks — the two-trap absorption
above is exactly what the standing guidance already predicts, and predicting
it in prose did not substitute for actually invoking it. When a controller
states a fact about the tree with confidence, ask what the *next* hop would
show before writing it into a brief; the four wrong facts this campaign
produced were each one command away from being caught.
