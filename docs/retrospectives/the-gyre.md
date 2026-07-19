# The Gyre — retrospective

Process lessons from shipping the ocean-current vector field (Weather Program
campaign 2) across hornvale + orrery. Product is in the chronicle.

## Ideonomy found the cleave, not just enriched it

The scope question looked binary — current-as-picture vs current-that-warms —
and the honest answer was neither pole as first stated. The ideonomy pass
(abstraction-lift + cross-domain over hierarchicalness/cyclicity/source)
reframed it into a three-level coupling hierarchy, surfaced a *hidden* middle
level, and then unmasked that middle as a grounding trap (the Faces/Reckoning
class — a rendered quantity the model doesn't believe). It also produced the
load-bearing distinction the campaign turned on: **cyclicity, not field
count, is what triggers the operator/relaxation refactor.** That is the second
campaign running where a ten-minute pass on the "obvious" decision changed the
decision. Run it on the settled-looking questions; that is where it pays.

## The visual pass, again — legibility this time, not physics

Last campaign the visual pass caught a physics bug five reviews missed. This
campaign it caught a *legibility* failure no test could: the currents rendered
as a faint scatter of 1px specks (WebGL caps line width), physically correct
and invisible. The fix was tuning (more, longer, brighter streaks), not
logic. The lesson generalizes: for any *rendered* deliverable, "is it legible
enough to read?" is a first-class gate the automated suite structurally cannot
answer. Budget the screenshot.

## A spec can contradict itself; the reviewer + the human resolve it

My spec §3 asked for both "sibling to `winds.ts`" (static arrows) and
"swept/faded/re-seeded" (animated) — a contradiction I wrote and didn't catch.
The Task-5 reviewer flagged it as a spec gap rather than waving it through on
the winds precedent, and it went to Nathan as a scope call (he chose
animated). The lesson for plan authors: when you cite a sibling *and* describe
a different treatment in the same breath, you have written an ambiguity;
resolve it in the spec, not at execution. The review loop is the net that
caught it.

## What worked

- **Byte-identity by level.** Choosing level 0 (no temperature coupling) kept
  the whole campaign — including the legibility retuning — byte-identical: no
  epoch, no census regen, no AWS. The determinism story is now a groove: an
  additive scene layer, a re-pin, a mean that never moves.
- **The producer-owns-the-global-field seam (Approach A)** was right: the
  coastline-dependent gyre closure is a whole-geosphere computation the client
  can't faithfully reconstruct, so the producer emits it and the client only
  parses + advects — no golden needed (parse, not recompute).
- Both final opus reviews verified the physics *by hand* (the Ekman rotation's
  tangency/magnitude preservation, the hemisphere sign composing to a
  clockwise-N gyre) — the model-vs-reality check the per-task reviews can't do
  alone.

## Absorption

The branch met main twice (Predicates/Tongues/Foresight earlier, then The
Explanations + The Freshet at close). The Freshet's almanac "Waters" section
collided with this campaign's "the seas" line in `AlmanacContext`'s type-audit
tag — both added a prose field; hand-resolved to the union. Additive,
byte-orthogonal work absorbs cheaply; the one real conflict was two campaigns
adding almanac prose in the same struct.
