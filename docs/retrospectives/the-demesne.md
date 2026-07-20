# Retrospective — The Demesne

One page of process, not product. The product is chronicled; this is what
the close learned that the code does not record.

## What went well

- **Most of the substrate already existed — this campaign was wiring, not
  invention, and treating it that way kept it small.** The resource-axis
  basis, per-species uptake vectors, the trophic/coexistence stack, and the
  stronghold/refugia machinery were all already built and load-bearing; the
  actual bug was one line — a per-species carrying-capacity site collapsing
  a five-axis uptake vector against a single scalar supply before the
  spatial field ever applied. Naming the bug precisely as a *rank
  collapse* (a vector projected to a scalar too early, so a niche could
  only rescale a shared number instead of selecting a different place)
  made the fix small and legible instead of a rewrite: build the missing
  per-axis supply fields, replace one multiplication with a dot product,
  leave everything downstream untouched. The four-task plan tracked that
  shape exactly (fields → wiring + rank-restoration keystone →
  recalibration → close).
- **Kobold staying at zero was measured as structural, not chased with a
  tuning knob.** The plan's own draft baseline had hoped for a new
  non-goblinoid *peopled* dominant as part of the emergence keystone; the
  measured reality is that all four peopled kinds' authored niches carry
  zero weight on any of the three axes this campaign made spatial, so no
  amount of field-shape or normalization tuning could have moved them —
  only the animal-prey axis (explicitly out of scope) or new species
  authoring (a different campaign) can. A 100x sweep of the one knob that
  does reach the peopled roster (the forage fraction) was run and
  confirmed it never changes which peoples dominate, before concluding the
  gap was structural rather than under-tuned. Reporting that finding
  plainly, instead of quietly narrowing the keystone to only the fauna half
  that did move, is what let the close name the peoples-diversity problem
  as a real open design question instead of an unexplained loose end.

## What the campaign taught mid-execution

- **A preregistered target that stops being reachable is a finding to
  report, not a bar to quietly lower — and review is what catches the
  difference.** The Menagerie's `≥ 6` distinct-dominants target, from an
  earlier campaign, was not reachable by this stage's abiotic-only scope
  (the dragon and treant blockers are both named, real, and orthogonal to
  what this campaign built). The first pass through this recalibration
  task rewrote the acceptance threshold down to `> 2` — technically true,
  but it silently erased the record that `≥ 6` had ever been the bar,
  which would have let a later reader believe 4 was always the goal.
  Review caught this as a correctness-of-record problem, not a style
  nitpick, and the fix was to do both things at once: pin the real,
  measured Stage-1 achievement (`≥ 4`, with the exact breakdown) as its
  own passing assertion, *and* keep the original `≥ 6` target alive as an
  explicitly still-`#[ignore]`d, honestly-unmet target with its own
  blockers named. A preregistered number's job is to be falsifiable later,
  not just passable now.
- **A settlement-count regression got a real investigation before a
  constant got touched.** Restoring the vector supply dropped seed 42's
  settlement count from a historical ~108 to 81 — a real, measured
  side-effect of the animal-prey axis reading zero rather than folded into
  the old scalar's total. The instinct to raise the one nearby knob that
  reaches this number (the forage fraction) was checked against its own
  documented physical meaning and an existing invariant test before being
  used: the value needed to restore 108 broke a tested "forage is a
  fraction of primary production, not a multiple of it" assertion, and
  even the physically-defensible ceiling only reached 92. Accepting 81 as
  the honest Stage-1 reading, with the sweep table and the disqualifying
  reasons recorded, was the discipline that kept this from becoming a
  fit-to-result edit dressed up as calibration.
- **The census stayed deferred for the whole campaign, and that discipline
  held without incident.** Every task ran `SKIP_CENSUS=1`; no task invoked
  a live census, `HV_CENSUS=1`, or a remote regeneration. Genesis moved
  (settlement composition and placement are visibly different on seed 42)
  without ever touching the thousand-seed census fixtures, which is the
  accepted, named trade for this campaign. Note: the *mechanism* of that
  deferral changed under the close (see below) — decision 0063 retired the
  AWS regeneration path this campaign had planned to hand the census off to.

## What the close taught

- **A genesis-changing campaign pays a downstream-test tax at close
  proportional to how many parallel campaigns pinned seed-specific
  composition — and every drift must be re-measured, not guessed.**
  Absorbing main at close surfaced nine drifted tests across three crates
  (`windows/book`, `windows/worldgen`, `windows/lab`), every one a genuine
  consequence of the recalibrated per-axis supply moving which cultures
  reach the organized rung and which subframe each holds: seed-1 hobgoblin
  flipped organized→folk, seed-3 hobgoblin folk→organized, seed-3 kobold
  dropped from placement, and one seed-2 verb lexeme re-derived
  (`drives`→`stalks`). Each was re-pinned to the merged tree's *measured*
  value (via throwaway probes dumping the live surface), never to a guess —
  the same book artifact (`the-book.md`) the merge had already regenerated
  was the cross-check. The tax is real and unavoidable for a genesis change;
  the discipline that keeps it honest is measure-first re-pinning.
- **One false positive hid among the true drifts — the collision wave is
  not uniformly "re-pin to the new value."** The lab's uncanny-literalism
  guard (`differing_subframes_do_not_share_one_verb`) reddened not because
  a value drifted but because the recalibration first satisfied its
  long-dormant precondition (a second subsistence subframe appeared, as
  seed-5 hobgoblin began herding) while leaving the sample degenerate (that
  Mounted culture is folk-only, so it emits no verb to compare). The honest
  fix was to *harden the guard* (require ≥2 subframes to actually produce
  verbs before alleging a collapse), not to re-pin a value — proof that
  each collision needs its own diagnosis, not a blanket re-pin.
- **Absorbing main at close can change the very policy the campaign planned
  around.** This campaign was designed and executed to defer its census to
  a backgrounded AWS regeneration (per the then-current decision 0046).
  Between the last plan-stage absorption and the close, decision 0063
  landed on main: AWS is abandoned, the census regenerates locally on a
  single canonical Linux box (~7 min), and the "fixtures deliberately lag"
  trade is retired. The plan's entire census posture was obsolete before it
  could be exercised. Because this close ran on a non-canonical macOS
  machine (0063 forbids it from committing census goldens — the two boxes
  disagree on ~0.1% of discrete-count metrics), the final census/calibration
  regeneration and the fast-forward were handed to a session on the
  canonical box. Lesson: a campaign's save-format/regeneration assumptions
  are only as current as its last absorption — re-read the decision log at
  the close, not just at the spec.

## Follow-ups promoted from the working register

The full working register lived at
`.superpowers/sdd/followups.md` during execution; the durable items are
promoted here so they survive the branch.

- **The animal-prey field / the trophic food web is the single
  highest-leverage next step.** A derived herbivore-biomass field —
  prey density as a function of the plant supply herbivores graze, times
  their own density — closes the trophic loop this campaign left open:
  predators and dragons gain a place to hunt, the full menagerie
  strongholds target re-enables, and (independently) the peopled roster's
  forage-only competition gains its missing second axis, which is also the
  most direct lever on the depressed settlement count.
- **Richer, denser biomes and dynamics** — many plants and animals sharing
  one landscape, seasonal prey, predator-prey cycles, migration — sit
  behind the food-web layer, not this one.
- **A companion peoples-authoring campaign** — a wider roster of peopled
  kinds, once the spatial supply gives them genuinely distinct terrain to
  compete over.
- **A fresh-water resource axis** and **a real, spatial dead-matter
  field** — both named refinements to the resource basis this campaign
  left at a condition-term and an ambient constant, respectively.
- **Wiring the same per-axis supply into other consumers** (habitat and
  biome description, eventual agriculture/subsistence) once it exists as a
  shared quantity, not just an input to carrying capacity.

### Open design challenge — peoples-diversity (not solved here)

The core problem this campaign exposed and did not solve: the spatial
supply diversifies fauna (the mineral-eater now holds real ground) but not
the small peoples, because their authored niches are nearly identical and
carry no weight on any of the axes that now vary in space. Three candidate
directions were raised and each found wanting: distinct authored niches per
people (doesn't feel like the actual mechanism); homelands or centers of
origin with a distance-based affinity field (order-independent, gives real
territories, but not settled on); and a global abundance penalty computed
as a fixed point (order-independent by construction, but produces
sparseness rather than territory, and territory is the thing wanted). The
binding constraint on any future answer: it must be a field or a fixed
point, never a sequential or cumulative tally, because order-dependence
would break both determinism and the discipline that nothing is ever
authored per-cell. This is its own open brainstorm, not a follow-up task —
the three candidates above are the floor to beat, not a shortlist to pick
from.
