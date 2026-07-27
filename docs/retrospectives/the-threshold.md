# The Threshold — retrospective

Process lessons, not product. Product lives in
[the chronicle](../../book/src/chronicle/the-threshold.md).

## The headline: four checks confirmed the stated thing while the right thing went unasked

This campaign hit the same failure four times, in four different registers, and
it is the only lesson here that generalises without qualification.

1. **A guard that could not fail.** `precondition_reads_committed_state` took
   `_a` and returned a bare `false`, with a test asserting it was `false` for
   movement actions — `assert!(!false)`, green under every input — and a doc
   comment claiming it "fails loudly." It protected nothing. Fixed by matching
   every variant by name, so adding an `Action` fails to *compile*.
2. **A test that could not discriminate.** The seam-arrival test used a fixture
   with no `Ground` anchor, so `Narrow` and `Broad` landings returned the same
   id. It would have passed with the seam kind ignored entirely.
3. **A check of the wrong predicate.** The Task 4 review asked "does
   `Occupancy::walk` consult `Interior::neighbors`?" It did. But a hearth
   attaches to its alcove by *containment*, so every walk stalled one hop short
   of the fire, silently, for two tasks — until a creature was made to actually
   walk it.
4. **An acceptance protocol measuring the wrong artifact.** Every Stage A task
   verified byte-identity with `hornvale new --seed 42`. Genesis does not touch
   the vessel layer at all, so that check could never have failed. Four tasks of
   byte-identity evidence were vacuous. The spec's own protocol named the right
   artifact — the possession *galleries*, which run `possess` — and the per-task
   steps reached for the world file instead.

The pattern is one thing: **an assertion about a mechanism is not an assertion
about the property the mechanism was built to guarantee.** The fix that
generalises is structural — an exhaustive match that fails to compile, a fixture
where the two answers genuinely differ, one shared definition rather than two
agreeing ones. Every one of these was caught by a *reader*, never by the suite.

## Nulls are only worth having if each one eliminates something

The campaign's preregistered prediction failed four times, and the sequence is
the useful artifact:

| | What it tested | What it ruled out |
|---|---|---|
| 1 | warmth at 1.0 °C, 0.125 °C at the landing anchor | — |
| 2 | recalibrated to 15.0 °C from an energy balance | **magnitude** |
| 3 | the creature genuinely crosses to the fire | — |
| 4 | the sampler reads where the creature actually stands | **instrument** |

What remains is a fact about the world rather than about the code: cold-built
rooms are bimodal — creatures are either inside their own tolerance, or 40–80 °C
beyond any help a fire offers. There is no middle band for a hearth to rescue.

**A null that eliminates nothing is a wasted run.** Designing each measurement
to kill a specific candidate explanation is what turned four failures into a
result.

## The discipline that made the recalibration honest

`HEARTH_WARMTH` moved 1.0 → 15.0 **after** a null. That is the shape of
p-hacking, and the only thing separating it from p-hacking was order: the
physical argument (envelope, infiltration, hearth power, radiant crowding) was
written down and **committed before any re-measurement**, and the constant was
not touched again after the result came back still null.

Worth stating as a rule: *a constant may be changed after seeing a result, if
the reason it changes is independent of the result and is recorded first.* The
test is whether the argument would read as correct to someone who never saw the
null.

The distinction was drawn explicitly at the time — calibration is answering
"what is physically right"; tuning is answering "what makes my prediction come
true" — and the owner made that call rather than the controller.

## Planning against an unbuilt dependency paid, twice

The spec was written against The Hearth's spec and plan while The Hearth had
zero code. That was a deliberate bet: writing the downstream artifact finds
upstream errors while they are still free.

It returned four findings that The Hearth adopted — attachment composition
replacing a degenerate hub, growing the inventory *before* arming made growth an
epoch, the live/reachable vocabulary split, and name-keyed selection. And when
The Hearth's revision came back, it **broke one of this campaign's own
implementations**: `landing` had been resting on `ids()[0]`, which coincided
with the hub only by accident of inventory order.

The bet's own precondition — *re-read the real code against every Interfaces
block before dispatching* — is what caught it, and it caught two more besides.
**Write the precondition into the plan, addressed to the person who will resume
it.** It is worth more than the plan's confidence.

## A frozen prediction can be outlived by its world

At close, absorbing main broke the four-times-replicated null — and broke it
**in the predicted direction**. Another campaign's history rework had moved seed
13 from 92 settlements to 104, and on that world the hearth lowers prevalence by
about 0.004, entirely within one species.

The temptation is obvious and was declined. A preregistration is a claim about a
specific world, and this one was sealed against a world that no longer exists;
reading a favourable delta off a changed world afterwards is the same post-hoc
move the campaign had already refused twice (once over the calibration constant,
once over the cold-temperature gate). The four nulls stand as measured. The new
delta is recorded, unclaimed, with re-measurement named as the follow-up.

Two durable things came out of it:

- **A test's name is a claim.** `..._shows_no_measurable_effect_...` had become
  false, so it was renamed to what it now asserts — that the effect stays small
  and never harms. A test whose name outlives its assertion is a lie with a
  green tick beside it.
- **Bound the surprise rather than asserting its absence.** The A/B now asserts a
  *safety* property (a hearth never makes a creature worse) and a *magnitude
  bound* (the delta stays near where it was recorded), with the message telling
  the next person to re-run the protocol rather than widen the bound. That
  survives a moving world in a way `assert_eq!` could not, and it still fails
  loudly if the effect ever becomes real.

This is the third pin in this campaign rewritten from a value to an invariant,
and the reason is always the same: parallel campaigns move the world underneath
each other, and a pin on someone else's physics reddens for their improvements
while saying nothing about your own claim.

## Smaller notes

- **A default chosen for a good reason becomes the permanent answer.**
  `Terrain::is_built` defaulted to `false` so Stage A would stay
  byte-identical — correct — and nothing ever supplied the real one, so the
  arming task moved nothing at all. The same shape as the parent campaign's own
  unreachable v1, rebuilt one level down. When a default exists to preserve a
  property *during* a campaign, the task that retires it belongs in the plan.
- **The rarity estimate was wrong and cost a detour.** "Cold settlements are
  rare" came from the health battery's fixed ten-creature sample, not from the
  world: ~40% of seeds carry one and seed 13 is cold-*dominated*. Going to look
  was cheap; reasoning from the sample was not.
- **131 commits of drift at close**, with one real semantic conflict in the file
  this campaign had modified — both edits legitimate, resolution was to keep
  both. Parallel campaigns are now constant here; absorbing at stage boundaries
  rather than once at close would have made this a series of small merges.
- **Subagents did not park**, across fifteen dispatches. Two independently hit
  the same heredoc failure building multi-line commit messages and both
  recovered with `git commit -F`; that belongs in the dispatch preamble rather
  than being rediscovered.
