# The Reckoning

**July 2026 · 20 commits · outcome: implementation complete, pending merge —
the star gets an age and every moon an origin story; a design that was
supposed to move only the worlds it touched instead moved two thirds of them,
and the reason why is the best thing the campaign learned**

## What was attempted

Two clocks were running without anyone having set them.

**The star had a clock with no zero point.** The Long Count shipped
main-sequence brightening — `b = 0.10·M^2.5` per gigayear, Sol-calibrated,
scaled by the main-sequence lifetime `t_MS ∝ M^-2.5` — and anchored
`luminosity_at` so that `t = 0` is genesis. The model therefore knew how fast
its star brightens and could integrate forward from today. It had no idea how
far along the star already was. It reasoned about `t_MS` without ever placing
the star on it.

**The moons had no origin story.** Each moon drew a mass, a distance, an
inclination in `[0, 10)°`, and a node longitude, and those numbers floated free
of one another — four independent draws standing in for what should be one
fact. Nothing said *where the body came from*, and in reality that one fact
predicts nearly all of them. The consequence surfaced from a parked sibling:
The Moons could not honestly derive a moon's density, because the generator
drew mass but never composition, and was about to assume lunar density for
every body in every world. An assumption standing in for a missing concept is
exactly the shape of debt this project pays down rather than refinances.

## A bound, not a cosmology

Drawing a stellar age looks like stellar physics until you try to write the
number down. A 0.6 M☉ K dwarf's main-sequence lifetime is **35.9 Gyr** —
longer than our universe has existed. So *any* answer commits the project to a
cosmology: cap the age and you have declared when things could have begun;
decline to cap it and you have permitted a 34-Gyr star, asserting an old or
eternal universe by omission. There is no neutral option, and drawing a
*fraction* of `t_MS` does not dodge it, because `f · t_MS` is still measured in
gigayears.

The recommendation was 13.8. The ruling was **15**, and the reasoning is worth
preserving because it is not a rounding: **13.8 would have imported our Big
Bang wholesale**, settling a metaphysical question this project deliberately
holds open — when, or whether, the universe began — as a side effect of a
generator constant. Fifteen gigayears is a **bound without a cosmology**. It
says no star in Hornvale is older than 15 Gyr while declining to say when the
universe began.

The bound still buys its physics, and buys it in the way this project prefers.
A 0.6 M☉ K dwarf is capped at roughly **42%** of its main-sequence life, so it
has *necessarily* brightened little — which is true of real K dwarfs, and which
**emerges from the bound rather than being asserted anywhere**. The star's age
is drawn as `age = U(0.05, 0.95) · min(t_MS(M), T_MAX)` off a new stream, the
guard rails keeping it off the pre- and post-main-sequence edges where none of
the model's physics applies. The planet's age is *derived*:
`planet_age = max(0, star_age − 0.05 Gyr)`, since terrestrial accretion
finishes within ~30–100 Myr of its star — a gap under 1%, modelled only so the
number exists and is honest about its own precision.

## The refusal

The most important decision in the campaign is a thing it declined to do.

An age invites an obvious physical correction: a main-sequence star that has
been burning for four gigayears really *is* brighter than its zero-age value,
and the model now has both the elapsed time and the slope to say by how much.
Applying it would have been tidier physics and a catastrophe. `Star::luminosity`
is derived `M^3.5`; the habitable zone derives from luminosity; orbit admission
derives from the zone; insolation derives from the orbit; and **climate derives
from insolation**. An age-corrected luminosity would have moved every world's
habitable zone, orbit, and climate — an epoch across the entire simulation,
from a campaign authorized to move moons.

So **age describes pre-genesis history only**. It says how long the star has
been on the main sequence before day 0 and it never touches `luminosity`. The
corrected value stays exactly one division away — `L_ZAMS = L / (1 + b·age)` —
for anyone who wants it, and nothing in the sim calls it. The containment is
not a hope: the campaign's first-written and hardest-watched battery pins
luminosity, habitable zone, and the anchor's admitted orbit byte-identical to
the pre-campaign generator across a seed battery, and a reviewer **mutation-
tested it** by forcing age into luminosity and confirming the guard fails. A
test that cannot fail proves nothing; this one can, so its passing means
something.

The general shape is worth naming, because it recurs: *the obvious physical
reading was the one that had to be refused.* Fidelity is not a direction you
can always walk in one step, and a model that is locally more correct
everywhere can still be the wrong campaign.

## Capture is the deliverable, not the cost

A terrestrial world gets two mechanisms, `{GiantImpact, Capture}`.
Co-accretion is deliberately absent, and for a physical reason rather than a
budgetary one: it needs a massive circumplanetary disk, which is a
**giant-planet** mechanism — the Galilean moons, Titan. Hornvale's anchor is
terrestrial. Modelling co-accretion would be modelling something these worlds
do not have. Fission is discredited and excluded.

The cheap variant of this campaign — ages plus giant-impact only — was offered
and **declined**, with the price understood. Without `Capture` the epoch mostly
evaporates: inclination stops moving, the eclipse batteries mostly hold, the
census probably survives. It would also leave every world uniform, every moon
regular, and the density derivation still half-assumed. Nathan's rationale
belongs in the record because it *is* the campaign's purpose: *"part of the
point here is 'to explore strange new worlds'… this can contribute to making
worlds that are more expressive and more interesting."* The thing that makes
the campaign expensive **is** the thing the campaign is for. Uniformity was
never the goal.

What that bought is visible on the canonical demo seed. **Seed 42 now carries a
retrograde captured moon at 117°**, and its eclipse recurrence moved from
roughly **423 days to roughly 3112**. This is not a tuning artifact — it is the
physics arriving. A retrograde moon crosses the ecliptic on a completely
different cadence from a prograde one, and often, for a given observer, never
usefully at all. Across the 1000-seed census: **405 of 1381 moons** are
captured (29.3%), **222 of those 405 are retrograde** (54.8%), and **211 are
icy** (52.1%).

## What an origin story predicts

The point of a formation mechanism is that quantities stop being independent.

**Density stopped being an assumption.** A giant-impact moon is 3.34 g/cm³ not
by fiat but *because* it is re-accreted mantle debris with no iron core — which
is precisely why Luna is 3.34 against Earth's 5.51. A captured body came from a
different reservoir entirely and draws from one of two representative classes,
rocky 3.0 or icy 1.6. **Radius then follows from mass and a *real* density**,
`r = (3M/4πρ)^{1/3}`, rather than from mass and a guess — the honesty upgrade
the parked Moons campaign was blocked on, delivered by unblocking its premise
rather than by working around it.

**Age stopped being uniform.** A giant-impact moon is coeval with its planet
(Luna comes out 4.51 Gyr against Earth's 4.54); a captured body formed
elsewhere and its age decouples.

**Inclination became the signature.** An earlier draft of the spec had the
mechanism drawn *first*, biasing mass and distance on the theory that causal
conditioning beats post-hoc conditioning. Reading the generator killed that on
two counts, and the first is a genuine piece of physics: the draft claimed
captured moons are small, and **Triton falsifies it** — the seventh-largest
moon in the solar system, and captured. **Mass is not a capture signature.
Inclination is**: a high, often retrograde orbit is the *defining* trait of an
irregular satellite. Conditioning mass would have asserted a correlation the
solar system contradicts. So mechanism draws per moon *after* the distance
sort, from its own stream, weighted by distance — which is both physical (an
impact child forms close and tidally recedes; irregulars are distant) and free,
since distance is already in hand. Both branches then consume exactly one draw
from the same existing inclination stream:

```
  GiantImpact -> inclination = roll * 10.0            <- identical to before
  Capture     -> inclination = 20.0 + roll * 140.0    <- irregular; >90° retrograde
```

**Masses and distances therefore never move, in any world.** That half of the
property held, and still holds.

## The weighting gained a falsifier

The spec called the distance weighting "a plausibility rule, not a population
synthesis" — which is honest, and also unfalsifiable by construction. A rule
that claims nothing checkable cannot be wrong, and cannot be right either.

A review measured the shipped map on the production path and found a check it
*could* fail. Because the distance fraction is roughly uniform, a **lone** moon
averages mid-range, so the planned linear map `clamp(frac, 0.10, 0.85)` gave it
`p_capture ≈ 0.5`. Single-moon worlds are 422 of 845 mooned worlds — half the
population — and they came out only 40.5% giant-impact. A solitary moon was 59%
likely to be a captured stray. And **Luna is exactly that case**: a lone moon at
a known distance, 384.4 Mm from Earth, `frac ≈ 0.386`. The linear map called the
real Earth–Moon system — the `GiantImpact` variant's own namesake — **a capture
39% of the time**. The model contradicted its own exemplar.

Cubing suppresses the middle of the range hard while pinning both ends
(`0.386³ ≈ 0.057`). Under `p_capture = clamp(frac³, 0.02, 0.85)`, Luna reads as
an impact child **94%** of the time. The weighting is still a plausibility rule
— it is not a population synthesis and the model card says so — but it now has
an empirical anchor and a way to be shown wrong, which is a strict upgrade to
the card. Twenty-nine percent of moons remain captured: the recalibration
trimmed misclassified strays, not strangeness.

One number resisted its own explanation, and the correction is instructive.
Under the linear map the innermost moon came out an impact child **68.1%** of
the time, and the first account of *why* was that the mutual-spacing constraint
pushes it outward when a sibling needs room. An A/B falsified that: spacing
pushes the innermost moon **inward** (mean distance fraction 0.163 against
0.308), the tide cap — which forbids close-in moons — pushes it **outward**,
and the two cancel to about one part in ten thousand. The real cause has no
physics in it at all. It is an order statistic: **the minimum of `k` uniform
draws averages `1/(k+1)` of the range**, so the innermost moon's distance
fraction averages ~0.32 whatever the constraints do, and a map linear in that
fraction reads it straight off as a 32% capture chance — 68% impact. The
generator delivers 0.6818 against a pure-uniform prediction of 0.6819; the
constraints contribute essentially nothing. Under the shipped cube the same
slot reads 0.9396.

## The epoch went total, and the reason is the interesting part

The campaign was designed around a property: because both inclination branches
consume one draw from the same stream, a world whose moons **all** draw
`GiantImpact` should be byte-identical to before. Only worlds actually
receiving a captured moon would move. A partial epoch, and a much smaller
census bill.

The Task 4 review found that the epoch pushes the eclipse model **outside its
own declared validity range**. `moon_ecliptic_latitude_deg` computed
`β = i·sin(u)` and `node_crossing_chance` computed `(2/π)·asin(threshold/i)` —
both **linear in `i`**, both written when inclination was always `[0, 10)`, both
declaring the small-angle assumption in their own doc comments. At `i = 160°`
the old form claims a moon reaches ecliptic latitude **160°**, which is not a
physically possible latitude — the maximum is 90°, and the true amplitude is
`min(i, 180−i) = 20°`. Worse, physics requires orbits at `i` and `180−i` to
eclipse *identically*, since `sin i = sin(180−i)`; the model returned an
**8.007× difference** between 20° and 160°. Measured against the exact form, the
error runs ~2% at 20°, 23% at 63°, **130% at 117°, and 717% at 160°** — worst
exactly where this campaign's headline deliverable lives. Seed 42's new moon was
understated 1.87×.

The fix is the exact spherical form, `β = asin(sin i · sin u)`, with
`node_crossing_chance`'s statistical twin becoming
`(2/π)·asin(sin(threshold)/sin(i))`. It is bounded by `±min(i, 180−i)` for any
inclination and it restores the `i`/`180−i` symmetry. Note the direction of
travel, which is rare on this model card: **a quantity moved from *approximated*
toward *derived*.**

And it is unconditional. The exact form perturbs low-inclination moons too — the
`O(i³)` term — so an all-giant-impact world's **eclipse dates** now move as well.
The property splits along a seam worth stating precisely:

- **Masses and distances never move, in any world.** Intact.
- **An all-`GiantImpact` world's inclinations are byte-identical** — the draw
  itself never moved. The property **holds at the draw level**.
- **Its eclipse dates are not** — the geometry correction changed how any
  inclination maps to a latitude. The property **fails at the world level**.
- **Moonless worlds are byte-identical.** The only surviving byte-identity claim
  at the world level, and it survived intact: **0 of 155**.

The measured blast radius is **633 of 1000 seeds**: 373 from the inclination
epoch, and **260 from the eclipse-geometry correction alone** — worlds the epoch
proper never touches, 260 of the 627 seeds that are all-impact or moonless. The
arithmetic closes; an isolation build with the epoch present and the geometry
fix reverted matches the baseline on all 627, confirming the epoch really is a
no-op there.

## What the property actually was

The estimate going in was that the correction's `O(i³)` perturbation would be
under `1e-9°` — the tenth significant digit of a five-degree value — and that
since this project quantizes serialized floats to eight significant digits, the
"total" epoch might cost **nothing**. It was a good argument. It was wrong by
four orders of magnitude: the shift is **0.0185°**, and applied across thousands
of threshold checks in a hundred-year scan it flips a majority of the
low-inclination population's day-level counts. The reasoning error was
generalising from the curve's **peak** — at `u = 90°` the exact and small-angle
forms agree *identically* — to the whole curve.

Which leaves the campaign's deepest finding, and the one worth ending on. The
partial epoch looked like a tradeoff: accuracy purchased at the price of a
cheap regen. Flip it, and it is not a tradeoff at all. **The partial-epoch
property was never a property of the world — it was a property of an
approximation error.** All-giant-impact worlds were byte-identical to each
other only because a wrong formula was being applied to them uniformly. Once
the formula is right, the "property" evaporates, because there was never
anything there to preserve. The campaign was not trading accuracy for
cheapness; **the cheapness was an artifact of the inaccuracy**, and it was
always going to be repaid. The only real question was when, and by whom.

## What it cost, and what it left

Three new streams (`star-age`, `moon-formation`, `moon-density`) and the ages
and origins committed as ledger facts a myth or lexicon consumer can name.
`scene/moons/v1` — a released cross-repo contract that main's sibling campaign
*The Faces* had shipped days earlier, deriving moon radius at an assumed
constant lunar density — was unified onto the real density rather than left to
disagree with the sim, so the repo does not carry two answers for one quantity;
the orrery renders changed radii for captured moons, an icy body at ρ=1.6 being
~28% larger than scene previously reported. The word `bright-icy` already
existed there as a surface class, firing off a hash-derived albedo: the client
had shipped the vocabulary for an icy moon while the model had no concept of
ice, and this campaign supplied the referent.

The census fixtures are **knowingly stale for 633 of 1000 seeds**. The AWS
regeneration was declined at close and the staleness accepted as debt for a
future campaign to batch — the measured number, not an estimate, is what that
campaign inherits. The spec's flagged contention ("two epochs, one census
budget") was answered by events rather than adjudication: rift-and-fit's regen
had already run, and terrain went first.

Left standing, and honestly: a captured moon's age bound of `[0.05, 0.95] ·
planet_age` is an approximation, not a derivation — a captured body can be
*older* than its planet, as Triton most likely predates Neptune's final
assembly. `maria_fraction` remains composition-blind; an icy moon can still
carry basaltic maria, damped rather than solved. And **temporary** capture — a
body held for a few orbits and then lost, as Earth has really had twice this
century — stands as the honest sibling of the permanent capture shipped here,
banked as a future direction and naming the one assumption this campaign left
undisturbed: that a world's roster of moons is a constant fixed at genesis, and
not a function of `WorldTime`.
