# 0089. Peoples-diversity is a terminal value; rubberbanding is multi-axis contest, never handicap

**Status:** Accepted (2026-07-29) · **Decider:** Nathan · **Relates:**
[0064](0064-potency-is-challenge-rating-over-thirty.md),
[0065](0065-socialform-and-the-nested-capacity-lattice.md),
[0079](0079-census-goldens-are-authored-on-one-enforced-host.md)

In the context of a brainstorm asking where Hornvale's growth functions could
be made to compose into powerful synergies — and finding that the history bake
evaluates every people on **one scalar axis**, so that weakness there is
absolute — we decided that **the persistence of weak peoples is a terminal
value of the project**, that **the only sanctioned mechanism for it is
multi-axis contest**, and that **an authored handicap is never sanctioned**.

## The axiom

> A world that loses its goblins has gotten worse, however faithfully it did
> so. Peoples-diversity is a design objective in its own right, not merely an
> outcome to be measured.

Stated positively, because the negative form invites the wrong fix: the world
should be one in which a militarily hopeless people has *something else that
is true of it*, and survives on that. Not one in which the simulation
intervenes on its behalf.

Two supporting facts of record:

- **The goblin is the origin of the species vector space.** From
  `domains/species/src/lib.rs`: *"Goblin is the baseline: scalars 0.5, default
  enum variants; every downstream modulation is the identity function at this
  vector."* Every other people in Hornvale is a displacement from the goblin.
  The axiom is therefore not sentimental about one kind — it is a statement
  about the coordinate system the whole biosphere is written in.
- **The project already believes this in space and not in time.** MAP-22
  diagnosed per-cell fitness-argmax as *manufacturing* monoculture and
  replaced it: `domains/demography/src/coexist.rs` ships a `K^β` softmax at
  β = 2.0 with a viability floor, a founder floor reserving each people its
  best cell, and refugia. Meanwhile `windows/worldgen/src/history_bake.rs`
  resolves every contest with `strength <= holder_strength * RAID_MARGIN` —
  one scalar, one comparison. **The bake runs the exact failure mode the
  coexistence stack already diagnosed and fixed**, one campaign earlier, in
  the other half of the same world.

## Clause 1 — the mechanism is multi-axis contest

Three independent literatures give the same requirement, and it is the
requirement rather than any one of them that binds:

- **Ricardo.** Comparative advantage exists only when there is more than one
  good. On a single axis there is only *absolute* advantage, and the weaker
  party has nothing to offer.
- **Chesson.** Coexistence requires each species be limited more by itself
  than by its rivals. On a single shared limiting factor, one wins.
- **Ammann.** A creature's tactics derive from its *contour* — the pattern of
  peaks in its stat block, not the magnitude. Weak creatures do not win the
  strength contest; they decline to have it, and change *when*, *where*,
  *how many*, or *in what currency* it is fought.

**Weakness is only absolute when there is one axis.** Therefore: diversity is
held open by giving contest more than one dimension, so that being poor on one
is compatible with being rich on another. Rubberbanding is then a
*consequence* of the model — the same way *The Tithe*'s single discount rate
generated the Danegeld, tax farming and the bust-out as a family rather than
as a list — and never a rule that fires.

**Forbidden by this clause:** any term whose input is a people's *rank*,
*weakness*, or *disadvantage as such*. A floor constant that exists to keep
somebody alive is an authored handicap regardless of what it is named.

**Permitted and encouraged:** terms that are indifferent to who benefits.
Marginal ground is cheap to hold *and* not worth taking; that it shelters the
weak is a byproduct, not a purpose.

## Clause 2 — "Shit Happens" over "Miracles Happen"

Rubberbanding decomposes into **suppress-the-leader** and
**support-the-laggard**, and these are not counterparts.

They differ **structurally**: annihilation is an *absorbing state* a people
falls into; domination is a *limit* a people approaches. Preventing an
absorbing state and creating one are different operations, and a saturating
response gives the downside tail for free while foreclosing the upside one.

They differ in **legitimacy**, which is the operative half. Every
leader-suppression mechanism has a real referent and a literature —
overextension, Turchin's elite overproduction, monoculture fragility, imperial
overstretch, war exhaustion, diseconomies of scale. Laggard support has none:
the world does not owe the weak a break, and a sim that gives them one is
doing precisely the thing clause 1 forbids, with better manners.

**Therefore the design leans on the cost of dominance.** Laggard survival is
admitted only as a byproduct of mechanisms that would exist anyway.

**Corollary — the naming discipline.** "Miracles Happen" and "Shit Happens"
are sanctioned as **names for the tails of mechanisms that already exist**,
never as rules that fire on their own. A miracle is what a refugium looks like
from inside it; Shit Happening is what a paleoclimate excursion looks like to
an empire caught mid-expansion. This is the same commitment MAP-69 already
states for the underworld — *a mechanism whose emergent output resembles the
trope, never an authored trope* — held at world scale. In a world with
interventionist deities a miracle may have a cause, a ledger fact and a
provenance; what it may never have is no cause.

## Clause 3 — rare extremes require asymptotes, not clamps

A world with no goblins, and a world under one government, must both be
**reachable and rare**. That is a structural requirement on the response form,
not a tuning target:

- A **clamp** hits its bound and the probability of exceeding it is exactly
  zero, at any input.
- An **asymptote** is approached, and every excursion retains decreasing but
  nonzero probability.

Hornvale's saturating bounds are presently clamps — population against cell
capacity, `tech_weight` against 3.0, the `FLOOR` in `coexist.rs`. **No input
distribution can produce a rare extreme against a clamp.** Where the design
intends "rare but possible," the response must be written as an asymptote.

Rarity is to be carried by **variation in the input**, not by drawing the
response's own shape per world. `coexist.rs`'s β is a save-format constant and
a direct knob on the outcome; drawing it per world would make the diversity
measurement a readout of its own input.

## Consequences

- **Binding on design, not on measurement.** This decision does not license
  tuning a constant to rescue a diversity number, and does not weaken
  preregistration (0016). A campaign that measures monoculture reports
  monoculture. The axiom governs *what mechanisms are admissible*, not what
  results are reportable.
- **The bake is now known to be out of compliance** with clause 1, and that is
  the standing charge *The Contour* opens against.
- **Sequencing consequence.** Any proposal to hold diversity open by a floor,
  a cap on the leader, a minimum population, or a per-people bonus is refused
  by clause 1 and should be redirected to a second contest axis.
- **This is Constitution-adjacent but not Constitutional.** It governs the
  domains' modelling choices; it does not amend the spec's Constitution, and
  the spec still governs on conflict.
