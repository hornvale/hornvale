# SOC-household — decision ledger

Campaign: **SOC-household** — the individual, household, kinship, lifecycle,
and social-relation substrate. Branch `campaign/soc-household`. Autopilot is
engaged; G3 spec review and G6 close remain hard stops.

## #1 [G1] — What is the foundational household architecture?

**Decision:** use a temporal event-sourced relation graph with derived group
and household projections. Use multi-party relation events where care,
residence, inheritance, or institutional roles cannot be represented honestly
as dyadic edges.

**Why:** The Grammar and The Murrain establish a two-tier population model;
The Lot requires lifecycle facts, not a universal domestic container. The
event graph preserves provenance through migration, separation, dissolution,
adoption, death, and recomposition while allowing multiple cultural household
forms.

**Alternatives discarded:** household-first containers make one family form
foundational; a pure role-slot hypergraph is more abstract than the first
substrate and risks becoming an unbounded ontology.

**Ideonomy passes / overturns:** three passes (substitution/abstraction-lift,
tree-finding/dimension-identification, and tree-finding/substitution). No
recommendation overturn. The passes sharpened the decision by identifying
households as projections, death and dissolution as non-deleting transitions,
and multi-party care as the main case where pairwise edges are insufficient.

**Capture:** architecture and trade-offs are in §3; the event, projection,
and truth-class boundaries are in §§4–6.

## #2 [G2] — What must remain distinct?

**Decision:** keep sex traits, reproductive role, body plan, social gender,
personal identity, and transition history as separate concepts. Keep
possibility, typicality, realization, and institutional recognition as
separate truth classes.

**Why:** The Grammar's BIO-3/SOC-2 contract explicitly exposes reproductive
roles and transition history without social interpretation. Collapsing these
axes would make biology social destiny and would prevent synthetic societies
from expressing non-human or non-binary arrangements.

**Alternatives discarded:** deriving gender from reproductive role; treating
sex as a single immutable scalar; treating institutional recognition as the
relation itself.

**Ideonomy passes / overturns:** three passes as in #1; no overturn. The
passes added explicit handling for recognition as a separate interpretation
layer and for historical transition as an event family.

**Capture:** §§4–5 and the Lot constraints in §8 of the design.

## #3 [G2] — What is the minimum campaign scope?

**Decision:** define the cohort-to-person boundary, temporal relation/event
vocabulary, derived group projections, cultural interpretation seam, synthetic
probe panel, and Lot observation contract. Defer full economy, institutions,
magic, detailed anatomy, and existing-species canon.

**Why:** This is the smallest substrate capable of answering The Lot's
household and biography silences without turning the campaign into a complete
social simulation.

**Alternatives discarded:** implementing a full household economy; reopening
BIO-3; adding a complete institution or magic system; assigning household
canon to existing peoples.

**Ideonomy passes / overturns:** three passes; no overturn. The convergence
added anti-vacuity probe requirements and made The Lot's silence rule an
explicit acceptance boundary.

**Capture:** §§2, 7–9 of the design; follow-up decisions belong here as they
arise after G3 review.

## Follow-ups

- Confirm whether the first implementation should split the person/event
  substrate and group projections into separate crates or keep a minimal
  shared domain boundary; decide only after repository precedent review in the
  approved implementation plan.
- Coordinate the final SOC-household interface with The Murrain before
  implementation, since The Murrain currently defers its household lattice.
- Keep all synthetic probe names test-only until a later campaign explicitly
  authors species or society canon.

## #4 [G4] — How should the approved design become executable work?

**Decision:** use five independently testable tasks: aggregate substrate,
realized person/event contracts, synthetic cohort realization, derived kinship
and group projections, and The Lot observation. Keep domain ownership split by
existing layering: demography owns pure aggregate/projection logic, person
owns person predicates, history owns append-only events, and worldgen composes
them.

**Why:** the repository already separates person predicates, history facts,
demography summaries, worldgen composition, and Lot reads. The decomposition
keeps sibling domains independent and makes each boundary reviewable before
the next one is wired.

**Alternatives discarded:** one large household task would hide boundary
mistakes; a new cross-domain social crate would violate the current domain
layering unless its vocabulary proved kernel-wide; implementing The Lot first
would make the observation surface drive the simulation model.

**Ideonomy passes / overturns:** three prior convergence passes; no new
overturn. The plan preserves their result by making household projection a
late task and anti-vacuity tests mandatory at every realization boundary.

**Capture:** the complete task decomposition is in
`docs/superpowers/plans/2026-09-07-soc-household.md`.
