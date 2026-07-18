# Retrospective — The Quickening (The Walk Milestone 2: first liveness)

One page of process, not product. The product is chronicled; this is what
the close learned that the code does not record.

## What went well

- **The demography-inert-actor finding is this campaign's measure-don't-
  narrate win, and it happened before a single line of implementation code
  was written.** The original crux resolution (#2) picked settlement
  demography growing toward carrying capacity as the first liveness actor —
  reusable substrate, genuinely discrete milestones, no new agent concept
  needed. Rather than starting the plan from that premise, the next pass
  went and read what a built world's `population` field actually holds:
  values of 2–8, and the code that produces them calls the result, in so
  many words, a "conserved catchment readout... conserved against total K
  by construction." Logistic growth toward a K the population is already at
  is flat, forever, on every seed — zero tier milestones, an inert
  campaign wearing a working test suite. This is the identical shape as
  UNI-22's "bugbear sorts first alphabetically, places on 0/30 seeds"
  finding from a prior campaign: the natural, uncontested story
  ("settlements grow, so let growth be the actor") was never checked
  against the actual numbers it would operate on, and checking it — cheaply,
  at plan time, before any code existed to defend — is what caught the false
  premise. The pivot (decision-ledger #8) replaced the actor with a derived
  NPC's deterministic daily movement, reusing more existing substrate than
  the demography actor would have (no trajectory model, no carrying-capacity
  read) and landing the true shape the metaplan names: NPCs are the same
  agent spine, taking deterministic daily actions.
- **The observation-payoff gap was caught by review, not assumed away, and
  the fix generalized rather than special-cased.** Task 3's first cut derived
  NPCs from the *k* most-populous settlements while the possessed agent
  starts at the *first* settlement listed — two independent selections with
  no guaranteed overlap, so on most seeds no NPC would ever share the
  player's room and `wait` would report a content-free "N stirred." This is
  the same failure shape as the demography pivot, one layer up: a
  demonstration that doesn't actually demonstrate. The review caught it
  before close, and the fix (`ordered_for_derivation`, guaranteeing the
  possessed agent's own settlement survives k-truncation regardless of its
  population rank) is proven adversarially — a home settlement with the
  *lowest* population in the roster still lands first, ahead of every larger
  rival — rather than patched to pass on seed 42 alone. The lesson
  generalizes past this one campaign: shipping the *mechanism* for an
  observable payoff is not the same as verifying the payoff actually
  fires, and the gap between them
  is exactly where a demo can look complete and be silently inert.
- **Four independently-motivated substrate campaigns composed on the first
  attempt.** The tick (ECS Campaign 6), latest-wins dated reads (ECS
  Campaign 5), the agent spine (Milestone 1), and the room mesh all
  predate this campaign by weeks and were built for reasons that had
  nothing to do with liveness. `AgentMovements` is the first real domain
  system ever run through `tick`; `agent_position`'s latest-committed-else-
  derived read is c5's `kind_of` pattern applied to a position instead of a
  kind; an NPC is `mint_flagship`'s own derivation run a second time. None
  of the four needed to change. That is the actual return on a substrate
  program that shipped six campaigns before this one had a reason to exist.

## What the campaign taught mid-execution

- **A closing task inherits open trade-offs, and the report that flags them
  is what makes closing them tractable.** Task 3's own report explicitly
  named that the frozen day-0 gallery transcript would go stale once `wait`
  became real (its committed `wait 90` line still showed the old
  room-redescribe text) and deliberately deferred the fix to this close,
  per the project's regenerate-at-campaign-boundary convention. Finding
  this drift during Task 4's own regen-and-diff step could have read as a
  genuine violation of the "frozen transcript must stay byte-identical"
  contract this campaign's own spec states twice — the difference between
  "known, flagged, deferred" and "unexpectedly broken" is entirely the
  earlier report's paper trail. Closing tasks should keep reading prior
  tasks' "known trade-off" sections as literally as their own brief; a
  flagged trade-off with no due date is still due at close.
- **A recount without a date is a pile, not a history.** The historiography
  window's `recount` function has replayed an entity's committed facts,
  predicate docs, and provenance since it was first built — but never the
  day a fact was asserted, because nothing it had recounted before this
  campaign was ever committed more than once per predicate per entity.
  `agent-at` is the first *non-functional*, repeatedly-committed, dated
  predicate historiography has ever had to render, and without the day
  every position an NPC has ever held reads as an undated, unordered list —
  exactly the gap the brief's own example sentence ("the herder was at the
  river on day 5") named as the actual deliverable. The fix is a small,
  general addition (render the day when a fact carries one) rather than an
  `agent-at`-specific special case, so every future dated, non-functional
  predicate inherits a real timeline for free.

## Followups (register, promoted verbatim)

1. **NPC needs / goals / the GOAP motivation engine** — the routes are
   fixed deterministic schedules this slice; the next liveness campaign
   gives agents needs (setpoints), goals, and the planner that closes the
   gap. The activity-cycle phase is a placeholder for a real drive.
2. **NPC interaction** — agents affecting each other (encounters,
   conversation, trade, conflict); this slice is parallel non-interacting
   routines. The social graph rides this.
3. **Belief / inference** — the possessed agent's knowledge of NPC
   movements as projected/inferred belief (Milestone-2-later); today `look`
   and `why` both read ground-truth positions.
4. **Richer routines** — multi-stop routes, need-driven / seasonal
   schedules, errands; the two-point home⇌destination schedule is the
   coarse first behavior (fidelity note, spec §4.2).
5. **A demographic time-model** — the substrate has no birth/death/growth
   dynamic (population is a conserved equilibrium, decision-ledger #8);
   introducing one so settlements can actually grow/decline over sim time
   is its own campaign, and would revive the original tier-milestone idea
   with a real dynamic under it.
6. **Player-acts-mutate** — the sibling slice: the player's own verbs
   committing dated events, the confabulation seam, "why is X?" over the
   player's actions rather than an NPC's.
7. **The over-time transcript as a live book exhibit** — like The Casement
   did for the frozen possess loop; a wasm exhibit of the world moving on
   `wait` (deferred; The Casement's `make vessel-check` gate is the
   precedent). This close ships the over-time transcript as a second
   *recorded* gallery artifact only — a live in-browser version stays
   future work.

## Why liveness now, not ECS Campaign 7

ECS Campaign 6 (*The Ordination*) closed the entity-component substrate
program's sixth campaign and named Campaign 7 (spatial partition) as its
seventh and last. That campaign remains reserved, not cancelled — decision
0037 already scopes it, but nothing has yet measured a trigger for it (no
room-tier ledger has grown large enough to need chunk-partitioning). Rather
than build reserved capacity against a demand nobody has measured yet,
Nathan chose to pivot to liveness: the substrate program's own retro
(*The Ordination*) had already predicted that the taste-gated liveness arc,
not another substrate campaign, is what would first move a Confidence
Gradient bet. This campaign is that arc's first slice, run ahead of
Campaign 7 rather than after it — the tick mechanism c6 built sat idle for
exactly one campaign before a real domain system used it.

## Confidence Gradient

Checked `book/src/open-questions.md` for a moved bet (grepped `walk`,
`liveness`, `agent`, `NPC`, `possess`, `inhabited`, `game layer` — no hits
beyond an unrelated "walk" inside "O(1) integer neighbour walk," a room-mesh
term). The phrase "someone walks in it" that motivated this campaign's own
plan document is the Constitution's **Year-3 roadmap milestone name**
(`docs/superpowers/specs/2026-07-05-hornvale-longterm-plan-design.md`), not
a row in this chapter — the Confidence Gradient is scoped to Year-1/Year-2
*world-generation* research bets (refinement at scale, emergent economics,
historiography worth reading, the deep-time forcing horizon), none of which
reference agents, NPCs, or the game layer. No bet in this chapter moved.
This is the same conclusion *The Ordination*'s retro reached one campaign
earlier, for the identical reason: this campaign is substrate and its first
real payoff, sitting *beneath* the generation/taste bets this chapter
tracks, not a resolution of one of them. The Year-3 "someone walks in it"
horizon itself has now taken its first real step — the world's first
autonomous motion — but that horizon is not this chapter's bookkeeping to
re-score; it belongs to whatever chapter eventually tracks the game layer's
own confidence gradient, which does not yet exist.

The registry (`book/src/frontier/idea-registry.md`) IS this campaign's
correct home for tracking the idea-level movement, and it was updated:
PSY-6 (the motivation engine) now notes that its setpoint idea shipped only
conceptually — a fixed activity-cycle phase, not a planned goal — with the
planner itself still fully deferred; UNI-20 (the derived-view architecture)
now notes this campaign as its first architecture actually running, not
merely designed: an NPC's position is a genuine VIEW (latest committed
`agent-at`, else the derived schedule) over one event ledger, the seam rule
made concrete for the first time outside a toy system.
