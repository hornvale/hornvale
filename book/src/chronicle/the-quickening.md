# The Quickening

Six campaigns built a world that never moved. Astronomy turned a sky,
climate reorganized its bands, terrain carved a coastline, settlement
placed a village, culture and religion mythologized it — all of it derived,
all of it deterministic, all of it committed once, at genesis, and frozen
forever after. Milestone 1 of The Walk let you *walk* that frozen world —
`possess --seed 42` gave a look/go/examine loop over a pure step function —
but a step function over a frozen ledger is still a photograph, however
detailed. Nothing in it was alive. This campaign gives the world its first
autonomous motion: while you possess one agent and let time pass, another
agent goes about its day, and you perceive it when you look back.
*Quickening*, in its oldest sense, names exactly this — the first
perceptible stirring of life, the first fetal motion — after six campaigns
of a fetus that never kicked.

## The actor that wasn't there

The plan going in was settlement demography. Every settlement in a Hornvale
world already carries a `population` fact and a carrying capacity `K`; the
obvious first liveness slice was to let population climb toward `K` over
sim time and commit a settlement's tier promotion — hamlet to village to
town — as the discrete, observable event. It reused existing substrate, it
needed no new agent concept, and demographic growth is about as
uncontroversial a "the world moves" story as a deterministic simulation
gets.

It was also wrong, and finding out *why* is the campaign's real headline.
Before writing a line of the growth model, the plan-time pass went and
looked at what `population` actually holds in a built world — and the
values are 2 to 8. Not "2,000 to 8,000 people, abbreviated." Two to eight.
The code that produces the number calls it, in so many words, a "conserved
catchment readout... conserved against total K by construction" — a
distributional allocation, sized so a handful of exemplar settlements
divide up a region's carrying capacity, not a headcount anyone had ever
intended to grow. Logistic growth toward a K that population is already
*at* is flat from day zero. Zero milestones, ever, on any seed. The
demography actor was not a coarse first model that needed tuning; it was
an actor with nothing to do, and no amount of tuning fixes an inert actor.

This is measure-don't-narrate doing its actual job: the natural story to
tell — "settlements grow, so let them grow" — was never checked against
the numbers it would operate on, and checking it, cheaply, before the plan
committed to it, is what caught the false premise. A campaign that had
instead trusted the story would have built the growth model, built the
tier thresholds, run the tick, and watched every seed report zero
crossings — a working, tested, reviewed, entirely inert campaign, the
liveness equivalent of a settlement that "sorts first alphabetically" but
places on zero of thirty seeds. The fix wasn't a patch to the growth
curve. It was recognizing that the physical fields (sky, climate) already
move, the social fields (population, culture) are static equilibria by
construction, and if the world is going to move at all, the motion has to
come from somewhere with no equilibrium to sit at: an agent, choosing
where to be.

## The routine and the happening

The replacement actor is smaller than the one it replaced: a handful of
NPCs, derived exactly the way the possessed agent already is (Milestone
1's `mint_flagship` gives an `Agent` with a `RoomAddr` position; an NPC is
the same derivation, minus the player), each anchored to a settlement and
carrying its species' activity-cycle — diurnal, nocturnal, crepuscular,
already-authored data with nothing yet reading it. Each NPC's day is a
two-point schedule: home while resting, one fixed neighboring room while
active. `scheduled_position(npc, t)` computes it — a pure function of the
NPC and the time, total and deterministic over every `t`, checked by a
property test that also stands as the campaign's own anti-inert guard
(the mistake that killed the demography actor gets a permanent regression
test here: an NPC's schedule must *provably* differ between its rest and
active phases, not merely be assumed to).

That schedule is never committed. It doesn't need to be — it's a smooth,
periodic, fully reversible function of the seed and the clock, exactly the
kind of state The Walk's seam rule says to *derive*, not store. What gets
committed is the discrete thing the schedule implies: the moment an NPC's
position actually changes. A `TickSystem` — `AgentMovements`, run through
the tick mechanism ECS Campaign 6 (*The Ordination*) built and proved but
never exercised on a real domain — evaluates every NPC's schedule at the
new time and, only where the answer differs from the last committed
position, emits one dated `agent-at` fact. `wait <N>` is the verb that
triggers it: it advances the possess session's own `WorldTime`, runs the
tick, and returns whatever an observer standing in the room would actually
notice. The distinction the whole architecture turns on is the one between
the *routine* — reversible, recomputed, worth nothing to store — and the
*happening* — the one moment a position changed, worth remembering because
it might be asked about later. Storing the routine would mean committing a
position fact for every NPC on every observed day, forever, whether or not
anything changed; storing only the happening means the ledger grows
exactly as fast as the world actually diverges from what could be
re-derived, and not one fact faster.

## Four campaigns, one tick

What makes this the first *real* liveness, rather than another derived
field with better PR, is that it is the first place four separate pieces
of substrate — built across four different campaigns, for four different
reasons — run together at once, on purpose, for the first time:

- **The tick** (ECS Campaign 6, *The Ordination*) — `TickSystem` and `tick`
  existed as a proven mechanism with a toy system exercising it. This
  campaign is the first time a *real* domain system runs through it.
- **Latest-wins reads** (ECS Campaign 5, *The Individuation*) — an NPC's
  `agent-at` is registered non-functional, exactly the "a subject's kind
  changes over sim time" shape c5 built the dated-latest-value read for.
  `agent_position` is: the latest committed `agent-at`, else the derived
  schedule — the same read pattern as a kind-change, applied to a position
  instead of a kind.
- **The agent spine** (The Walk, Milestone 1) — an NPC *is* the possessed
  agent's own derivation, run again minus the player. Nothing new had to
  be invented for "what is an agent"; the type already existed, at the
  frozen-world layer, waiting for a second instance.
- **The room mesh** — an NPC's fixed destination is one hop away via the
  mesh's O(1) neighbor walk, the same primitive `possess`'s `go` verb
  already uses to move the player.

None of these four pieces changed to make this work. They were built,
separately, for separate reasons, over separate campaigns that had no idea
liveness was coming — and they composed on the first attempt. That is the
substrate program's actual return on investment, made visible in one
`wait` verb.

## What you can now observe — and ask

The payoff has to be perceptible to count, so the campaign's own review
went and checked that it actually was. The first cut of `wait` derived NPCs
from the *k* most populous settlements, and the possessed agent starts at
the *first* settlement listed — two selections with no guaranteed overlap.
On the majority of seeds, no NPC would ever share the player's room, and
`wait` would report a content-free "N stirred" — a demonstration that
demonstrates nothing, the identical failure shape as the dead demography
actor, just one layer up the stack. The fix guarantees the possessed
agent's own settlement always contributes one of the derived NPCs
(`ordered_for_derivation`, proven by an adversarial test: a home settlement
with the *lowest* population in the roster still lands first, ahead of
every larger rival), so the observation is grounded rather than contrived.
What you actually see, on seed 42, `wait 1`:

```
> wait 0.5
Time passes. You watch hobgoblin of Feefaenoagoo go.
> look
[room 815726603, day 0.5]
...
> wait 0.5
Time passes. You notice hobgoblin of Feefaenoagoo here now.
```

A committed `agent-at` is a dated, provenanced fact, so the world doesn't
just move — it *remembers* having moved. This campaign gives the possess
session two verbs to ask it: `npcs` lists who shares the world with you and
their entity ids, and `why <who>` (by label or id) recounts everything
committed about that entity, in order, each dated line naming the day it
was asserted:

```
> why hobgoblin
hobgoblin of Feefaenoagoo:
- canonical name of an entity: hobgoblin of Feefaenoagoo (asserted by the-quickening)
- an agent's position on a day: 815333387 (asserted by the-quickening, day 0.5)
- an agent's position on a day: 815726603 (asserted by the-quickening, day 1)
```

`why` is not new code built for this occasion — it is the existing
domain-agnostic historiography window (`recount`, already backing the
CLI's `why <id>` over beliefs and settlements) handed the session's own
evolving ledger instead of a frozen genesis one, plus one small addition
that pays for every future dated predicate at once: the recount now names
the day a fact was asserted, not only the value and who asserted it. An
undated fact reads exactly as it always has; a dated, non-functional one —
like `agent-at`, one fact per position change — reads as an actual
timeline rather than an unordered pile of "was somewhere, once."

## The shadow this campaign keeps

The determinism contract this campaign had to satisfy going in was the
strictest of any liveness slice: **the world moves only inside a possess
session, and never at genesis.** A freshly built world — the thing the
census, the almanacs, and every gallery and laboratory artifact measure —
commits exactly the facts it always has; a pinned test asserts a genesis
world holds zero `agent-at` facts, full stop. Liveness lives entirely in
the vessel window's session state: a cloned ledger and registry, mutated
only by `wait`'s tick, discarded when the session ends, never written back
to anything committed. Same seed and the same sequence of verbs produce a
byte-identical session ledger — no player-visible randomness anywhere, the
route and the tick both pure functions of the seed and elapsed time. No
epoch was cut, no census was regenerated, and the campaign's own drift
check confirms it: `book/src/laboratory/` and `cli/tests/fixtures/` carry
not one changed byte across all four tasks.

One frozen artifact *did* need touching, and it is worth naming exactly
why that isn't the same thing as genesis drifting. `wait`'s meaning changed
mid-campaign — from a no-op that re-described the room to a verb that
actually runs the tick — and the day-0 gallery transcript (recorded before
this campaign existed) happens to contain a `wait 90` line. Once `wait`
became real, that recorded line's *output* necessarily changed too: not
because anything about genesis moved, but because the verb being
transcribed does something different now. This was flagged, by name, the
task it happened in, and deliberately deferred to this close rather than
patched mid-task — the project's committed convention that gallery
artifacts regenerate at campaign boundaries, not on every intermediate
commit. Regenerating it now is that deferred sweep, not new drift; a
**second**, new transcript sits beside it — the frozen one still opens on
a still world, and the new one `wait`s across a full day so the departure,
the return, and the `why` recount are all visible in one recorded session.

## What remains

Nothing here plans. The routes are fixed, authored, two-point schedules on
a bare activity-cycle phase — not needs, not goals, not the GOAP autonomy
ladder the motivation engine eventually wants; that stays exactly as
deferred as it was before this campaign, now with one concrete example of
what it would schedule *for*. NPCs don't interact with each other or with
the player,
belief about an NPC's whereabouts is read as ground truth rather than
inferred, and the substrate still has no birth/death/growth dynamic — the
tier-milestone idea this campaign scrapped is not dead, only waiting for a
demography model with an actual gradient to promote through. All of it is
captured, not lost, in the followup register this close promotes into the
retrospective. What exists now, for the first time in six campaigns and
one pivot: a world where something happens whether or not you're looking,
and a question — `why` — that gets you a truthful, dated answer about what.
