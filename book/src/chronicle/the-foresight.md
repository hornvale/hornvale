# The Foresight

The Wanting gave a routine its first *want* — a homeostatic drive that closes
a gap by moving. But the gap it could close was never more than one hop away:
the resource sat a single mesh step from home, so the reactive controller's
whole intelligence amounted to noticing it was thirsty and walking toward
water it could already see. Ask *how* it knew the way and the honest answer
was "there was nowhere else to go." This campaign gives the routine
**foresight** — the capacity to search a space of possible futures for one
that reaches a goal not directly reachable, and to commit to the sequence of
actions that gets there. The agent stops reacting and starts *planning*.

## GOAP is A*, and that is the whole idea

Goal-Oriented Action Planning sounds like its own discipline, with its own
machinery. It is not. Strip away the game-AI branding and GOAP is a
world-state (a set of facts), a set of actions (each gated by a precondition,
each producing an effect), a goal test, and a search for the least-cost
sequence of actions from the current state to a goal state. That search is
**A\***. Once a drive names a goal — *be hydrated* — the planner's entire job
is graph search: treat every reachable world-state as a node, every action as
a weighted edge, and find the cheapest path to a state where the goal test
passes.

This campaign's action space is deliberately the smallest one that is
*genuinely* GOAP rather than dressed-up pathfinding: two heterogeneous
actions, `MoveTo(room)` (precondition: adjacency; effect: a new position) and
`Drink` (precondition: standing in the water room; effect: hydrated). The
`PlanState` A* searches over is `{ position, hydrated }`, and the goal test is
simply `hydrated`. That's it. No verb DSL, no authored content, no arbitration
between competing wants — just enough structure for a search to have
something worth finding.

## The precondition chain is the difference from pathfinding

Here is the detail that makes this GOAP and not merely "the NPC walks to
water": the goal is not *be at the water room*, it is *be hydrated*, and the
**only** action that produces that effect, `Drink`, has an unmet
precondition — the agent is not yet standing where `Drink` is legal. A*
cannot satisfy the goal directly; it must first find a path that
**establishes the precondition**, then chain the resource-satisfying action
onto the end of it. The plan A* returns is never just a route — it is
`[move, move, …, drink]`, with the very last step being the one that actually
matters and every step before it existing solely to make that last step
legal.

This is the load-bearing distinction decision-ledger #5 drew before a line of
search code existed: if the goal had stayed "be at the water" (The Wanting's
model, where drinking was automatic on arrival), A* would degenerate into
ordinary shortest-path — real, but not GOAP. Making `Drink` a first-class,
precondition-gated action is what forces the planner to *chain*
preconditions, which is GOAP's defining move and the reason the algorithm
needed a general graph search instead of a simple traversal in the first
place.

## The general kernel A*, and why it belongs there

The planner itself does not live in `windows/vessel` beside the two
authored actions — it lives in the **kernel**, as `hornvale_kernel::astar`
over a `SearchSpace` trait (`State: Ord`, `successors`, `goal`, `heuristic`).
The GOAP space (`GoapSpace`, `PlanState`, `Action`) is a thin instantiation
that supplies the vessel-specific semantics; the search itself knows nothing
about drives, thirst, or rooms.

The reason is a framing stated before this campaign ever ran: A\* is the rare
natively-deterministic game-AI primitive, so one small kernel planner serves
navigation, GOAP, confabulation, prophecy — each with its own state space and
cost. A* is a search over a small trait, fully
domain-agnostic; building it once, in the kernel, and instantiating it per
tense is the right abstraction, not an indulgence. This campaign builds
**exactly one** instantiation — GOAP — and deliberately does not build the
others now. Navigation over the terrain graph, backward confabulation
(Campaign IV's plausible-history search, cost = implausibility), and forward
prophecy (fate as A* toward a foretold outcome) are each a future campaign
that reuses the same kernel function with its own `SearchSpace`. The
discipline mattered as much as the code: one engine, one instantiation,
everything else named as a followup rather than spec'd into existence
because it was cheap to imagine.

## Path determinism: the property that lets a determinist sim plan

Planning is a search over hypothetical futures, and a naive implementation
of "search over hypothetical futures" sounds like exactly the kind of thing
that could quietly break byte-identity — ties between equal-cost paths,
iteration order over a frontier, some structure whose enumeration order
depends on hash seeds or allocator layout. None of that is true of A* done
correctly, and the kernel's implementation is built to make that provable
rather than assumed: the open and closed sets are `BTreeMap`/`BTreeSet` (no
`HashMap` anywhere — the constitution's ban is not incidental here, it is
the whole reason A* is safe), and the frontier's priority is a **total
order** over `(f-cost, g-cost, state)` — `State: Ord` is a trait bound the
kernel enforces, not a convention callers might skip. No randomness enters
the search at any point.

The result is the keystone property the decision ledger named before
implementation: **A\* is a pure deterministic function of its graph and its
tie-break.** Given the same successors, goal, and heuristic, it returns the
same path byte-for-byte — even, and especially, when multiple paths of
equal cost exist. The kernel's own test suite plants exactly that case (two
distinct equal-cost routes to one goal) and asserts the returned plan is
stable across a hundred repeated calls. This is *why* a determinist sim is
even allowed to have a planning agent: A* is the one canonical game-AI
technique that needs no dice roll to be interesting, so "models author, dice
roll" and "the agent has foresight" are not in tension here the way they
would be for almost any other planning algorithm.

## The reserved seam pays off

The Wanting did not build a planner; it built a *seam* for one, on purpose.
Its `decide(view, home, resource, params) -> Intent` function was, by that
campaign's own account, "one function shaped correctly, so that the
eventual campaign which adds... the A\* search... changes `decide`'s body,
not the tick, the commit path, or anything reading its result." This
campaign is that eventual campaign, and the payoff is exact: `decide`'s
signature is unchanged (still reading a `Perceived` view, never the world
directly), `Intent` still generalizes — from `{GoTo, Hold}` to
`{Do(Action), Hold}` — and the tick still depends on nothing but `Intent`'s
output. The body swap is total: where `decide` once returned a target room
to walk toward, it now runs `plan_to_water` (or `plan_to_room`, for the
return leg) and returns the **first action of the least-cost plan**. No
re-plan caching — every decision point re-runs A* from scratch, which is
fine because the search is deterministic and cheap; caching a stable
computation is a followup for when it is measured to matter, not assumed in
advance.

The `view` seam survives untouched for the same reason: `decide` reads a
`Perceived { position, drive }`, which is ground truth today and will be
the agent's *belief* — possibly false — the moment a future projection
layer fills that slot instead. Nothing about this campaign's planner cares
whether `view` is truth or belief; that is precisely the point of having
reserved the seam rather than hard-wiring a direct world read.

## What you can watch and ask

`why <npc>` already replayed an NPC's dated history in The Wanting; this
campaign gives that history a second kind of fact to replay. A journeyed,
thirsty NPC's recount now shows the whole precondition chain as a story —
the moves that established the precondition, then the fact that finally
satisfied the goal:

```
> why hobgoblin
hobgoblin of Feefaenoagoo:
- canonical name of an entity: hobgoblin of Feefaenoagoo (asserted by the-quickening)
- an agent's position on a day: 815562763 (asserted by walking to water (thirst), day 5.7666667)
- an agent satisfied its sustenance goal: true (asserted by drank (thirst sated), day 5.7666667)
- an agent's position on a day: 815726603 (asserted by walking home (sated), day 5.8666667)
```

`recount` (the historiography window) needed no change at all to produce
this — it replays whatever facts `facts_about(entity)` yields, using
whatever the registry's predicate doc and each fact's own provenance say.
The new `drank` predicate (registered by the session, exactly as `agent-at`
was in The Quickening) simply joins the recount the moment it exists. The
same domain-agnostic seam that made a moved NPC's history legible now makes
a *satisfied goal* legible too, for free.

`needs` still reads a co-located NPC's felt state as diegetic prose
("looks parched," "seems content," "could do with a drink"), now against
`drive_at`'s new fold over `drank` events (time since the last drink) rather
than over the old gradual rise-and-fall. The prose contract is unchanged;
what changed underneath it is that satisfaction is no longer automatic on
arrival — it is a **planned, discrete action** the agent chooses to take.

## The shadow this campaign keeps

Genesis commits nothing new. The planner is a pure function — no serialized
state, no RNG — and it runs only inside a possess session, exactly as The
Wanting's drive did; a freshly built world commits no plan, no goal fact,
and no planned move. The `drank` predicate is registered per-session, never
at genesis, mirroring `agent-at`'s own precedent. The genesis-zero pin holds
unmoved for both predicates, and no epoch, no census regeneration, and no
new save-format contract were needed anywhere in this campaign.

One frozen artifact needed re-baselining for the same reason as every
campaign before it that touched the tick's body: the over-time possession
transcript recorded `wait`/`why`/`needs` output against the reactive model,
and once the planner replaced it, that recording's *output* changed with
it — narrated journeys arrive and complete faster (a planned round trip is
two `MoveTo` hops plus an instant `Drink`, not a multi-day physiological
fall), and `why`'s recount gained the `drank` line above. The day-0
transcript's own `wait 90` line moved too, for the identical reason The
Wanting's close named: the NPC layer's tick changed body, so every
transcript that happens to contain a `wait` is a candidate to drift,
regardless of which one a task's own plan anticipated. Both are the
expected game-layer delta, confirmed against the diff that actually
matters: `book/src/laboratory/`, `cli/tests/fixtures/`, and the seed-42 sky
almanac carry not one changed byte across this campaign's four tasks.

## What remains reserved

This slice keeps exactly one drive and exactly one goal — there is nothing
yet to arbitrate between, so the planner never has to choose among
competing wants. Every action costs the same, uniformly, so there is no
personality in how a plan is found, only in whether one exists. And the
planner reads ground truth through the preserved `view` seam, not belief —
so nothing here can yet be deceived. Each of these is a body-swap the
architecture is already shaped to receive, not a redesign: arbitration
(a second drive forcing a choice) changes `decide`'s goal-selection; the
psychology-vector cost (a bold agent finds a risky path cheap, a timid one
expensive) changes `successors`' cost function; planning over belief (the
decisive move — a plan that can be *wrong*) changes what fills
`view`. A richer, data-authored action vocabulary (authored actions as data,
not two hardcoded variants) and plan caching are named followups too, each
waiting on its own trigger rather than built speculatively now. What exists
for the first time, at the close of this campaign: an agent that does not
merely react to the world it is in, but searches the space of worlds it
could reach, and walks toward the one it wants.
