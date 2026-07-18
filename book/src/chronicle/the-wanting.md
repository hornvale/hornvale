# The Wanting

The Quickening gave the world its first autonomous motion: a derived NPC
walks a fixed daily route — home while resting, one neighboring room while
active — on a clock built from its species' activity-cycle. It was real
motion, dated and committed and recountable. It was also, on inspection,
motion with **no reason**. The NPC goes to that neighboring room because a
clock says to, not because it wants anything there; ask `why` and the
honest answer was "because it was that time of day." A puppet on a very
good route is still a puppet. This campaign gives the routine its first
**want** — a homeostatic need with memory, that the NPC acts to close the
gap on — and in doing so turns the puppet into the world's first *agent*.

## A destination becomes a resource

The Quickening's neighbor room was arbitrary — any room one hop from home
would have served the schedule identically. This campaign's first move is
to give that room a *meaning*: it becomes the **resource** the NPC's drive
seeks, chosen as the lowest-elevation of home's three mesh neighbors — "the
water," in the plainest sense a room-scale world can mean it. The rename
from `destination` to `resource` in `Npc` is not cosmetic; it is the
difference between a place an NPC walks to and a place an NPC *needs*.

## The drive is a fold, and a fold is a view

The drive itself — how parched an NPC currently is — is never stored. It is
computed the same way an NPC's *position* already was in The Quickening: a
pure function of the ledger's own committed history, re-derived on demand
and identical whether asked once or a thousand times, before or after a
save/reload round trip. `drive_at` walks an NPC's dated `agent-at` facts in
order, integrating a piecewise-linear accumulation — rising while away from
the resource, falling while at it — from an authored initial value. Nothing
about the drive's *current level* is ever committed; only the NPC's
*position*, which the drive-driven decision changes, is a real event.

This is not a coincidence dressed up as an architecture; it is the same
architecture, applied a second time. The project's derived-view synthesis
already named belief, the social graph, and the query indexes as folds over
the one event ledger — lossy reductions computed fresh each time,
never a second source of truth. A drive is now a fourth instance of exactly
that shape: **drive = fold = belief = index**, four names for one idea,
applied to four different questions. A reloaded session's NPC is exactly as
parched as it was the moment it was saved, for the same reason a reloaded
session believes exactly what it believed — nothing about either was ever
stored in the first place; both are re-derived from the one thing that was.

## The thermostat, not the toggle

A drive that simply flips — parched the instant it isn't at the resource,
sated the instant it is — thrashes: go to the resource, become sated
immediately, leave, become parched immediately, arrive, and so on forever,
one committed fact per tick. The fix is the oldest one in control theory: a
**thermostat**. Two thresholds, not one — an `act` threshold where the
rising drive triggers a trip to the resource, and a lower `sated` threshold
where the falling drive triggers a return home — with a dead-band between
them where the NPC simply holds. The drive *accumulates* rather than merely
toggling, and that accumulation is precisely why this rung needs **memory**:
reflex has none, and a memoryless drive is indistinguishable from a
twitching reflex that never stops twitching. The dead-band is what makes an
oscillation into a *cycle* — parched, seek, sated, hold, parched again — a
shape a player can actually read as behavior rather than noise.

## A tick that jumps to where it matters

Because the drive accumulates linearly within any interval where the NPC's
position doesn't change, the moment it next crosses a threshold is not
something that needs simulating step by step — it is solvable in closed
form: `t_cross = t_last + (threshold − drive_last) / rate`. `wait <N>`'s
tick advances an NPC through every genuine crossing inside the interval, in
order, committing one dated `agent-at` at each — a **discrete-event
simulator**, not a fixed-timestep integrator. A forty-day wait against a
drive cycle of about seven days commits on the order of ten crossings, not
forty daily samples of which almost all would be redundant; a
misconfigured drive (thresholds collapsed to nothing) is caught by a
strict-day-progress guard rather than allowed to spin forever recommitting
the same instant. The tick is exact, cheap, and — because accumulation is
monotone between crossings, never chaotic — trivially safe under the
project's Lorenz guard-rail: nothing here is a forward integrator that
quantized floats could destabilize on reload.

## What you can now feel, and ask

The drive itself stays invisible — an NPC's inner state was never meant to
be read off as a number — but its *action* and its *reason* are both
visible, and this campaign adds a third way to perceive it directly.
`needs` renders every NPC sharing the possessed agent's room as diegetic
prose, never a figure: a parched NPC "looks parched," a satisfied one
"seems content," and one in the dead-band "could do with a drink." `why`
already recounted an NPC's dated history; now that history's provenance
names the drive by name, not just the fact that motion happened:

```
> needs
The hobgoblin of Feefaenoagoo seems content.
> wait 5
Time passes; the world keeps its shape.
> needs
The hobgoblin of Feefaenoagoo could do with a drink.
> wait 1
Time passes. You watch hobgoblin of Feefaenoagoo go.
> needs
No one else is here to read.
> wait 1
Time passes. You notice hobgoblin of Feefaenoagoo here now.
> why hobgoblin
hobgoblin of Feefaenoagoo:
- an agent's position on a day: 815562763 (asserted by sought water (thirst), day 5.6666667)
- an agent's position on a day: 815726603 (asserted by sought water (thirst), day 6.8333334)
```

`needs` deliberately reads the co-located *NPCs*, not the possessed agent
itself — a plan-time correction worth stating plainly. The possessed
agent's own moves are never committed as `agent-at` (only NPCs' are), so
asking the drive model about the player would fold an empty history and
report eternal, meaningless thirst. The player's own felt drive waits on
player-acts-mutate (a sibling campaign): once the player's own actions
commit events, the same fold applies to them for free. This slice makes the
drive visible on the agents whose history is real, which is every NPC —
and pairs cleanly with `why`'s recount either way.

## The GOAP throughline: a seam, reserved

The motivation engine ranks want by autonomy — reflex, drive,
emotion, goal, value, project — and this campaign lands exactly the second
rung: a reactive controller that closes a homeostatic gap, with no planning
above it. That stopping point is not modesty; it is load-bearing.
**Cardinality matters here in a specific, checkable way**: a single drive
needs no arbitration, because there is nothing competing with it for the
NPC's attention. It is only once a second drive exists — thirst *and*
fatigue, say — that the NPC must *choose*, and choosing between competing
setpoints is exactly what the **goal rung** (a deterministic A* planner
over authored actions) exists to do. One drive, by construction, never
reaches that fork; building the planner now would have nothing to
arbitrate, and would be exactly the kind of speculative machinery this
project's `models author, dice roll` discipline exists to keep out.

So the campaign reserves the seam instead of building past it. Three small
pieces, interface shape only: a `Goal` naming the sole desired state (be at
the resource); a `decide(npc, world_view, t) -> Intent` function whose
signature is already the shape a real planner will fill — the tick depends
only on its output, an intended position, never on the drive's internals,
so the body is replaceable without touching the caller; and a `world_view`
parameter standing in for "what the agent perceives," ground truth today,
the belief layer tomorrow. None of this is a planner in miniature.
It is one function shaped correctly, so that the eventual campaign which
adds a second drive and the A* search to arbitrate between them changes
`decide`'s *body*, not the tick, the commit path, or anything reading its
result. What stays exactly as deferred as before this campaign: arbitration
between competing drives, the A* search itself, the species/culture
psychology vector that will cost its actions, and planning over belief
rather than ground truth. The line is drawn at the one place a single-drive
slice can draw it without guessing at machinery it cannot yet measure a
need for.

## The shadow this campaign keeps

The determinism contract carries over from The Quickening unchanged: the
drive lives only inside a possess session, never at genesis. A freshly
built world — the thing the census, the almanacs, and every laboratory and
gallery artifact measure — commits no drive fact and no drive-driven
`agent-at`; the genesis-zero pin from The Quickening still holds, unmoved.
The drive-state is derived, so a saved session reloads to the identical
value it held when saved, with no second source of truth to drift out of
sync. Accumulation is monotone and non-chaotic, so there is nothing here a
quantized reload could destabilize. No RNG anywhere — the crossings are
exact functions of seed and history, so the same seed and the same waits
still produce byte-identical session ledgers.

One frozen artifact needed touching for reasons that are the same shape as
last campaign's, not new drift: the over-time possession transcript
recorded a `why` and a `wait 90` against the old clock model, and once the
tick itself changed body (Task 3, mid-campaign), that recording's *output*
necessarily changed with it. This close re-baselines it, along with the
day-0 transcript's own `wait 90` line — flagged, expected, and confirmed
against the one diff that actually matters: `book/src/laboratory/`,
`cli/tests/fixtures/`, and the seed-42 sky almanac carry not one changed
byte across every task in this campaign.

## What remains

The drive is one, the resource is one, and the NPC is still alone with it —
nothing here makes NPCs interact, nothing here reads belief instead of
ground truth, and nothing here builds the planner the seam was drawn to
receive. Fatigue, safety, and social drives are each a real material
question this campaign deliberately left unasked (fatigue overlaps the
existing activity-cycle too closely to be a clean second drive; safety
needs threats that don't yet exist; social needs the wider relational graph
and other agents to be social *with*). The player's own felt drive rides
player-acts-mutate. All of it is captured, not lost, in the followup
register this close promotes into the retrospective. What exists now, for
the first time: an NPC that moves because something inside it changed, a
question that gets you its actual reason, and a seam waiting exactly where
the next campaign needs it to be.
