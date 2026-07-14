# The Seam

**July 2026 · outcome: merged — the first game-layer campaign. Five
interfaces are born in one window crate, wired end to end, and after this
lands Hornvale is a thing someone walks in: `possess` drops you into a
frozen world that re-describes itself as you move**

## What was attempted

Every campaign before this one built the world or an instrument that reads
it from outside — a map, an almanac, a census. None of them stood *inside*.
The Walk metaplan named the deliverable that changes that: Chunk 0, "The
Game Seam" — five interfaces, each with a trivial tier-0, wired together so
that a single command puts an observer on the ground and every later chunk
refines one stub without disturbing the others. The five are **Agent** (who
stands there), **Projection** (what they know), **Vantage-query** (what is
observable from here, now), **Focalizer** (how it reads as prose), and
**Local-space** (the room graph they walk). The design's whole bet is that
these five, and not a dozen subsystems, are the entire load-bearing skeleton
of a game layer.

The metaplan was written before the Room Mesh and the Locale Window shipped,
and its Local-space tier-0 — "a single room with no exits" — had fallen
*below* the floor: `windows/locale` already derives a walkable room graph
with typed exits from any world. Stubbing what has shipped would be a
regression wearing discipline's clothes. So this campaign landed the
**walkable** Seam: Chunk 0's five interfaces with Local-space bound to the
real locale window and a minimal read-only verb loop riding along, so the
exit criterion is not printing one line but *walking a frozen world that
re-describes as you move*.

## Five interfaces, one crate, five tier-zeros

Everything lives in one new window crate, `windows/vessel` — nothing touches
the kernel or a domain. That placement is a deliberate wager about evidence:
the five signatures are *named* here but **not frozen** here. When The Vessel
(Milestone I) validates the projection's shape against a real second
consumer, the subset contract can graduate toward the kernel *with* that
evidence and not before. Window-first, graduate later.

Each interface ships its most honest trivial tier. The Agent is minted
lazily, game-side, and **never committed to the ledger** — possessing a world
must not change it, the metaplan's reversibility rule taken literally. The
tier-0 mint drops the observer onto the flagship settlement's best cell, with
that settlement's majority species supplying a **perception vector** — one
value ships, but the slot is signature discipline: the goblin in a caste
ladder and the solitary dragon are already the same type. The Focalizer
renders one honest templated line from real fields and facts; the Vantage
bundles the locale room, its exits, the overlapping facts, and the field
values at (position, day). Templated repetition across rooms is accepted on
purpose — prose *variety* is another campaign's job, and it feeds this exact
surface later.

## The subset contract is the interface

Projection is where the seam's epistemics live, and the campaign's sharpest
decision is what "knowledge" means. The tier-0 is the identity projection —
the coarse facts and field values at the agent's location — but knowledge is
not a snapshot of the current room. It **accumulates over the session**: the
loop folds each visited room's projection into a running `Knowledge`, so
`knows` is the observation *history* of the walked path, exactly the
metaplan's definition ("what an agent *has observed*") taken at its word,
session-local and deterministic given the path.

What makes this an interface rather than a data structure is the one property
it guarantees: the accumulated `Knowledge` is provably a **subset** of ground
truth at every step. Nothing is known that the world does not contain. False
belief, inference, fog, forgetting — every way knowledge can *diverge* from
truth — is deferred to The Vessel; the tier-0's entire discipline is that it
never diverges at all. The walker battery asserts this path-dependently, so
the subset relation is not a claim in prose but a checked invariant of every
walk the tests take.

## The examine contract, and the enemy it is named against

The classic text-adventure realism break is `LOOK SUN → I don't see any such
thing here`: the description mentions a thing the parser then denies exists.
The campaign forbids this structurally with the **examine contract** — the
examinable nouns are *exactly* the vantage's named constituents, and the
focalized line is their catalog. **If `look` mentions it, you can examine
it.** No hidden nouns, no unexaminable scenery. This is not a courtesy check
run over hand-authored rooms; it is enforced mechanically, as one of the four
walker-battery invariants, so a room that names a thing it cannot resolve
fails the gate rather than a playthrough.

The ground noun the focalizer examines is worth naming precisely, because it
records a mid-campaign reconciliation. The Uncommon Ground merged into main
while this campaign was in flight, and with it the room's texture aspect
became the regime **descriptor** — `locale/room/v2`, the strangeness
overlay's rendered departure from the biome default. So `examine`-ing the
ground reports the regime ("a fern-choked draw shaded dry in a hollow
(strangeness 0)"), not the retired v1 texture. The variety campaign is not a
future promise threading into this surface; it is already feeding the walk.

## Walking one scale, and `wait` that never thaws

The walk's extent was ratified as "one locale," and reconciled against the
shipped mesh at planning time. The room mesh has no locale boundary —
lateral walking is globally lazy and uniform in cost — so the bound the
choice meant to protect (no fresh derivation surface) is realized by
**scale**, not area. The walk stays at the canonical walk depth
(`globe_level + 6`); lateral compass exits traverse freely; and the
**vertical** `Enter`/`Exit` exits — the actual seams between scales — render
diegetically but *refuse* to traverse. Scale travel is a later chunk, and
the world says so in the second person rather than hiding the door.

Time enters at flag cost and never becomes a clock. `--day D` and the `wait
[N]` verb both move the possession's `WorldTime`, but that time is a
**freeze point**, not a simulation step. `wait` re-parameterizes the freeze:
the sky wheels and the season turns while the ground holds perfectly still —
nothing steps, nothing mutates. In the committed transcript the observer
waits ninety days in place and the moons advance a full phase while the draw
and its exits are unchanged:

```text
> wait 90
[room 634912876, day 90]
You stand in temperate rainforest — a fern-choked draw dry on a rise — in the lands of Zhxokngaknged. The sky above: Night. The vast moon shows its first-quarter face. The small, distant moon shows its last-quarter face. …
Ways on: NW, S, NE.
```

This threads world-time through the whole seam and pre-stages the later
milestone's liveness plumbing without simulating anything — the interactive
form of the same freeze the almanac has always rendered.

## The proof: a battery, a transcript, and buffer-driven loops

Determinism is checked in three tiers. The **gallery transcript** is a
committed scripted walk over seed 42, regenerated and drift-checked by CI
alongside the almanacs — the metaplan's Chunk 0 exit criterion, verbatim,
and its breadcrumbs record each step's room id so the walked path is diffable
at the room level and not only the prose. The **walker battery** takes
deterministic pseudo-random walks seeded from a kernel `Stream` and asserts
four invariants along every one: projection subset (knowledge stays derivable
from ground truth), exit reciprocity (`go` then its reverse round-trips, the
mesh's seam-gluing asserted from above), focalizer totality (every room
renders non-empty prose without panicking), and examine totality (every noun
the focalizer emits resolves). The third tier is the familiar
buffer-driven loop test — feed a command sequence, assert the output —
covering the diegetic Exit refusal, `back`, `--day` variation, and the repl
handoff. Both front doors enter the same I/O-generic core: a new `hornvale
possess` subcommand and a repl `possess`/`release` round-trip, so `possess
--seed 42` works verbatim off the composition root.

## What is fenced out, and who owns each fence

The Seam is a floor, and its scope discipline is mostly a map of what it
refuses so a later campaign inherits the seam cleanly. **Liveness, events,
time-stepping** — the world stays frozen; `wait` parameterizes, it does not
thaw (Milestone IV/V). **Scale travel** — vertical exits refuse diegetically
until a later chunk earns them. **Prose variety** — The Uncommon Ground's
job, running independently and absorbing into the Focalizer surface (and, as
above, already doing so). **Projection beyond identity** — fog, inference,
false belief are The Vessel's, gated on a real second consumer. **The verb
chemistry** the metaplan's later chunks will build is not
stubbed but correctly *absent*: every verb here is read-only against the
frozen world, so there is no reaction engine to stub. And the **anti-phenomenon**
read — meaningful absence surfaced against the field prior — waits, as
designed, for Focalized Sight. Naming each fence
against its owning campaign is what lets the next builder pick one up without
re-deriving where it belongs.

## The road ahead

Chunk 0 of The Walk is done — the metaplan's foundational, CI-cheap
deliverable, the one every later chunk refines. The five interfaces exist as
running code with the cheapest honest tier behind each, the subset contract
is a checked invariant rather than an aspiration, and Hornvale has crossed
from a world one *reads about* to a world one can *stand in*. The Vessel will
graduate the projection with evidence; liveness will thaw the freeze;
variety and verbs and anti-phenomena each have a fence with their name on it.
What this campaign settled is the skeleton they all hang from — five
signatures, one crate, and a first observer who walks a frozen seed and finds
it already describes itself.
