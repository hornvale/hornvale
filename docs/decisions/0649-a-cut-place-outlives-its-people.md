# 0649. A cut place outlives its people: `Made` reads the occupation, and tenancy decides the tense

**Status:** Accepted (2026-09-03) · **Decider:** Nathan (G3) / autopilot ·
**Relates:**
[0647](0647-a-made-chamber-is-written-from-the-ledger-at-the-walk.md) (the
writer this keys the tense off),
[0646](0646-the-inhabited-reading-is-a-function-of-the-plan.md) (the roles the
tense inflects),
[0648](0648-the-hoarder-sits-at-the-sanctum-on-what-lies-there.md) (the same
boolean decides *kept* against *moves in the dark*) ·
[The Plat](../../book/src/chronicle/the-plat.md)

In the context of deriving a column's chamber origins from the ledger's
occupation records, facing the question of what to do with an occupation that
has **ended**, we decided that **`Made` reads the occupation whether or not it
is alive — a people that cut a place cut it, and the rock stays cut — and the
occupation's liveness decides only the TENSE: `Tenancy::Inhabited` where any
occupation seating that rung `is_alive()`, `Tenancy::Abandoned` where one
seated it and none does now, `Tenancy::Wild` where none ever did** — accepting
that the past-tense half of the vocabulary is exercised in this campaign
through the test seam rather than by a walk, because seed 42 has no reachable
ruin.

## Context

The first draft keyed `Made` to a *living* occupation, which would have made
an abandoned column read as untouched rock. That is wrong about the world:
a hall someone cut two centuries ago is still a cut hall. It is also wasteful,
because the ledger already carries the distinction and the alternative reading
costs one boolean.

The occupation record has an `ended` field. Reading it as *tense* rather than
as *existence* produced the ruin for free — the same rooms, the same roles,
the same graph, read in the past tense — and produced it without a new
attribute, a new fact, a new predicate or a new kind.

The general form is worth keeping: **the derived quantity should be the one
the substrate makes cheapest to vary.** Keying `Made` to liveness would have
merged two questions (was it cut, is anyone here) into one boolean and thrown
the second away.

## Decision

`Tenancy` is three-valued and derived per rung alongside the origin:

| ledger at this rung | origin | tenancy |
| --- | --- | --- |
| no occupation ever seated here | `Found` | `Wild` |
| an occupation seats here and `is_alive()` | `Made` | `Inhabited` |
| an occupation seated here, none is alive | `Made` | `Abandoned` |

`is_alive()` is the tense, and it is the *only* thing tenancy decides. Origin
— and therefore workmanship, and therefore whether a door row is admissible on
that level — reads the occupation's existence and never its liveness, so an
abandoned hall keeps its doors.

The place sentence inflects on it, eight sentences over four roles and two
tenses, with one landing clause shared by both:

> This is the entry of a cut place; the rock is squared where it was worked.
>
> This was the entry of a cut place, long empty; the squared rock has dulled.

The hoarder's datum inflects on the same boolean: *kept* in an inhabited rung,
*moves in the dark* otherwise (0648).

## Consequence

- **A ruin costs one boolean and no new machinery.** No attribute, fact,
  predicate or kind was added for it.
- **An abandoned hall still hangs its doors.** Workmanship is keyed to origin,
  not tenancy, so a ruin is a worked level with a dead occupation and the
  locks stay where the grammar put them.
- **The tenancy split is a report-only readout.** Seed 42's 26 cut columns are
  24 inhabited and 2 abandoned; seeds 7 and 1234 are all inhabited. The number
  is printed rather than predicted.
- **Seed 42 has no ruin a possession can reach, and this is a finding rather
  than a defect.** Only 5 of the 26 cut columns have a mouth that is both open
  and unbarred — the population a walk can enter — and all 5 are inhabited.
  The past-tense sentences are pinned by a session test that enters through
  the documented seam with an `Abandoned` origin. What is unproven on this
  world is that a player can ever *arrive* at a ruin, and the cause is a
  property of the barrier draw crossed with the ledger's occupation spans,
  upstream of everything this record decides.
- **The ruin is the natural home for a later reading of a drawn character.**
  A `DrowTier` character drawn at a vertex where no people ever settled is a
  tier nobody founded; reading it as a ruin rather than as a live tier is the
  shape that campaign should take.

## See also

- [The Plat design](../superpowers/specs/2026-09-03-the-plat-design.md) §3.2,
  §3.4, §7.2.
- [The Plat ledger](../superpowers/ledgers/2026-09-03-the-plat.md) #2
  (enrichment ii), Task 5 and its controller review.
- [The Plat chronicle](../../book/src/chronicle/the-plat.md).
