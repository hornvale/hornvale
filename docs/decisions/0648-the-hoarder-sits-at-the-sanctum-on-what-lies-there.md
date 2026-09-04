# 0648. The hoarder sits at the sanctum on what lies there, and holds nothing

**Status:** Accepted (2026-09-03) · **Decider:** Nathan (G3) / autopilot ·
**Relates:** [0646](0646-the-inhabited-reading-is-a-function-of-the-plan.md)
(the reading that sites it),
[0400](0400-custody-is-an-observable-not-a-vital.md) (custody is an
observable),
[0398](0398-a-capability-nothing-can-reach-is-not-a-capability.md) (a
capability nothing can reach is not a capability),
[0070](0070-wounds-commit-health-folds.md) (how an encounter would resolve —
it says nothing about tenancy, and nothing here resolves one),
[0547](0547-a-resident-is-a-living-person-derived-on-demand-and-never-generated.md)
(a resident is derived on demand) ·
[The Plat](../../book/src/chronicle/the-plat.md)

In the context of a rung's dominant inhabitant already being derived from its
own substrate and energy budget and then drawn at "the last standable cell in
ascending order" — a rule about the grid rather than about the place — facing
the question of how much more of a creature to build, we decided that **the
hoarder is sited by the reading and by nothing else: it stands on the first
standable cell of the `Sanctum` node's region, WHO and WHETHER stay the energy
field's, WHERE becomes the plan's, its hoard is a fold over the `located-in`
facts naming that region, and it holds nothing** — accepting that it remains a
species mark with no identity, no turn and no custody until a transfer verb
exists.

## Context

The frontier's own condition for a creature that belongs to a world rather
than to a designer is that it "falls out of the world's own energy budget;
nobody sited it". Two of the three questions already met that condition —
which species, and whether there is one at all, are answered by
`dominant_inhabitant` from the rung's substrate and energy, drawn by nobody.
The third did not: the last standable cell in ascending `(x, y)` order is an
artifact of iteration order, chosen so the creature would not be standing at
the stairs when the possession arrived.

The materiality ladder was written out before choosing a rung of it: a name in
prose; a mark at a geometric cell (the state before this campaign); a mark at
a **structural** node, on the things that lie there; a Thing with identity; a
Body on the turn roster; an agent with drives. This record takes exactly one
step, and it is the step that costs no epoch and no new type.

Three richer shapes were refused, each for a stated reason:

- **A Body on the roll.** No home kind exists for `(vertex, rung)`; `roll_of`
  is keyed by settlement room or herd vertex; and the one thing a body would
  have bought — holding a key — is refused by the Roll spec's §7.
- **A Thing with identity.** Registering a kind is a concept accession epoch,
  and nothing here needs the creature to be examinable as an individual.
- **Realizing the `Dangerous` gate stamp.** Dormans' dangerous route is a
  creature *in a passage* — a patroller, a moving body. The hoarder is a
  sitter at the end of the plan. They are two creatures, and the second one
  needs an underground roster. It is also the one candidate that fails
  "derived, not sited": the pattern stamp would place it.

## Decision

`resident_cell(&Underground)` returns the **first standable cell, in ascending
`(x, y)` order, of the region belonging to the level's `Sanctum` node** — the
mirror of the rule that stands the possession on the first standable cell of
the `Entry`'s region. A rung with no standable cell there has no hoarder,
which is today's rule unchanged. The mark the pane draws reads its cell from
this same function, so the mark moves with the creature rather than being
placed a second time.

`inhabitant_datum` gains the **hoard**: the things whose `located-in` fact
names the sanctum's region (`region_key`, the key `drop` already posts
against), listed the way `look` lists a floor —

> A xorn moves in the dark here, drawn to iron-bearing stone, sitting on: a
> key.

— and unchanged when nothing lies there. It is a fold over facts that already
exist: no new kind, no new predicate, no identity, no accession.

In a rung whose origin is `Made`, the same creature is narrated as **kept**
rather than as moving in the dark — one word keyed to the rung's `Tenancy`
(0649) — because a people that cut the place did not leave its innermost
chamber to whatever wandered in.

**It holds nothing.** The Roll spec §7 refuses moving a key onto a body before
a transfer verb exists, because a held key with no way to unhold it makes the
only lock in the game unopenable. That is the ceiling on this creature's
materiality, and it is a consequence of an absent verb rather than a design
preference.

## Consequence

- **WHO and WHETHER did not move.** `dominant_inhabitant`'s roster filter, fit
  threshold and tie-break are untouched. Only WHERE changed, and it changed
  from an accident to a derivation.
- **A hoarder does not sit on a key by construction.** The grammar puts a key
  at the deepest node on 13–23% of levels that carry one, so the hoard is
  usually what the possession itself left there — which is exactly the point:
  a hoard the world guarantees is a fixture, and a hoard the world sometimes
  provides is a place.
- **The reading is computed for every plan, not only cut ones.** A beast's den
  is the innermost node of a wild cave too. Only the *vocabulary* of 0646 is
  keyed to `Made`; the siting is universal.
- **The next step up the ladder is named and costed.** Residents standing in a
  cut rung, and a patroller in a passage, are both bodies, and both want a
  home kind keyed to a column and a rung plus a place on the turn roster.
- Decision 0070 governs how an encounter *would* resolve and says nothing
  about tenancy. Nothing here resolves one.

## See also

- [The Plat design](../superpowers/specs/2026-09-03-the-plat-design.md) §3.5.
- [The Roll design](../superpowers/specs/2026-09-01-the-roll-design.md) §7.
- [The Plat ledger](../superpowers/ledgers/2026-09-03-the-plat.md) #2 (the
  materiality scale).
- [The Plat chronicle](../../book/src/chronicle/the-plat.md).
