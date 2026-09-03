# 0658. The wait line minutes the held body's acts

**Status:** Accepted (2026-09-03) · **Decider:** Nathan · **Relates:**
[0168](0168-the-effect-of-an-act-belongs-to-the-body-not-the-driver.md);
[0226](0226-a-possessed-host-is-co-present-not-displaced.md);
[0656](0656-a-held-bodys-walk-commits-what-it-does.md);
[The Minute spec](../superpowers/specs/2026-09-03-the-minute-design.md) §3.4;
[ledger](../superpowers/ledgers/2026-09-03-the-minute.md) #3

In the context of a `wait` that now commits what the held body did, facing a
report line that said `Time passes; the world keeps its shape.` while the
body it describes had crossed fifteen rooms, we decided that **the wait line
minutes the driven body's own committed facts — a room change named first
and suppressing the arrival/departure comparison, then one clause per need
served — attributed to the possessor's will rather than to "you"**, accepting
that a room change ends the line rather than describing the new room.

## Context — the comparison the move breaks

`narrate_motion` compares a `before` snapshot of who was present against
`here`, read after the tick. `here` is the driven slot's position. When the
driven body has moved, that comparison is between two different rooms:
everyone in the room the body left reads as departed, everyone in the room it
arrived at reads as arrived. The line would be confidently wrong about the
whole population.

So a room change is named first **and stops there**. `look` is one keystroke
away and already answers for the new room; a second, half-correct description
of it is worse than none.

## The register: the will, not "you"

The sentence names the possessor's will, never the player's own hand.
Decision 0168 puts an act's effect with the **body**, not the driver, and
0226 makes the host **co-present**, not in control of the arbitration — so
the walk is the body's own resolution, not a choice the player made. What the
player did was hold the body while it happened, and that is what the line
says. The exact wording is the implementer's within that constraint. Three
lines were **measured**:

```
Time passes. The will that holds you walks this body elsewhere.
Time passes. You sense movement nearby (201 stirred). The will that holds you drinks and rests.
Time passes. You sense movement nearby (201 stirred).
```

— seed 7's first seeking wait, seed 42's **second** wait, and a **free**
body's line at the same tick, byte-identical to what it read before this
campaign.

What the tests **hold** is narrower, and it is four assertions over four
lines, not three. On seed 7's first seeking wait: a substring
(`walks this body elsewhere`) and a negative — the line carries no stirred
count and no arrival or departure text. On the quoted seed-42 second wait: a
substring (`drinks`). On seed 42's **first** wait, a line the block above does
not quote: a suffix (`The will that holds you rests.`), because the population
itself stirs on that tick, so only the minute suffix is stable. And on the
free body's: an absence (`The will that holds you` does not occur). The
quoted lines are the measurement; those four assertions are the guard.

## Consequences

- The population's `moved` count is untouched. `Time passes; the world keeps
  its shape.` still means the population did not move, and a tick where only
  the player's own body moved says exactly that instead of reporting one
  creature stirred.
- The wait line is the only out-of-character surface a held player has:
  `ask` is in-character and refuses while held. Naming the body's acts there
  is what makes the held body's inner life readable at all.
- The minute clause is built from the walk's **committed facts**, not from
  the felt-state trio. One clause per predicate present, in the order the
  predicates first appear in the commit, with `rested` and `slept` folded
  into one.
- A free body commits no driven facts, so its line is unconditionally
  unchanged — which is why no committed transcript or session fixture moved.
