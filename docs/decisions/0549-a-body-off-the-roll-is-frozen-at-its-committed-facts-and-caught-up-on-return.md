# 0549. A body off the roll is frozen at its committed facts and caught up on return

**Status:** Accepted (2026-09-01, ratified at G3) · **Decider:** Nathan · **Campaign:** The Roll

In the context of a roll that ticks the bodies within call and no others,
facing three ways to treat the ones it leaves behind, we decided that a body
off the roll is **frozen at its last committed facts** and replayed forward by
`catch_up` when it re-enters, accepting that a villager the player leaves for
thirty days is, on return, where they were thirty days ago plus at most one
thousand steps of catch-up.

## Context

Decision 0546 makes the ticked set a pure function of the observer's room.
That leaves a question it deliberately does not answer: what is true of a body
that is not in it. Three answers were on the table (ledger #4):

1. **frozen + catch-up** — its state is whatever it has committed, exactly as
   `agent_position` already reads it, and `catch_up` replays it forward under
   the existing `CATCH_UP_STEP_CAP` when it returns; ← chosen
2. **a slow tier** — one decision per world-day while off the roll;
3. **field advancement** — advance dormant bodies statistically.

**This is the campaign's leading fidelity choice, and it was put to Nathan
that way at the spec stop (G3, 2026-09-01) rather than buried.**

## The rule

Frozen, with the mechanism a sleeping body already uses. Nothing new is built:
`agent_position` reads a body's latest `agent-at` fact at or before the day
asked for, else its home; `catch_up` (`liveness.rs`) replays from the last
committed day under `CATCH_UP_STEP_CAP`.

**The slow tier is named and not built.** It is a second visible mechanism
with its own purity obligations under 0546 and its own cost (7,400 bodies ×
one decision per world-day is cheap per tick but is a new term), and it is not
needed to answer the brief. The registry row `SOC-off-roll-slow-tier` carries
it. Field advancement contradicts nothing but has no field to advance a
*position* by.

## Consequences

- **The spectrum between "frozen" and "ticked" has an unnamed middle, and this
  campaign names it without occupying it.** A successor that wants the middle
  should start at the row, not at this record.
- **A dormant body's divergence is exactly its committed facts**, which is The
  Walk §4.1's refinement rule ("stored only if it is an irreversible
  divergence") applied to a person.
- **A settlement out of call commits nothing at all**, which is asserted:
  `a_body_off_the_roll_is_frozen`. That test is worth reading for its own
  history — the brief's prescribed mutation was a null at seed 42 (the
  flagship stands on a river, so its residents drink in place and commit no
  position whether ticked or not) and again at seed 2, and it holds only at a
  seed whose residents actually walk. "Frozen" and "ticked" can be observably
  identical, which is a real limit on what this decision can be tested to.
- **Nothing about this is visible to a player who stays.** It is a cost paid
  entirely by the returning traveller, and it is the price of 0546's purity.

## See also

- `docs/superpowers/specs/2026-09-01-the-roll-design.md` §3.7, §9.
- `book/src/frontier/idea-registry.md` — `SOC-off-roll-slow-tier`.
- Decision 0546 (the roll is a pure function of the observer's room).
- `book/src/chronicle/the-roll.md`.
