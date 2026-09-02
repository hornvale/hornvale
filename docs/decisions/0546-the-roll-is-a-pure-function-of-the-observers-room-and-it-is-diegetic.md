# 0546. The roll is a pure function of the observer's room, and it is diegetic

**Status:** Accepted (2026-09-01, ratified at G3) · **Decider:** Nathan · **Campaign:** The Roll

In the context of a walk that can afford to tick roughly a hundred bodies
against a world holding about 7,400 inhabitants, facing the need to choose
*which* of them are simulated at all, we decided that the chosen set — the
**roll** — is a pure function of world state and the observer's room,
accepting that a body off the roll gets no simulation at all rather than a
cheaper one.

## Context

The Penstock metaplan's spine (§2b) states the rule this record mints: which
entities are *simulated at all* is **observable** — the player can see who is
here, talk to them, watch them walk — so the boundary is diegetic, and a
diegetic boundary must be a function of the world rather than of the program's
history. A working set decided by memory, by query history, by which rooms
happened to be visited first, or by wall-clock time is a boundary the player
can perturb by walking in circles, and it makes two sessions of the same seed
disagree about who exists.

`windows/vessel/src/roll.rs` is that function. `roll_of` takes the bodies, the
observer's room and two budgets, and returns a mask; nothing it reads is
mutable state accumulated by play.

## The rule

- **Membership is by HOME**, never by where a body has wandered (ledger #13).
  A resident's settlement room and a herd's attractor vertex are world state;
  a body's current position is its own history. A resident who has walked out
  of the window while its settlement is still in call is still ticked.
- **`ROLL_HOPS` and `ROLL_BUDGET` are deterministic budgets, not world facts.**
  They are named constants in `windows/vessel` whose doc comments say so and
  point at the spec. Initial values `R = 2` (a 5×5 window of ~1.1 km rooms)
  and `B = 128`.
- **The ordering key is world state too.** A body sorts by (hop distance,
  settled-before-wild, parent, species, ordinal) — a resident by its
  settlement and ordinal, a wild body by its attractor vertex, species and
  member index. Never by a body's position in a list: Task 7's fix round
  found that keying wild members by roster index made a truncating budget
  **route-dependent**, so the same herd would be on or off the roll depending
  on the direction the player approached from.
- **Time is not an argument.** `roll_of` carried a `t: WorldTime` until Task 7
  established that membership is time-invariant by construction — a body's
  home does not move — so the signature is strictly purer without it
  (ledger #16, and spec §3.2 is amended to match).

## Consequences

- **A second session at one seed running one script produces the same roll**,
  which is asserted rather than asserted-about:
  `the_roll_agrees_across_two_independent_sessions`.
- **The budgets can be tuned without a determinism claim being broken**, and
  they are the knob spec §8's M2 measures. `ROLL_BUDGET` never bound at seed
  42 (68 bodies on the roll against a budget of 128), so the truncation is
  exercised by tests with a small budget rather than by any live session.
- **What is given up is the cheap middle.** A body off the roll is not ticked
  slowly; it is not ticked at all (decision 0549). That is the fidelity cost
  this purity buys, and it is recorded there rather than hidden here.
- **The invisible half of the Penstock is not built by this** — no view cache,
  no eviction, no fan-out. Those are memory, and memory is exactly what this
  record forbids the roll from consulting.

## See also

- `docs/superpowers/specs/2026-09-01-the-roll-design.md` §2.3, §3.2.
- `docs/superpowers/specs/2026-08-22-the-penstock-metaplan.md` §2b.
- Decisions 0549 (dormancy), 0547 (what a resident is), 0227 (possession
  selects a body).
- `book/src/chronicle/the-roll.md`.
