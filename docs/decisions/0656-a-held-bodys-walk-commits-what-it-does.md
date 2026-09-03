# 0656. A held body's walk commits what it does

**Status:** Accepted (2026-09-03) · **Decider:** Nathan · **Amends:**
[0226](0226-a-possessed-host-is-co-present-not-displaced.md) (two consequence
bullets — the discard, and the untestability it implied; see that record's
2026-09-03 amendment, which leaves its ruling standing) · **Relates:**
[0168](0168-the-effect-of-an-act-belongs-to-the-body-not-the-driver.md);
[0226](0226-a-possessed-host-is-co-present-not-displaced.md);
[0228](0228-a-controller-is-a-parameter-of-the-tick.md);
[The Hand spec](../superpowers/specs/2026-08-23-the-hand-design.md) §3.3;
[The Minute spec](../superpowers/specs/2026-09-03-the-minute-design.md) §1, §3.1;
[ledger](../superpowers/ledgers/2026-09-03-the-minute.md) #1

In the context of a session tick, where the body a player holds runs its own
arbitration and acts, facing a `wait` that bound that walk's facts to a
discarded name and so let a body's felt state diverge from its own ledger for
as long as the possession ran, we decided that **the walk's facts are
committed to the session ledger, unconditionally on which controller drove
the walk**, accepting that a Holding walk's empty commit is now a load-bearing
guarantee rather than an accident of the code's shape.

## Context — what the discard cost, measured

`Session::wait` asked the driven body through `step_one_with_controller`,
kept the felt state it returned, and bound its facts to `_driven_facts`. The
comment beside that binding said what the underscore did: *discarded
unconditionally, regardless of what `step_one_with_controller` returns*.

Measured before the fix, `!possess` then eight `!wait 5`:

- **Seed 42, water in the home room.** The walk drank in place on every tick
  from the second onward — 29 facts emitted across the script, **0
  committed**. The felt state, read off the walk, said `Content` for forty
  days while every fold over `drank` — thirst, `learned_helplessness`, the
  resident summaries `!needs` renders for everyone else — read a body that
  had never drunk.
- **Seed 7, water not in the room.** The walk sought water for fourteen rooms,
  then fifteen, ended somewhere the ledger never heard of, and the next tick
  restarted from the origin room. Nothing accumulated. The position column
  never moved, correctly: The Rack had already made it follow the ledger
  rather than the discarded walk.

The discard was never a design. The Hand's §3.3 drew
`commit(advance_one(body, intent))` for every body; what shipped asked the
solo walk through a fresh `PlayerController` whose intent is unconditionally
`Hold`, so its facts were always empty and dropping them cost nothing. The
Coercion then swapped in a controller that **acts**, measured the felt-state
consequence, and recorded the ledger half as a property of possession.

## Unconditional, not "when possessed"

The natural patch branches on `self.possessor().is_some()`. It produces the
same bytes today and is the wrong shape. The controller **is** the branch
(0228): a `PlayerController` with nothing queued Holds, and a Holding walk
emits nothing, so committing its facts is a no-op today and becomes the one
commit path the moment a queued verb is routed through
`PlayerController::queue` — which The Hand's pseudocode always intended. A
possession-gated commit would leave that path to be re-plumbed later, and
would make the ledger's honesty depend on who is driving, which is what 0168
forbids.

The commit **order** is a determinism contract from the day it lands:
population facts first, in their existing order, then the driven body's, then
the hostility loop. Every driven fact has a different subject from the
population's, so no fold crosses the boundary; the order is fixed so a saved
played world's ledger is reproducible from its script.

## Consequences

- **A Holding walk emitting nothing is now load-bearing**, where before it was
  a coincidence nothing depended on. It is pinned by
  `a_free_walk_emits_nothing_and_ends_in_the_column`, measured across seeds
  42 and 7 over four `!wait 5` each: zero facts on every tick, and the walk
  ending in the column's own room every time.
- **The driven slot's column moves through the ledger.** `Roster::write`
  (position and felt together) replaces the felt-only write; The Rack's
  VIEW ≡ SCAN invariant holds for the driven slot across a nine-wait script.
- **`Roster::resolve` is deleted with its only caller.** A second writer that
  is only ever correct when it agrees with the first is a second way to be
  wrong.
- **A saved played world now carries walk-authored `drank`/`eaten`/`rested`/
  `slept`/`agent-at` facts for the player's own body** — no new predicate, no
  new provenance shape, the same constructors a creature's facts use.
- **`Session.wake_at` is not reached by the commit**, and this is the one
  place the "everything downstream is a fold" reading was wrong. The gate's
  `Asleep` row reads a session field the `sleep` verb sets, so a
  walk-committed sleep that outlasts the tick sets the field by the same
  keep-the-later-wake rule, or a released body would be awake at the gate
  while its ledger said otherwise.
