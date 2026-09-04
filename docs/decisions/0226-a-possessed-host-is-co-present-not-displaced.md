# 0226. A possessed host is co-present, not displaced

**Status:** Accepted (2026-08-23) · **Decider:** Nathan · **Relates:**
[0167](0167-a-driver-is-interchangeable-and-a-possessed-body-is-a-creature.md)
(the requirement this arc closes) ·
[0228](0228-a-controller-is-a-parameter-of-the-tick.md) (the mechanism that
makes it true) · **Settles:** the `PLAY-what-happens-to-the-host` registry row

In the context of a possessed body now walking the same tick as every other
body, we decided that **the host is co-present — arbitration runs for a body
while it is ridden, so it keeps its own drives, its own mode and its own
affect throughout** — accepting that this settles the design's largest open
possession question as a deliberate act rather than as a side effect of a
refactor.

## Context

`PLAY-what-happens-to-the-host` names three candidates. **Displaced**: the
host is dormant and returns afterwards, which makes possession a borrowing.
**Co-present**: they are in there, aware, which makes every possession a
relationship. **Consumed**: each one is a killing.

The tempting implementation shape settles the question the other way by
accident. A possessed body joins the roster so other creatures can perceive
it, and the tick then *skips* it, because the player is already deciding. That
is displacement, mechanically: a body whose arbitration does not run has no
inner life to be aware with.

`PLAY-host-is-a-narrator` is what makes that unacceptable — *"the host stays
aware and tells you things — affect, local belief and dread, all three already
computed and all three currently without an honest route to the player."*
Arbitration is where those three are computed. Skip it and the row is
unbuildable, and every row downstream of it (`PLAY-host-may-refuse`,
`PLAY-host-names-you`, `PLAY-affect-becomes-testimony`,
`PLAY-vacated-host-testifies`) is unbuildable with it.

So the choice was not between two equally reachable designs. One of them
foreclosed a published branch of the design and the other did not, and the
cheap-looking one was the foreclosing one.

## Consequences

- **`Session::wait` runs the driven body through the same `advance_one` every
  other body's walk calls**, in a solo band-of-one walk, and records the mode
  its own arbitration reached (`Session::driven_mode`). A test asserts that
  mode varies with which seed's population the body was drawn from, not merely
  with elapsed time — arbitration observably ran, rather than a constant being
  stored.
- **The host's own commitment mode is computed and retained every tick — the
  substrate the gap needs — but the gap itself is not yet readable.**
  `Session::driven_mode` holds the `Mode` the driven body's own arbitration
  reached (`Pursuing(Fatigue)`, `Idle`, and the rest): **drive** granularity,
  not the action. The `Intent` arbitration chose never leaves `advance_one`,
  which returns `bool`; `Intent` does not appear in `session.rs` at all. So
  reading "it wanted to run and you made it stay" additionally needs the intent
  surfaced out of `advance_one` — a signature change, and therefore a small
  piece of new machinery this arc did not ship. An earlier draft of this bullet
  claimed the gap was derivable "with no new machinery" and that every tick
  computes both halves; the retained half is the mode, and that claim is
  withdrawn. What is true is the load-bearing part: `PLAY-host-may-refuse`,
  `PLAY-soul-autonomy` and `PLAY-motive-drift` all rest on a possessed body
  having a computed inner state at all, and it now does.
- **The driven walk's facts are discarded, and that is not displacement.**
  What the player types is what the body *does* and it commits through the
  ordinary verb path (decision 0168); the walk supplies only what the host
  *wants*. Committing both would give one body two competing sources of
  position. Co-presence is a claim about the host's *inner state* being
  computed, not about its drives moving it while someone else steers.
- **A consequence the campaign had to be honest about:** because the discard
  is unconditional, the spec's argument that "a driven body commits nothing
  because it holds" is untestable in this design rather than confirmed by it.
  The measurement is flat whatever the controller answers. See the amendment
  to the spec's risk 2 and the campaign retrospective.
- The other two candidates are closed. Reopening either needs new information,
  and the information that would matter is a demonstration that a host's
  computed affect has no use — which `PLAY-host-is-a-narrator` exists to
  refute.

## Amendment (2026-09-03, The Minute)

Additive correction of two consequence bullets. **The ruling above — the host
is co-present, not displaced — is unchanged**, and nothing here reopens it;
what changes is the mechanism two of its bullets described.

[0656](0656-a-held-bodys-walk-commits-what-it-does.md) makes the driven
walk's facts commit to the session ledger, unconditionally on which
controller drove the walk. Both bullets below were written about the code as
it stood between this decision and The Minute, and both now read as false in
the present tense.

- **"The driven walk's facts are discarded, and that is not displacement …
  Committing both would give one body two competing sources of position."**
  The facts are no longer discarded, and there are not two sources. The
  walk's own committed `agent-at` **is** the source of position, and the
  roster's `position` column is a view that follows it — held to the ledger's
  own fold by `a_possessed_sessions_columns_are_the_ledgers_too`
  (`windows/vessel/tests/suite/the_rack.rs`). The provenance rule the bullet
  was defending survives untouched: [0168](0168-the-effect-of-an-act-belongs-to-the-body-not-the-driver.md)
  still puts an act's effect with the BODY rather than the driver, which is
  what makes committing the walk correct rather than what forbade it.
- **"Because the discard is unconditional, the spec's argument that 'a driven
  body commits nothing because it holds' is untestable in this design."**
  It is testable again, and tested. Because the commit is unconditional on
  the CONTROLLER, a session that never possesses still gets its empty ledger
  from the walk's own emptiness: its controller is a fresh `PlayerController`
  whose intent is `Hold`, and a Holding walk emits nothing. Forcing that
  controller to answer `Do(Rest)` — the mutation that left
  `the_driven_walks_own_facts_never_reach_the_ledger_while_the_player_says_nothing`
  (`windows/vessel/tests/suite/controller_swap.rs`) GREEN before The Minute —
  would redden it today.

What the discard actually cost is measured in 0656 and in the chronicle: at
seed 42 a held body's walk emitted 29 facts across 40 days and committed
none of them, while its felt state read `Content` throughout.
