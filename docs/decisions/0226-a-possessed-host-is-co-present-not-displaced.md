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
- **The gap between what the host wanted and what the body did is derivable
  from this moment on, with no new machinery.** Every tick now computes both.
  That gap is the substrate under `PLAY-host-may-refuse`, `PLAY-soul-autonomy`
  and `PLAY-motive-drift`; none of them needs a mechanism this arc did not
  ship.
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
