# 0657. Off the walk band a held body holds

**Status:** Accepted (2026-09-03) · **Decider:** Nathan · **Relates:**
[0226](0226-a-possessed-host-is-co-present-not-displaced.md);
[0656](0656-a-held-bodys-walk-commits-what-it-does.md);
[The Minute spec](../superpowers/specs/2026-09-03-the-minute-design.md) §3.3;
[ledger](../superpowers/ledgers/2026-09-03-the-minute.md) #2

In the context of a possessed body standing inside a dwelling, underwater or
underground — frames the session holds and the ledger does not — facing a
walk that would now commit a mesh move out from under the frame that named
the house the body was standing at, we decided that **while a held body is
off the walk band its solo walk is asked through a Holding controller: its
arbitration still runs and its felt state is still written, and it commits
nothing**, accepting that such a body does not drink, eat or sleep on its own
during `wait`.

## Context — the frames are session state, and the walk has no model of them

`Session.inside`, `submerged` and `underground` are session-only frames. The
body's ledger position stays at the walk band throughout a descent —
`Session.inside`'s own doc says so — and `out`/`surface`/`climb` restore the
player to the room the frame was entered from. A walk that committed an
`agent-at` to another mesh room while a frame was open would strand it: the
frame would name a house the body is no longer standing at, and the exit
verb would return the player to nowhere.

The creature walk has no model of a lattice, a cell, or a chamber index. The
two alternatives both fail on that:

- **Clear the frame when the walk leaves the room.** The walk does not know
  it was in a frame, so nothing in it can decide to clear one.
- **Let the walk act but drop only its `agent-at`.** A drink taken in a
  house standing on dry land, with no water in the house — the walk reasons
  at the band and the frame does not exist there.

Teaching the walk the frames is the campaign that gives every creature the
same frames, and that is a campaign, not a branch.

## This is a fidelity cut, and it was flagged as one

It was put to the owner as a fidelity cut rather than taken by autopilot. Its
cost, stated plainly: a held body indoors, underwater or underground does not
act on its own drives, and its ledger thirst grows exactly as every body's
did before 0656 — but **honestly** now, because the felt state written by the
Holding walk agrees with the ledger. Before this campaign the same body read
`Content` over a drink that was thrown away.

The branch is not a new one. The Coercion already asks
`if self.possessor().is_some()` to choose a controller; the frame test joins
that condition.

## Consequences

- Registry row `PLAY-held-body-off-the-band-holds` carries the gap forward:
  the campaign that lifts it is the one that gives derived creatures the
  same frames, not a patch to `wait`.
- Indoors the felt state and the ledger agree, which they did not before.
- Pinned at seed 14's four-chamber dwelling: `enter`, then possess, then a
  five-day wait commits zero driven facts and leaves the frame intact. The
  mutation that drops the frame test from the condition is not vacuous — the
  same body commits facts within one `!wait 5` without it (five against
  four).
