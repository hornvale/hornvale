# 0596. A creature feels what its own last resolution felt

**Status:** Accepted (2026-09-02) · **Decider:** Nathan · **Campaign:** The Rack

In the context of a possession session standing among sixty-seven residents,
facing a turn that re-derived every one of their felt states from the ledger
on every snapshot, we decided that **the felt state a creature reports is the
one its own last tick resolved** — stored in the roster's `felt` column by the
walk that produced it — accepting that a body's reported feeling lags until
its next tick, and that the wording of `needs` and `sensed.present[*].felt`
changes for worlds that did not otherwise move.

## Context

`affect_of` and its memoised siblings compute "the same arbitration a walk
step runs, but stateless": no alarm field, `Mode::Idle`, an empty frontier,
the session's day rather than the walk's own instant. Every turn called it
once per present body.

The reason that read existed is historical, not modelling. The tick already
computed the real thing — `WalkState.affect`, with the alarm field, with mode
hysteresis, with the walk's own water belief and frontier — and then dropped
it (`windows/vessel/src/liveness.rs`, the end of each body's walk). The
Confidant adopted the stateless read because the tick's value was not
reachable by any reader. It was a workaround for an absence, and this campaign
removes the absence.

## The decision

1. **The tick's resolution is the felt state.** `step_with_occupancy` returns
   one `Written { entity, position, felt }` per body it walked, and `wait`
   writes each into that body's slot. `Felt` is the durable residue of a
   `Resolution` — the `Affect`, the `Mode` the walk carried forward, and the
   drives it found active and did not pursue — and nothing else: `Intent` is
   an instantaneous choice with no meaning after the tick that made it.

2. **Between ticks a body does not re-feel.** A room hop that advances the day
   by a tenth does not make sixty-seven bystanders re-integrate their thirst.
   A reader that finds a stale-looking value is reading a real fact about a
   body that has not been advanced — never a cache miss. This is what makes
   `felt` **content** rather than a view, and it is the whole difference from
   the `position` column beside it, which is a projection of the ledger and
   must agree with `agent_position` at every read (decision 0597).

3. **A never-walked body carries its push-time seed.** `Roster::push` seeds
   `felt` with one stateless `affect_of` read, which is what turn 0 and every
   newly appended resident report until their first tick. `Roster::written`
   distinguishes the two cases, and it is what `Session::driven_mode` and
   `driven_affect`'s documented `None` before the first `!wait` now rests on.

4. **The stateless `affect_of` family stays** — for the lab's health battery,
   for tests, and for that one seeding read. What it no longer is is the
   session's read: after this campaign the only `affect_of*` callers on any
   session path are the two seeding sites.

## Consequences

- **The session goldens move, and that movement is the campaign's positive
  control.** At the SHA this shipped on, `sensed.present[*].felt` moved 69
  leaves in `windows/vessel/tests/fixtures/snapshot-seed-0-chamber-occupied.json`
  and 58 in `clients/game/core/tests/fixtures/session-seed-14-carrying.json`,
  and 214 replaced lines (428 diff lines) of
  `book/src/gallery/possession-over-time-seed-42.md` — every one of them a
  felt-state line, and no JSON key other than `felt` moved anywhere. No world artifact
  moved and the census that followed moved zero columns.
- **A crowd's `needs` reply reads flatter after a tick at seed 42** — 63 of 67
  bodies share one phrase, because sixty-seven co-present residents resolved
  the same way and `felt_phrase` buckets an `Affect` into prose. **This is not
  new.** The pre-change gallery was already 63-of-67 identical (`settles down
  to rest` ×63 before, `grows restless` ×63 after; distinct-phrase counts per
  block 5/5/5/8/7 → 5/5/5/6/7). The ruling changed *which* phrase a crowd
  shares, not how many phrases a crowd has. Whether `felt_phrase` should carry
  the affect's object or magnitude so a crowd reads as a crowd is a design
  question, open as `RENDER-felt-phrase-buckets-a-crowd` in the idea registry.
- **`needs` remains the omniscient read it always was**, but by a different
  mechanism: it reports each co-located creature's own last resolution rather
  than re-running an arbitration on its behalf. The Confidant's chronicle is
  amended where it names the old mechanism.
- The alternative that keeps today's wording — compute the *stateless*
  quantity inside the tick and store that — was discarded: it pays the fold
  once per body per tick and pins a re-imagining of the creature as its
  canonical feeling. Storing both and letting the client choose was discarded
  as two truths on the wire.

## See also

Spec §3.4 (`docs/superpowers/specs/2026-09-02-the-rack-design.md`); decision
0597 (the roster this column lives in); decision 0579 (affect is component
data, never a fact — `felt` is serialized nowhere); decision 0069 (FRAME-tier
state); `windows/vessel/src/liveness.rs` (`Felt`, `Written`);
`windows/vessel/src/roster.rs` (`felt`, `written`, `resolve`);
`docs/superpowers/ledgers/2026-09-02-the-rack.md` entries #2 and the Task 4
correction; `book/src/chronicle/the-rack.md`;
`book/src/chronicle/the-confidant.md`.
