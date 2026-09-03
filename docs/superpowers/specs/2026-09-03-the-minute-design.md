# The Minute — a held body's acts are minuted

**Date:** 2026-09-03 · **Registry row:**
`PLAY-imposed-controller-diverges-felt-state` (`elaborated`, high — the
finding this campaign closes) · **Program:** The Bridle metaplan
(`docs/superpowers/specs/2026-08-19-the-bridle-metaplan.md`), a repair
inside Arc III (The Coercion), before anything is built on possession ·
**Ledger:** `docs/superpowers/ledgers/2026-09-03-the-minute.md`

Decision block: 0656–0665.

The minutes of a sitting are taken by someone who did not act. Since The
Coercion a possessed body acts during `wait` — it drinks, eats, sleeps and
walks toward water on its own arbitration — and nothing minutes it: the
walk's facts are discarded at the one call site that runs it. Nathan's
brief: *"A possessed body's walk facts are discarded. `wait` throws away the
driven walk's drank, eaten and movement facts, so a possession's thirst
grows monotonically and diverges from its own ledger the longer it runs.
Pre-existing since The Coercion, but The Rack's VIEW ≡ SCAN test made it
reproducible. Worth a small campaign before anyone builds on possession."*

Autopilot engaged. Branched from main `d9749623b`.

---

## 1. The problem, measured

`Session::wait` (`windows/vessel/src/session.rs`, the block beginning "The
driven body's OWN arbitration") runs the possessed body through
`step_one_with_controller` under an `ImposedController`, keeps the
`Written` it returns (felt state into the roster's driven slot), and binds
its facts to `_driven_facts` — a name whose leading underscore is the whole
mechanism. The comment beside it says so: *"discarded UNCONDITIONALLY,
regardless of what `step_one_with_controller` returns."*

Measured 2026-09-03 with a throwaway in-module probe (deleted; the numbers
are the record), `!possess` then eight `!wait 5` at two seeds. For each
tick the probe re-ran the same solo walk against the pre-wait ledger to see
what had been thrown away, then read the ledger and the felt state:

```
seed 42 (flagship, water in the home room)
  wait#  walk emitted                          ledger drank  felt
  0      slept                                 0             Idle / Content
  1      drank, rested, slept                  0             Idle / Content
  2      drank, eaten, rested, rested, slept   0             Idle / Content
  3..7   drank, eaten, (rested), slept         0             Idle / Content
  total  29 facts emitted, 0 committed, 40 days, 0 drank on the ledger

seed 7 (water is not in the room)
  wait#  walk emitted                          ledger drank  felt
  0      slept                                 0             Idle / Content
  1      slept + 14 agent-at (seeking)         0             Pursuing(Thirst) / Searching
  2      15 agent-at                           0             Pursuing(Thirst) / Searching
  3..7   4 agent-at each                       0             Pursuing(Thirst) / Helpless
  position column: unchanged across all 40 days
```

Two shapes of the same defect:

- **Seed 42 — the body is content and the ledger says it has not drunk in
  40 days.** The walk drinks in place every tick, the drink is discarded,
  the next tick's walk begins from a ledger with no `drank` and drinks
  again. The felt state is read off the walk, so it reads `Content`
  forever; every ledger fold over `drank` (thirst, `learned_helplessness`,
  the resident folds `!needs` renders for everyone else) reads a body that
  has never drunk.
- **Seed 7 — the body learns helplessness from a walk it never finished.**
  The walk seeks water for 14 then 15 rooms, ends somewhere the ledger never
  hears of, and the next tick restarts from the origin room. Nothing
  accumulates. At day 20 `learned_helplessness` (`liveness.rs`, over the
  LEDGER's `last_drank`) crosses `HELPLESS_ONSET_DAYS = 15`, the body gives
  up seeking, and from then on it probes four rooms a tick and comes back.
  The column `Session::position` reads never moved, because The Rack
  (correctly) made it follow the ledger rather than the discarded walk.

The `!wait` line says `Time passes; the world keeps its shape.` at both
seeds, and the player, watching from inside the body, is told nothing.

**Why it exists.** The discard is not a design. The Hand's spec §3.3 drew
the shape `commit(advance_one(body, intent))` for every body; what shipped
was a solo walk asked through a fresh `PlayerController` whose intent is
unconditionally `Hold` — so its facts were always empty and discarding them
cost nothing. The player's verbs (`go`, `sleep`) commit directly through
`Session::commit_agent_at` and `slept_fact`, which is why the walk's own
facts were never wired to the ledger: there was nothing to wire. The
Coercion then swapped in an `ImposedController` that ACTS, measured the
felt-state consequence ("visible in testimony"), and recorded the ledger
half as "ledger-inert" — a true description of the discard, read as a
property of possession. The Rack found the position half of the
consequence, fixed the column, and parked the rest as pre-existing. This
campaign is the rest.

## 2. What must survive

- **Decision 0168 — the effect of an act belongs to the body.** The walk's
  facts are built by the same constructors a creature's are
  (`agent_at_fact`, `drank_fact`, `eaten_fact`, `rested_fact`,
  `slept_fact`), so committing them adds nothing a creature could not have
  committed. H3 of The Coercion (`liveness.rs::h3_the_act_trail_under_an_
  imposed_controller_is_byte_identical_to_the_default_controller`) is
  untouched: it compares two controllers' walks directly and never reads
  `Session::wait`.
- **Decision 0226 — co-present, not displaced.** Arbitration still runs
  for the held body every tick and its felt state is still the walk's own
  last resolution (The Rack, spec §3.4). This campaign changes what happens
  to the walk's *facts*, never whether the walk runs.
- **The Rack's VIEW ≡ SCAN invariant** — `roster.positions()[slot] ==
  agent_position(&ledger, body, day)` at every read, for the driven slot
  too (`the_rack.rs::a_possessed_sessions_columns_are_the_ledgers_too`).
  The fix must move the column THROUGH the ledger, never beside it.
- **A free body's session is byte-identical.** No committed fixture
  possesses (`scripts/possession-*.txt` carry no `!possess`; verified by
  grep), and a free body's walk under `PlayerController` emits no facts at
  all (§4, P4's positive control). So the session goldens, the game-core
  fixtures and every gallery transcript must not move by one byte.
- **The Coercion's headline stands, narrowed to what it always meant.**
  "Possession is invisible in the ledger" was the claim that the ledger
  cannot tell WHO chose — decision 0168's provenance argument — and that
  survives. The sentence that does not survive is "the walk's facts are
  discarded either way", which described the defect.

## 3. Design

### 3.1 The driven walk's facts are committed, unconditionally

`Session::wait` commits `driven_facts` into `self.ledger` through the same
loop shape the population's facts use — `Ledger::commit`, in order,
counting `Ok(true)`, an `Err` routed to `Turn::Out("Time falters: …")` —
immediately after the population's commit loop and before the roster
write-back.

**Unconditionally, not "when possessed".** The natural patch is a branch
on `self.possessor().is_some()`. It is the wrong shape, for the reason the
ideonomy pass surfaced (ledger #1): the controller is already the branch.
A `PlayerController` with nothing queued Holds, and a Holding walk emits
nothing, so committing its facts is a no-op today and becomes the ONE
commit path the moment later Bridle work routes a queued verb through
`PlayerController::queue` — which The Hand's §3.3 pseudocode always
intended. A possession-gated commit would leave that path to be re-plumbed
and would make the ledger's honesty depend on who is driving, which is
exactly what 0168 forbids.

The commit ORDER is a determinism contract from the day it lands:
population facts first (in `step_with_occupancy`'s existing order), then
the driven body's, then the First Mark's `turned-hostile` loop, unchanged.
Every fact has a different subject from the population's, so no fold
crosses the boundary; the order is fixed so that a saved played world's
ledger is reproducible from its script, not because any reader depends on
it.

### 3.2 The driven slot's position follows the ledger, through the walk

After the driven commit loop, `self.roster.place(driven_slot,
driven_written.position)` replaces the current `resolve`-only write with
`write` (position and felt together) — the pairing `Roster::write`'s own
doc reserves for "a walk whose facts were COMMITTED, and only such a walk",
which the driven walk now is. `driven_written.position` is the walk's own
`st.pos`, and every move that advanced `st.pos` emitted an `agent-at` the
loop just committed, so the column and `agent_position(&ledger)` agree by
construction; P3 pins it rather than trusting the sentence.

`Roster::resolve` keeps its job — the write for a body whose arbitration
resolved something the ledger did not record — but its doc's example ("the
driven body is that case, and it is not an edge one") is deleted, because
it is no longer that case. It stays as a method because the off-band rule
(§3.3) still needs a felt-only write.

### 3.3 Off the walk band, a held body holds

`inside`, `submerged` and `underground` are session-only frames: the
body's ledger position stays at the walk band throughout a descent
(`Session.inside`'s own doc), and `out`/`surface`/`climb` restore the
player to the room the frame was entered from. A walk that committed
`agent-at` to another mesh room while a frame is open would strand it — the
frame names a house the body is no longer standing at.

So when the driven body is possessed AND any of the three frames is
`Some`, `wait` asks the solo walk through a `PlayerController` (Hold)
instead of the `ImposedController`. Arbitration still runs — the felt state
is written by `resolve`, exactly as a free body's is indoors — but the walk
cannot act off the band it does not understand. This is one branch, the
same branch The Coercion already has (`if self.possessor().is_some()`),
with the frame test added to its condition.

**This is a fidelity cut and is flagged for G3 (§7).** Its cost: a held
body indoors, underwater or underground does not drink, eat or sleep on
its own during `wait`, and its ledger thirst grows exactly as every body's
did before this campaign — but honestly now, with the felt state (Hold →
`Pursuing(Thirst)`) agreeing with the ledger instead of reading `Content`
over a discarded drink. The alternative — teaching the creature walk the
player-only frames, or clearing a frame when the body walks out of it —
is a campaign of its own (the frames carry lattices, cells and chamber
indices the walk has no model of) and is recorded as a registry row rather
than built.

### 3.4 The wait line minutes what the body did

`narrate_motion` gains the driven body's own committed facts. Two rules:

- **A room change is named first, and suppresses the arrival/departure
  comparison.** `before` was copied in the room the body has since left;
  comparing it against `here` (the new room) would narrate everyone in the
  old room as departed and everyone in the new room as arrived. When the
  driven position moved, the line says the will that holds the body walked
  it elsewhere, and stops — `!look` is one keystroke away and already
  answers for the new room.
- **Needs served are named as clauses,** one per predicate present among
  the committed driven facts: drank, eaten, rested/slept. Their order is
  the predicates' first appearance in the commit.

Exact wording is the implementer's, with two constraints: the sentence
names the possessor's will, not "you" — under 0168 the act is the body's
and under 0226 the choice was not the player's — and a free body's line is
byte-identical to today's, which holds trivially because a free body
commits no driven facts.

The population's `moved` count is not touched: the driven facts are
counted separately, so `Time passes; the world keeps its shape.` still
means the population did not move, and a tick where only your own body
moved says so rather than reporting one stirred.

### 3.5 What the commit reaches, and what it does not

Everything downstream of the ledger is now current for a held body without
further work, because it was always a fold: `Session::needs`' resident
folds, `learned_helplessness`, the catch-up in `step_one_with_controller`
(which reconstructs decisions "since the last committed room-entry" and now
has one), the gate's `Asleep` row after a `slept` the walk committed, and
the snapshot's `self` entry. One consequence is worth its own assertion: a
body that fell asleep under the hold is asleep when `!unpossess` releases
it, and IC verbs refuse for that reason until the span ends.

Not reached, deliberately: the driven walk's within-room `Occupancy` is
built and dropped inside `step_one_with_controller` today and still is —
pre-existing, and a within-room seat is not a committed fact (decision
0069). Followup, not scope.

## 4. Preregistered measurement (decision 0016)

Frozen before the code. A falsified prediction is a finding.

- **P1 — seed 42, the minuted drink.** `!possess` then eight `!wait 5`.
  Today: 29 facts emitted, 0 committed, felt `Content` throughout. After:
  the driven body's committed `drank` count is ≥ 7 (one per tick from
  wait#1, matching what the walk emitted today) and its felt state still
  reads `Idle`/`Content` — the felt state was right; the ledger was wrong.
- **P2 — seed 7, progress accumulates.** Same script. Today: `Helpless`
  from day 20, 0 `drank`, position frozen. Prediction: the body's position
  column moves on wait#1 and the ledger holds ≥ 1 `drank` by day 40,
  because a walk that resumes from where it stopped reaches water a walk
  that restarts cannot. **The null is a finding:** if the body is still
  `Helpless` at day 40 with 0 `drank`, it is now helpless thirty rooms from
  home rather than in it, and the chronicle reports that the water was
  never reachable.
- **P3 — VIEW ≡ SCAN survives the new writer.** `the_rack.rs::a_possessed_
  sessions_columns_are_the_ledgers_too` extended with the seed-7 script
  above, green; positive control: the mutation The Rack's own test names
  (writing `driven_written.position` without committing the facts, or
  committing without `place`) reddens it.
- **P4 — a free body is byte-identical.** Positive control first: a
  probe/test asserting the free solo walk at seeds 42 and 7 emits zero
  facts over four `!wait 5` (measured before code: see ledger #1). Then
  `make rebaseline` and the drift diff over `docs/generated-paths.txt`.
  Decision rule: anything under `clients/game/core/tests/fixtures/`,
  `windows/vessel/tests/fixtures/`, or a gallery transcript moving → STOP,
  the free path is not inert and §3.1's premise is wrong; only
  `docs/audits/` moving (a `pub` surface changed) → regenerate and commit
  in the same commit; nothing moving → expected.
- **P5 — off the band, nothing commits.** Seed 14's flagship (the
  four-chamber dwelling `session.rs`'s custody tests use): `enter` FIRST
  (it is in-character and would refuse once held), then `!possess`, then
  `!wait 5` commits zero driven facts and leaves `inside` intact;
  the positive control is the same body outdoors under the same script
  committing at least one.
- **P6 — the trail still cannot tell.** `h3_*` and `h4_*` in `liveness.rs`
  untouched and green; `possession_facts.rs` unchanged.
- **P7 — the line.** Seed 7 wait#1's line names the move and not a stirred
  count; seed 42 wait#1's names the drink; a free body's line at both seeds
  is byte-identical to today's (P4's positive control makes this
  unconditional).

Decision rules for artifacts: as P4. The census is not touched by a
session-only change and is not queued for it.

## 5. Capture

- `PLAY-imposed-controller-diverges-felt-state` — Where gains The Minute;
  the row's claim is narrowed at close to the half that survives (visible
  in testimony; the ledger-inert half was the defect).
- New row `PLAY-held-body-off-the-band-holds` (raw): a possessed body
  indoors, underwater or underground does not act on its walk, because the
  frames are player-only state the creature walk has no model of; the
  campaign that teaches it is the campaign that gives NPCs the same frames.
- New row `PLAY-free-body-cannot-drink` (raw): a free possession has no
  `drink`/`eat` verb — the IC roster is `go`/`sleep`-shaped — so its own
  thirst is monotone by construction; `controller.rs`'s doc claims a
  `drink` verb exists and is corrected in this campaign. The remedy is the
  queued-verb path §3.1 leaves ready.
- Followup: the driven walk's `Occupancy` is dropped (§3.5).
- Decisions, from 0656: a held body's walk commits what it does, and the
  commit is unconditional on the controller; off the walk band a held body
  holds; the wait line minutes the body's own acts and suppresses the
  arrival/departure comparison when the body moved.

## 6. Definition of Done

Chronicle (`book/src/chronicle/the-minute.md`), retrospective, the
decision records above, the registry flips, and a freshness sweep of every
sentence that described the discard as a property: `the-coercion.md`'s
"What the ledger cannot tell" section, `the-rack.md`'s parked finding,
`controller.rs`'s `ImposedController` and `PlayerController` docs,
`roster.rs`'s `resolve` doc, `step_one_with_controller`'s doc, and the two
`wait` comments. The P1/P2 readings pasted into the chronicle. Stage gate
at the one plan-stage boundary; merge through the sluice.

## 7. Flagged for G3 — owner decisions, not autopilot's

1. **Save-format adjacent.** A saved played world now carries walk-authored
   `drank`/`eaten`/`rested`/`slept`/`agent-at` facts for the player's own
   body. No new predicate, no new provenance shape, the same constructors —
   but The Bridle metaplan's risk 5 named "whether a player's trail is
   wanted in a saved played world" as a decision The Deed made for verbs;
   this extends it to the walk.
2. **Fidelity cut (§3.3).** A held body off the walk band holds. The
   alternative is a frames-aware creature walk; not built.
3. **Narration register (§3.4).** The line attributes the act to the
   possessor's will. The wording is the implementer's within that rule.

## 8. Deliberately not in this campaign

A frames-aware walk; a `drink` verb for a free body; the driven
`Occupancy`; any change to `ImposedController::intend`; a possessing
creature with intent of its own (The Coercion §5 defers it to a species
campaign); mortality.
