# The Minute — decision ledger

Campaign: `campaign/the-minute` · Spec:
`docs/superpowers/specs/2026-09-03-the-minute-design.md` · Plan:
`docs/superpowers/plans/2026-09-03-the-minute.md` · Decision block: 0656–0665.

Written on the campaign's own branch as each ruling is made (The Cartulary,
decision 0486). Entries before the plan exists use the numbered form.

### #1 [G1] — how is the discarded walk repaired?

**Question:** `Session::wait` binds the possessed body's walk facts to
`_driven_facts` and drops them. What is the fix's shape?

**Decision:** Commit the driven walk's facts into the session ledger,
UNCONDITIONALLY on the controller, immediately after the population's
commit loop; then `Roster::write` the driven slot (position and felt) from
the walk's `Written`. Spec §3.1–3.2.

**Why (precedent cited):** The Hand's spec §3.3 already draws
`commit(advance_one(body, intent))` for every body; the discard shipped
because a `PlayerController` Holds and a Holding walk emits nothing, so
there was nothing to commit — measured today, not inferred: a free body's
solo walk at seeds 42 and 7 emitted **0 facts** on each of four `!wait 5`
and ended in the column's own room every time. Decision 0168 (the effect
of an act belongs to the body) forbids making the ledger's honesty depend
on who is driving, which a possession-gated commit would do. The Rack's
VIEW ≡ SCAN invariant is kept by moving the column through the ledger
(`write` after the commit), which is the pairing `Roster::write`'s doc
already reserves for a committed walk.

**Alternatives discarded:**

- *Commit only when possessed* — the same bytes today, the wrong shape:
  leaves the queued-verb path to be re-plumbed later and branches on the
  driver.
- *Revert The Coercion's swap (ask the held body through a Holding
  controller)* — restores ledger/felt agreement by making the held body do
  nothing; contradicts the metaplan's model that a held body acts like a
  creature, and deletes The Coercion's measured finding.
- *Carry the walk's end-state across ticks in a session field* — a second
  ledger; violates "a world is a seed plus a ledger" and reintroduces the
  drift The Rack removed.
- *Commit the need facts but not `agent-at`* — the body drinks water in a
  room it never entered; incoherent by construction.

**Ideonomy passes / overturns:** 1 / 0. Tuple: combination × matrix,
dimension prompts intentionality and homogeneity. Intentionality placed the
discard as *accidental-then-rationalised* (designed version = The Hand's
pseudocode), which is what turned "commit when possessed" into "commit
unconditionally". The matrix (fact class × where session state lives)
produced three enrichments recorded as #2–#4 below.

**Capture actions:** spec §3; registry rows in spec §5; this entry.

### #2 [Q] — what does a held body do off the walk band?

**Question:** `inside`/`submerged`/`underground` are session-only frames
whose ledger position stays at the walk band. A walk that commits a mesh
move while a frame is open strands the frame. What happens?

**Decision:** When possessed AND a frame is open, the solo walk is asked
through a `PlayerController` (Hold): arbitration runs, the felt state is
written (by `write`, since #7 deleted `resolve`), no fact commits. Spec §3.3. **Flagged for G3 as a
fidelity cut.**

**Why:** `Session.inside`'s doc: "the possessed agent's own `position`
stays at the WALK band throughout — descent is recorded here, not there."
The creature walk has no model of a lattice, a cell or a chamber index;
teaching it one is the campaign that gives NPCs the same frames. The
Coercion's own `if self.possessor().is_some()` branch is the seam; the
frame test joins its condition.

**Alternatives discarded:** clear the frame when the body walks out (the
walk does not know it was in one, and `out` would then return the player
to nowhere); let the walk act but drop only its `agent-at` (a drink in a
house on dry land, with no water in the house — the walk reasons at the
band, the frame does not).

**Ideonomy passes / overturns:** produced inside #1's matrix pass (the
`agent-at × frame` cell), 0 overturns.

**Capture actions:** registry row `PLAY-held-body-off-the-band-holds`
(raw); decision 0657 at close.

### #3 [Q] — does the wait line say anything?

**Question:** Today `!wait` reports `Time passes; the world keeps its
shape.` while the held body walked fifteen rooms. Once the facts commit,
`!look` shows a different room with no explanation.

**Decision:** `narrate_motion` minutes the driven body's own committed
facts: a room change is named first and suppresses the arrival/departure
comparison (whose `before` was copied in a room the body has left); needs
served are named as clauses per predicate present. The population's
`moved` count is unchanged. Spec §3.4. Wording is the implementer's, under
two constraints (attribute to the possessor's will; free body's line
byte-identical).

**Why:** The `before`/`here` comparison in `narrate_motion` is
slot-indexed against `here = positions[driven]` read AFTER the tick; with
the driven body moving, `was_here`/`is_here` would misreport everyone in
both rooms. `PLAY-host-is-a-narrator` and 0226 want the held body's inner
life readable; the wait line is the only OOC surface a held player has
(`ask` is IC and refuses).

**Alternatives discarded:** count driven facts into `moved` (one stirred
would then mean "you moved"); narrate nothing and let `!look` carry it
(the player is told the world kept its shape while their body crossed a
valley).

**Ideonomy passes / overturns:** produced inside #1's matrix pass (the
`agent-at × narration` cell), 0 overturns.

**Capture actions:** spec §3.4; decision 0658 at close.

### #4 [Q] — is the free path really inert?

**Question:** §3.1 rests on "a Holding walk emits nothing". Claim or
measurement?

**Decision:** Measurement, taken before the spec was written: throwaway
in-module probe, free body, seeds 42 and 7, four `!wait 5` each — `facts=0`
and `end_pos_eq_column=true` on all eight ticks. Recorded in spec §4 P4 as
the positive control; the drift diff over `docs/generated-paths.txt` is
the mechanical check at execution, with a decision rule rather than a
prediction.

**Why:** Autopilot's own rule — a generated-artifact claim needs a
command-and-output pair beside it at drafting time.

**Alternatives discarded:** none; the alternative was not measuring.

**Ideonomy passes / overturns:** none run — a verification, not a design
question; stated explicitly rather than left blank.

**Capture actions:** spec §4 P4.

### #5 [Q] — the campaign's name

`the-tally` and `the-docket` both exist as worktrees; `the-minute` has no
branch, worktree, spec or chronicle. Trivial; no pass.

### #6 [G4] — the spec said the gate's `Asleep` row was a fold; it is a field

**Question:** Spec §3.5 claimed a walk-committed `slept` would make the
gate read `Asleep` "without further work".

**Decision:** False, and corrected in the spec (pre-merge, own record).
`Session::body_state` matches on `self.wake_at`, which only `Session::sleep`
sets. The plan adds `wake_after(facts, now) -> Option<WorldTime>` and sets
the field from the driven commit by the verb's own rule (Task 3).

**Why:** Read `body_state` (`session.rs:3494`) and `sleep` (`:3762`) while
writing Task 3, rather than the doc comment that described the gate. The
walk's `slept` fact carries its span as the object (`bout_fact`) and
`advance_one` lets a sleep run past `to`, so the case is real.

**Alternatives discarded:** derive `wake_at` from the ledger for BOTH the
verb and the walk (a bigger change to a field the verb already owns; a
followup if anyone wants `body_state` to be a pure fold).

**Ideonomy passes / overturns:** none run — a correction of a false claim,
not a design choice; stated explicitly.

**Capture actions:** spec §3.5 amended; retrospective (Task 6) carries the
lesson: the claim was written from reasoning about a fold and the cure was
reading the two functions.

### #7 [G4] — `Roster::resolve` goes

**Question:** Spec §3.2 kept `resolve` for the off-band arm's felt-only
write.

**Decision:** Delete it; `write` is correct for a Holding walk too, since
its position equals the column. Spec §3.2 amended.

**Why:** Its only caller is the line the campaign replaces
(`grep -rn "\.resolve(" windows/vessel` → one site in `wait`, one inside
`write` itself). A second writer that is only ever correct when it agrees
with the first is a second way to be wrong.

**Ideonomy passes / overturns:** none run — a code-grounded simplification
found while writing the plan; stated explicitly.

**Capture actions:** spec §3.2; plan Task 2 step 3.

### G4 — plan self-review

Spec coverage, placeholder scan and type consistency are recorded at the
foot of the plan. Two corrections came out of writing it (#6, #7); no
section of the spec is without a task.

## Followups (promoted at close — each now has a home)

- The driven walk's within-room `Occupancy` is built and dropped inside
  `step_one_with_controller`; the population's is not. Pre-existing; not
  a committed fact (0069). **Home:** the retrospective's Followups section
  and the chronicle's closing section; spec §3.5 names it out of scope.
- `controller.rs`'s `PlayerController` doc said "today's verb loop — `go`,
  `drink`, … — still commits directly"; there is no `drink` verb
  (`IN_CHARACTER_VERBS`, `session.rs:185`). The doc was corrected in Task 5's
  freshness sweep; the gap itself is `PLAY-free-body-cannot-drink`.
  **Home:** that registry row, whose Where cell now links the chronicle.
- A free seed-42 body reads `Pursuing(Thirst)` from its second wait and
  has no way to drink. Same row. **Home:** as above; the chronicle's "what
  stays open" states it and names the commit path a queued verb would use.
- A frames-aware creature walk (the §3.3 fidelity cut). **Home:** decision
  0657 and registry row `PLAY-held-body-off-the-band-holds`, whose Where
  cell now links the chronicle.

## Task 1 — complete (commits 46f6ccec2..358f72801)

The red witnesses and the free-path control. Two repo ratchets the brief
did not name fired on the commit gate and were satisfied mechanically: a
seed-looping test needs a `claim:` tag (`cli/tests/suite/claim_shape.rs`,
decision 0093), and every `build_world` call site is rostered in
`cli/tests/fixtures/world-build-sites.tsv` (decision 0606). Review approved
with one plan-mandated Important — a message-less assertion in the plan's
own P1 code — ruled trivial and carried into Task 2's dispatch. No ideonomy
pass; a hygiene ruling.

## Task 2 — complete (commits 358f72801..2953a2d53, one fix round)

The fix. `Session::wait` commits the driven walk's facts after the
population's loop; the driven slot goes through `Roster::write`;
`Roster::resolve` is deleted. `make rebaseline` moved nothing under
`docs/generated-paths.txt` — not even `docs/audits/`, since `resolve`'s
signature carried no tagged primitive.

**P1 held: 7 `drank` facts on the held body by day 40 at seed 42, felt
state `Idle`/`Content` throughout.**

**P2 is THE NULL, and it is the campaign's finding.** At seed 7 the column
moves on the first seeking wait (the mechanism half, green), and the body
still commits 0 `drank` by day 36 and reads `Helpless`. A walk that resumes
from where it stopped did NOT reach water that a restarting walk could not;
whatever keeps seed 7's held body from water is not the discard. Recorded
as an `assert_eq!` on the measured count, not retuned. Ruling: the
chronicle characterises it (per-tick trail, nearest-water distance) at
close rather than this task guessing — what it costs if wrong: a chronicle
paragraph, not code.

**A literal the plan got wrong, corrected by measurement:** the plan
predicted the held session's ledger would carry ONE more fact than the
free one after the first wait (the walk's `slept`). Measured: two, because
`!possess` itself commits a `possessed-by` fact with the body as subject.
The review then asked for the narrower assertion the plan had meant —
exactly one `slept` for the held body, zero for the free — and it was
added beside the corrected count. Review: one Important (a "three waits"
message left stale by the nine-wait extension), fixed in round 1;
re-review clean. No ideonomy pass for either ruling; both are
measurement corrections.

## Task 3 — complete (commits 28f562469..03348c633)

Off the walk band a held body holds; a walk-committed sleep that outlasts
the tick sets `wake_at`. The off-band mutation (dropping the frame test)
was non-vacuous at seed 14 within one `!wait 5`: five committed facts
against four. Review approved; one deferred minor on a comment's wording
(the merge keeps the later wake, which `Session::sleep` has no occasion to
do), to be reworded in Task 5's sweep. No ideonomy pass; nothing was
decided here that the spec had not already.

## Task 4 — complete (commits 072b30def..29c18f5f3)

The wait line minutes the held body's acts. The three lines P7 MEASURED:
seed 7's first seeking wait, `Time passes. The will that holds you walks
this body elsewhere.`; seed 42's second wait, `Time passes. You sense
movement nearby (201 stirred). The will that holds you drinks and rests.`;
the free body's, `Time passes. You sense movement nearby (201 stirred).` —
byte-identical to before the campaign, and `make rebaseline` moved no
gallery transcript or fixture. **What P7 HOLDS is four assertions over four
lines, not three** (`the_minute.rs:132-181`): a substring plus a
no-stirred/no-arrival negative on seed 7's line; a `drinks` substring on the
quoted seed-42 second wait; a `The will that holds you rests.` SUFFIX on seed
42's FIRST wait, a fourth line none of the three quoted above; and an absence
on the free body's. Review approved; three minors folded into
Task 5 (a paraphrase of 0168/0226 to tighten; one narration branch — a
stationary body with minutes under zero population movement — with no
end-to-end assertion; helper placement). No ideonomy pass; the wording was
the plan's and the review found it true to §3.4.

## Task 5 — complete (commits 9ac029b73..633ec6cb0, one fix round)

The doc sweep, the commit gate, the absorption of main (45 commits, a
clean auto-merge, so `make gate-commit` was re-run by hand — a clean merge
fires no hook), the post-merge regeneration (only `docs/timings.md`
moved), and the stage gate: `req-0a985e944cb8-20260903T190153Z`,
`reported`, four phases green in 1057 s, main untouched.

**The sweep missed a sentence ten lines from one it fixed.** The false
claim "the same rule `Session::sleep` applies" stood in two comments; the
brief named one site and the implementer fixed that site. The review found
the twin by grepping the CLAIM. A sweep brief should name the sentence to
grep for, never the line to edit. The other Important: a rewritten doc
asserted the wait line reads the felt-state trio; it reads the walk's
committed facts. Both fixed in round 1; re-review clean.

**Ruling: no second stage gate for the fix-round commit.** It is prose
only, on top of a green stage gate, and the merge gates the final SHA
anyway. What it costs if wrong: a red merge where a stage gate would have
been red first — the same information, one queue slot later.

**Measured for P7's stationary branch:** seed 42's first held wait is not
stationary for the population (67 stirred), so the assertion pins the
suffix `The will that holds you rests.` rather than the whole line, and
the test's doc says which branch it covers. No ideonomy pass; a sweep.

## Task 6 — complete

The close: three decision records, the chronicle, the freshness sweep, the
registry, the retrospective, and this section. No ideonomy pass — nothing was
DECIDED here that the spec had not already; this task writes down what the
campaign found.

**Characterising the null, before writing a word about it.** The chronicle
had to say WHY seed 7's held body still commits 0 `drank`, measured rather
than guessed, so a throwaway in-module probe (deleted; `git diff --stat`
confirms `session.rs` untouched at commit) ran the eight-wait script at three
configurations and read the trail per tick.

Held, seed 7 — cumulative `agent-at` on the ledger, and a breadth-first hop
search **from the room the body is standing in on that tick**, run to a
**forty-hop ceiling** (probe 1's ceiling; the deeper 120-hop search below is a
separate measurement, taken from the home room only):

```
  start  moved=—      agent-at=0   Idle/Content                water > 40 hops
  wait#0 moved=false  agent-at=0   Idle/Content                water > 40 hops
  wait#1 moved=true   agent-at=29  Pursuing(Thirst)/Searching  water > 40 hops
  wait#2 moved=true   agent-at=60  Pursuing(Thirst)/Searching  water > 40 hops
  wait#3 moved=true   agent-at=65  Pursuing(Thirst)/Helpless   water > 40 hops
  wait#4 moved=true   agent-at=70  Pursuing(Thirst)/Helpless   water > 40 hops
  wait#5 moved=true   agent-at=74  Pursuing(Thirst)/Helpless   water > 40 hops
  wait#6 moved=true   agent-at=78  Pursuing(Thirst)/Helpless   water > 40 hops
  wait#7 moved=true   agent-at=82  Pursuing(Thirst)/Helpless   water > 40 hops
```

Held, seed 42 (the positive): the body never moves, water is in its own room
(0 hops), and `drank` climbs 1 per tick from wait#1 to 7 at wait#7, felt state
`Idle`/`Content` throughout. Free, seed 7 (the control): never moves,
`agent-at=0` and `drank=0` on every tick.

A second probe answered the mechanism question directly. Its search runs
**from the home room only**, to a 120-hop ceiling — this is the sole source of
the 120-hop / 59,049-room figure, and it says nothing about the rooms the body
later walked to beyond the 40 hops probe 1 measured:

```
  seed 7  home=…[3,0,3,1,3,2,2,1,1,1,2,3,0]  resource == home = TRUE
          nearest fresh water FROM HOME: NONE within 120 hops, 59,049 rooms searched
  seed 42 home=…[2,3,3,1,0,0,1,2,0,2,3,3,1]  resource == home = TRUE
          nearest fresh water FROM HOME: 0 hops (the home room itself)
```

**So the null is not the repair and not helplessness — it is that there is no
water.** `Body.resource` is resolved once at derivation as
`nearest_water(home, terrain, PLAN_BUDGET).unwrap_or(home)`, and at seed 7 it
falls back to the home room, which `liveness.rs`'s own test already documents
as "a real, legitimate outcome, not a derivation failure". The seeking the
repair made real is genuine exploration with no destination: 29 rooms on the
first seeking wait, 31 more on the second, then the first `Helpless` reading at
day 20.5 and per-tick deltas of 5, 5, 4, 4, 4 thereafter. The body is now helpless thirty-odd rooms from home instead
of helpless in it, and the ledger says so.

**What remains unexplained, and is stated as such in the chronicle.** The free
control reads `Searching` at every one of the eight FIVE-day waits, while the
same free body sampled every THREE days reads `Helpless` at most of them.
`learned_helplessness` lifts for one day in every `HELPLESS_PROBE_DAYS = 5`, so
a five-day cadence samples one phase of that cycle every time — the
cadence-dependence is measured; the exact phase each cadence lands on was not
derived, and nothing in the campaign depends on it.

**Written at close.** Decisions 0656 (a held body's walk commits what it does),
0657 (off the walk band a held body holds), 0658 (the wait line minutes the
held body's acts), plus three rows on `docs/decisions/README.md`.
`book/src/chronicle/the-minute.md` with both of spec §1's tables, the trail
above, P1's seven drinks, the null and its cause, the off-band cut, and what
stays open; listed in `book/src/SUMMARY.md` after The Nettle. Freshness sweep
with dated in-place corrections: `the-coercion.md` (the "ledger-inert" and
"discarded either way" sentences), `the-rack.md` (the VIEW≡SCAN paragraph and
the parked finding, now marked closed), `the-hand.md` (three claims resting on
the discard), and `book/src/open-questions.md`, which the task did not name and
which grepping the CLAIM turned up carrying two more present-tense statements
of the discard. `windows/vessel/tests/suite/overrides.rs`'s file doc named a
`driven_suppressed` accessor that The Rack removed; corrected to
`suppressed_drives`.

**The Confidence Gradient moves no bet.** Grepped
`book/src/open-questions.md` for possession: the nearest scored entry is the
lazy-derivation bet, whose supporting claim was already corrected by The Deed
on exactly this axis (a possessed body's in-character acts are persistent
world state) and whose own text records that the bet does not move for it.
The Minute extends that same already-corrected boundary from verbs to the
walk; committing on *action* is still not committing on *observation*, and
promotion-on-touch is exactly as unbuilt as it was. No re-score.

**Two findings routed here rather than lost with the scratch.**

- *`narrate_motion`'s `None` arm inside the room-change branch is unreachable
  by construction* (Task 4's report, flagged for the controller and never
  homed). A room change can only arise from a walk-committed `agent-at`, and
  `minutes_of` always turns one of those into `Minute::Moved`, so the minute
  line is never `None` when the driven position moved. It is a defensive
  fallback written exactly as the plan specified, not a bug. **Parked as a
  deferred simplification for the final review to triage** — deleting it means
  proving the invariant rather than assuming it.
- *Deriving `wake_at` from the ledger for BOTH the verb and the walk, so
  `Session::body_state` becomes a pure fold* (ledger #6's discarded
  alternative). Set aside as a bigger change to a field the verb already owns;
  it is the shape to reach for if anyone wants the gate's `Asleep` row to stop
  being a session field. **Named as a followup** here and in the
  retrospective's Followups list.

**Registry.** `PLAY-imposed-controller-diverges-felt-state` keeps status
`elaborated`; its Idea cell is narrowed so "ledger-inert" reads as the defect
The Minute repaired rather than a property of possession, while the
visible-in-testimony half survives verbatim (584 chars, under the 600 cap, no
waiver added). All three rows' Where cells gain the chronicle link; the two new
rows stay `raw`.

