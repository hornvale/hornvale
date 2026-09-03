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

## Followups (promoted to the retrospective at close)

- The driven walk's within-room `Occupancy` is built and dropped inside
  `step_one_with_controller`; the population's is not. Pre-existing; not
  a committed fact (0069).
- `controller.rs`'s `PlayerController` doc says "today's verb loop — `go`,
  `drink`, … — still commits directly"; there is no `drink` verb
  (`IN_CHARACTER_VERBS`, `session.rs:185`). Corrected in this campaign's
  freshness sweep; the gap itself is `PLAY-free-body-cannot-drink`.
- A free seed-42 body reads `Pursuing(Thirst)` from its second wait and
  has no way to drink. Same row.
