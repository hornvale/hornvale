# The Bridle — the program for one action system

*Metaplan. Five arcs, three of them this program (I–III); arcs IV–V are
named here and specced later. Status: all unstarted. Arc I is named
**The Deed**.*

## 1. What occasioned it

Hornvale has two action systems that share nothing.

Creatures act through a five-variant `Action` enum
(`windows/vessel/src/liveness.rs:5575`) — `MoveTo`, `MoveWithin`, `Drink`,
`Rest`, `Eat` — planned by a kernel A\* over a GOAP search space, each act
committing a fact and charging time. The player acts through roughly thirty
string literals matched in `Session::handle`
(`windows/vessel/src/session.rs:1259–1367`). The two never meet:
`session.rs` does not mention the `Action` type anywhere, and there is no
player verb for `drink`, `eat`, `rest`, or `sleep` — three of the five
things a creature can do, the player cannot.

The asymmetry is not merely lexical. It is about **what the world
remembers**:

```
                 creature                     player
  movement    MoveTo -> commits agent-at   go -> mutates session state,
                                                 commits nothing
  effects     Drink/Rest/Eat each commit   no such verb exists
  time        each act charges time        only `wait` charges time
  exception   --                           provoke/soothe DO commit,
                                           provenance "player: provoke"
```

A grep for facts carrying the possessed agent as **subject** returns
nothing. The player has an entity (`Session::agent_entity`,
`session.rs:2846`) and appears in the ledger only as the *object* of other
creatures' facts (`turned-hostile -> player`). Stated plainly:

> **The world can record what is done to you. It cannot record what you
> do.**

Those two committing verbs are the proof the boundary was never designed.
Nobody decided the player should be outside the record; the vessel began as
a read-only verb loop, creature drives arrived across four later campaigns,
and `provoke`/`soothe` punched through because one campaign needed
disposition. There is no considered choice here to overturn — only one that
was never made.

**Two corrections to the record, both made during this brainstorm.** First,
this program was initially framed against a "frozen day"; that is false.
`Session::wait` advances `self.day` and ticks the whole creature layer
forward. What is true is narrower and worse: *only* `wait` charges time, so
the player's acts are the only acts in the world that do not pay for
themselves. Second, a grep for out-of-character precedent returned nothing,
and that was right about the word and wrong about the thing — see §3.5.

## 2. Keystone

The motivating case is mind control: an aboleth or mind flayer driving
another creature's body. It is not a feature request. It is the sharpest
available test of whether the action system is coherent, because it forces
one invariant:

> When a dominated fisherman walks to the river and drinks, the ledger must
> record exactly what it would have recorded had the fisherman gone
> himself. Nothing in the trace may reveal that a different mind chose.

From which the whole program follows. If the record must not vary with who
chose, then **the effect of an act belongs to the body performing it, never
to the driver commanding it** — and "who is driving" becomes a parameter of
one mechanism rather than a fork between two.

This also generalises decision
[0116](../../decisions/0116-possession-is-a-parameter-not-a-fixture.md).
That record crosses *focalized* (whose senses filter) against *commanded*
(whose body acts) and leaves `commanded = an EXISTING creature` empty on
purpose. But its grid assumes the **player** is the one commanding. An
aboleth commanding a fisherman is a creature commanding a creature, which
the grid has no axis for. The program's structural claim, which neither
0116 nor [0098](../../decisions/0098-hornvale-is-single-player.md) states:

```
  commanded  (whose body acts)      FULLY SYMMETRIC
                                    any creature may command any creature,
                                    the player's body included

  focalized  (whose senses filter)  SINGULAR, by constitution
                                    exactly one observer, forever (0098)
```

0098 makes single-player constitutional but never says which of 0116's two
parameters the singularity binds. It binds `focalized` only. `commanded`
was never required to be singular — it simply never had a second user. That
is what licenses mind control without touching 0098.

## 3. Core commitments

These bind every arc. Each is testable.

### 3.1 The effect sink belongs to the body

An act's facts and time cost are determined by the body performing it and
by nothing else. A driver selects *which* act; it never alters *what the
act does*. This is the keystone restated as an implementation rule.

### 3.2 In-Character and Out-of-Character are first-class, and the gate is the distinction

Every action is invoked either **in character** — subject to the body's
state — or **out of character**, which bypasses the body. Instruments
(`look`, `map`, `why`) are **not** a third category; they are a subset of
OOC. Neither category consumes time by default, but an IC act almost
always should, and an OOC act may (`!wait` moves the clock).

### 3.3 The mood lives on the request, not on the action

`examine` and `!examine` are the *same act* invoked with different
authority. Nothing about verb, target, effect, or cost differs — only
whether the gate is consulted. So there is **one** action suite, and each
*invocation* carries its mood.

Rejected: two parallel enums (duplicates the suite, which is the defect
this program exists to remove); a session-wide debug *mode* (wrong
lifetime — scripted fixtures need to mix IC and OOC turn by turn); a flag
baked into the action definition (would make IC-ness a property of the
verb, so `examine` could not be both).

### 3.4 OOC bypasses the body, not the world

**An OOC act may bypass the body's state. It may never bypass the world's
rules.** It cannot commit a contradictory fact, break determinism, or
produce anything the concept registry refuses.

The comparator is Rust's `unsafe`, whose three properties all lift down
cleanly: it enables *specific enumerated* extra powers and leaves every
other invariant standing; it is lexically greppable (→ 3.6); and it is
contagious upward (→ a trace that used OOC cannot be claimed as a valid
history without saying so).

### 3.5 An OOC act that cannot be honoured refuses with a reason

Inherited, not invented. **A worldgen pin is already an OOC action** —
`--plates 7`, `--ocean-fraction 0.6` are the operator imposing a state the
simulation's own process did not choose. Decision
[0007](../../decisions/0007-seed-is-identity.md) already governs that
class: pins fail loudly with the physical reason and generation never
retries. An OOC act obeys the same rule; it never silently no-ops.

### 3.6 Every act stamps its provenance

`Fact.provenance` already carries `"player: provoke"`, so the slot exists.
IC and OOC acts stamp distinguishably, so any scripted trace can be audited
for whether an OOC verb put the world in a state the sim's own rules would
never produce.

### 3.7 The gate is a table, not a check

The body-state gate is built as a table from the first arc, even when it
has one row, because crossing the states against the two moods shows every
row is the same row:

```
                     IC        OOC
  asleep             refused   permitted
  unconscious        refused   permitted
  dominated          refused   permitted
  dead               refused   permitted
  blind            (partial)   permitted
  target invisible   refused   permitted
```

Sleep is not a special case to be generalised later; it is the first row of
a table whose shape is already known. Building the table with one row costs
almost nothing and stops Arc III re-plumbing it.

**Consequence, free:** spectator-after-death. Death stops being a terminus
and becomes the state where IC is gated off permanently and OOC persists —
which is the precondition for `PLAY-death-is-traversal` (*"death is
transport, not terminus — the guard that killed you is a body you may
take"*, elaborated / high).

### 3.8 The daybook is the book of original entry

**Every accepted command is recorded — IC or OOC, performed or refused.**
A typo is not a command and is not recorded; the filter is *acceptance*,
not *effect*.

That cannot all be facts. Decision
[0100](../../decisions/0100-fact-phenomenon-myth.md)'s test is *"could I
recompute this from the seed alone?"*, stated for use as **commit
consequence, derive presence** — and an OOC `!examine` has no consequence.
Committing one would put operator activity into the contradiction-checked
register. So the record is **two books**, and the structure is
double-entry bookkeeping's, unchanged since the 1300s:

```
  the DAYBOOK  chronological; every accepted command in order; the
               "book of original entry" -- if the two disagree, it wins
  the LEDGER   the resulting world-state, posted from it; facts only
```

Hornvale has had the ledger since genesis and never had the daybook. Its
required slots:

```
  turn  day        mood  command               verdict          cost     by
  ----  ---------  ----  --------------------  ---------------  -------  ---------
  0017  412.34000  IC    go north              performed        10,000t  self
  0019  412.44150  OOC   !examine poltergeist  performed             0t  operator
  0020  412.44150  IC    examine poltergeist   refused: unseen       0t  self
  0021  412.44150  IC    go south              refused: held         0t  self
  0022  412.44150  IC    go west               performed        10,000t  aboleth#4
```

**The last column is the keystone made mechanical.** The daybook records
*who commanded*; the ledger does not. Line 0022 posts `agent-at <you>`
with no trace of the aboleth, which is §2's invariant arriving as a
required field rather than an aspiration. Line 0021 is what makes
domination legible at all: the record of what you tried and were denied.
Refused lines cost zero ticks and post no fact, yet appear — which is
exactly what "accepted, not typos" means.

**A world is a seed. A playthrough is a seed plus a daybook.** This
extends decision [0007](../../decisions/0007-seed-is-identity.md) rather
than contradicting it: the daybook is the *only* non-derivable object in
the system. The world, the creatures and the outcomes all follow from
the seed; the human's choices follow from nothing else.

**Consequence — the play-ledger is a cache, and the spec must say so.**
If the daybook is stored, the fact trail is recomputable from
`(seed, daybook)`, and 0100 rule 5 forbids committing a balance.
Materialising it is still right, because replaying a long session is
expensive — but it is a *checkpoint*, not the authority, exactly as a PGN
move list is authoritative and an embedded FEN is a seek optimisation.
Say it out loud in the code, or someone will eventually hand-edit the
ledger and expect it to stick.

**Stored beside the world, never inside it.** Both source domains keep two
books rather than one composite; it avoids a `World` schema change; and
`possess --script` already exists as an input format whose replays are
byte-golden. **The daybook is a script you can replay** — which makes it a
debugging and fixture instrument at no extra cost, and is the independent
payoff §4 claims for the consolidated suite.

*Naming:* "journal" is the accounting term of art, but The Journal is
already a merged campaign in this repo. **Daybook** is the same term of
art, unused anywhere in the tree, and puns usefully on `WorldTime { day }`.

**Undo, redo and retry fall out — and they are what the checkpoint is
FOR.** With a replayable daybook over a deterministic world, undo is
*truncate at turn N and replay*, redo is *re-append the tail*, and retry is
*truncate, then append something else*. Nothing new is needed.

This corrects the justification given above for materialising the ledger.
It is not merely a load-time saving: replay cost is O(distance to the
nearest checkpoint), so **checkpoints should be periodic rather than
terminal**, and undo latency is the property that sets their spacing. This
is the standard shape — write-ahead log plus checkpoints, event sourcing
plus snapshots, rollback netcode — arrived at from the game side.

**It does not reopen save-scumming, and the registry already says why.**
`PLAY-determinism-is-anti-scum`: in a world that is a pure function of
`(seed, pins)` there is no random table to roll against, so scumming
"reduces to *playing differently*, which is simply playing".
`PLAY-no-reroll` puts the failure mode strictly upstream of the first
decision — *"a start that varies breeds save-scumming"* — and undo of one's
own commands is downstream by construction. Both rows endorse this rather
than constrain it.

**One future tension, recorded now while it is visible.**
`PLAY-eviction-costs-depth` closes the death-farming exploit "against the
conserved quantity rather than with a new rule": a voluntary step *spends*
depth, an eviction *loses* it. Free undo would let a player rewind an
eviction and recover the lost depth, reopening exactly that exploit.
Neither depth nor undo exists yet, so nothing is decided here — but the
campaign that builds either must read this paragraph, because by then the
connection will not be obvious.

## 4. The arcs

```
  ARC I.a - THE TACKLE      extract the action layer, rename
                            affordance, give the body a mass
                            (BYTE-IDENTICAL: nothing moves)
                                    |
  ARC I.b - THE DEED        one suite, one execution path, IC/OOC
                            first-class, the daybook, IC acts charge
                            time and post facts
                                    |
        +---------------------------+---------------------+
        |                                                 |
  ARC II - THE HAND                              ARC IV - THE OFFER
  controller stack as a                          objects advertise
  parameter on every body                        their verbs
        |                                                 |
  ARC III - THE COERCION                         ARC V - THE CHEMISTRY
  a controller imposed by                        verbs authored as data
  another creature
```

| Arc | Ships | Acceptance test |
|---|---|---|
| **I.a — The Tackle** | extract the action layer out of `liveness.rs`; rename `Drive::affordance` → `proposal` (6 impls, 22 sites); give the possessed body a `mass_kg` | **byte-identical.** Not one committed artifact moves. Any drift is a bug, caught immediately |
| **I.b — The Deed** | one action type; IC/OOC on the request; the gate table with `asleep`; the daybook; player verbs routed through, charging time and posting facts | the player's walk leaves an `agent-at` trail indistinguishable from a creature's; the player sleeps and IC refuses while OOC works |
| **II — The Hand** | the controller stack; GOAP demoted to "the default controller" | swap controllers — a creature on player-input and a player body on GOAP both produce well-formed traces |
| **III — The Coercion** | an imposed controller; `dominated` joins the gate table | **an aboleth dominates the player**: IC refuses, OOC still works, and the ledger cannot tell |
| **IV — The Offer** | objects advertise verbs (MAP-19 + MAP-27) | a key says "unlock me with this"; no verb×object table exists anywhere |
| **V — The Chemistry** | verbs authored as data | a new verb ships with no Rust change |

Sequencing was decided deliberately: **act first, affordances later**.
MAP-27 stages it this way itself — The Actants shipped what that row calls
its "front porch" (the registry reconciliation, the destructure tripwire)
and left the DSL as followup. Designing a verb data format with no live
consumer is how you get a format that does not fit.

### Why Arc I needs one body-condition

Without a condition, IC and OOC differ only in name. The cheapest real one
already exists: creatures sleep (`Action::Rest`, `next_awake_day`, species
activity cycles) and the player cannot. So Arc I's sharp test —
*the player sleeps; IC verbs refuse; OOC verbs work* — exercises the whole
axis on machinery that is already built and already tested.

### Why OOC is not a tooling afterthought

If a mind flayer takes your body, IC commands refuse; that is the point.
But you are still there, watching. **OOC is what lets you observe your own
domination.** Without it, being dominated is indistinguishable from the
game having hung. The same holds for sleep, unconsciousness and death. OOC
is the interface for every state in which your body stops obeying you,
which is why it belongs in Arc I rather than being deferred to a debug
sub-project.

### The independent argument for one consolidated suite

The committed test material is already script-generated: the gallery
transcripts come from `possess --script`, and the client fixtures are
captured sessions. Today a fixture that wants to test a river must *walk to
one*. With OOC verbs in the same suite it can be *placed* at one
deterministically — cheaper fixtures, more targeted, less brittle. This is
a measurable payoff rather than an aesthetic preference, and it is why the
consolidated suite is worth the discipline §3.4 imposes on it.

## 5. Deliberately NOT in this program

- **Command parsing and the `!` sigil.** The IC/OOC *mechanism* is in
  scope; the surface syntax that selects it is not. `!examine` is used
  throughout this document as a readable notation, not a committed
  grammar.
- **A full body-condition system.** Arc I ships the table with `asleep`;
  Arc III adds `dominated`. `unconscious`, `blind`, `invisible` are named
  in §3.7 to fix the table's shape, not scheduled.
- **Durations and interruption.** `Action::Rest` is already a multi-day act
  modelled as one tick that jumps `st.day` — a duration wearing an
  instant's clothes. No act can currently be interrupted. Mind control will
  eventually force this (what happens to an act in flight when a body is
  seized?), but it is named as a risk, not a deliverable.
- **Redesigning the played-world save path.** `possess --out` already
  saves a played world (§6.5); this program adds facts that travel through
  it but does not change how it works. If those facts turn out to need
  their own epoch or schema treatment, that is its own campaign.
- **Aboleths and mind flayers as content.** Arc III ships the *mechanism*
  of an imposed controller. Which creatures possess which, and the biology
  behind it, is a species-domain question for a later campaign.

## 6. Risks

1. **Byte-identity of committed artifacts.** Charging time for player acts
   moves three gallery transcripts (`possession-seed-42.md`,
   `possession-live.md`, `possession-over-time-seed-42.md`) and three
   client fixtures. All are declared in `docs/generated-paths.txt` and
   regenerable via `make rebaseline`. *This is expected drift, not
   breakage* — but it must land in the same commit as the change, and a
   reviewer must read the diff rather than accept it.
2. **`liveness.rs` is 14,768 lines; `session.rs` is 5,776.** The action
   system lives in the first and the player's verbs in the second. Arc I
   cannot be done well without extracting the action/execution layer into
   its own module. That extraction is in scope for Arc I and should be its
   own reviewable task.
3. **The `affordance` rename touches six drives and 22 call sites.**
   Thirst, Thermal, Fatigue, Hunger, Danger, Social. Mechanical and
   behaviour-free, but a wide diff that will sit alongside a semantic
   one, and it drifts the type-audit report. Land it as its own commit.
   *Counted carefully:* `grep 'impl Drive for'` finds only three, because
   three drives are written `impl<'a> Drive for Thermal<'a>` and the
   pattern requires a space after `impl`. Count `fn affordance` instead
   (7 = 1 trait declaration + 6 impls).
4. **The time-charging model already exists — do not rebuild it.** An
   earlier draft of this metaplan called it undefined. False.
   `windows/vessel/src/clock.rs` carries a complete cost model:
   `base_ticks` per action (`MoveTo` 10,000 ticks = 0.1 day, `MoveWithin`
   1,000, `Eat` 3,000, `Drink`/`Rest` 150), scaled by `tempo(mass_kg)` and
   by `climb_factor` (uphill only), converted by `days_of` against a
   rotation-derived tick rate that makes a local day an exact integer
   number of ticks. Its match on `Action` is exhaustive by variant on
   purpose, so *a new action fails to compile rather than silently
   becoming free*.

   Decisively, its signature is `cost_ticks(action, mass_kg,
   terrain_factor)` — **there is no driver parameter.** The cost of an act
   is already a function of the act, the body, and the ground. It was
   built keyed on the body exactly as §3.1 requires and simply never had a
   player routed through it.

   The real gap is small and mechanical: `mass_kg` lives on the NPC struct
   (`liveness.rs:94`, sourced from the biosphere by species) and the
   possessed agent has no such field. It has a species, so it derives the
   same way. This is Arc I.a work, not a fidelity question.
5. **The played world outlives the session — a correction.** An earlier
   draft of this metaplan stated that nothing written during a possession
   is written back, on the strength of `Session::ledger`'s own doc comment
   (*"a clone of the frozen world's ledger ... Never written back"*). That
   comment describes the INPUT `--world` file, which is indeed read-only,
   and it is misleading about everything else. `Session::into_played_world`
   moves `self.ledger` wholesale into a fresh `World`, and `possess --out`
   saves it, printing `played world written to {out} ({n} facts)` (The
   First Mark, Task 4 — *"the played world outlives the session"*).

   So when player in-character acts commit facts, **those facts reach saved
   world files.** Consequences the arcs must carry:

   - Arc I must decide whether a player's `agent-at` / `drank` / `rested`
     trail is *wanted* in a saved played world, or must be filtered at the
     `into_played_world` boundary. Both are defensible; neither is free.
   - `AGENT_AT` is registered per-session with a fixed doc string
     (`session.rs:655`). `Registry::register_predicate` is idempotent for
     an IDENTICAL definition and errors `ConflictingDefinition` otherwise
     (verified in `kernel/src/registry.rs`), so re-possessing a saved
     played world works today — but **the doc string of every
     per-session-registered predicate is a de-facto save-format contract
     the moment a played world is saved.** Changing one breaks reload of
     every world saved before the change. This is true now, undocumented
     now, and this program adds predicates to that set.

6. **GOAP's planner is in Dijkstra mode** (`heuristic() == 0`) with a
   1,000-node budget. Routing more actors through it raises the chance of
   budget exhaustion, whose failure mode is a *frozen creature*, not an
   error — this already bit once, when a remembered-danger penalty of 20
   froze roughly 900 seed-42 fauna and had to be cut to 5. Arc II should
   measure before it multiplies the planner's callers.

## 7. Decisions to record

- **The effect of an act belongs to the body, not the driver.** (Arc I;
  the keystone.)
- **In-Character / Out-of-Character is a property of the request, and OOC
  bypasses the body but never the world.** (Arc I; no precedent exists —
  this is new constitutional ground.)
- **`commanded` is symmetric; `focalized` is singular.** (Arc III; amends
  the reading of 0116 and clarifies the scope of 0098.)
- **A world is a seed; a playthrough is a seed plus a daybook.** (Arc I.b;
  extends 0007. Carries the two-book split, the "book of original entry"
  precedence rule, and the statement that the play-ledger is a cache.)
- **Every accepted command is recorded; the daybook names the commander
  and the ledger does not.** (Arc I.b, enforced in Arc III — this is what
  makes domination invisible in the world's record and visible in the
  operator's.)

## 8. Flagged for G3 — owner decisions, not autopilot's

1. **Player acts now write to the ledger and charge time, and those facts
   REACH SAVED WORLD FILES** (ledger #1, Nathan's call at the first gate;
   corrected at §6.5 below). This is save-format-adjacent after all — see
   the correction — and it is the largest commitment in the program.
2. **`AGENT_AT` is registered per-session, not at genesis**
   (`session.rs:655`), so giving the player a trail costs no genesis or
   save-format change. Confirmed by reading, and worth Nathan confirming
   the reading.
3. **The `Drive::affordance` rename** is a public-API change inside
   `windows/vessel`. Low risk, wide diff, and it makes the type-audit
   report drift.
4. **WITHDRAWN — the time-charging rule.** An earlier draft flagged this
   as a fidelity question for Nathan. It is not a question at all: the
   cost model exists, is body-keyed, and is driver-agnostic (risk 4). The
   only work is giving the possessed body a mass.
5. **Every accepted command is recorded** (Nathan's ruling, this
   brainstorm) — IC or OOC, performed or refused; typos are not commands.
   This introduces the **daybook** (§3.8), a second book beside the
   ledger, and with it the claim that **a playthrough is a seed plus a
   daybook** — an extension of decision 0007 that deserves its own
   record.
6. **The play-ledger becomes a cache** (§3.8). Once the daybook is
   authoritative, the materialised fact trail is a checkpoint rather than
   the source of truth, which is a meaningful change in what the ledger
   *means* even though no byte of its format moves.

## 9. Definition of done (per campaign; decisions 0013 and 0020)

Each arc: chronicle entry in `book/src/chronicle/`; a freshness sweep of
stale chapters, re-scoring any Confidence Gradient bet it moves; a
retrospective in `docs/retrospectives/`; idea-registry rows updated
(`MAP-27`, `RENDER-possession-still-mints`, `PLAY-death-is-traversal`,
`PLAY-consent-siblings`); regenerated artifacts committed in the same
commit as the change that drifted them.

## 10. Provenance

Brainstormed 2026-08-19 with Nathan, under `campaign-autopilot`. Three
`ideonomy-plain` passes: substitution + organon-construction (tree) on
intentionality / rate / symmetry, which produced the origin-of-intent
taxonomy and the symmetry test in §2; abstraction-lift + combination (map)
on longevity / modularity / size, which produced §3.3, §3.4, §3.5, §3.7
and the `affordance` resolution; and cross-domain re-instantiation
(notation) on predictability / age, which produced §3.8 entire — the
double-entry and PGN re-instantiations, the `by` column that makes the
keystone mechanical, and the play-ledger-is-a-cache consequence. No pass
overturned the framing; all three enriched it. Full ledger:
`.superpowers/sdd/decision-ledger.md` on `campaign/the-bridle`.

Reads: decisions 0007, 0098, 0116; idea-registry rows `MAP-19`, `MAP-27`,
`UNI-20`, `UNI-21`, `RENDER-possession-is-a-grid`,
`RENDER-possession-still-mints`, `PLAY-death-is-traversal`,
`PLAY-consent-siblings`, `PLAY-inheritance-blocker`;
`docs/design/room-scale/cycle-03-the-living-layer.md`.
