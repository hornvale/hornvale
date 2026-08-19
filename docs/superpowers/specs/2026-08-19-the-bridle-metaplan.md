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

### 3.2 In-Character and Out-of-Character are first-class, and the
distinction is the gate

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

## 4. The arcs

```
  ARC I - THE DEED          one suite, one execution path, IC/OOC
                            first-class, IC acts charge time and
                            write facts
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
| **I — The Deed** | one action type; IC/OOC on the request; the gate table with `asleep`; the player's in-character verbs routed through it; `Drive::affordance` renamed (6 impls, 22 sites) | the player's walk leaves an `agent-at` trail indistinguishable from a creature's; the player sleeps and IC refuses while OOC works |
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
- **Persisting a session.** Nothing written during a possession is written
  back; the session ledger is a clone, and creatures are re-derived every
  session. "The player becomes an inhabitant" is bounded to the session.
  That is the lazy-derivation model working as designed, not a gap.
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
4. **The time-charging model is undefined.** How much does an act cost?
   Creature acts charge uniformly in *plan* space (every edge costs 1) but
   the world-time charge is separate. Arc I must state the rule
   explicitly rather than inherit one by accident; a wrong uniform charge
   is the kind of thing that reads as correct and silently distorts every
   downstream fold (hunger, fatigue, thirst all integrate over days).
5. **GOAP's planner is in Dijkstra mode** (`heuristic() == 0`) with a
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

## 8. Flagged for G3 — owner decisions, not autopilot's

1. **Player acts now write to the ledger and charge time** (ledger #1,
   Nathan's call at the first gate). Not a save-format change — the
   session ledger is never written back — but it is the largest behavioural
   commitment here, and it moves committed artifacts.
2. **`AGENT_AT` is registered per-session, not at genesis**
   (`session.rs:655`), so giving the player a trail costs no genesis or
   save-format change. Confirmed by reading, and worth Nathan confirming
   the reading.
3. **The `Drive::affordance` rename** is a public-API change inside
   `windows/vessel`. Low risk, wide diff, and it makes the type-audit
   report drift.
4. **Risk 4 (the time-charging rule)** is a fidelity question, and
   fidelity questions are Nathan's by standing policy.

## 9. Definition of done (per campaign; decisions 0013 and 0020)

Each arc: chronicle entry in `book/src/chronicle/`; a freshness sweep of
stale chapters, re-scoring any Confidence Gradient bet it moves; a
retrospective in `docs/retrospectives/`; idea-registry rows updated
(`MAP-27`, `RENDER-possession-still-mints`, `PLAY-death-is-traversal`,
`PLAY-consent-siblings`); regenerated artifacts committed in the same
commit as the change that drifted them.

## 10. Provenance

Brainstormed 2026-08-19 with Nathan, under `campaign-autopilot`. Two
`ideonomy-plain` passes: substitution + organon-construction (tree) on
intentionality / rate / symmetry, which produced the origin-of-intent
taxonomy and the symmetry test in §2; and abstraction-lift + combination
(map) on longevity / modularity / size, which produced §3.3, §3.4, §3.5,
§3.7, and the `affordance` resolution. Neither pass overturned the
framing; both enriched it. Full ledger:
`.superpowers/sdd/decision-ledger.md` on `campaign/the-bridle`.

Reads: decisions 0007, 0098, 0116; idea-registry rows `MAP-19`, `MAP-27`,
`UNI-20`, `UNI-21`, `RENDER-possession-is-a-grid`,
`RENDER-possession-still-mints`, `PLAY-death-is-traversal`,
`PLAY-consent-siblings`, `PLAY-inheritance-blocker`;
`docs/design/room-scale/cycle-03-the-living-layer.md`.
