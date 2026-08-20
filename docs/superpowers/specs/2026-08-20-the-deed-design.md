# The Deed — one action suite, in character and out

*Arc I.b of The Bridle. Ships the unified action suite, the `!` namespace,
the body-state gate, and in-character acts that charge time and post facts.
The daybook, which the metaplan assigned to this arc, is carved out to Arc
I.c (**The Waybill**) — see §2.5.*

**Program spec:** `2026-08-19-the-bridle-metaplan.md`. That document holds
the motivation, the keystone, and the arc sequence; this one does not
restate them. Where the two disagree, §2 below says so explicitly and this
spec governs for Arc I.b.

---

## 1. Scope

**Ships.** One `Action` type covering both creature and player acts. `!` as
an out-of-character namespace. The body-state gate as a table with one row
(`asleep`). Player in-character acts routed through the existing cost model
so they charge time, and through the ledger so they post facts.

**Acceptance test** (unchanged from the metaplan's arc table):

> The player's walk leaves an `agent-at` trail indistinguishable from a
> creature's; the player sleeps, in-character verbs refuse, out-of-character
> verbs work.

**Does not ship.** The daybook (Arc I.c). The controller stack (Arc II).
Imposed control (Arc III). Object-advertised verbs (Arc IV). Authored verbs
as data (Arc V). A hint path for bare instrument forms (registry row,
§9).

---

## 2. Corrections to the metaplan

The metaplan was written before its own code was read closely. Five of its
claims moved during this brainstorm. They are recorded here rather than
edited into the program document, so the reasoning stays legible.

### 2.1 `!` is a namespace, not a mood — §3.3 is REVERSED

The metaplan held that *"the mood lives on the request, not on the action"*:
`examine` and `!examine` were to be the same act invoked with different
authority, *"Nothing about verb, target, effect, or cost differs — only
whether the gate is consulted."*

**That is not what the out-of-character forms are for.** `!examine` may show
information the in-character form cannot: entity IDs, invisible entities,
objects the body has no way to perceive. `!map` renders the objective map
where `map` renders the subjective, player-known one. These are **different
acts with similar syntax**, and the exclamation point is a namespace.

So in-character-ness is a property of the **action**, which the metaplan
§3.3 explicitly rejected on the grounds that it *"would make IC-ness a
property of the verb, so `examine` could not be both."* The objection
dissolves because `examine` is not meant to be both. There remains **one**
suite — the rejection of two parallel enums stands — but each action in it
knows its own mood.

**What this simplifies.** The gate stops being a cross-product of acts
against moods and becomes a predicate on the action itself. The metaplan's
§3.7 table survives unchanged in content: for every body state, in-character
refuses and out-of-character permits.

### 2.2 An out-of-character act CAN commit facts

The metaplan reasoned from `!examine` — *"an OOC `!examine` has no
consequence. Committing one would put operator activity into the
contradiction-checked register"* — and generalised from the one
out-of-character example that happens to be inert.

`provoke` and `soothe` are operator instruments (§3.2), and they commit
`disposition-shift` today, carrying `provenance: "player: provoke"`.

### 2.3 `provoke`/`soothe` are not an accident — they are §3.5's precedent

The metaplan called them *"the proof the boundary was never designed"*, a
pair that *"punched through because one campaign needed disposition."*

They are better read as the metaplan's own §3.5 arriving early. That section
establishes that **a worldgen pin is already an out-of-character action** —
`--plates 7` is the operator imposing a state the simulation's own process
did not choose. `provoke` is the operator imposing a disposition the
simulation's own process did not choose. Same category, at play-time instead
of genesis-time.

This makes §3.6's provenance stamping load-bearing rather than hygienic: it
is the only thing that can distinguish operator-imposed world-state from
simulation-produced world-state inside a saved world.

### 2.4 §6.5 is resolved: player facts are NOT filtered

The metaplan left open whether a player's `agent-at` / `drank` / `rested`
trail is wanted in a saved played world or must be filtered at the
`into_played_world` boundary, calling both *"defensible"*.

The keystone decides it. §2 of the metaplan requires that *"nothing in the
trace may reveal that a different mind chose."* **Filtering is such a
trace** — a body that walked would return from disk having been nowhere.
Worse, it contradicts facts already in the same ledger: other creatures
commit `turned-hostile -> player`, so a filtered save would hold a creature
who was acted upon but never acted. That is the metaplan §1's original
complaint promoted from a runtime asymmetry into a save-format one.

**Decision: no filter.** In-character player acts persist exactly as a
creature's do. Out-of-character acts persist stamped, which makes a saved
world *auditable* for operator intervention rather than merely contaminated
by it.

**Accepted cost, stated plainly:** a long possession commits many facts and
saved worlds grow. Creatures already pay this.

### 2.5 The daybook is carved out to Arc I.c (The Waybill)

The metaplan lists the daybook in Arc I.b's "Ships" column, but it appears
nowhere in that arc's acceptance test — and it carries its own CLI surface,
its own render path, its own concept, and a checkpoint-spacing question tied
to undo latency.

The carve follows the one Nathan already made when Arc I split into I.a and
I.b: keep the plumbing away from the semantics.

**The distinction that made the carve clean** is that a minimal input log
and a readable transcript are two artifacts, not one — a point Nathan made
during this brainstorm and which the genre settled long ago. The Z-machine
separates them at spec level: output stream 2 is the *transcript*
(commands echoed with responses, for reading), stream 4 is the *command
record* (input only, for replay). Hornvale already has three of the four
corners:

| corner | mechanism | state |
|---|---|---|
| transcript (out) | `possess --script in > out` | exists — the gallery pages *are* transcripts |
| command replay (in) | `possess --script <PATH>` | exists, byte-golden |
| played world (out) | `possess --out <PATH>` | exists — the checkpoint, a cache |
| **daybook (record)** | — | **The Waybill's delta** |

*(The Z-machine reading is design provenance, from knowledge rather than
measurement; nothing in this repo depends on it being exactly right.)*

---

## 3. Design

### 3.1 One suite, each action carrying its own mood

`hornvale_vessel::action::Action` grows from the five creature variants to
cover the player's acts as well. Each variant is in-character or
out-of-character by construction. The compile-time roster tripwire
(`action_variants_must_all_be_rostered`, a no-wildcard match) is preserved
and extended: a new variant must fail to compile until it is rostered,
named, and classified.

### 3.2 The verb classification

The 26 session verbs, plus `possess` (not yet a verb; named here so the
group is complete):

| group | verbs | shape |
|---|---|---|
| **A. Operator instruments** | `why` `npcs` `help` `eyes` `whoami` `provoke` `soothe` | out-of-character only; no in-character counterpart is meaningful |
| **B. Both forms** | `map` `examine` `look` `needs` `knows` `wait` | bare = in-character (subjective, gated); `!` = out-of-character (objective, ungated) |
| **C. In-character world-acts** | `go` `back` `enter` `out` `dive` `surface` `delve` `climb` `write` `consult` | gated by body state; charge time; post facts |
| **D. Session control** | `release` `quit` `exit` `possess` | not acts; outside the suite |

*(26 = 7 + 6 + 10 + 3; `possess` is the non-verb. An earlier count of 25
missed `quit`, which sits mid-line in `"release" | "quit"` and escapes a
line-anchored extraction — the count is derived from every quoted token in
the dispatch, not from a pattern that assumes one verb per line.)*

**Group A's bare forms are retired**, all seven: `!why`, `!npcs`, `!help`,
`!eyes`, `!whoami`, **`!provoke`**, **`!soothe`**. The last two are named
explicitly because they are the surprising ones — they commit facts, and
elsewhere this document discusses them bare when describing what they do
*today*. The target spelling is sigilled, like the rest of group A.
Consistency is the point: an operator instrument that does not look like one is how
`Session::needs` came to be a side channel (§3.3). The hint path for a bare
form typed out of habit is deferred to a registry row (§9) — until it
exists, a bare group-A verb is an ordinary unknown-verb refusal.

**Group C notes, from reading rather than from the names.** `write` mutates
`self.knowledge` — it absorbs a line of Common, so it is a *learning* act,
not a note-taking one. `consult` reads The Reckoning. Both are in-world
literacy acts and belong in C, not among the instruments.

### 3.3 The gate

Built as a table from the first arc, per metaplan §3.7, with one row
(`asleep`). Its justification is **empirical, not anticipatory**, which is a
stronger claim than the metaplan made for it.

`Session::needs` already carries a perception gate, and its own comment
records why:

> *"GATED ON SIGHT... Ungated this verb was a side channel straight around
> the structural redaction `snapshot` had just performed: it named — by
> label AND by felt state — a creature the pane had withheld one verb
> earlier."*

So the project has already shipped a per-verb gate, already discovered a
verb that walked around it, and already patched that verb individually (The
Sighting, fix round 2). A table is what makes the next omission a compile
error instead of a bug report.

**And the sharper case is still OPEN.** `Session::chart`'s doc comment
records that `purview_scene` marks every derived NPC **ungated**, so reaching
it from inside a chamber would disclose exactly the creature the chamber band
withheld — *"straight past four gated verbs"* (The Sighting, fix round 5).
Nothing does today, and the comment is careful to say that was verified
rather than assumed, enumerating the four dispatch arms that prevent it. Then
it names the hazard exactly:

> *"every one of those is a fact about **dispatch**, and this method is
> `pub`: a caller that has not read `handle` can reach it from inside a
> chamber with nothing to stop them."*

A guarantee distributed across four dispatch arms and held by hand-verified
accident is the thing a table replaces. This arc does not close that hazard
— `purview_scene` is not in its scope — but the gate it builds is where the
closure would live.

**The table consolidates two axes the metaplan treated as one**: body state
(`asleep`, later `unconscious`/`dominated`/`dead`) and perception (`blind`,
`target invisible`). Both resolve identically — in-character refuses,
out-of-character permits — which is why one table holds them.

### 3.4 Time and facts

**No new cost model.** `clock::cost_ticks(action, mass_kg, terrain_factor)`
already exists, is exhaustive by `Action` variant, and takes **no driver
parameter** — it was built keyed on the body exactly as the keystone
requires and simply never had a player routed through it. The possessed
body's mass comes from `clock::mass_for_species`, which The Tackle extracted
for this purpose.

An in-character act therefore: consults the gate, charges
`cost_ticks` against the body's own mass, and commits its fact with
in-character provenance. An out-of-character act skips the gate, charges
nothing by default (`!wait` is the exception that moves the clock), and, if
it commits at all, commits with operator provenance.

**`!wait` is what makes out-of-character a capability rather than a debug
aid.** The metaplan argues that out-of-character is *"the interface for
every state in which your body stops obeying you"* — but observing a state
you cannot act in requires a clock you can still advance. Without `!wait`,
being asleep or dominated is indistinguishable from the game having hung.

### 3.5 Concepts and the lexicon

Every action keeps a **required** concept name. The alternative considered
and rejected was `concept_name() -> Option<&str>` for instruments with no
in-world referent: it would let the reconciliation in `cli/src/concepts.rs`
silently shrink its own denominator, which is the failure mode where an
allow-list gate cannot see its list go short.

Two consequences follow, and both have existing machinery:

**Accession.** New concepts are **appended as a new `EPOCH_COHORTS`
cohort**, never inserted into an existing one. Proto-root assignment is a
global ordered walk with rejection-probing, so a mid-alphabet insertion
moves words derived from later concepts. This is enforced, not merely
documented: `cli/tests/suite/accession.rs` checks cohort/registry parity in
**both** directions and carries an anti-vacuity floor, so a forgotten entry
reddens rather than silently defaulting to epoch 0.

**Lexicalisation.** `domains/language` already models "this concept has no
word here, for a recountable reason" as `ExposureClass::Unknown { reason:
GapReason }`, and `common_vocab.rs` states the discipline: *"a gap always
means something true about the world... and never an authoring hole."*

None of the three existing `GapReason` variants fits an out-of-character
action, and the reason is the interesting part. `Experiential` (this culture
never met the referent), `Perceptual` (senses have not resolved it) and
`Unnameable` (*"the referent is real and objective"*) are all gaps that
**could in principle close**. An out-of-character gap can never close,
because there is no referent in the world at all. `!why` is not a thing no
goblin has encountered; it is a thing no goblin *could* encounter.

**This arc adds a fourth variant, `GapReason::Extradiegetic(String)`**, and
this is the arc's one change outside `windows/vessel`.

`CommonVocabulary` stays total — Common is the author's register and has no
speakers, so an operator instrument having a Common word is correct rather
than awkward.

**The concept enumeration is a plan-time task, not a spec-time count.**
`Action::concept_name` already folds `MoveTo` and `MoveWithin` onto one
`move`, reasoning that *"a language has a word for going, not two words
separated by how far."* By that rule `go`/`back`/`enter`/`out`/`dive`/
`surface`/`delve`/`climb` are all *going* and want one concept, not eight.
**The rule is the deliverable here; the enumeration is derived against the
live registry during planning**, never from a count written into this spec.

---

## 4. Drift

Committed artifacts move. Per metaplan §6.1 this is expected drift, must
land in the same commit as the change, and must be **read** by a reviewer
rather than accepted.

Stated as decision rules rather than predictions, because a prediction can
be wrong and a branch table covering the responses cannot:

| observation after `make rebaseline` | response |
|---|---|
| the three renamed command echoes moved (`whoami`, `npcs`, `why` gaining sigils) in `scripts/possession-walk.txt` / `-over-time-walk.txt` output | expected; commit with the change |
| day-stamps moved after the first movement verb in either gallery transcript | expected — this is time-charging arriving; commit with the change |
| `clients/game/core/tests/fixtures/session-seed-42-chamber.json` moved | expected if and only if a charged verb runs before the snapshot; verify which, then commit |
| `session-seed-42-turn-0.json` moved | **STOP.** Its own generator comment says no verb runs before that snapshot, so nothing in this arc should reach it |
| `docs/audits/type-audit-report.md` moved by more than the new pub-boundary primitives | **STOP.** The report is an aggregate with zero per-symbol rows; unexplained movement means a `type-audit:` tag was dropped or duplicated |
| any file under `docs/generated-paths.txt` moved that is not named above | **STOP** and account for it before committing |

The input-side rename is small and was measured: across both gallery
scripts, exactly three lines need sigils, because every other verb in them
is group B or C, whose bare form is the in-character form and already does
the in-character thing.

---

## 5. Risks

1. **`liveness.rs` is 14,429 lines and `session.rs` is 5,776.** The Tackle
   moved the action layer out to `action.rs` (352 lines) so this arc's
   semantic work does not happen inside the large files. Routing the
   player's verbs through it must not undo that — new dispatch logic
   belongs in the action layer or its own module, not back in `session.rs`'s
   match.

2. **Per-session predicate registration is a de-facto save-format
   contract.** `AGENT_AT` is registered per session with a fixed doc string
   (`session.rs:655`); `Registry::register_predicate` is idempotent for an
   identical definition and errors otherwise. So **the doc string of every
   per-session-registered predicate becomes a save-format contract the
   moment a played world is saved** — changing one breaks reload of every
   world saved before the change. This is true today, undocumented today,
   and this arc adds predicates to that set.

3. **The gate is where a side channel hides.** §3.3's precedent is a verb
   that bypassed a redaction. Every group-B in-character form must be
   checked against its out-of-character twin for information it should not
   carry — the table makes this checkable, it does not make it automatic.

4. **The suite's growth hits the roster tripwire by design.** Every new
   variant breaks `action_variants_must_all_be_rostered`, `Action::all`,
   and `concept_name` until rostered. That is the intended cost and should
   not be worked around with a wildcard arm.

---

## 6. Decisions to record

- **In-character / out-of-character is a property of the action, not the
  invocation; `!` is a namespace.** (New constitutional ground; reverses
  the metaplan's §3.3.)
- **An out-of-character act bypasses the body, never the world — and it may
  commit, stamped.** (Extends metaplan §3.4 with §2.2 above.)
- **The effect of an act belongs to the body performing it, not the driver.**
  (The keystone, from the metaplan; this arc is where it becomes code.)
- **A player's acts are not filtered out of a saved played world.**
  (Resolves metaplan §6.5; derived from the keystone.)
- **A concept with no possible referent in the world is lexicalised as an
  extradiegetic gap, never as a missing word.** (New `GapReason` variant.)

---

## 7. Flagged for G3

1. **Player acts now write to the ledger and charge time, and those facts
   reach saved world files** (metaplan ledger #1, §8 item 1). Save-format
   adjacent. §2.4 above resolves the sub-question the metaplan left open,
   in the direction of *no filtering*, which is the larger of the two
   commitments.
2. **Group A's bare forms are retired** — `why` becomes `!why`. A
   deliberate break in the command surface, with the hint path deferred.
3. **A fourth `GapReason` variant lands in `domains/language`**, making
   this a cross-domain arc rather than a `windows/vessel` one.
4. **Per-session predicate doc strings are save-format contracts** (risk 2).
   True today and undocumented today; this arc widens the set.

---

## 8. Deliberately NOT in this arc

- **Command parsing beyond the `!` sigil.** The sigil is now committed
  grammar, since it selects the action. Everything else about the parser is
  unchanged.
- **A full body-condition system.** One row (`asleep`). `unconscious`,
  `blind`, `invisible`, `dominated` fix the table's shape; only `asleep` is
  scheduled.
- **Durations and interruption.** `Action::Rest` remains a multi-day act
  wearing an instant's clothes. Named as a risk by the metaplan, not a
  deliverable.
- **The hint path** for bare group-A forms.

---

## 9. Definition of done

Per decisions 0013 and 0020, and the metaplan §9: chronicle entry in
`book/src/chronicle/`; a freshness sweep of stale chapters, re-scoring any
Confidence Gradient bet this arc moves; a retrospective in
`docs/retrospectives/`; regenerated artifacts committed in the same commit
as the change that drifted them.

Idea-registry work for this arc:

- a **new row** for the CLI hint path on retired bare instrument forms;
- a **new row** for the origin-of-intent taxonomy (The Tackle's F-1, held
  unminted pending program approval);
- a **new row** for durations and interruption (The Tackle's F-2, likewise);
- existing rows to re-check for movement: `MAP-27`,
  `RENDER-possession-still-mints`, `PLAY-death-is-traversal`,
  `PLAY-consent-siblings`.

Row IDs are minted against the merged tree at close, never reserved in
advance.

---

## 10. Provenance

Brainstormed 2026-08-20 with Nathan, under `campaign-autopilot`, continuing
The Bridle's program brainstorm of 2026-08-19. Nathan's rulings in this
session: `!` is a namespace and the forms are distinct acts; concept names
stay required with selective lexicalisation; `provoke`/`soothe` are operator
instruments; `wait` takes both forms; `possess` is session control; bare
group-A forms are retired for consistency; the daybook is carved to its own
arc.

Read and verified against the tree at `74aac3f8`: `windows/vessel/src/`
(`action.rs`, `clock.rs`, `session.rs`, `purview.rs`, `agent.rs`),
`cli/src/concepts.rs`, `cli/tests/suite/accession.rs`,
`domains/language/src/` (`lexicon.rs`, `common_vocab.rs`, `accession.rs`),
`scripts/regenerate-artifacts.sh`, and both gallery walk scripts.
