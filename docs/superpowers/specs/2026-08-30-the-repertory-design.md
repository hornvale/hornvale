# The Repertory — a corpus of scenes the world must be able to play

**Campaign:** The Repertory
**Date:** 2026-08-30
**Decision block:** 0476-0485
**Status:** spec, awaiting G3

## 1. The problem

Hornvale has an extraordinary apparatus for *is it correct* — a census whose
committed table runs to 229 columns, five-valued trope verdicts, byte-goldens,
mutation-tested assertions, a default-deny type audit. It has no instrument at
all for *does a scene play out*, and the gap has a symptom the project owner
states directly: after a long run of dense landings he still "can't actually
speak with anything."

The diagnosis is not that the substrate is shallow. It is that nothing orders
**seams**. The ladder in `sentences/` orders rungs; `tropes/` orders
situations; `systems/` orders capabilities. A rung can be perfect and reach
no other rung, and no gate has an opinion about it.

## 2. The principle this operationalizes

> "We should be able to explicitly author anything that we want to be able to
> generate procedurally."

That is not a vision statement. It is a testable engineering principle, and
it is a claim about a **gap** between two poles: what an author can specify,
and what the world produces on its own. This corpus measures that gap.

## 3. Relationship to the sibling families

`repertory/` is the fourth corpus family and the first whose verdict comes
from a run rather than a static resolution.

| family | the question | resolved against |
| --- | --- | --- |
| `tropes/` | can the **world** represent this situation? | the concept registry |
| `systems/` | does the **program** implement this capability? | repository facts |
| `sentences/` | can the **grammar** produce or parse this utterance? | a hand-maintained declaration |
| `repertory/` | does this **scene** play out? | **an actual run** |

It inherits the family discipline unchanged: the corpus is **data** and the
resolver is **code** (decision 0011); nothing in `windows/vessel` reads a
corpus file; an item freezes before it is measured (decision 0016).

It deliberately does **not** inherit the weakness `sentences/` names in
itself. `cli/tests/suite/sentence_corpus.rs`'s module doc states that
`IMPLEMENTED_DEMANDS` "is a hand-maintained declaration and nothing
mechanically proves it... a token added on optimism moves the score without
moving the grammar, which would make the instrument worse than no instrument,
because it would read as evidence." **No `repertory/` item may be satisfiable
by a declaration.** An item that could go green by adding a registry row
belongs in `tropes/` or `systems/`; this is the discriminating test at
authoring time.

## 4. The authorship scale

Found-external scenes (transcribed interactive fiction) and authored-original
scenes are not alternatives. They are positions on one axis — how much of a
scene the author fixes versus how much the world supplies — and they measure
opposite things.

```
  control   what the author fixes           exemplar
  -------   -----------------------------   -----------------------------
       0%   a place and a time              a goblin village bustling
      20%   the setting                     a market square at dawn
      40%   the participants                a drow and a goblin, a room
      60%   + an intent                     the drow wants the orange
      80%   + the opening moves             the beggar asks; the elf refuses
     100%   every beat                      `echo` in the Loud Room
```

Interactive fiction is an authored artifact — every beat specified — so it
tests whether the sim can **host** a fixed scene. Original scenes at the low
end test whether the world **produces** one unprompted. A found corpus also
carries an epistemic property an authored one cannot: Zork, Galatea and
Christminster were written by people who never heard of Hornvale, which is
the same reason Polti and Wolverson make trustworthy instruments.

`echo` in the Loud Room is the sharpest single case, because it is the
**anti-simulation** case: an authored exception the world does not have to
know about. It probes whether the architecture can host an authored exception
at all, which is the tension living inside the principle in §2.

## 5. Two verdicts

Every scene carries two independent verdicts.

- **AUTHORED** — specify the scene fully; does the sim carry it?
- **REACHED** — do *not* specify it; across many worlds, does the scene arise
  anyway, and **at what rate**?

The diagnostic value is in the middle state. **AUTHORED but not REACHED**
names the next campaign: the world can hold this scene but never generates
it, so a pressure is missing or something cheaper is out-competing it. This
is the fundamental-versus-realized niche distinction, and it carries its
follow-up question built in — *what is crowding this out?*

**REACHED is a rate, never a boolean.** A scene arising in 1 world in 1000
and one arising in 900 are both "reached" under a flag and mean opposite
things. Measuring it as a rate also unifies it with the separate requirement
that a scene be able to **resolve differently across seeds** — a scene with
one outcome is a cutscene, not a simulation. Those are one measurement.

**This campaign implements AUTHORED only.** Detecting a scene in an
unscripted run is scene recognition over a transcript or ledger — a harder
and separate problem. The schema carries **no unresolved reach field**:
shipping one would reproduce §3's named defect in a brand-new instrument on
day one. REACHED is the next arc, and §14 records it as such.

## 6. Scenes are found, not staged

An item declares participant **constraints**; the resolver searches
`(seed, day, target)` for a world satisfying them, then drives the script
against that world. It never constructs the situation.

Precedent: `hornvale scout` already scans seeds for ones satisfying pins
(`--from-seed` / `--limit` / `--max-scan`, `cli/src/main.rs:322`).

The reason is not economy, it is validity. Staging would author the world,
and an AUTHORED verdict obtained in a staged configuration is evidence about
the staging, not about Hornvale — it would let an item pass in a configuration
the world could never produce.

**The search is amortized.** A found `(seed, day, target)` is recorded on the
item as a witness, so ordinary re-resolution is one run rather than a search.
A recorded witness that stops reproducing is itself a red, and a meaningful
one: the world changed under a scene that used to play.

## 7. The scene record

One JSON file per corpus, in the sibling idiom (`repertory/<name>.scene.json`).

```
  field            meaning
  --------------   ------------------------------------------------------
  id               stable slug; append-only once frozen
  title            one line, human
  provenance       "authored" | "found:<work>, <author>, <year>"
  control          0..100, the §4 scale position
  participants[]   { role, kind, constraints }
  setting          constraints on the room / band / day
  beats[]          { id, description, assertion }
  witness          { seed, day, target } | null   (§6, filled by search)
  beta             true | false                   (§10)
```

**`kind` is not typed to creatures**: it admits `creature | place | object |
institution | weather`. Nathan's own examples require it — the Loud Room's
actor is a room and Galatea is a statue — and a creature-only slot would
silently exclude two of the six founding examples.

**Beats are mandatory, not decorative.** "The burglar wakes the family"
decomposes into {enters} + {they sleep} + {noise} + {waking} + {reaction}. A
monolithic red tells an implementer nothing; a beat-level red names the
missing capability, which is what makes the corpus generate campaigns rather
than merely score them.

**Assertions are properties, never golden strings**, so that prose changes do
not redden a scene. Where the property is structural, it is asserted against
the `vessel/session/v2` snapshot rather than the transcript.

**Negative-space items are a first-class class.** A corpus that only ever
asserts what *can* happen ratchets toward permissiveness, and a world that can
play out anything has no physics. Some items assert what must **not**: the
statue does not answer; the goblin does not know the drow's intent.

## 8. The resolver

`cli/tests/suite/repertory_corpus.rs`, following `sentence_corpus.rs`'s
placement and shape. Corpus is data, resolver is code (0011).

Measured driving surface, verified by running it rather than inferred:

| fact | value | how established |
| --- | --- | --- |
| `possess --script <PATH>` | works; markdown transcript to stdout; exit 0 | ran it, seed 42 |
| `--snapshot <PATH>` | writes `vessel/session/v2`, 20,372 bytes | ran it, seed 42 |
| snapshot top-level keys | `day, known, narration, schema, self, sensed, social, spatial, turn` | `json.load`, same run |
| `social[]` per co-located entity | `entity, label, grievance, hostile` | same run |
| `self` | `agent, species, settlement, population, room` | same run |
| cost, warm binary | **5.4 s** per run | `time`, second invocation |
| cost, cold | 39.8 s | `time`, first invocation |

At 5.4 s per authored run, a 40-item roster resolves in under four minutes,
so AUTHORED belongs in the gate ladder rather than the heavy tier. The
placement decision is deferred to the plan, against a measured roster.

## 9. Verdicts, states, and the ratchet

Five-valued, in the idiom of `tropes check`, seam-guard, and the type audit's
`waiver(...)`:

```
  ABSENT      no path; the first failing beat is named          RED on regress
  PARTIAL     the scene runs; a named beat fails                RED on regress
  AUTHORED    every beat passes against a found witness         the ratchet
  DECLARED    known-absent, WITH A REASON (reasonless = error)  green, printed
  STALE-DECL  declared absent, but it now passes                RED
```

A binary gate over a forty-item roster goes red on day one and stays red,
which trains everyone to ignore it — the same reasoning that made seam-guard
three-valued. `STALE-DECL` is what keeps a declaration honest: a one-directional
acknowledgement can only ever be satisfied, so it rots, and this one fails the
moment the scene starts working.

**The ratchet:** once an item reaches AUTHORED it may not regress without a
recorded reason. When REACHED lands, the two verdicts ratchet **independently**
— a scene can stop arising while still being hostable, and that is a distinct
and interesting failure.

## 10. The beta cut

The corpus is open-ended and grows forever. **"Beta" is a marked subset**
(`beta: true`), not a state of the corpus. The roster grows without the beta
target moving, which is what stops an endless yardstick from reading as a
treadmill.

## 11. Seed items — the positive control

The corpus ships with items that **already pass**, drawn from the verbs
`!help` lists today. If every item were ABSENT on day one, the resolver would
never once have demonstrated it can produce AUTHORED, and every later green
would be a green nothing had ever earned.

Candidates, all against verbs verified present in the live verb list:

- a body walks a compass exit and the room it senses changes (`go`)
- a body examines a thing the room's narration names (`examine`)
- a body waits and the sky above it moves (`wait`)
- a body sleeps and only `!` verbs answer meanwhile (`sleep`)
- a co-located creature's felt state can be read (`needs`)
- a negative-space item: a body cannot walk a closed direction

The plan fixes the final set; the requirement is that at least one item
reaches AUTHORED in the same commit that introduces the resolver.

## 12. The first scene: `the-orange`

> A drow wants a ripe orange that a goblin has.

Chosen by the project owner as the founding scene. Both species exist as
authored kinds (`domains/species/src/lib.rs` carries `"drow"`, `"drow-kind"`,
`"goblin"`, `"goblin-kind"`), so it is not trivially unsatisfiable.

Its expected day-one verdict is **ABSENT at an early beat**, and that is the
point of choosing it. Established by reading the code, not predicted:

- **no verb addresses another creature.** The live in-character verb list has
  25 entries. `ask` asks "the body you are wearing" how *it* feels; `write`
  speaks a line of Common absorbed into the player's *own* margin. `!provoke`
  and `!soothe` shift a co-located NPC's disposition but are operator
  instruments that carry no utterance.
- **objects have no instances.** `windows/vessel/src/affordance.rs` is "only
  the vocabulary and the table" — a kind-keyed `ObjectProperty` store held
  "vessel-locally — build-state, not world-state... nothing here is
  serialized." The affordance query itself is "a later task."
- **nothing represents possession.** The `vessel/session/v2` snapshot has no
  inventory, carried-items, or holdings channel (grepped `snapshot.rs`; no
  match for any of them).

So the scene decomposes into beats that are each a campaign-sized target,
and that decomposition **is** the first arc:

```
  beat                                            needs
  ---------------------------------------------   ------------------------------
  1  a drow and a goblin are co-located           search only; may hold today
  2  the goblin holds an orange                   object instances + possession
  3  the drow addresses the goblin                the conversational edge
  4  the goblin understands the request           parse -> intent, another mind
  5  the goblin decides                           a disposition response to a request
  6  the orange changes hands, or does not        transfer
  7  the outcome varies across seeds              the decision must be contingent
```

**The Repertory does not build any of beats 2-7.** It builds the instrument
that makes them legible, ordered, and individually exercisable, and leaves
`the-orange` standing red at its first missing beat.

## 13. What this campaign does *not* do

- It does not add the conversational edge. That is the next campaign, and
  `the-orange`'s beat 3 is its acceptance criterion.
- It does not author the roster. The roster is the project owner's authorship;
  cuts and fidelity are his call.
- It does not implement REACHED (§5). Followup.
- **It does not measure whether the world is alive.** A fully green repertory
  is compatible with a market nobody wants to stand in. That question belongs
  to the Confidence Gradient (`book/src/open-questions.md`) as a bet only a
  human playing can move, per decision 0030 — never to a metric, because a
  metric for aliveness in this project would be optimized against.

## 14. Followups

- **REACHED**: scene recognition over unscripted runs; the reach rate; the
  independent second ratchet (§5, §9).
- **The IF corpus**: transcribing found scenes from interactive fiction as a
  high-control, externally-authored corpus (§4).
- **Search cost**: `scout`'s pin scan is astronomy-only at genesis and cannot
  answer creature co-location; the §6 search runs at full world cost. Bounding
  it is unsolved and belongs to the plan.

## 15. Flagged for review

1. **A new top-level corpus directory** (`repertory/`) needs a
   `docs/generated-paths.txt` decision: the corpus itself is hand-authored
   data and is *not* generated, so it should NOT be listed — but the first
   commit must still `git add` it, since `git diff --exit-code` is silently
   vacuous against an untracked path.
2. **No save-format, epoch, or determinism-contract change** is proposed. The
   resolver reads committed surfaces and drives the CLI; nothing serializes.
3. **`the-orange` will be red for several campaigns.** That is designed
   (§12), and it is the one thing most likely to read as a broken gate to a
   future session. §9's `DECLARED` state exists so the redness is *stated*
   rather than merely tolerated.
4. **Low-confidence, no precedent either way:** whether AUTHORED runs belong
   in `gate-commit` or the stage gate. 5.4 s/item is cheap, but the resolver
   builds and runs the CLI binary, which the sub-floor tier does not otherwise
   do. Deferred to the plan, to be settled against a measured roster.
