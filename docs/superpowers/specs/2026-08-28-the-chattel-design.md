# The Chattel — objects that can be held

**Campaign:** The Chattel · **Branch:** `campaign/the-chattel` ·
**Decision block:** 0396–0405 (main ceiling 0376 at reservation) ·
**Drafted:** 2026-08-28 · **Status:** Draft, at G3.

**Arc IV.c of The Bridle.** Predecessors: The Offer (IV.a,
`docs/superpowers/specs/2026-08-27-the-offer-design.md`), The Latch (IV.b,
`docs/superpowers/specs/2026-08-28-the-latch-design.md`).

*A chattel is movable property, as opposed to a fixture. The distinction is
the campaign's cost axis, not a flourish: a fixture may be identified by
where it is, and a chattel may not.*

---

## 1. What occasioned it

The Offer shipped advertisement: objects derive their verbs from kind-level
properties, filtered by the acting body and the observer's knowledge. The
Latch shipped one durable precondition: a cave passage that stays open once
cleared. Both deliberately stopped short of an object *model*, and both said
so — The Offer has "no way to open anything"; The Latch's non-goals are
"objects as entities; containers; contents; inventory; take/drop/put; a
name→entity lookup; re-closable passages … All of these are IV.c."

The arc's own acceptance test has been waiting for this campaign since the
metaplan: *a key says "unlock me with this"; no verb×object table exists
anywhere* (`docs/superpowers/specs/2026-08-19-the-bridle-metaplan.md`). IV.a
shipped the saying. IV.b shipped one durable precondition. Neither could ship
the key, because nothing in Hornvale has ever had an identity that moves.

### 1.1 The mechanism was reserved a month ago and left unbuilt

This is not an invention. The Hearth (2026-07-25) specified **promotion on
touch** in its §4 — *"an anchor is derived — no identity, no storage — until
something interacts with it, at which point it is promoted to a committed
entity with a real `EntityId`, and deltas reference that"* — and reserved
*"items as entities whose position folds over custody events"* in its §11.

The Hearth's own G3 asked whether to build it then, and named this campaign
as the alternative: *"Ship the mechanism now for the epoch guarantee, or
defer it with the items work and accept that the first modification campaign
must solve it under pressure?"* It was deferred. Its retrospective records
promotion-on-touch as "designed, deliberately unbuilt — no consumer in v1."
The Threshold then named a *second* future consumer (monotone seasonal
change: "a log rots, a house burns, a pond dries for good"). This campaign is
the first consumer to arrive.

## 2. Where the cut falls: IV.c does not climb the axis, it fills it

The Offer's ordinal axis — *how much state an affordance's precondition
reads* — with this campaign placed on it:

```
  reads     position                          status
  --------------------------------------------------------------
   0%   nothing                            shipped
  10%   position only                      shipped
  30%   derived object properties          shipped (IV.a)
  50%   the observer's knowledge           shipped (IV.a), NEVER FIRED  <- IV.c fires it
  70%   the playthrough's daybook          shipped (I.b)
  90%   committed world facts              shipped, ONE SLICE (IV.b)    <- IV.c widens it
 100%   another agent's committed mind     far future
```

IV.c adds no rung. It takes the 90% rung from *one passage per chamber
address* to *every object in the world*, and it gives the 50% rung its first
firing case in the project's history (§3.6).

**Lifetime is a separate axis and this spec will not conflate the two.**
Decision 0368 was minted because The Latch's spec did: *"the axis's rungs name
what a precondition READS, never how long that state lives."* IV.c's state
lives exactly as long as IV.b's — the session by default, and into a new world
file when `possess --out` is asked for, because a played world is a fork
(0368). Nothing here changes that, and §6's acceptance criteria say which
lifetime each one claims.

## 3. The mechanism

### 3.1 Three types, and promotion is the bridge

```
  ANCHOR (exists)            THING (new)                  KIND (new store)
  a region of a room         an entity with identity      what a thing IS
  derived every bubble       id derived from lineage      ThingTraits, owned by
  never serialized (0069)    promoted on first fact       a new domain, joined
  AnchorKind, 14 variants    position = its ROOM          into WorldComponents
```

A strongbox is an anchor today: a derived region, free, with no ledger entry.
Touch it and it **promotes** to a Thing with a real `EntityId`; the anchor
becomes merely where it stands. A key is born a Thing and never was an anchor.
Fixtures and portables therefore share one mechanism and differ only in
whether they began as a region.

### 3.2 Identity is a pure function of lineage, so it costs nothing until it changes

`derive_entity_id` (`kernel/src/ledger.rs:564`) derives an `EntityId` from
`Lineage { parent, role, ordinal }` (`:543`) and nothing else — no counter, no
allocation, no ledger read. `Ledger::reuse_or_mint_entity` (`:199`) is the
idempotent form, and **its own doc names this campaign's requirement against a
different subject**:

> "a possession session re-deriving the NPCs of a world it has already played
> and saved: the herder of a settlement is the same herder in every session,
> so re-deriving must FIND it."

The strongbox of a room is the same strongbox in every session, for exactly
the same reason. `liveness.rs` already runs this twice for NPCs (settlement
NPCs from `village.id`, wild NPCs rooted). IV.c is its third caller and its
first non-creature one.

**The lineage is keyed on `(room facet, thing-kind, ordinal)` and never on the
anchor index.** That is the whole of this campaign's compliance with decision
0069, and the reason is mechanical rather than stylistic:

```
  candidate lineage                     0069 verdict
  ----------------------------------    -------------------------------------
  role = "thing@<facet>/<anchor-id>"    FORBIDDEN. The id would point into the
                                        fine layer, which 0069 licenses to
                                        "regenerate differently forever" -- the
                                        orphaned delta The Hearth SS4 built
                                        promotion to prevent.
  role = "thing@<facet>/<kind>"         LEGAL. A facet is the coarse layer; a
  ordinal = n-th of that kind here      kind is authored. Nothing stored points
                                        into the fine layer.
```

**The ordinal is the one place the fine layer can still leak in, and this
spec does not know the answer.** The ordinal exists to distinguish two
strongboxes in one room, and its only obvious source is the interior's own
derivation order — a `Vec` order, which is fine-layer and which 0069 licenses
to change. So *does any production room place two anchors of one kind*
decides whether an ordering rule is needed at all. **Task 1 measures it, and
the response is a branch table rather than a prediction:**

```
  measurement                       response
  -------------------------------   -------------------------------------------
  no room places two of one kind    the ordinal is always 0. State the invariant,
                                    assert it by test over the production gate
                                    combinations, and stop.
  some room does                    a stable ordering rule is needed, keyed on
                                    something a layout epoch may not change. Do
                                    NOT key it on derivation order, and bring the
                                    finding back here rather than choosing inside
                                    a task -- a silently unstable ordinal is a
                                    wrong entity id, and no gate in this tree can
                                    see one.
```

**Promotion preserves a thing, not a slot.** A promoted thing's position is
its room — exactly what 0069 requires of a creature — and which anchor it
rests at is re-derived and re-bound on every entry, the same way `interior_of`
already re-derives the whole interior. 0069 is obeyed, not amended.

### 3.3 One position predicate, three location types

```
  located-in    subject: a thing    object: Value::Text(room)
                                          | Value::Entity(holder or container)
                non-functional, append-only, latest-wins
  openness      subject: a thing    object: Value::Flag(bool)
                non-functional, append-only, latest-wins; ABSENT means
                "whatever the seed drew"
```

`Value` already carries both `Text` and `Entity` (`kernel/src/ledger.rs:55`),
and `agent_at_fact` established places-as-values, so **one predicate covers
in-a-room, in-a-chest and in-a-hand.** RCC-8's declared transitivity of `in`
(The Hearth §5) then yields "a key in a chest in a room is in the room" as a
property of the vocabulary rather than a special case inside the fold.

Rejected: separate `held-by` / `contained-by` / `located-in` predicates. Three
folds to hold in agreement, and the transitivity would have to be re-asserted
for each pair — a rule duplicated without a two-way agreement test (0261).

**Both reads exist, and they are not interchangeable.** `latest_value_of`
(`:511`) answers *where is it now* and is indexed on (subject, predicate). The
as-of-day read applies the same `day' <= day` filter 0366 already rules for,
and is what a replay must use. 0366's rule is unchanged and inherited whole: a
precondition that reads world state reads it through a fold evaluated at the
instant being asked about, and caches nothing a replay could observe.

### 3.4 Latency, and the first NEGATIVE fold in the tree

An object is **latent** when the pattern grammar implies it and no fact about
it exists. Latency costs nothing: no id is minted, no fact is written, and the
id is derivable without consulting the ledger at all.

The cycle a thing moves through:

```
   (1) LATENT ---------> (2) NAMED --------> (3) ADDRESSED
   the grammar implies   the noun catalog    derive_entity_id from
   a strongbox here      resolves the word   (facet, kind, ordinal)
   cost: nothing         cost: nothing       cost: NOTHING -- no fact yet
        ^                                             |
        |                                             v
   (7) RE-DERIVED <---- (6) RELEASED <---- (5) HELD <- (4) PROMOTED
   next entry re-offers  located-in names   located-in  first fact commits
   the untouched ones    a new room         names a     (instance-of)
                                            holder      cost: 1 entity, 1 fact
```

**Phase 3 is what makes the design cheap, and The Hearth's binary
derived/promoted model had no name for it.** A thing can be identified,
compared and matched at zero ledger cost. "A key says unlock me with this"
needs identity *matching*, not persistence. Only a change pays.

**Phase 7 is this campaign's own hazard, and The Latch never faced it.** If
room 17's strongbox is promoted and carried to room 40, room 17 must stop
offering a strongbox. A cave mouth cannot move, so The Latch's fold never
needed a negative direction. Latency therefore reads:

```
  latent(facet, kind, n)  ==  the grammar offers it
                          AND no committed located-in fact places
                              derive_entity_id(facet, kind, n) elsewhere
```

Because the id is derivable *before* the ledger read, this is one indexed
`latest_value_of` per latent slot per room entry — the same fold shape as
everything else, running in the one direction nothing in the tree has run
before. **This is where the campaign should expect its defects**, and §7's
first task measures it before anything is built on it.

### 3.5 A new domain, and the corpse is why

Thing-kinds live in a new domain, `domains/thing`, owning `ThingTraits` and
joined into `WorldComponents` (`windows/worldgen/src/components.rs:26`) so
that `mint_instance_of_kind` (`windows/worldgen/src/lib.rs:9043`) can validate
against `kinds()` (`components.rs:216`). `WorldContext` already holds
`wc: Option<WorldComponents>` (`windows/vessel/src/session.rs:489`) and
assembles it, so a session reaches the roster without new plumbing.

The Offer's §3.1 named this move and deferred it: *"Joining requires a
`KindId` key and a domain that owns object kinds — and no object domain
exists, so joining means creating one."* Three existing stores — `deity`,
`culture`, `material` — carry no biosphere row, so a trait store for kinds
with no body is precedented rather than novel.

**The argument that decides it is not layering, it is the corpse.** MAP-27
states that animacy is a *mutable* property verbs transform ("kill:
animate→inert; raise: →undead"). That boundary is meant to be crossable. Two
disjoint kind rosters break on the first corpse — an animate kind gone inert,
and takeable — because it would have to change registries. One shared `KindId`
space with a new trait store survives it. **No corpse ships in IV.c** (§5);
the point is that the identifier space must not foreclose one.

**Thing-kinds also get concepts, and this is where the golden moves.**
The Actants' precedent is explicit — it minted `ConceptKind::Act` so that "a
verb must be nameable before it can be a reaction gated by properties." A
thing must be nameable before it can be taken. `ConceptKind`
(`kernel/src/registry.rs:26`) has nine variants — Substance, Living,
Celestial, Terrain, Social, Body, Kin, Quality, Act — and **none fits a made,
portable object**. So IV.c adds a variant and registers thing-kinds **at
genesis**, which is exactly what The Latch dodged by registering per-session.

That moves `cli/tests/fixtures/world-seed-42.json`, the byte-golden
`make rebaseline` never writes and whose guarding tests are not in the subfloor
roster — so `gate-commit` compiles them and never runs them. Only
`make rebaseline-goldens` writes it. **This bounced The Offer from the chamber
once.** It gets its own planned task (§7), not a discovery.

### 3.6 The knowledge gate fires, and 0369 is answered rather than superseded

Decision 0369 records that The Offer's knowledge gate has never denied
anything and that The Latch could not give it a firing case: `offered_to_observer`
(`windows/vessel/src/affordance.rs:450`) takes an `AnchorKind`, an
interior-object enum with no cave-mouth variant, while a cave mouth is a
`Vertex`/`ChamberAddr`. **The obstacle is addressing, not durability** — that
is 0369's rule and it is correct.

0369 listed two remedies and called them owner decisions: a new `AnchorKind`
plus a design for what a cave mouth offers, or anchors for chambers. **There
is a third it did not enumerate, and it is the one this campaign takes:** make
the cave mouth a *thing*. The gate's currency changes from `AnchorKind` to
thing-kind, and a cave mouth is then expressible in it. Ruled by Nathan at the
§3 stop; 0369's rule stands untouched, only its enumeration was short.

Consequently `offered_to_observer` is re-keyed, which edits code The Offer
shipped and mutation-proved. That is the campaign's most delicate diff and
§7 gives it its own task.

**Re-keying means ONE property table, not two, and that resolves a
contradiction an earlier draft of this spec carried between this section and
§3.8.** The Offer holds properties in a `ComponentStore<AnchorKind,
ObjectTraits>` held vessel-locally. If thing-kinds got a second table, a
strongbox's properties could disagree between its latent form (an anchor) and
its promoted form (a thing) — the same object at two lifecycle stages, with
two sources of truth. That is a silent bug class, and §3.1 makes it reachable
by construction.

So the table re-keys to `KindId` and every `AnchorKind` maps to a thing-kind.
**The Offer forecast exactly this and named the campaign that would do it:**
"Joining is IV.b's move, when objects become mintable entities validated
against `WorldComponents::kinds()`." IV.b did not join; IV.c does. An anchor
kind with no verb anyone can reach (Screen, Ground) is simply a thing-kind
nothing promotes, which costs a roster row and no behaviour.

### 3.7 Passages join the object model, superseding 0367

Decision 0367 made the latch monotone and said why a closing act was deferred:

> "Re-closing is not hard to write; it is hard to write *once*. A closing act
> belongs with doors, lids, and containers, because they are the same
> mechanism seen from three angles, and a `passage-closed` predicate shipped
> now would be designed against one of the three."

IV.c is the first campaign in which all three angles are visible. So the cave
mouth becomes a thing-kind carrying `AffordsPassage` and `Openable`, and
`passage-cleared` retires in favour of `openness`. The Latch's `effective_state`
keeps its exact shape:

```
  effective_state(thing, day) =
      Open                     if the latest openness fact at-or-before day is true
      barrier_of(seed, addr)   if it is false, or if there is none
```

Monotonicity is what goes away, and that is the deliverable. 0367 is
superseded by the thing it asked for, and its own closing consequence — *"a
monotone latch cannot express a trap … the first thing anyone will want"* —
becomes expressible.

**This breaks saved worlds carrying `passage-cleared`, and the blast radius
was measured rather than estimated.** `grep -rl 'passage-cleared'
--include='*.json' .` returns nothing: no committed fixture carries the
predicate, so the break reaches only hand-made `possess --out` saves written
since The Latch landed. Decision 0189 is the precedent for a deliberate break
("a world file written before this flip does not load — regenerate it from its
seed and pins").

### 3.8 The property vocabulary grows by three, each earned by a shipped verb

The Offer's discipline is inherited whole: *minimal and earned — each property
exists because a verb needs it, and a property no shipped verb gates was cut
rather than kept.* MAP-19's cautionary bound is Cyc: enumerate nothing.

```
  property      carried by                verb it gates        status of verb
  ------------------------------------------------------------------------
  Portable      key                       take / drop          NEW
  Openable      strongbox, cave-mouth     open / close         NEW
  Lockable      strongbox                 open (requires a     NEW
                                          key in custody)
```

`Lockable` is the arc's acceptance test made mechanical, and it extends The
Offer's query by one argument:

```
  affordances(object, body, observer)              -- IV.a
  affordances(object, body, observer, custody)     -- IV.c
```

A lockable thing's `open` requires a thing carrying the matching property **in
the body's custody**. That is still M+N: the lock declares what it requires,
the key declares what it carries, and neither names the other.

**Which kinds carry which property beyond the ones named here is the
implementer's call, made from the code.** The Offer's §3.3 states why: "a spec
author choosing them from outside has been wrong here every time it has been
tried."

**Contents must be authored, because nothing is ever inside anything today.**
The Offer's Task 6 ran a census over all 60 production gate combinations and
found the grammar's only `within` relation anywhere is `{(Hearth, Alcove)}` —
*nothing is ever placed inside a strongbox*. So a pattern that places a thing
within a container is new authored data in `interior/pattern.rs`, and without
it the container half of this campaign would report nothing, forever, exactly
as The Offer's `Encloses` nearly did.

## 4. Risks

1. **The negative fold (§3.4) is the campaign's likeliest real cost**, in both
   correctness and time. It runs per latent slot per room entry, in a
   direction nothing in the tree runs today. §7's Task 1 measures it before
   the design rests on it.
2. **`offered_to_observer`'s re-keying edits shipped, mutation-proved code.**
   The Offer's tests are the asset here; the risk is re-keying in a way that
   passes them while changing what they mean.
3. **The keystone golden (§3.5).** Planned, not discovered — but it is written
   here because a plan that omits it produces a green `gate-commit` and a red
   chamber.
4. **Six new verbs is six chances to repeat The Latch's one shipped defect.**
   See §6.6; the mitigation is a per-verb checklist, not a per-task
   instruction.
5. **A new domain crate touches the layering test and the dependency
   allowlist** (`cli/tests/architecture.rs`). `domains/thing` must depend on
   `hornvale-kernel` and nothing else, and adding it must not require editing
   an existing domain — the constitutional rule it is most likely to strain,
   since `WorldComponents` is where it joins and that is worldgen, not a
   sibling.

## 5. Non-goals

Creation and destruction of objects — **nothing burns, breaks, is consumed, or
is made**. This yields a checkable invariant, *every object that will ever
exist is already latent*, and it puts both where MAP-27 already puts them, in
Arc V's chemistry. No corpse ships (§3.5 argues only that the identifier space
must not foreclose one). No NPC takes, drops or opens anything — the
predicates are agent-neutral by construction, as The Latch's were, so that is
a later flip with no schema change. No trade, no ownership, no theft. No
weight-limited carrying beyond what §3.8's properties gate. No verb DSL: verbs
remain Rust variants, which is Arc V.

## 6. Acceptance

1. **A thing is taken in one room and used in another**, and the world file
   written by `possess --out` carries it — the identity-that-travels claim,
   which is the whole campaign.
2. **A key opens a lockable strongbox and the same body without the key
   cannot** — the arc's own acceptance test, made mechanical, and the first
   time a precondition in Hornvale reads a *second* object.
3. **A container opens, closes, and re-opens**, and a passage does the same
   through the same fold — one mechanism, three angles (§3.7).
4. **The knowledge gate DENIES something**, shown by a test that fails if the
   gate is removed. This is The Latch's struck-through criterion 5, and IV.c
   claims it only because §3.6 changes the gate's currency. If it does not
   pass, it is recorded unmet with its reason, following 0369's own precedent
   rather than being quietly dropped.
5. **A promoted thing carried away is not re-offered where it came from** —
   the negative fold (§3.4), asserted across two entries into the same room.
6. **Every new verb has three things**: a roster entry in `IN_CHARACTER_VERBS`
   (`windows/vessel/src/session.rs:126`), a `HELP` line (`:392`), and a
   body-state refusal test. **A verb in neither roster is invisible to every
   test** — `every_bare_verb_help_lists_is_classified` checks HELP→roster and
   roster→HELP, so a verb in *neither* is iterated by neither loop. The Latch
   shipped `clear` that way and a sleeping body could commit a fact through
   it, with 715 tests green. The template is `warm`.
7. **No verb×object table**, by source scan, held two-way as The Offer's
   criterion is: a new thing-kind gains verbs with no dispatcher edit, and a
   new verb appears on every qualifying thing with no kind edit. Decision 0350
   records that The Offer's own new verb reintroduced the coupling once before
   a structural scan caught it; the scan's stated direction is inherited.
8. **Every regression test names the mutation it must fail against, and the
   red is pasted in its doc comment** (decision 0353). No test is specified by
   the property it should assert, and **no mutation is prescribed from outside
   the code** — the plan names the property a mutation must demonstrate and
   the implementer finds one.

## 7. Task shape (detail belongs to the plan)

- **Task 1 — measure the negative fold before anything rests on it.** How many
  latent slots does a production room offer, how many `located-in` facts does a
  played session accumulate, and what does one room entry cost when every
  latent slot needs an indexed position read? This is the campaign's own
  falsification step, and it runs first so that a bad answer moves the cut at
  the cheapest moment. **0366's "a read costs a scan" must not be inherited in
  either direction**: `Ledger::find` (`kernel/src/ledger.rs:388`) consults
  `positions_for_predicate` when the index is present, so the cost is one
  predicate's own traffic rather than world history — which makes the warning
  smaller than its prose and still real, because IV.c reads far more often
  than IV.b did.
- Then, in dependency order: the domain and its kind roster; the `ConceptKind`
  variant and the genesis registration **with its golden refresh in the same
  commit**; identity and promotion; the two predicates and their folds; the
  negative fold; the property vocabulary; the six verbs; the name→entity
  lookup; the passage unification and the `offered_to_observer` re-keying; the
  wire; artifacts.

Each task regenerates and commits its own artifacts in its own commit — never
a terminal sweep task. The final task is a sweep that **asserts the diff is
empty**, which is a finding about earlier tasks rather than routine labour.

**Absorb main at every stage boundary** (`make sluice-stage`). The Latch did
(97 commits, then 26) and The Offer did not and paid at close. Main moved 23
commits between this campaign's handoff and its first hour, so this is not
hypothetical.

**Verify each task's brief against the code immediately before dispatching
it.** The Latch's fourteen defects all originated in controller prose and none
in implementer code; that step caught four across three tasks. The expensive
half is not grepping identifiers — it is asking of each brief, *do these two
sentences describe the same buildable object?*

## 8. What drifts, and why

Expected drift, landing in the same commit as its cause:

- `cli/tests/fixtures/world-seed-42.json` — the genesis concept registration
  (§3.5). **`make rebaseline` does not write this**; `make rebaseline-goldens`
  does.
- `docs/audits/type-audit-report.md` — any pub-boundary change drifts it, and
  a new domain crate is a large one. An aggregate: never text-merge it.
- `book/src/reference/layering-generated.md` — written by
  `cli/tests/suite/architecture.rs`, which discovers crates from `cargo
  metadata`, so `domains/thing` moves it automatically. **Added at plan-writing
  time**: the first draft of this section missed it, because the drift it
  causes is authored by a *test* rather than by `regenerate-artifacts.sh`.
- `docs/digest/decisions-in-force.md` — 0367 superseded, new records added.
- `book/src/reference/concept-registry-generated.md` and the stream manifest —
  the new `ConceptKind` variant and any new stream labels.
- `book/src/gallery/possession-*.md` — the transcripts contain `examine`
  output, and the noun catalog changes.
- `clients/game/core/tests/fixtures/session-seed-42-*.json` — the wire gains
  `NounEntry.affordances` (The Offer's §5.1: `#[serde(default)]`, additive, no
  version bump) and carried things.

The drift check reads its path list from `docs/generated-paths.txt` and
nothing runs it for you.

## 9. Decisions taken during execution

**Why this section exists, and why it is numbered 9.** The plan's Task 14
called for execution decisions to be promoted into "the spec's §8"; §8 was
already *What drifts, and why*, so they land here rather than displacing it.
The source is the campaign's own `.superpowers/sdd/` ledger, which is
git-ignored and dies with the worktree — everything below was written there
first and would otherwise be gone. It is transcribed rather than summarised
wherever a future reader would need the reasoning.

### 9.1 The `#D-*` rulings

Nineteen, all resolved under autopilot gate G5 except `#D-t12-2`, which was
Nathan's.

| ID | Ruling | Outcome |
|---|---|---|
| `#D-t4-1` | `ThingError` does **not** implement `std::error::Error`. The sibling it wraps (`FacetError`) does not either, so `source()` could only chain through one of two arms, and no consumer boxes it. | Deliberately not built. Revisit when a call site needs `Box<dyn Error>`. |
| `#D-t4-2` | `promote` does **not** validate `kind` against `THING_KINDS` in Task 4 — the dependency would move a byte-golden for a check with nothing to catch. **Tripwire: the first task typing a kind *literal* at a call site reopens this.** | Deliberately not built. |
| `#D-t5-1` | `room_of` stops at a body (a key in a hand resolves `None`). The fix is a **tripwire test pinning the current `None`** plus a narrowed doc — *not* the `AGENT_AT` fallback. A witness, not a promise. | Built. |
| `#D-t5-2` | The room-key spelling is fixed **in Task 5**, not deferred: the tree carried three spellings and the window closes at the first production caller. | Built. |
| `#D-t5-3` | `is_open`'s two-valued `None` is **flagged, not fixed** — a malformed latest openness fact collapses to `None` and hides a valid earlier one. | Deliberately not built. |
| `#D-t7-1` | The table scanner must widen again: `KindId(pub &'static str)` lets a hardcoded table be written `match kind.0 { "bed" => … }`, blind to both markers. Prefer a **key-agnostic** invariant — no match arm inside the dispatch function's own body may construct `OfferedVerb`s — over a third marker. | Built. |
| `#D-t7-2` | A retired claim survives in **production**: `offered`'s doc said every carrier "carries exactly one property"; `strongbox` now carries three. | Built. |
| `#D-t8-1` | Hand-add the two renamed test ids to `docs/timings/subfloor-roster.tsv`. **nextest is green and silent on a filter naming a nonexistent test**, so the commit gate had been running exactly one passage test. | Built. |
| `#D-t8-2` | **Widen the sweep rather than narrow the sentence**: a test claiming "the nine strings" was measured against sixteen. | Built. |
| `#D-t9-1` | Route `AnchorKind` roster membership through the compiler's exhaustive-match requirement. The old guard covered **re-points, not additions** — a fifteenth variant left 1,209 tests green. | Built. |
| `#D-t9-2` | A **filtered**-mutation citation ("41 tests run") survived in the doc claiming to state the honest shape; unfiltered it reds three. | Built. |
| `#D-t11-1` | `open`'s **success half is unreachable and unasserted** — a vacuous custody read left 850 tests passing. State it as an unmet half with its reason, or pin it now by committing a holding fact directly (`held_by` is a fold, so a test can). | Built. |
| `#D-t11-2` | The 0398 commit's doc says a played walk reaches "only the first two" of four states; it reaches exactly **one**. | Built. |
| `#D-t11-3` | The concealment deferral is priced against a fix nobody would write, and its positive-control sentence is **false of the gallery** — `possession-walk.txt` does one `enter further in` and never reaches chamber index ≥ 2, where every `Store` role lives. | Built. |
| `#D-t11-4` | `LOCKED_WITHOUT_A_KEY_REFUSAL`'s doc is wrong about *why* M+N is preserved: the required property is a literal `ObjectProperty::Portable` inside `open_or_close`. **The day a second thing becomes portable, every portable object opens every lock.** | Doc built; hazard carried into Task 12. |
| `#D-t12-1` | **Add one appended pattern placing a key in a role a strongbox never occupies**, with a census re-run and a reachability-sweep re-run. | **RULED, LEDGERED, NEVER DISPATCHED.** Built only in Task 13's fix round, after a reviewer found the coupled defect. See the retrospective §1. |
| `#D-t12-2` | **Nathan.** Closing is not locking: closed and locked are separate states; `close` shuts and never locks; a seeded strongbox still starts locked; "in the lock" is a location distinct from "in the container", constraining a future `lock` verb. | Built; decision 0399. |
| `#D-t13-1` | Do **not** unblock `NounEntry.affordances` by making the chamber band build `Noun`s — new capability at campaign end, in the surface with committed client fixtures, with no review budget. Record it as the named prerequisite instead. | Deliberately not built; the prerequisite is recorded here and in the retrospective. |

**Two of these were "carried to `followups.md`", which is per-worktree
scratch that dies with the campaign.** `#D-t4-2`'s tripwire and `#D-t13-1`'s
prerequisite are therefore written out above; that is their durable home.

### 9.2 The pre-dispatch brief findings

Twenty-six numbered findings (`F1`–`F26`) over Tasks 5–13, plus unnumbered
verifications on Tasks 1–4. **Every one originated in controller prose.** The
ones that are durable knowledge rather than campaign trivia:

- **F1** — a false binary in a branch table where the wrong half was the
  expensive one. An out-of-session reader registers the predicate into its own
  registry idempotently, so the keystone golden did not need to move again.
- **F3** — the plan cited `last_fact_day_at_or_before`, which is **private**.
  It can only be a discipline to copy, never a function to call. (Its
  unstated tie-break, recovered here: two facts at one instant resolve by
  commit order, last posting wins.)
- **F4** — **a signature specified with no possible caller.** The brief's rule
  was a conjunction and `is_latent`'s signature could implement only its
  second half: no `Terrain`, no seed, no world.
- **F5** — a file in `tests/suite/` with no `mod` line **compiles to nothing
  and reds nothing.**
- **F6** — a second on-disk room encoding would be self-consistent,
  round-trip through its own decoder, and red nothing, while the two
  predicates quietly stopped describing the same place.
- **F7** — **a task that moves the keystone byte-golden with a step that
  cannot see it.** The golden's guard is not in the sub-floor roster and
  `cli/tests/fixtures/` is deliberately not in `docs/generated-paths.txt`:
  green gate, clean drift check, stale golden.
- **F8** — two `KindId`-keyed property tables would have disagreed.
  Resolved by **deleting the unread field**; a field with no consumer is
  invisible until a second one arrives.
- **F9** — `BarrierState`'s derived `Ord` is load-bearing for pinned goldens.
  The change is to the fold, never the enum.
- **F10** — a brief instructing someone to extend a check that turned out to
  be **prose rather than code**. (The third time in this campaign.)
- **F11** — **the plan's warning about a stale citation was itself a stale
  citation.** Give the implementer the shape, not the line.
- **F12** — the cheapest repair (deleting a name from the frozen roster) is
  the wrong one; that is the tripwire working.
- **F14** — "adding a field breaks every full-literal caller" **predicted an
  empty compiler list**: exactly one full-literal `Noun { … }` exists, inside
  `Noun::new` itself.
- **F15** — the real blast radius was `PartialEq`, which the plan never
  mentioned.
- **F16** — "`entity` is not serialized" was true but not via `serde(skip)`:
  `Noun` derives no `Serialize` at all.
- **F18** — exclusion figures **3.5× stale** (13.4 s / 12.5 s written; 3.753 s
  / 3.772 s measured). The conclusion survived, but a wrong number attached to
  good advice invites a reader to discount both.
- **F20** — attribution ambiguity between two campaigns' 60-combination
  censuses. Remedy: **re-run it**, do not adjudicate whose it was.
- **F21** — the safety net catches **one of two** wrong choices.
  `session_control_is_never_an_in_character_verb` asserts only one direction,
  so an act wrongly declared control is silently accepted and **bypasses the
  body-state gate — a sleeping body could run it.**
- **F24** — Task 11 armed a tripwire Task 12 could set off, and **its cheapest
  repair is the wrong one**: the repair is a design act, never deleting the
  assertion.
- **F25** — a STOP row firing correctly. The chamber band never builds
  `Noun`s, corroborated three ways, so the field would have been empty for
  every entry.
- **F26** — the rule was misattributed to The Quire's spec (which never
  mentions inventory); it lives in a client module doc. Smaller obstacle than
  the plan implied, but still a decision plus a doc correction.

### 9.3 Measurements this campaign owns

- **The gate census (Task 1, §3.2).** All 60 production combinations —
  `selection(built, cold)` ×4 plus `selection_for(role, built, cold,
  populous)` over 7 roles × 2×2×2. **Zero duplicate anchor kinds in every
  row**, which is what licenses `ordinal: 0`. Held permanently by
  `interior::pattern::tests::no_production_room_composes_two_anchors_of_one_kind`.
  The harness itself was scratch and was deleted, so these numbers are the
  surviving record — see the retrospective §4.2.
- **Slot count (Task 1, §3.4).** Anchors per composed interior: min 2, median
  2.5, max 7. Bounded by slot count, not world history.
- **Read cost (Task 1, §3.4).** `Ledger::latest_value_of` ≈ **90 ns/read**
  (89.88 and 91.97 ns over two release runs, 200,000 iterations) against the
  busiest single-entity posting list. Played ledger: **22,880 facts**. This
  corrected *neither* of decision 0366's adjectives — never "a scan", never
  "free".
- **Reachability (Task 11 / decision 0398).** 48-seed sweep through the
  shipped CLI: `with_strongbox=8 with_key=8 played_LOCKED=8`, seeds 1, 4, 8,
  13, 14, 17, 23, 34; `got_indoors=48` in both arms. Flipping the two literals
  back gives 0.
- **Latency, played (Task 14).** Seed 1's structure offers **16 latent slots
  across 4 rooms**; a play driving every promoting verb at every noun promotes
  **3** — the door key, the chest, and the chest's key.

### 9.4 Acceptance, as measured

§6's eight criteria, recorded here because nothing else records them:

1. **A thing taken in one room and used in another, carried by `possess
   --out`** — MET, driven end to end including a JSON round trip.
2. **A key opens a lockable strongbox and the same body without it cannot** —
   MET.
3. **A container opens, closes and re-opens through the passage's fold** —
   MET; and 0399 split "closed" from "locked", which the criterion had
   conflated.
4. **The knowledge gate DENIES something** — MET, and this is 0369's
   struck-through criterion 5 discharged (decision 0397). **With a residual,
   named rather than closed:** it cannot deny through a *live* `Session`,
   because knowledge absorption is unconditional. That half needs a different
   lever.
5. **A promoted thing carried away is not re-offered where it came from** —
   MET.
6. **Every new verb has a roster entry, a HELP line and a body-state refusal
   test** — MET; `IN_CHARACTER_VERBS` 24 → 28.
7. **No verb×object table, held two-way by source scan** — MET, and the scan
   was widened twice under `#D-t7-1` when a key-agnostic form was found.
8. **Every regression test names its mutation and pastes the red** — MET, with
   the campaign's own recurring hazard attached: **a pasted red rots.** Line
   numbers in pasted mutation output went stale in Tasks 2, 3, 7 and 9. A
   pasted run is a record of a moment; rewriting it would make it a claim
   about now.

### 9.5 Still open at the close

1. `subfloor_roster_coverage.rs`'s prose is silent about the fact that not
   declaring a crate reds `gate-commit` locally until a chamber run — and the
   **third route** this campaign found out of that dilemma (hand-add the rows
   the chamber will later reproduce exactly, *and* delete the declaration) is
   written down nowhere else.
2. `register_concepts`'s idempotency branch is a no-op rather than re-calling
   `register_manifest`.
3. `BORROWED`'s owner→crate-name mapping relies on the `hornvale-<domain>`
   convention through `format!` rather than a typed link.
4. Five panic sites now sit in `register_concepts` / `concept_doc` — a
   panic-density question the implementer raised itself.
5. A report quoting "N/N sub-floor tests pass" is quoting **one chunk's**
   summary from `subfloor-run-chunked.sh`, not the tier's ~3,568 entries.
6. `PLAY-closed-container-conceals-nothing` — the room's prose renders from the
   grammar with no ledger and no latency filter, in **both** directions: it
   omits what the ledger holds (a dropped key) and asserts what the ledger
   denies (a taken key still listed, a shut chest still listing its contents).
